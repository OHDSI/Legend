/************************************************************************
Copyright 2018 Observational Health Data Sciences and Informatics

This file is part of Legend

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
************************************************************************/
{DEFAULT @cdm_database_schema = 'cdm.dbo' } 
{DEFAULT @eoi_table = '#eoi'}
{DEFAULT @drug_ancestor_table = '#drug_ancestor'}
{DEFAULT @min_frequency = 11}

-- Find most frequent lengh of exposure (>0) per drug concept ID
SELECT days_exposed,
	drug_concept_id
INTO #most_frequent_length
FROM (
	SELECT days_exposed,
		drug_concept_id,
		ROW_NUMBER() OVER (
			PARTITION BY drug_concept_id ORDER BY frequency DESC
			) AS rn
	FROM (
		SELECT days_exposed,
			drug_concept_id,
			COUNT(*) AS frequency
		FROM (
			SELECT DATEDIFF(DAY, drug_exposure_start_date, drug_exposure_end_date) days_exposed,
				drug_concept_id
			FROM @cdm_database_schema.drug_exposure
			INNER JOIN @cdm_database_schema.concept_ancestor
				ON drug_concept_id = descendant_concept_id
			WHERE (
					ancestor_concept_id IN (
						SELECT cohort_id
						FROM @eoi_table
						)
					OR ancestor_concept_id IN (
						SELECT descendant_concept_id
						FROM @drug_ancestor_table
						)
					)
				AND drug_exposure_start_date != drug_exposure_end_date
			) days_per_exposure
		GROUP BY days_exposed,
			drug_concept_id
		HAVING COUNT(*) >= @min_frequency
		) aggregated_frequencies
	) ordered_frequencies
WHERE rn = 1;

-- Create drug exposure table with imputed values
--HINT DISTRIBUTE_ON_KEY(person_id)
SELECT person_id,
	ancestor_concept_id AS drug_concept_id,
	drug_exposure_start_date,
	CASE 
		WHEN drug_exposure_start_date = drug_exposure_end_date
			THEN CASE 
					WHEN most_frequent_length.days_exposed IS NULL
						THEN drug_exposure_end_date
					ELSE DATEADD(DAY, most_frequent_length.days_exposed, drug_exposure_end_date)
					END
		ELSE drug_exposure_end_date
		END AS drug_exposure_end_date,
		CAST(0 AS INT) AS days_supply
INTO #drug_exposure
FROM @cdm_database_schema.drug_exposure
INNER JOIN @cdm_database_schema.concept_ancestor
	ON drug_concept_id = descendant_concept_id
LEFT JOIN #most_frequent_length most_frequent_length
	ON drug_exposure.drug_concept_id = most_frequent_length.drug_concept_id
WHERE ancestor_concept_id IN (
		SELECT cohort_id
		FROM @eoi_table
		)
	OR ancestor_concept_id IN (
		SELECT descendant_concept_id
		FROM @drug_ancestor_table
		);
