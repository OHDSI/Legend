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
{DEFAULT @cdm_database_schema = 'cdm.dbo'}
{DEFAULT @cohort_database_schema = 'scratch'}
{DEFAULT @exposure_era_table = 'exposure_era'}
{DEFAULT @exposure_cohort_table = 'exposure'}
{DEFAULT @exposure_combi_table = '#exposure_combi'}
{DEFAULT @drug_concept_ids = 123, 456}
{DEFAULT @include_procedures = FALSE}
{DEFAULT @procedure_ancestor_table = '#procedure_ancestor'}
{DEFAULT @procedure_duration = 30}
{DEFAULT @washout_period = 365} 
{DEFAULT @max_gap = 30} 

{DEFAULT @max_date_difference = 0}

-- Find drugs (and procedures) of interest. Store them in #exposures table
IF OBJECT_ID('tempdb..#exposures', 'U') IS NOT NULL
	DROP TABLE #exposures;	
	
--HINT DISTRIBUTE_ON_KEY(person_id)
SELECT person_id,
	concept_id,
	exposure_start_date,
	exposure_end_date
INTO #exposures	
FROM (
	SELECT person_id,
		ancestor_concept_id AS concept_id,
		drug_exposure_start_date AS exposure_start_date,
		CASE 
			WHEN drug_exposure_end_date IS NULL
				THEN DATEADD(DAY, days_supply, drug_exposure_start_date)
			ELSE drug_exposure_end_date
			END AS exposure_end_date

	FROM @cdm_database_schema.drug_exposure
	INNER JOIN @cdm_database_schema.concept_ancestor
		ON drug_concept_id = descendant_concept_id
	WHERE ancestor_concept_id IN (@drug_concept_ids)
	
{@include_procedures} ? {	
	UNION ALL
	
	SELECT person_id,
		ancestor_concept_id AS concept_id,
		procedure_date AS exposure_start_date,
		DATEADD(DAY, @procedure_duration, procedure_date) AS exposure_end_date
	FROM @cdm_database_schema.procedure_occurrence
	INNER JOIN @procedure_ancestor_table
		ON procedure_concept_id = descendant_concept_id
		OR procedure_source_concept_id = descendant_concept_id
}

) all_exposures;

-- Create eras of continuous exposure. Store them in @cohort_database_schema.@exposure_era_table
IF OBJECT_ID('@cohort_database_schema.@exposure_era_table', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.@exposure_era_table;

--HINT DISTRIBUTE_ON_KEY(subject_id)	
SELECT ends.person_id AS subject_id,
	ends.concept_id AS cohort_definition_id,
	MIN(exposure_start_date) AS cohort_start_date,
	ends.era_end_date AS cohort_end_date
INTO @cohort_database_schema.@exposure_era_table
FROM (
	SELECT exposures.person_id,
		exposures.concept_id,
		exposures.exposure_start_date,
		MIN(events.end_date) AS era_end_date
	FROM #exposures exposures
	JOIN (
		--cteEndDates
		SELECT person_id,
		    concept_id,
			DATEADD(day, - 1 * @max_gap, event_date) AS end_date -- unpad the end date by @max_gap
		FROM (
			SELECT person_id,
				concept_id,
				event_date,
				event_type,
				MAX(start_ordinal) OVER (
					PARTITION BY person_id ORDER BY event_date,
						event_type ROWS UNBOUNDED PRECEDING
					) AS start_ordinal,
				ROW_NUMBER() OVER (
					PARTITION BY person_id ORDER BY event_date,
						event_type
					) AS overall_ord -- this re-numbers the inner UNION so all rows are numbered ordered by the event date
			FROM (
				-- select the start dates, assigning a row number to each
				SELECT person_id,
					concept_id,
					exposure_start_date AS event_date,
					0 AS event_type,
					ROW_NUMBER() OVER (
						PARTITION BY person_id ORDER BY exposure_start_date
						) AS start_ordinal
				FROM #exposures
				
				UNION ALL
				
				-- add the end dates with NULL as the row number, padding the end dates by @max_gap to allow a grace period for overlapping ranges.
				SELECT person_id,
					concept_id,
					DATEADD(day, @max_gap, exposure_end_date),
					1 AS event_type,
					NULL
				FROM #exposures
				) rawdata
			) events
		WHERE 2 * events.start_ordinal - events.overall_ord = 0
		) events
		ON exposures.person_id = events.person_id
			AND exposures.concept_id = events.concept_id
			AND events.end_date >= exposures.exposure_end_date
	GROUP BY exposures.person_id,
		exposures.concept_id,
		exposures.exposure_start_date
	) ends
GROUP BY ends.person_id,
	concept_id,
	ends.era_end_date;

-- Create new-user cohorts of single exposures. Store them in #new_users_single
IF OBJECT_ID('tempdb..#new_users_single', 'U') IS NOT NULL
	DROP TABLE #new_users_single;

--HINT DISTRIBUTE_ON_KEY(subject_id)
SELECT first_exposure.subject_id,
	first_exposure.cohort_definition_id,
	first_exposure.cohort_start_date,
	first_exposure.cohort_end_date
INTO #new_users_single
FROM (
	SELECT subject_id,
		cohort_definition_id,
		MIN(cohort_start_date) AS cohort_start_date,
		MIN(cohort_end_date) AS cohort_end_date
	FROM @cohort_database_schema.@exposure_era_table
	GROUP BY subject_id,
		cohort_definition_id
	) first_exposure
INNER JOIN @nesting_cohort_table nesting_cohort
	ON nesting_cohort.subject_id = first_exposure.subject_id
		AND nesting_cohort.cohort_start_date <= first_exposure.cohort_start_date
		AND nesting_cohort.cohort_end_date >= first_exposure.cohort_start_date
INNER JOIN @cdm_database_schema.observation_period
	ON observation_period.person_id = first_exposure.subject_id
		AND DATEADD(DAY, @washout_period, observation_period.observation_period_start_date) <= first_exposure.cohort_start_date
		AND observation_period.observation_period_end_date >= first_exposure.cohort_start_date;

-- Create new-user cohorts of exposure combinations. Store them in #new_users_combi
IF OBJECT_ID('tempdb..#new_users_combi', 'U') IS NOT NULL
	DROP TABLE #new_users_combi;

--HINT DISTRIBUTE_ON_KEY(subject_id)
SELECT first_combi.subject_id,
	first_combi.cohort_definition_id,
	first_combi.cohort_start_date,
	CASE 
		WHEN exposure_era_1.cohort_end_date < exposure_era_2.cohort_end_date
			THEN exposure_era_1.cohort_end_date
		ELSE exposure_era_2.cohort_end_date
		END AS cohort_end_date
INTO #new_users_combi
FROM (
	SELECT subject_id,
		cohort_definition_id,
		exposure_id_1,
		exposure_id_2,
		MIN(cohort_start_date) AS cohort_start_date
	FROM (
		SELECT exposure_1.person_id AS subject_id,
			exposure_combi.cohort_definition_id,
			exposure_combi.exposure_id_1,
			exposure_combi.exposure_id_2,
			CASE 
				WHEN exposure_1.exposure_start_date > exposure_2.exposure_start_date
					THEN exposure_1.exposure_start_date
				ELSE exposure_2.exposure_start_date
				END AS cohort_start_date
		FROM @exposure_combi_table exposure_combi
		INNER JOIN #exposures exposure_1
			ON exposure_1.concept_id = exposure_combi.exposure_id_1
		INNER JOIN #exposures exposure_2
			ON exposure_2.concept_id = exposure_combi.exposure_id_2
				AND exposure_1.person_id = exposure_2.person_id
				AND ABS(DATEDIFF(DAY, exposure_1.exposure_start_date, exposure_2.exposure_start_date)) <= @max_date_difference
		) combi_prescribing
	GROUP BY subject_id,
		cohort_definition_id,
		exposure_id_1,
		exposure_id_2
	) first_combi
INNER JOIN @cohort_database_schema.@exposure_era_table exposure_era_1
	ON first_combi.subject_id = exposure_era_1.subject_id
		AND first_combi.exposure_id_1 = exposure_era_1.cohort_definition_id
		AND first_combi.cohort_start_date >= exposure_era_1.cohort_start_date
		AND first_combi.cohort_start_date < exposure_era_1.cohort_end_date
INNER JOIN @cohort_database_schema.@exposure_era_table exposure_era_2
	ON first_combi.subject_id = exposure_era_2.subject_id
		AND first_combi.exposure_id_2 = exposure_era_2.cohort_definition_id
		AND first_combi.cohort_start_date >= exposure_era_2.cohort_start_date
		AND first_combi.cohort_start_date < exposure_era_2.cohort_end_date;

		
-- Merge single and combi exposures into single table. Store them in @exposure_cohort_table
IF OBJECT_ID('@cohort_database_schema.@exposure_cohort_table', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.@exposure_cohort_table;
	
--HINT DISTRIBUTE_ON_KEY(subject_id)
SELECT subject_id,
		cohort_definition_id,
		cohort_start_date,
		cohort_end_date
INTO @cohort_database_schema.@exposure_cohort_table
FROM (
	SELECT subject_id,
		cohort_definition_id,
		cohort_start_date,
		cohort_end_date
	FROM #new_users_single
	
	UNION ALL
	
	SELECT subject_id,
		cohort_definition_id,
		cohort_start_date,
		cohort_end_date
	FROM #new_users_combi

) temp;

-- Cleanup
TRUNCATE TABLE #exposures;
DROP TABLE #exposures;

TRUNCATE TABLE #new_users_single;
DROP TABLE #new_users_single;

TRUNCATE TABLE #new_users_combi;
DROP TABLE #new_users_combi;