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
{DEFAULT @attrition_table = 'attrition'}
{DEFAULT @exposure_combi_table = '#exposure_combi'}
{DEFAULT @drug_concept_ids = 123, 456}
{DEFAULT @include_procedures = FALSE}
{DEFAULT @procedure_ancestor_table = '#procedure_ancestor'}
{DEFAULT @procedure_duration = 30}
{DEFAULT @washout_period = 365} 
{DEFAULT @max_gap = 30} 

-- Find drugs (and procedures) of interest. Store them in #exposure
IF OBJECT_ID('tempdb..#exposure', 'U') IS NOT NULL
	DROP TABLE #exposure;	
	
--HINT DISTRIBUTE_ON_KEY(person_id)
SELECT person_id,
	concept_id,
	exposure_start_date,
	exposure_end_date
INTO #exposure	
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

-- Create eras of continuous exposure. Store them in #single_exposure_era
IF OBJECT_ID('tempdb..#single_exposure_era', 'U') IS NOT NULL
	DROP TABLE #single_exposure_era;

--HINT DISTRIBUTE_ON_KEY(subject_id)	
SELECT ends.person_id AS subject_id,
	ends.concept_id AS cohort_definition_id,
	MIN(exposure_start_date) AS cohort_start_date,
	ends.era_end_date AS cohort_end_date
INTO #single_exposure_era
FROM (
	SELECT exposure.person_id,
		exposure.concept_id,
		exposure.exposure_start_date,
		MIN(events.end_date) AS era_end_date
	FROM #exposure exposure
	JOIN (
		--cteEndDates
		SELECT person_id,
		    concept_id,
			DATEADD(DAY, - 1 * @max_gap, event_date) AS end_date -- unpad the end date by @max_gap
		FROM (
			SELECT person_id,
				concept_id,
				event_date,
				event_type,
				MAX(start_ordinal) OVER (
					PARTITION BY person_id ,concept_id ORDER BY event_date,
						event_type ROWS UNBOUNDED PRECEDING
					) AS start_ordinal,
				ROW_NUMBER() OVER (
					PARTITION BY person_id, concept_id ORDER BY event_date,
						event_type
					) AS overall_ord -- this re-numbers the inner UNION so all rows are numbered ordered by the event date
			FROM (
				-- select the start dates, assigning a row number to each
				SELECT person_id,
					concept_id,
					exposure_start_date AS event_date,
					0 AS event_type,
					ROW_NUMBER() OVER (
						PARTITION BY person_id, concept_id ORDER BY exposure_start_date
						) AS start_ordinal
				FROM #exposure
				
				UNION ALL
				
				-- add the end dates with NULL as the row number, padding the end dates by @max_gap to allow a grace period for overlapping ranges.
				SELECT person_id,
					concept_id,
					DATEADD(day, @max_gap, exposure_end_date),
					1 AS event_type,
					NULL
				FROM #exposure
				) rawdata
			) events
		WHERE 2 * events.start_ordinal - events.overall_ord = 0
		) events
		ON exposure.person_id = events.person_id
			AND exposure.concept_id = events.concept_id
			AND events.end_date >= exposure.exposure_end_date
	GROUP BY exposure.person_id,
		exposure.concept_id,
		exposure.exposure_start_date
	) ends
GROUP BY ends.person_id,
	concept_id,
	ends.era_end_date;


-- Create eras of continuous combination exposures. Store them in #combi_exposure_era
IF OBJECT_ID('tempdb..#combi_exposure_era', 'U') IS NOT NULL
	DROP TABLE #combi_exposure_era;

--HINT DISTRIBUTE_ON_KEY(subject_id)
SELECT overlapping_eras.subject_id,
	overlapping_eras.cohort_definition_id,
	overlapping_eras.cohort_start_date AS cohort_start_date,
	overlapping_eras.cohort_end_date AS cohort_end_date
INTO #combi_exposure_era
FROM (
	SELECT exposure_era_1.subject_id,
		exposure_combi.cohort_definition_id,
		CASE 
			WHEN exposure_era_1.cohort_start_date < exposure_era_2.cohort_start_date
				THEN exposure_era_1.cohort_definition_id
			ELSE exposure_era_2.cohort_definition_id
			END AS first_exposure_id,
		CASE 
			WHEN exposure_era_1.cohort_start_date = exposure_era_2.cohort_start_date
				THEN CAST(1 AS INT)
			ELSE CAST(0 AS INT)
			END AS simultaneous,
		CASE 
			WHEN exposure_era_1.cohort_start_date > exposure_era_2.cohort_start_date
				THEN exposure_era_1.cohort_start_date
			ELSE exposure_era_2.cohort_start_date
			END AS cohort_start_date,
		CASE 
			WHEN exposure_era_1.cohort_end_date < exposure_era_2.cohort_end_date
				THEN exposure_era_1.cohort_end_date
			ELSE exposure_era_2.cohort_end_date
			END AS cohort_end_date
	FROM @exposure_combi_table exposure_combi
	INNER JOIN #single_exposure_era exposure_era_1
		ON exposure_era_1.cohort_definition_id = exposure_combi.exposure_id_1
	INNER JOIN #single_exposure_era exposure_era_2
		ON exposure_era_2.cohort_definition_id = exposure_combi.exposure_id_2
			AND exposure_era_1.subject_id = exposure_era_2.subject_id
			AND exposure_era_1.cohort_end_date > exposure_era_2.cohort_start_date
			AND exposure_era_2.cohort_end_date > exposure_era_1.cohort_start_date
	) overlapping_eras
LEFT JOIN #exposure subsequent_exposure
	ON overlapping_eras.subject_id = subsequent_exposure.person_id
		AND overlapping_eras.first_exposure_id = subsequent_exposure.concept_id
		AND overlapping_eras.cohort_start_date <= subsequent_exposure.exposure_start_date
		AND overlapping_eras.cohort_end_date >= subsequent_exposure.exposure_start_date
WHERE simultaneous = 1
	OR subsequent_exposure.person_id IS NOT NULL;

-- Merge single and combi exposure eras into single table. Store them in@ cohort_database_schema.@exposure_era_table
IF OBJECT_ID('@cohort_database_schema.@exposure_era_table', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.@exposure_era_table;
	
--HINT DISTRIBUTE_ON_KEY(subject_id)
SELECT subject_id,
	cohort_definition_id,
	cohort_start_date,
	cohort_end_date
INTO @cohort_database_schema.@exposure_era_table
FROM (
	SELECT subject_id,
		cohort_definition_id,
		cohort_start_date,
		cohort_end_date
	FROM #single_exposure_era
	
	UNION ALL
	
	SELECT subject_id,
		cohort_definition_id,
		cohort_start_date,
		cohort_end_date
	FROM #combi_exposure_era

) tmp;

-- Create new-user cohorts. Store them in @cohort_database_schema.@exposure_cohort_table
IF OBJECT_ID('@cohort_database_schema.@exposure_cohort_table', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.@exposure_cohort_table;

--HINT DISTRIBUTE_ON_KEY(subject_id)
SELECT first_exposure.subject_id,
	first_exposure.cohort_definition_id,
	first_exposure.cohort_start_date,
	first_exposure.cohort_end_date
INTO @cohort_database_schema.@exposure_cohort_table
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

-- Create attrition table
IF OBJECT_ID('@cohort_database_schema.@attrition_table', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.@attrition_table;
	
SELECT exposure_id,
	CAST(-1 AS BIGINT) AS target_id,
	CAST(-1 AS BIGINT) AS comparator_id,
	sequence_number,
	description,
	subjects
INTO @cohort_database_schema.@attrition_table
FROM (
	SELECT cohort_definition_id AS exposure_id,
		CAST(1 AS INT) AS sequence_number,
		CAST('Exposed' AS VARCHAR(255)) AS description,
		COUNT(DISTINCT subject_id) AS subjects
	FROM @cohort_database_schema.@exposure_era_table
	GROUP BY cohort_definition_id
	
	UNION ALL
	
	SELECT cohort_definition_id AS exposure_id,
		CAST(2 AS INT) AS sequence_number,
		CAST('New users' AS VARCHAR(255)) AS description,
		COUNT(DISTINCT subject_id) AS subjects
	FROM (
		SELECT subject_id,
			cohort_definition_id,
			MIN(cohort_start_date) AS cohort_start_date,
			MIN(cohort_end_date) AS cohort_end_date
		FROM @cohort_database_schema.@exposure_era_table
		GROUP BY subject_id,
			cohort_definition_id
		) first_exposure
	INNER JOIN @cdm_database_schema.observation_period
		ON observation_period.person_id = first_exposure.subject_id
			AND DATEADD(DAY, @washout_period, observation_period.observation_period_start_date) <= first_exposure.cohort_start_date
			AND observation_period.observation_period_end_date >= first_exposure.cohort_start_date
	GROUP BY cohort_definition_id
	
	UNION ALL
	
	SELECT cohort_definition_id AS exposure_id,
		CAST(3 AS INT) AS sequence_number,
		CAST('Having indication' AS VARCHAR(255)) AS description,
		COUNT(DISTINCT subject_id) AS subjects
	FROM @cohort_database_schema.@exposure_cohort_table
	GROUP BY cohort_definition_id
	) TEMP;


-- Cleanup
TRUNCATE TABLE #exposure;
DROP TABLE #exposure;

TRUNCATE TABLE #single_exposure_era;
DROP TABLE #single_exposure_era;

TRUNCATE TABLE #combi_exposure_era;
DROP TABLE #combi_exposure_era;