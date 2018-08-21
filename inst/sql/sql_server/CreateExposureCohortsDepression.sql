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
{DEFAULT @drug_cohort_ids = 1, 2}
{DEFAULT @procedure_cohort_ids = 3, 4}
{DEFAULT @drug_class_cohort_ids = 5, 6}
{DEFAULT @custom_ancestor_table = '#custom_ancestor'}
{DEFAULT @procedure_duration = 30}
{DEFAULT @washout_period = 365} 
{DEFAULT @max_gap = 30} 

-- Create nesting cohort. Store in #nesting_cohort
SELECT depression.person_id AS subject_id,
	depression_start_date AS cohort_start_date,
	CASE 
		WHEN other_start_date IS NULL
			THEN observation_period_end_date
		ELSE other_start_date
		END AS cohort_end_date
INTO #nesting_cohort
FROM (
	SELECT person_id,
		MIN(condition_start_date) AS depression_start_date
	FROM @cdm_database_schema.condition_occurrence
	WHERE condition_concept_id IN (
			SELECT descendant_concept_id
			FROM @cdm_database_schema.concept_ancestor
			WHERE ancestor_concept_id IN (4152280) -- Major depressive disorder
			)
	GROUP BY person_id
	) depression
INNER JOIN @cdm_database_schema.observation_period
	ON observation_period.person_id = depression.person_id
		AND observation_period_start_date <= depression_start_date
		AND observation_period_end_date >= depression_start_date
LEFT JOIN (
	SELECT person_id,
		MIN(condition_start_date) AS other_start_date
	FROM @cdm_database_schema.condition_occurrence
	WHERE condition_concept_id IN (
			SELECT descendant_concept_id
			FROM @cdm_database_schema.concept_ancestor
			WHERE ancestor_concept_id IN (
					435783,
					436665
					) -- Schizophrenia, Bipolar disorder
			)
	GROUP BY person_id
	) other
	ON depression.person_id = other.person_id
		AND observation_period_start_date <= other_start_date
		AND observation_period_end_date >= other_start_date
WHERE other_start_date IS NULL
	OR depression_start_date < other_start_date;

-- Find drugs, procedures , and classes of interest. Store them in #exposure
IF OBJECT_ID('tempdb..#exposure', 'U') IS NOT NULL
	DROP TABLE #exposure;	
	
--HINT DISTRIBUTE_ON_KEY(person_id)
SELECT person_id,
	concept_id,
	exposure_start_date,
	exposure_end_date
INTO #exposure	
FROM (
	-- drugs
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
	WHERE ancestor_concept_id IN (@drug_cohort_ids)
	
	UNION ALL
	
	-- procedures (assume 30 day exposure)
	SELECT person_id,
		ancestor_concept_id AS concept_id,
		procedure_date AS exposure_start_date,
		DATEADD(DAY, @procedure_duration, procedure_date) AS exposure_end_date
	FROM @cdm_database_schema.procedure_occurrence
	INNER JOIN @custom_ancestor_table
		ON procedure_concept_id = descendant_concept_id
		OR procedure_source_concept_id = descendant_concept_id
	WHERE ancestor_concept_id IN (@procedure_cohort_ids)
		
	UNION ALL
	
	-- drug classes
	SELECT person_id,
		custom_ancestor.ancestor_concept_id AS concept_id,
		drug_exposure_start_date AS exposure_start_date,
		CASE 
			WHEN drug_exposure_end_date IS NULL
				THEN DATEADD(DAY, days_supply, drug_exposure_start_date)
			ELSE drug_exposure_end_date
			END AS exposure_end_date
	FROM @cdm_database_schema.drug_exposure
	INNER JOIN @cdm_database_schema.concept_ancestor
		ON drug_concept_id = concept_ancestor.descendant_concept_id
	INNER JOIN  @custom_ancestor_table custom_ancestor
		ON concept_ancestor.ancestor_concept_id = custom_ancestor.descendant_concept_id
	WHERE custom_ancestor.ancestor_concept_id IN (@drug_class_cohort_ids)
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
INNER JOIN #nesting_cohort nesting_cohort
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
	
CREATE TABLE @cohort_database_schema.@attrition_table (
		exposure_id BIGINT,
		target_id BIGINT,
		comparator_id BIGINT,
		sequence_number INT,
		description VARCHAR(255),
		subjects INT
);
	
INSERT INTO @cohort_database_schema.@attrition_table (
	exposure_id,
	target_id,
	comparator_id,
	sequence_number,
	description,
	subjects)
SELECT exposure_id,
	CAST(-1 AS BIGINT) AS target_id,
	CAST(-1 AS BIGINT) AS comparator_id,
	sequence_number,
	description,
	subjects
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