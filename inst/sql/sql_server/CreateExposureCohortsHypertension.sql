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
{DEFAULT @exposure_cohort_table = 'exposure'}
{DEFAULT @attrition_table = 'attrition'}
{DEFAULT @exposure_combi_table = '#exposure_combi'}
{DEFAULT @drug_ancestor_table = '#drug_ancestor'}
{DEFAULT @eoi_table = '#eoi'}
{DEFAULT @impute_exposure_length_when_missing = FALSE}
{DEFAULT @washout_period = 365} 
{DEFAULT @max_gap = 30} 

-- Find drugs and drug classes of interest. Store them in #exposure
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
{@impute_exposure_length_when_missing} ? {
	FROM #drug_exposure
} : {
	FROM @cdm_database_schema.drug_exposure
}
	INNER JOIN @cdm_database_schema.concept_ancestor
		ON drug_concept_id = descendant_concept_id
	INNER JOIN @eoi_table eoi
		ON ancestor_concept_id = eoi.cohort_id
	WHERE eoi.exposure_type = 'Drug'
	
	UNION ALL
	
	SELECT person_id,
		drug_ancestor.ancestor_concept_id AS concept_id,
		drug_exposure_start_date AS exposure_start_date,
		CASE 
			WHEN drug_exposure_end_date IS NULL
				THEN DATEADD(DAY, days_supply, drug_exposure_start_date)
			ELSE drug_exposure_end_date
			END AS exposure_end_date
{@impute_exposure_length_when_missing} ? {
	FROM #drug_exposure
} : {
	FROM @cdm_database_schema.drug_exposure
}
	INNER JOIN @cdm_database_schema.concept_ancestor
		ON drug_concept_id = concept_ancestor.descendant_concept_id
	INNER JOIN  @drug_ancestor_table drug_ancestor
		ON concept_ancestor.ancestor_concept_id = drug_ancestor.descendant_concept_id

) all_exposures
WHERE exposure_end_date >= exposure_start_date;

-- Create eras of continuous exposure. Store them in #exposure_era
IF OBJECT_ID('tempdb..#exposure_era', 'U') IS NOT NULL
	DROP TABLE #exposure_era;

--HINT DISTRIBUTE_ON_KEY(subject_id)	
SELECT ends.person_id AS subject_id,
	ends.concept_id AS cohort_definition_id,
	MIN(exposure_start_date) AS cohort_start_date,
	ends.era_end_date AS cohort_end_date
INTO #exposure_era
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

-- Create mono therapy first-line first use cohort. Store in #mono_new_user
IF OBJECT_ID('tempdb..#mono_new_user', 'U') IS NOT NULL
	DROP TABLE #mono_new_user;

--HINT DISTRIBUTE_ON_KEY(subject_id)
SELECT first_line.subject_id,
	first_line.cohort_definition_id,
	first_line.cohort_start_date,
	first_line.cohort_end_date
INTO #mono_new_user
FROM (
	SELECT first_use.subject_id,
		first_use.cohort_definition_id,
		first_use.cohort_start_date,
		first_use.cohort_end_date
	FROM (
		-- First use of drug or class:
		SELECT subject_id,
			cohort_definition_id,
			MIN(cohort_start_date) AS cohort_start_date,
			MIN(cohort_end_date) AS cohort_end_date
		FROM #exposure_era
		GROUP BY subject_id,
			cohort_definition_id
		) first_use
	-- Require washout period
	INNER JOIN @cdm_database_schema.observation_period
		ON observation_period.person_id = first_use.subject_id
		AND DATEADD(DAY, @washout_period, observation_period_start_date) <= first_use.cohort_start_date
		AND observation_period_end_date >= first_use.cohort_start_date
	-- First line: no prior HT exposure:
	LEFT JOIN #exposure_era era
		ON era.subject_id = first_use.subject_id
		AND era.cohort_start_date < first_use.cohort_start_date
	WHERE era.subject_id IS NULL
	)  first_line
-- Mono therapy: only 1 drug within 7 day period after initiation:
INNER JOIN #exposure_era concurrent
	ON concurrent.subject_id = first_line.subject_id
	AND concurrent.cohort_start_date >= first_line.cohort_start_date
	AND concurrent.cohort_start_date <= DATEADD(DAY, 7, first_line.cohort_start_date)
INNER JOIN @eoi_table eoi
	ON concurrent.cohort_definition_id = eoi.cohort_id
WHERE eoi.exposure_type = 'Drug'
GROUP BY first_line.subject_id,
	first_line.cohort_definition_id,
	first_line.cohort_start_date,
	first_line.cohort_end_date
HAVING COUNT(DISTINCT concurrent.cohort_definition_id) = 1;
	

-- Create duo therapy first-line first use cohort. Store in #duo_new_user
IF OBJECT_ID('tempdb..#duo_new_user', 'U') IS NOT NULL
	DROP TABLE #duo_new_user;

--HINT DISTRIBUTE_ON_KEY(subject_id)
SELECT first_line.subject_id,
	first_line.cohort_definition_id,
	first_line.cohort_start_date,
	first_line.cohort_end_date
INTO #duo_new_user
FROM (
	SELECT first_use.subject_id,
		first_use.cohort_definition_id,
		first_use.cohort_start_date,
		first_use.cohort_end_date
	FROM (
		-- First use of drug combination or class combination:
		SELECT first_use_1.subject_id,
			exposure_combi.cohort_definition_id,
			CASE 
				WHEN first_use_1.cohort_start_date < first_use_2.cohort_start_date THEN first_use_1.cohort_start_date
				ELSE first_use_2.cohort_start_date
			END AS cohort_start_date,
			CASE 
				WHEN first_use_1.cohort_end_date < first_use_2.cohort_end_date THEN first_use_1.cohort_end_date
				ELSE first_use_2.cohort_end_date
			END AS cohort_end_date
		FROM (
			SELECT subject_id,
				cohort_definition_id,
				MIN(cohort_start_date) AS cohort_start_date,
				MIN(cohort_end_date) AS cohort_end_date
			FROM #exposure_era exposure_1
			GROUP BY subject_id,
				cohort_definition_id
			) first_use_1
		INNER JOIN (
			SELECT subject_id,
				cohort_definition_id,
				MIN(cohort_start_date) AS cohort_start_date,
				MIN(cohort_end_date) AS cohort_end_date
			FROM #exposure_era exposure_1
			GROUP BY subject_id,
				cohort_definition_id
			) first_use_2
			ON first_use_1.subject_id = first_use_2.subject_id
			AND ABS(DATEDIFF(DAY, first_use_1.cohort_start_date, first_use_2.cohort_start_date)) <= 7
		INNER JOIN #exposure_combi exposure_combi
			ON exposure_combi.exposure_id_1 = first_use_1.cohort_definition_id
			AND exposure_combi.exposure_id_2 = first_use_2.cohort_definition_id
		
		) first_use
	-- Require washout period
	INNER JOIN @cdm_database_schema.observation_period
		ON observation_period.person_id = first_use.subject_id
		AND DATEADD(DAY, @washout_period, observation_period_start_date) <= first_use.cohort_start_date
		AND observation_period_end_date >= first_use.cohort_start_date
	-- First line: no prior HT exposure:
	LEFT JOIN #exposure_era era
		ON era.subject_id = first_use.subject_id
		AND era.cohort_start_date < first_use.cohort_start_date
	WHERE era.subject_id IS NULL
	)  first_line
-- Duo therapy: only 2 drugs within 7 day period after initiation:
INNER JOIN #exposure_era concurrent
	ON concurrent.subject_id = first_line.subject_id
	AND concurrent.cohort_start_date >= first_line.cohort_start_date
	AND concurrent.cohort_start_date <= DATEADD(DAY, 7, first_line.cohort_start_date)
INNER JOIN @eoi_table eoi
	ON concurrent.cohort_definition_id = eoi.cohort_id
WHERE eoi.exposure_type = 'Drug'
GROUP BY first_line.subject_id,
	first_line.cohort_definition_id,
	first_line.cohort_start_date,
	first_line.cohort_end_date
HAVING COUNT(DISTINCT concurrent.cohort_definition_id) = 2;


-- Merge mono and duo therapy. Require hypertension diagnose. Store in @cohort_database_schema.@exposure_cohort_table
IF OBJECT_ID('@cohort_database_schema.@exposure_cohort_table', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.@exposure_cohort_table;

--HINT DISTRIBUTE_ON_KEY(subject_id)
SELECT DISTINCT new_user.subject_id,
	new_user.cohort_definition_id,
	new_user.cohort_start_date,
	new_user.cohort_end_date
INTO @cohort_database_schema.@exposure_cohort_table
FROM (
	SELECT subject_id,
		cohort_definition_id,
		cohort_start_date,
		cohort_end_date
	FROM #mono_new_user
	
	UNION ALL
	
	SELECT subject_id,
		cohort_definition_id,
		cohort_start_date,
		cohort_end_date
	FROM #duo_new_user
	) new_user
INNER JOIN @cdm_database_schema.condition_occurrence
	ON condition_occurrence.person_id = new_user.subject_id
	AND condition_start_date <= new_user.cohort_start_date
	AND condition_start_date >= DATEADD(DAY, -365, new_user.cohort_start_date)
WHERE condition_concept_id IN (
	SELECT descendant_concept_id
		FROM @cdm_database_schema.concept_ancestor
		WHERE ancestor_concept_id IN (316866) -- Hypertensive disorder
			);

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
	FROM #exposure_era
	GROUP BY cohort_definition_id
	
	UNION ALL
	
	SELECT cohort_definition_id AS exposure_id,
		CAST(2 AS INT) AS sequence_number,
		CAST('Mono-therapy new users' AS VARCHAR(255)) AS description,
		COUNT(DISTINCT subject_id) AS subjects
	FROM #mono_new_user
	GROUP BY cohort_definition_id
	
	UNION ALL
	
	SELECT cohort_definition_id AS exposure_id,
		CAST(2 AS INT) AS sequence_number,
		CAST('Duo-therapy new users' AS VARCHAR(255)) AS description,
		COUNT(DISTINCT subject_id) AS subjects
	FROM #duo_new_user
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

TRUNCATE TABLE #exposure_era;
DROP TABLE #exposure_era;

TRUNCATE TABLE #mono_new_user;
DROP TABLE #mono_new_user;

TRUNCATE TABLE #duo_new_user;
DROP TABLE #duo_new_user;

{@impute_exposure_length_when_missing} ? {
TRUNCATE TABLE #drug_exposure;
DROP TABLE #drug_exposure;
}
