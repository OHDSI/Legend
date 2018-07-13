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
{DEFAULT @cohort_database_schema = 'scratch.dbo'}
{DEFAULT @exposure_cohort_table = 'cohort'}
{DEFAULT @paired_cohort_table = 'cohort'}
{DEFAULT @paired_cohort_summary_table = 'exposure_cohort_summary'}
{DEFAULT @attrition_table = 'attrition'}

IF OBJECT_ID('tempdb..#ec_summary', 'U') IS NOT NULL
	DROP TABLE #ec_summary;

-- Summarize exposure cohorts. Store in #ec_summary
SELECT cohort_definition_id,
	COUNT(subject_id) AS num_persons,
	MIN(cohort_start_date) AS min_cohort_date,
	MAX(cohort_start_date) AS max_cohort_date
INTO #ec_summary
FROM @cohort_database_schema.@exposure_cohort_table tec1
GROUP BY cohort_definition_id;

-- Enumerate all cohort pairs. Store in #ec_pairs
IF OBJECT_ID('tempdb..#ec_pairs', 'U') IS NOT NULL
	DROP TABLE #ec_pairs;

SELECT target_id,
	comparator_id,
	min_cohort_date,
	max_cohort_date
INTO #ec_pairs
FROM (
	SELECT s1.cohort_definition_id AS target_id,
		s2.cohort_definition_id AS comparator_id,
		CASE 
			WHEN s1.min_cohort_date > s2.min_cohort_date
				THEN s1.min_cohort_date
			ELSE s2.min_cohort_date
			END AS min_cohort_date,
		CASE 
			WHEN s1.max_cohort_date < s2.max_cohort_date
				THEN s1.max_cohort_date
			ELSE s2.max_cohort_date
			END AS max_cohort_date
	FROM #ec_summary s1,
		#ec_summary s2
	WHERE s1.cohort_definition_id < s2.cohort_definition_id
	) t1;

	
-- Construct all cohorts as pairs. Store in @cohort_database_schema.@paired_cohort_table
IF OBJECT_ID('@cohort_database_schema.@paired_cohort_table', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.@paired_cohort_table;
	
	
--HINT DISTRIBUTE_ON_KEY(subject_id)
SELECT cohort_definition_id,
	target_id,
	comparator_id,
	subject_id,
	cohort_start_date,
	cohort_end_date
INTO @cohort_database_schema.@paired_cohort_table
FROM (
	-- Target filtered to common time
	SELECT cp1.target_id AS cohort_definition_id,
		cp1.target_id,
		cp1.comparator_id,
		ec1.subject_id,
		ec1.cohort_start_date,
		ec1.cohort_end_date
	FROM #ec_pairs cp1
	INNER JOIN @cohort_database_schema.@exposure_cohort_table ec1
		ON cp1.target_id = ec1.cohort_definition_id
			AND ec1.cohort_start_date >= cp1.min_cohort_date
			AND ec1.cohort_start_date <= cp1.max_cohort_date
	
	UNION ALL
	
	-- Comparator filtered to common time
	SELECT cp1.comparator_id AS cohort_definition_id,
		cp1.target_id,
		cp1.comparator_id,
		ec1.subject_id,
		ec1.cohort_start_date,
		ec1.cohort_end_date
	FROM #ec_pairs cp1
	INNER JOIN @cohort_database_schema.@exposure_cohort_table ec1
		ON cp1.comparator_id = ec1.cohort_definition_id
			AND ec1.cohort_start_date >= cp1.min_cohort_date
			AND ec1.cohort_start_date <= cp1.max_cohort_date
	) tmp;

	
-- Summarize cohort pairs
IF OBJECT_ID('tempdb..#ep_cohort_summary', 'U') IS NOT NULL
	DROP TABLE #ep_cohort_summary;

SELECT cohort_definition_id,
	target_id,
	comparator_id,
	COUNT(subject_id) AS num_persons,
	MIN(cohort_start_date) AS min_cohort_date,
	MAX(cohort_start_date) AS max_cohort_date
INTO #ep_cohort_summary
FROM @cohort_database_schema.@paired_cohort_table tec1
GROUP BY cohort_definition_id,
	target_id,
	comparator_id;

IF OBJECT_ID('@cohort_database_schema.@paired_cohort_summary_table', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.@paired_cohort_summary_table;

SELECT cp1.target_id,
	cp1.comparator_id,
	ecs1.num_persons AS target_persons,
	ecs1.min_cohort_date AS target_min_date,
	ecs1.max_cohort_date AS target_max_date,
	epcs1.num_persons AS target_paired_persons,
	ecs2.num_persons AS comparator_persons,
	ecs2.min_cohort_date AS comparator_min_date,
	ecs2.max_cohort_date AS comparator_max_date,
	epcs2.num_persons AS comparator_paired_persons,
	cp1.min_cohort_date AS paired_min_date,
	cp1.max_cohort_date AS paired_max_date
INTO @cohort_database_schema.@paired_cohort_summary_table
FROM #ec_pairs cp1
INNER JOIN #ec_summary ecs1
	ON cp1.target_id = ecs1.cohort_definition_id
INNER JOIN #ep_cohort_summary epcs1
	ON cp1.target_id = epcs1.cohort_definition_id
		AND cp1.target_id = epcs1.target_id
		AND cp1.comparator_id = epcs1.comparator_id
INNER JOIN #ec_summary ecs2
	ON cp1.comparator_id = ecs2.cohort_definition_id
INNER JOIN #ep_cohort_summary epcs2
	ON cp1.comparator_id = epcs2.cohort_definition_id
		AND cp1.target_id = epcs2.target_id
		AND cp1.comparator_id = epcs2.comparator_id;
	
	
-- Add to attrition table
INSERT INTO @cohort_database_schema.@attrition_table (
	exposure_id,
	target_id,
	comparator_id,
	sequence_number,
	description,
	subjects)
SELECT exposure_id,
	target_id,
	comparator_id,
	sequence_number,
	description,
	subjects
FROM (
	-- Restricted to common period: take final number
	SELECT epcs.cohort_definition_id AS exposure_id,
		epcs.target_id,
		epcs.comparator_id,
		CAST(4 AS INT) AS sequence_number,
		CAST('Restricted to common period' AS VARCHAR(255)) AS description,
		epcs.num_persons AS subjects
	FROM #ep_cohort_summary epcs	
) temp;
	
-- Cleanup
TRUNCATE TABLE #ec_summary;
DROP TABLE #ec_summary;

TRUNCATE TABLE #ec_pairs;
DROP TABLE #ec_pairs;

TRUNCATE TABLE #ep_cohort_summary;
DROP TABLE #ep_cohort_summary;
