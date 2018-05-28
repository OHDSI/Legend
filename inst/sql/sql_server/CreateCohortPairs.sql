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

SELECT pair_id,
	t_cohort_definition_id,
	c_cohort_definition_id,
	t_cohort_definition_id * 10000 + pair_id AS tprime_cohort_definition_id,
	c_cohort_definition_id * 10000 + pair_id AS cprime_cohort_definition_id,
	min_cohort_date,
	max_cohort_date
INTO #ec_pairs
FROM (
	SELECT ROW_NUMBER() OVER (
			ORDER BY s1.cohort_definition_id,
				s2.cohort_definition_id
			) AS pair_id,
		CAST(s1.cohort_definition_id AS BIGINT) AS t_cohort_definition_id,
		CAST(s2.cohort_definition_id AS BIGINT) AS c_cohort_definition_id,
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
	subject_id,
	cohort_start_date,
	cohort_end_date
INTO @cohort_database_schema.@paired_cohort_table
FROM (
	-- Target filtered to common time
	SELECT cp1.tprime_cohort_definition_id AS cohort_definition_id,
		ec1.subject_id,
		ec1.cohort_start_date,
		ec1.cohort_end_date
	FROM #ec_pairs cp1
	INNER JOIN @cohort_database_schema.@exposure_cohort_table ec1
		ON cp1.t_cohort_definition_id = ec1.cohort_definition_id
			AND ec1.cohort_start_date >= cp1.min_cohort_date
			AND ec1.cohort_start_date <= cp1.max_cohort_date
	
	UNION ALL
	
	-- Comparator filtered to common time
	SELECT cp1.cprime_cohort_definition_id AS cohort_definition_id,
		ec1.subject_id,
		ec1.cohort_start_date,
		ec1.cohort_end_date
	FROM #ec_pairs cp1
	INNER JOIN @cohort_database_schema.@exposure_cohort_table ec1
		ON cp1.c_cohort_definition_id = ec1.cohort_definition_id
			AND ec1.cohort_start_date >= cp1.min_cohort_date
			AND ec1.cohort_start_date <= cp1.max_cohort_date
			
	) tmp;


-- Summarize cohort pairs
IF OBJECT_ID('tempdb..#ep_cohort_summary', 'U') IS NOT NULL
	DROP TABLE #ep_cohort_summary;

SELECT cohort_definition_id,
	COUNT(subject_id) AS num_persons,
	MIN(cohort_start_date) AS min_cohort_date,
	MAX(cohort_start_date) AS max_cohort_date
INTO #ep_cohort_summary
FROM @cohort_database_schema.@paired_cohort_table tec1
GROUP BY cohort_definition_id;

IF OBJECT_ID('@cohort_database_schema.@paired_cohort_summary_table', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.@paired_cohort_summary_table;

SELECT cp1.pair_id,
	cp1.t_cohort_definition_id,
	ecs1.num_persons AS t_num_persons,
	ecs1.min_cohort_date AS t_min_cohort_date,
	ecs1.max_cohort_date AS t_max_cohort_date,
	cp1.tprime_cohort_definition_id,
	epcs1.num_persons AS tprime_num_persons,
	epcs1.min_cohort_date AS tprime_min_cohort_date,
	epcs1.max_cohort_date AS tprime_max_cohort_date,
	cp1.c_cohort_definition_id,
	ecs2.num_persons AS c_num_persons,
	ecs2.min_cohort_date AS c_min_cohort_date,
	ecs2.max_cohort_date AS c_max_cohort_date,
	cp1.cprime_cohort_definition_id,
	epcs2.num_persons AS cprime_num_persons,
	epcs2.min_cohort_date AS cprime_min_cohort_date,
	epcs2.max_cohort_date AS cprime_max_cohort_date
INTO @cohort_database_schema.@paired_cohort_summary_table
FROM #ec_pairs cp1
INNER JOIN #ec_summary ecs1
	ON cp1.t_cohort_definition_id = ecs1.cohort_definition_id
INNER JOIN #ep_cohort_summary epcs1
	ON cp1.tprime_cohort_definition_id = epcs1.cohort_definition_id
INNER JOIN #ec_summary ecs2
	ON cp1.c_cohort_definition_id = ecs2.cohort_definition_id
INNER JOIN #ep_cohort_summary epcs2
	ON cp1.cprime_cohort_definition_id = epcs2.cohort_definition_id;
	
-- Cleanup
TRUNCATE TABLE #ec_summary;
DROP TABLE #ec_summary;

TRUNCATE TABLE #ec_pairs;
DROP TABLE #ec_pairs;

TRUNCATE TABLE #ep_cohort_summary;
DROP TABLE #ep_cohort_summary;