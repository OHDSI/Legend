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
{DEFAULT @cohort_database_schema = 'scratch.dbo'}
{DEFAULT @paired_cohort_table = 'cohort'}
{DEFAULT @small_sample_table = 'small_sample'}
{DEFAULT @sample_size = 10000}

-- Sample cohorts. Store in @cohort_database_schema.@small_sample_table
IF OBJECT_ID('@cohort_database_schema.@small_sample_table', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.@small_sample_table;

	
--HINT DISTRIBUTE_ON_KEY(subject_id)
SELECT 1 AS cohort_definition_id,
	subject_id,
	cohort_start_date
INTO @cohort_database_schema.@small_sample_table
FROM (
	SELECT ROW_NUMBER() OVER (
			ORDER BY NEWID()
			) AS rn,
		subject_id,
		cohort_start_date
	FROM (
		SELECT DISTINCT subject_id,
			cohort_start_date
		FROM @cohort_database_schema.@paired_cohort_table
		) unique_entries
	) tmp
WHERE rn <= @sample_size;
