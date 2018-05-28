/************************************************************************
Copyright 2018 Observational Health Data Sciences and Informatics

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
SELECT row_id,
	CASE 
		WHEN distinct_count = 1
			THEN CAST(1999 AS BIGINT)
		WHEN distinct_count = 2
			THEN CAST(2999 AS BIGINT)
		WHEN distinct_count = 3
			THEN CAST(3999 AS BIGINT)
		WHEN distinct_count = 4
			THEN CAST(4999 AS BIGINT)
		ELSE CAST(5999 AS BIGINT)
		END AS covariate_id,
	1 AS covariate_value
FROM (
	SELECT @row_id_field AS row_id,
		COUNT(DISTINCT exposure_era.cohort_definition_id) AS distinct_count
	FROM @cohort_temp_table c
	INNER JOIN @cohort_database_schema.@exposure_era_table exposure_era
		ON exposure_era.subject_id = c.subject_id
	WHERE exposure_era.cohort_start_date <= DATEADD(DAY, @window_end, c.cohort_start_date)
		AND exposure_era.cohort_start_date >= DATEADD(DAY, @window_start, c.cohort_start_date)
{@cohort_id != -1} ? {		AND c.cohort_definition_id = @cohort_id}
	GROUP BY @row_id_field
	) tmp;

