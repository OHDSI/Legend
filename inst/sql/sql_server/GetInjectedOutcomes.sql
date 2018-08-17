SELECT o.subject_id, cohort_definition_id, cohort_start_date, cohort_end_date
FROM @output_database_schema.@output_table o
INNER JOIN @subjects_table subjects
ON o.subject_id = CAST(subjects.subject_id AS BIGINT)
WHERE cohort_definition_id IN (@cohort_ids)
