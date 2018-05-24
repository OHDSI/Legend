{DEFAULT @nesting_cohort_table = '#nesting_cohorts'}
{DEFAULT @cdm_database_schema = 'cdm.dbo'}

SELECT depression.person_id AS subject_id,
	depression_start_date AS cohort_start_date,
	CASE 
		WHEN other_start_date IS NULL
			THEN observation_period_end_date
		ELSE other_start_date
		END AS cohort_end_date
INTO @nesting_cohort_table
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
