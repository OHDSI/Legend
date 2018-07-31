{DEFAULT @nesting_cohort_table = '#nesting_cohorts'}
{DEFAULT @cdm_database_schema = 'cdm.dbo'}

SELECT hypertension.person_id AS subject_id,
	hypertension_start_date AS cohort_start_date,
	observation_period_end_date AS cohort_end_date
INTO @nesting_cohort_table
FROM (
	SELECT person_id,
		MIN(condition_start_date) AS hypertension_start_date
	FROM @cdm_database_schema.condition_occurrence
	WHERE condition_concept_id IN (
			SELECT descendant_concept_id
			FROM @cdm_database_schema.concept_ancestor
			WHERE ancestor_concept_id IN (316866) -- Hypertensive disorder
			)
	GROUP BY person_id
	) hypertension
INNER JOIN @cdm_database_schema.observation_period
	ON observation_period.person_id = hypertension.person_id
		AND observation_period_start_date <= hypertension_start_date
		AND observation_period_end_date >= hypertension_start_date;

