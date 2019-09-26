{DEFAULT @max_gap = 30}

-- Create a table with all ingredient-dose combinations we're interested in:
IF OBJECT_ID('tempdb..#doses', 'U') IS NOT NULL
	DROP TABLE #doses;

CREATE TABLE #doses (
	ingredient_concept_id INT,
	dose FLOAT,
	unit_concept_id INT
	);

INSERT INTO #doses
SELECT 1395058,
	12.5,
	8576 --Chlorthalidone

UNION ALL

SELECT 974166,
	25,
	8576;--Hydrochlorothiazide

-- Select the drug_exposures having the right ingredients, and compute dose:
IF OBJECT_ID('tempdb..#dose_exposure', 'U') IS NOT NULL
	DROP TABLE #dose_exposure;

--HINT DISTRIBUTE_ON_KEY(person_id)	
SELECT drug_exposure_start_date AS exposure_start_date,
	drug_exposure_end_date AS exposure_end_date,
	person_id,
	doses.ingredient_concept_id AS concept_id,
	CASE 
		WHEN days_supply IS NULL
			OR days_supply = 0
			THEN 0
		ELSE amount_value * quantity / days_supply
		END AS dose,
	amount_unit_concept_id AS unit_concept_id
INTO #dose_exposure
FROM @cdm_database_schema.drug_exposure
INNER JOIN @cdm_database_schema.concept_ancestor
	ON drug_exposure.drug_concept_id = descendant_concept_id
INNER JOIN @cdm_database_schema.drug_strength
	ON drug_exposure.drug_concept_id = drug_strength.drug_concept_id
		AND ancestor_concept_id = drug_strength.ingredient_concept_id
INNER JOIN #doses doses
	ON ancestor_concept_id = doses.ingredient_concept_id;

-- Create an overview of encountered doses for quality check:
IF OBJECT_ID('tempdb..#dose_overview', 'U') IS NOT NULL
	DROP TABLE #dose_overview;

SELECT concept_id,
	dose,
	unit_concept_id,
	COUNT(*) AS record_count
INTO #dose_overview
FROM #dose_exposure
GROUP BY concept_id,
	dose,
	unit_concept_id;

-- Filter to right doses. Allow difference of 1 unit:
IF OBJECT_ID('tempdb..#dose_selected', 'U') IS NOT NULL
	DROP TABLE #dose_selected;

SELECT exposure_start_date,
	exposure_end_date,
	person_id,
	concept_id,
	doses.dose
INTO #dose_selected
FROM #dose_exposure dose_exposure
INNER JOIN #doses doses
	ON dose_exposure.concept_id = doses.ingredient_concept_id
		AND dose_exposure.unit_concept_id = doses.unit_concept_id
		AND ABS(dose_exposure.dose - doses.dose) < 1;

-- Create eras from exposures:
-- Note: currently assumes one dose per ingredient (aggregates per ingredient concept ID)
IF OBJECT_ID('tempdb..#dose_eras', 'U') IS NOT NULL
	DROP TABLE #dose_eras;

--HINT DISTRIBUTE_ON_KEY(subject_id)	
SELECT ends.person_id AS subject_id,
	ends.concept_id AS cohort_definition_id,
	MIN(exposure_start_date) AS cohort_start_date,
	ends.era_end_date AS cohort_end_date
INTO #dose_era
FROM (
	SELECT exposure.person_id,
		exposure.concept_id,
		exposure.exposure_start_date,
		MIN(events.end_date) AS era_end_date
	FROM #dose_selected exposure
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
					PARTITION BY person_id,
					concept_id ORDER BY event_date,
						event_type ROWS UNBOUNDED PRECEDING
					) AS start_ordinal,
				ROW_NUMBER() OVER (
					PARTITION BY person_id,
					concept_id ORDER BY event_date,
						event_type
					) AS overall_ord -- this re-numbers the inner UNION so all rows are numbered ordered by the event date
			FROM (
				-- select the start dates, assigning a row number to each
				SELECT person_id,
					concept_id,
					exposure_start_date AS event_date,
					0 AS event_type,
					ROW_NUMBER() OVER (
						PARTITION BY person_id,
						concept_id ORDER BY exposure_start_date
						) AS start_ordinal
				FROM #dose_selected
				
				UNION ALL
				
				-- add the end dates with NULL as the row number, padding the end dates by @max_gap to allow a grace period for overlapping ranges.
				SELECT person_id,
					concept_id,
					DATEADD(day, @max_gap, exposure_end_date),
					1 AS event_type,
					NULL
				FROM #dose_selected
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

-- Cleanup
TRUNCATE TABLE #doses;

DROP TABLE #doses;

TRUNCATE TABLE #dose_exposure;

DROP TABLE #dose_exposure;

TRUNCATE TABLE #dose_selected;

DROP TABLE #dose_selected;
