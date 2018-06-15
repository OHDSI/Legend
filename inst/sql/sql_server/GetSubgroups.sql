SELECT row_id,
	covariate_id,
	covariate_value
FROM #cov_1

UNION ALL

SELECT row_id,
	covariate_id,
	covariate_value
FROM #cov_2

UNION ALL

SELECT row_id,
	covariate_id,
	covariate_value
FROM #cov_3

UNION ALL

SELECT row_id,
	covariate_id,
	covariate_value
FROM #cov_4

UNION ALL

SELECT row_id,
	covariate_id,
	covariate_value
FROM #cov_5

UNION ALL

SELECT row_id,
	covariate_id,
	covariate_value
FROM #cov_6;
