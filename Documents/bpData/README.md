Results from a sensitivity analysis in PanTher using blood pressure data to show balance.

The script used to generate these is [here](../extras/AdjustForBloodPressure.R).

Description of files below. Note that all analyses are done assymetrical. So if there is data for target A and comparator B, there will not be data for target B and comparator A.

### BalancePsMatching.csv

Computes standardized difference of mean before and after matching on propensity scores that did **not** use blood presure.

Primary key:

- targetId / targetName
- comparatorId / comparatorName
- covariateId / covariateName (BP diastolic or BP systolic)

Use these columns to select single or combination treatments:

- targetType (Single or Combination)
- comparatorType (Single or Combination)

Most relevant values:

- beforeMatchingStdDiff is the standardized difference of mean before matching
- afterMatchingStdDiff is the standardized difference of mean after matching

### BalanceAdjustBpPsMatching.csv

Computes standardized difference of mean before and after matching on propensity scores that **did** use blood presure.

Primary key:

- targetId / targetName
- comparatorId / comparatorName
- covariateId / covariateName (BP diastolic or BP systolic)

Use these columns to select single or combination treatments:

- targetType (Single or Combination)
- comparatorType (Single or Combination)

Most relevant values:

- beforeMatchingStdDiff is the standardized difference of mean before matching
- afterMatchingStdDiff is the standardized difference of mean after matching

### HrsData_all_PsMatching.csv

Computes the hazard ratio either matching on a propensity score that does include blood pressure, or matching on a propensity score that does not include blood pressure. Results are reported both with and without empirical calibration.

Primary key:

- targetId / targetName
- comparatorId / comparatorName
- outcomeId / outcomeName
- type (Orginal, meaning without blood pressure in the propensity model, or Adjusting, meaning with blood pressure in the propensity model)
- estimate (Uncalibrated or Calibrated)

Use these columns to select single or combination treatments:

- targetType (Single or Combination)
- comparatorType (Single or Combination)

Most relevant values:

- rr is the estimated hazard ratio
- ci95lb is the lower bound of the 95% confidence interval of the hazard ratio
- ci95ub is the lower bound of the 95% confidence interval of the hazard ratio
- p is the two-sideed p-value against the null hypothesis of no effect


