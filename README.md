Prediction Modeling from an EHR-Derived Allostatic Load Index with
Informatively Missing Biomarkers: A Case Study
================
Weavil, Rigdon, and Lotspeich (2026+)

## Installation

Installation of the `missALI` package from GitHub requires the
[`devtools`](https://www.r-project.org/nosvn/pandoc/devtools.html)
package and can be done in the following way.

``` r
# Install the package (run once) 
devtools::install_github(repo = "sarahlotspeich/missALI")
```

``` r
# Load the package (run every time you want to use it) 
library(missALI)
```

## Models

### Logistic regression with…

1.  [Complete-case
    proportion](analysis/logistic_regression/complete_case_proportion.R)
2.  [Counts of missing and unhealthy
    components](analysis/logistic_regression/count_missing_unhealthy.R)
3.  [Best-case imputation
    (summary)](analysis/logistic_regression/best_case_summary.R)
4.  [Worst-case imputation
    (summary)](analysis/logistic_regression/worst_case_summary.R)
5.  [Best-case imputation
    (separate)](analysis/logistic_regression/best_case_separate.R)
6.  [Worst-case imputation
    (separate)](analysis/logistic_regression/worst_case_separate.R)
7.  [Missingness as a
    category](analysis/logistic_regression/missing_category.R)
8.  [Pattern
    submodels](analysis/logistic_regression/pattern_submodels.R)

### Random forest with…

1.  [Complete-case
    proportion](analysis/random_forest/complete_case_proportion.R)
2.  [Counts of missing and unhealthy
    components](analysis/random_forest/count_missing_unhealthy.R)
3.  [Best-case imputation
    (summary)](analysis/random_forest/best_case_summary.R)
4.  [Worst-case imputation
    (summary)](analysis/random_forest/worst_case_summary.R)
5.  [Best-case imputation
    (separate)](analysis/random_forest/best_case_separate.R)
6.  [Worst-case imputation
    (separate)](analysis/random_forest/worst_case_separate.R)
7.  [Missingness as a
    category](analysis/random_forest/missing_category.R)
8.  [Pattern submodels](analysis/random_forest/pattern_submodels.R)

## Figures

- [Figure 1.](figures/Figure1_Missing_Data_Patterns.R) Patterns of
  missingness in the $10$ allostatic load index (ALI) for the $n = 707$
  patients in the sample from the electronic health records (EHR) at
  Atrium Health Wake Forest Baptist Hospital. Each row represents one of
  the $16$ distinct missing data patterns, and within that row the ALI
  component(s) with an “X” were missing.
- [Figure 2](figures/Figure2_Component_Status.R) Proportion of $n = 707$
  patients in the sample from the electronic health records (EHR) at
  Atrium Health Wake Forest Baptist Hospital with healthy, unhealthy,
  and missing values across the $10$ allostatic load index (ALI)
  components, after discretizing the original numeric biomarkers at
  their clinically meaningful thresholds from Table 1.
- [Figure 3](figures/Figure3_Upset.R) Missingness upset plot displaying
  combinations of allostatic load index (ALI) components that were
  missing together for patients.
- [Supplemental Figure S1](figures/FigureS1_Boxplot_Proportion_ALIs.R)
  Distributions of the allostatic load index (ALI) for the $n = 707$
  patients in the sample from the electronic health records (EHR) at
  Atrium Health Wake Forest Baptist Hospital after using the
  complete-case proportion and best/worst case imputation missing data
  methods.
- [Supplemental Figure S2](figures/FigureS2_Boxplot_Count_ALIs.R)
  Distributions of the allostatic load index (ALI) for the $n = 707$
  patients in the sample from the electronic health records (EHR) at
  Atrium Health Wake Forest Baptist Hospital after using the
  complete-case proportion and best/worst case imputation missing data
  methods.
- [Supplemental Figure
  S3](figures/FigureS3_LogReg_Summary_ROC.R)Receiver operating
  characteristic (ROC) curves for the four summary measure models using
  logistic regression based on the full sample data (top row) and
  $5$-fold cross-validation (bottom row). The area under the ROC curve
  (AUC) and its $95\%$ confidence interval ($95\%$ CI) are included.
- [Supplemental Figure
  S4](figures/FigureS4_LogReg_Separate_ROC.R)Receiver operating
  characteristic (ROC) curves for the four separate component models
  using logistic regression based on the full sample data (top row) and
  $5$-fold cross-validation (bottom row). The area under the ROC curve
  (AUC) and its $95\%$ confidence interval ($95\%$ CI) are included. The
  missingness as a category model could not be $5$-fold cross-validated
  due to extremely rare categories, which would sometimes appear in the
  test but not train data.
- [Supplemental Figure
  S5](figures/FigureS5_LogReg_Summary_Calibration.R)Calibration curves
  for the four summary measure models using logistic regression based on
  the full sample data (top row) and $5$-fold cross-validation (bottom
  row). The calibration intercept and slope with their $95\%$ confidence
  intervals ($95\%$ CIs) are included only for the cross-validated
  models; for the full-sample ones, logistic regression is almost
  guaranteed to appear perfectly calibrated when fit and evaluated on
  the same data.
- [Supplemental Figure
  S6](figures/FigureS6_LogReg_Separate_Calibration.R)Calibration curves
  for the four separate component models using logistic regression based
  on the full sample data (top row) and $5$-fold cross-validation
  (bottom row). The calibration intercept and slope with their $95\%$
  confidence intervals ($95\%$ CIs) are included only for the
  cross-validated models; for the full-sample ones, logistic regression
  is almost guaranteed to appear perfectly calibrated when fit and
  evaluated on the same data. The missingness as a category model could
  not be $5$-fold cross-validated due to extremely rare categories,
  which would sometimes appear in the test but not train data.
- [Supplemental Figure S7](figures/FigureS7_RF_Summary_ROC.R) Receiver
  operating characteristic (ROC) curves for the four separate component
  models using random forest classification based on the full sample
  data (top row) and $5$-fold cross-validation (bottom row). The area
  under the ROC curve (AUC) and its $95\%$ confidence interval
  ($95\%$ CI) are included. The missingness as a category model could
  not be $5$-fold cross-validated due to extremely rare categories,
  which would sometimes appear in the test but not train data.
- [Supplemental Figure S8](figures/FigureS8_RF_Separate_ROC.R)Receiver
  operating characteristic (ROC) curves for the four summary measure
  models using random forest classification based on the full sample
  data (top row) and $5$-fold cross-validation (bottom row). The area
  under the ROC curve (AUC) and its $95\%$ confidence interval
  ($95\%$ CI) are included.
- [Supplemental Figure
  S9](figures/FigureS9_RF_Summary_Calibration.R)Calibration curves for
  the four summary measure models using random forest classification
  based on the full sample data (top row) and $5$-fold cross-validation
  (bottom row). The calibration intercept and slope with their $95\%$
  confidence intervals ($95\%$ CIs) are included.
- [Supplemental Figure S10](figures/FigureS10_RF_Separate_Calibration.R)
  Calibration curves for the four separate component models using random
  forest classification based on the full sample data (top row) and
  $5$-fold cross-validation (bottom row). The calibration intercept and
  slope with their $95\%$ confidence intervals ($95\%$ CIs) are
  included. The missingness as a category model could not be $5$-fold
  cross-validated due to extremely rare categories, which would
  sometimes appear in the test but not train data.
