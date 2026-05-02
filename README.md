`missALI`: Overcoming missing data to predict hospitalization from the
ALI
================

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

## Functionality

Using the dataset `hosp_dat` for illustration, the `missALI` package
contains functions to fit prediction models with…

- **Two types of outcome:** binary (logistic regression, random forest
  classifier) or count (Poisson regression, zero-inflated Poisson
  regression),
- **Two types of ALI components:** binary (healthy/unhealthy) or
  numeric, and
- **Five approaches to handle missing components:** missingness
  indicators (only for binary ALI components),

Example code for each of these options follows.

## Summary of Non-Missing ALI Components

``` r
hosp_dat |> 
  dplyr::select(NUM_A1C:NUM_BP_SYSTOLIC) |> 
  tidyr::pivot_longer(cols = NUM_A1C:NUM_BP_SYSTOLIC, values_to = "val", names_to = "comp") |> 
  dplyr::group_by(comp) |> 
  dplyr::summarize(
    med = median(val, na.rm = TRUE), 
    q1 = quantile(val, probs = 0.25, na.rm = TRUE), 
    q3 = quantile(val, probs = 0.75, na.rm = TRUE)
  ) |> 
  dplyr::mutate(
    num_summ = paste0("$", round(med, 2), "$ $(", round(q1, 2), "-", round(q3, 2), ")$")
  )
```

    ## # A tibble: 10 × 5
    ##    comp                med    q1     q3 num_summ                  
    ##    <chr>             <dbl> <dbl>  <dbl> <chr>                     
    ##  1 NUM_A1C            5.73   5.3   6.68 $5.73$ $(5.3-6.68)$       
    ##  2 NUM_ALB            4.3    4.1   4.5  $4.3$ $(4.1-4.5)$         
    ##  3 NUM_BMI           29.2   25.5  34.6  $29.18$ $(25.52-34.56)$   
    ##  4 NUM_BP_DIASTOLIC  77.0   71.4  82.1  $77.04$ $(71.44-82.13)$   
    ##  5 NUM_BP_SYSTOLIC  125.   117.  135.   $125.06$ $(117.24-134.77)$
    ##  6 NUM_CHOL         183    161.  208    $183$ $(160.69-208)$      
    ##  7 NUM_CREAT_C      194.   176.  213.   $194.35$ $(175.52-213.18)$
    ##  8 NUM_CRP            2.8    0.8  38.4  $2.8$ $(0.8-38.45)$       
    ##  9 NUM_HCST          10.1    8.5  12.8  $10.1$ $(8.5-12.8)$       
    ## 10 NUM_TRIG         117     84.5 174.   $117$ $(84.5-173.54)$

## Modeling Different Types of Outcomes

### Binary Outcomes

Our binary outcome is called `ANY_ADMIT` and can be summarized by the
following count frequency table.

``` r
# Binary outcome: Any hospitalization (yes/no)
table(hosp_dat$ANY_ADMIT)
```

    ## 
    ##   0   1 
    ## 783 217

In the `missALI` package, there are functions for the various missing
data approaches. Each of these functions can handle *either* a binary or
count outcome. The user simply specifies which type of model they want
through the `family` argument, as they would with the built-in `glm()`
function in R.

For a binary outcome, we let `family = "binomial"` in all of the
situations that follow.

### Count Outcomes

Our binary outcome is called `NUM_ADMIT` and can be summarized by the
following count frequency table:

``` r
# Count outcome: Number of hospitalizations (0, 1, 2,...)
## Count frequency table
table(hosp_dat$NUM_ADMIT)
```

    ## 
    ##   0   1   2   3   4   5   6   7   8   9  12  13  14 
    ## 783 132  42  18  12   3   1   1   3   2   1   1   1

``` r
## And 6-number summary
summary(hosp_dat$NUM_ADMIT)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   0.000   0.000   0.000   0.427   0.000  14.000

In the `missALI` package, there are functions for the various missing
data approaches. Each of these functions can handle *either* a binary or
count outcome. The user simply specifies which type of model they want
through the `family` argument, as they would with the built-in `glm()`
function in R.

For a count outcome, we let `family = "poisson"` in all of the
situations that follow.

#### Zero Inflation

If you suspect that your count outcome may exhibit zero inflation,
specify which variables you think that the zero inflation depends upon
using the `zeros` argument in the functions that follow. If you suspect
zero inflation but that it does not depend on any additional variables,
set `zeros = "intercept"`.

## Using Different Missing Data Approaches

Each of the following missing data approaches is demonstrated for the
binary outcome, but can be applied with count outcomes instead by
replacing the `outcome` and `family` arguments as outlined above.

Most of the missing data approaches are specifically for when we use the
**binary** versions of the ALI components, rather than the numeric ones.
However, imputation can also be done on the original **numeric**
measurements from which the ALI components were derived.

### Approach 1: Proportion of Non-Missing Components That Are Unhealthy

Another way to adapt the original ALI definition is to convert it from a
count of unhealthy components to the *percent* of them. Then, we can
calculate each patient’s ALI as the proportion out of only their
nonmissing components (i.e., their complete case proportion of unhealthy
measurements). This approach effectively ignores the missing components
per patient; they do not count positively or negatively toward their
whole-person health.

``` r
# Calculate ALI as the proportion of nonmissing components that are unhealthy
## and fit a model with each component separately as predictors (+ other covariates)
mod_log_prop = cc_prop_approach(outcome = "ANY_ADMIT", 
                                covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                                data = hosp_dat, 
                                family = "binomial") 

# View the fitted model summary
mod_log_prop$fit |> 
  summary()
```

    ## 
    ## Call:
    ## glm(formula = as.formula(paste(outcome, "~ ", paste(c("PROP_UNHEALTHY", 
    ##     covar), collapse = "+"))), family = family, data = data)
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      -3.090065   0.331319  -9.327  < 2e-16 ***
    ## PROP_UNHEALTHY    1.473894   0.381619   3.862 0.000112 ***
    ## SEXMale          -0.010915   0.160326  -0.068 0.945724    
    ## AGE_AT_ENCOUNTER  0.026815   0.006338   4.231 2.33e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1046.2  on 999  degrees of freedom
    ## Residual deviance: 1001.8  on 996  degrees of freedom
    ## AIC: 1009.8
    ## 
    ## Number of Fisher Scoring iterations: 4

### Approach 2: Counts of Unhealthy and Missing Components

The original definition of the ALI (from Seeman et al.) was actually the
count of unhealthy components, taking of values from 0 to 10. When we
have missingness, however, this count alone can be misleading; it
inherently treats all missing values as 0s (meaning healthy). However,
we could try to include the count of unhealthy components *and* the
count of missing components, where the latter could be interpreted as
the sum of missingness indicators per person.

``` r
# Replace missing ALI components with "healthy" (the best case scenario)
## and fit a model with each component separately as predictors (+ other covariates)
mod_log_num = num_miss_approach(outcome = "ANY_ADMIT", 
                                covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                                data = hosp_dat, 
                                family = "binomial") 

# View the fitted model summary
mod_log_num$fit |> 
  summary()
```

    ## 
    ## Call:
    ## glm(formula = as.formula(paste(outcome, "~", paste(c("NUM_UNHEALTHY", 
    ##     "NUM_MISSING", covar), collapse = "+"))), family = family, 
    ##     data = data)
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      -3.160464   0.581547  -5.435 5.49e-08 ***
    ## NUM_UNHEALTHY     0.216141   0.066397   3.255  0.00113 ** 
    ## NUM_MISSING       0.033942   0.075659   0.449  0.65370    
    ## SEXMale          -0.010301   0.160259  -0.064  0.94875    
    ## AGE_AT_ENCOUNTER  0.026544   0.006789   3.910 9.25e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1046.2  on 999  degrees of freedom
    ## Residual deviance: 1004.6  on 995  degrees of freedom
    ## AIC: 1014.6
    ## 
    ## Number of Fisher Scoring iterations: 4

### Approach 3: Best/Worst Case Imputation

For each of the 10 ALI components, we can assume that the missing values
would have been healthy (for the best case scenario) or unhealthy (for
the worst case scenario). Then, we fit the model using the original
2-level categorical variable (unhealthy/healthy) for each component,
further controlling for age and sex.

``` r
# Replace missing ALI components with "healthy" (the best case scenario)
## and fit a model with each component separately as predictors (+ other covariates)
mod_log_best = case_approach(outcome = "ANY_ADMIT", 
                             covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                             data = hosp_dat, 
                             family = "binomial", 
                             best = TRUE) 

# View the fitted model summary
mod_log_best$fit |> 
  summary()
```

    ## 
    ## Call:
    ## glm(formula = as.formula(paste(outcome, "~ CASE_ALI +", paste(covar, 
    ##     collapse = "+"))), family = family, data = data)
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      -2.944173   0.322137  -9.140  < 2e-16 ***
    ## CASE_ALI          2.020370   0.584306   3.458 0.000545 ***
    ## SEXMale          -0.011646   0.160219  -0.073 0.942054    
    ## AGE_AT_ENCOUNTER  0.025585   0.006445   3.970  7.2e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1046.2  on 999  degrees of freedom
    ## Residual deviance: 1004.8  on 996  degrees of freedom
    ## AIC: 1012.8
    ## 
    ## Number of Fisher Scoring iterations: 4

The code above fits the model for the “best” case scenario. To instead
fit the model for the “worst” case scenario, rather than best, simply
switch the last argument in the call to the `case_approach()` function
to be `best = FALSE` instead.

## Prediction

For all approaches except multiple imputation, predicted probabilities
of hospitalization can be obtained using the usual `predict()` function
for a `glm` object. For example, we can predict from the missingness
indicator **logistic regression** model above as:

``` r
# Calculate predicted probabilities from logistic regression w/ best case imputation
pred_prob_best = mod_log_best$fit |> 
  predict(type = "response")
## View the first few
pred_prob_best |> 
  head()
```

    ##         1         2         3         4         5         6 
    ## 0.2514757 0.2730080 0.2389383 0.1332820 0.2457094 0.2580422

``` r
# Make the ROC curve 
roc_curve = pROC::roc(hosp_dat$ANY_ADMIT, pred_prob_best)
plot(roc_curve, 
     col = "#CD0BBC", 
     main = "ROC Curve", 
     print.auc = TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

## Other Models/Classifiers

### Poisson Model

For a **Poisson regression** model, using `predict(type = "response")`
will obtain the predicted *count* of hospitalizations for each patient.

``` r
# Replace missing ALI components with "healthy" (the best case scenario)
## and fit a model with each component separately as predictors (+ other covariates)
mod_pois_best = case_approach(outcome = "ANY_ADMIT", 
                              covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                              data = hosp_dat, 
                              family = "poisson",
                              best = TRUE) 

# Calculate predicted counts from Poisson regression w/ missingness indicator
pred_count_best = mod_pois_best$fit |> 
  predict(type = "response")
## View the first few
pred_count_best |> 
  head()
```

    ##         1         2         3         4         5         6 
    ## 0.2517278 0.2716508 0.2353560 0.1342981 0.2404682 0.2554566

``` r
# Make the ROC curve 
roc_curve = pROC::roc(hosp_dat$ANY_ADMIT, pred_count_best)
plot(roc_curve, 
     col = "#CD0BBC", 
     main = "ROC Curve", 
     print.auc = TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

### Zero-Inflated Poisson Model

Below, we demonstrate how the best-case imputation approach could be
used with a zero-inflated Poisson outcome model for the count of
hospitalizations. The zero inflation part of the model assumes that a
patient’s probability of never being hospitalized (i.e., being a
*structural zero*) depends on their imputed ALI, sex, and age (the same
explanatory variables as in the outcome model).

``` r
# Replace missing ALI components with "healthy" (the best case scenario)
mod_zip_best = case_approach(outcome = "ANY_ADMIT", 
                             covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                             zeros = c("CASE_ALI", "SEX", "AGE_AT_ENCOUNTER"), 
                             data = hosp_dat, 
                             family = "poisson", 
                             best = TRUE) 

# View the fitted model summary
mod_zip_best$fit |> 
  summary()
```

    ## 
    ## Call:
    ## zeroinfl(formula = as.formula(paste(outcome, "~ CASE_ALI +", paste(covar, 
    ##     collapse = "+"), "|", paste(zeros, collapse = "+"))), data = data, 
    ##     dist = family)
    ## 
    ## Pearson residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.7144 -0.5054 -0.4107 -0.2958  3.0486 
    ## 
    ## Count model coefficients (poisson with log link):
    ##                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)      -2.813100   0.290096  -9.697  < 2e-16 ***
    ## CASE_ALI          1.178952   0.523116   2.254  0.02421 *  
    ## SEXMale          -0.036466   0.138899  -0.263  0.79291    
    ## AGE_AT_ENCOUNTER  0.022048   0.005816   3.791  0.00015 ***
    ## 
    ## Zero-inflation model coefficients (binomial with logit link):
    ##                   Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)       -18.6230    14.6329  -1.273    0.203
    ## CASE_ALI         -173.9223   121.1428  -1.436    0.151
    ## SEXMale           -13.4504    10.8575  -1.239    0.215
    ## AGE_AT_ENCOUNTER    0.5288     0.4301   1.229    0.219
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
    ## 
    ## Number of iterations in BFGS optimization: 336 
    ## Log-likelihood: -527.9 on 8 Df

For a **zero-inflated Poisson regression** model, using
`predict(type = "response")` will obtain the predicted *count* of
hospitalizations for each patient.

``` r
# Calculate predicted counts from ZIP regression w/ best-case imputation
pred_count_zip_best = mod_zip_best$fit |> 
  predict()
## View the first few
pred_count_zip_best |> 
  head()
```

    ##         1         2         3         4         5         6 
    ## 0.2729254 0.2915958 0.2498931 0.1429050 0.2375017 0.2729326

``` r
# Make the ROC curve 
roc_curve = pROC::roc(hosp_dat$ANY_ADMIT, pred_count_zip_best)
plot(roc_curve, 
     col = "turquoise", 
     main = "ROC Curve", 
     print.auc = TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

### Machine Learning

If a **random forest** classifier is desired, rather than a logistic
regression model, then set the `use_glm` argument to `FALSE`. For a
binary outcome, we continue to let `family = "binomial"`.

``` r
# Make random forest reproducible
set.seed(415)

# Replace missing ALI components with "healthy" (the best case scenario)
mod_rf_best = case_approach(outcome = "ANY_ADMIT", 
                            covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                            data = hosp_dat, 
                            family = "binomial", 
                            use_glm = FALSE, 
                            best = TRUE) 

# View variable importance for the fitted model (instead of coefficients)
mod_rf_best$fit$variable.importance
```

    ##         CASE_ALI              SEX AGE_AT_ENCOUNTER 
    ##      0.013950085      0.002417677      0.013012130

``` r
# View the predicted probabilities of Y = 1 and Y = 0
mod_rf_best$fit$predictions |> 
  head()
```

    ##               1         0
    ## [1,] 0.28477633 0.7152237
    ## [2,] 0.26392265 0.7360774
    ## [3,] 0.16662605 0.8333739
    ## [4,] 0.08473186 0.9152681
    ## [5,] 0.43570316 0.5642968
    ## [6,] 0.30890172 0.6910983

``` r
# Make the ROC curve 
roc_curve = pROC::roc(hosp_dat$ANY_ADMIT, mod_rf_best$fit$predictions[, 1])
plot(roc_curve, 
     col = "darkolivegreen2", 
     main = "ROC Curve", 
     print.auc = TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

The resulting `mod_rf_best` contains two named slots.

1.  If you call `mod_rf_best$data`, you get the `hosp_dat` object back
    *but* with the missingness indicators applied to the 10 ALI
    components. (The data used to fit the model.)
2.  If you call `mod_rf_best$fit`, you get the `ranger` fitted model
    object, which you can then use to extract information like
    `$variable.importance` and `$predictions`.

## Cross-Validation

To get more realistic measures of accuracy, we can evaluate the models
using $k$-fold cross validation. We chose to leave this up to the user
and demonstrate how the functions discussed thus far can be used to
conduct $k$-fold cross validation.

``` r
# 5-fold cross validation of the logistic regression model with best-case imputation
kfold_log_best = kfold_validate(outcome = "ANY_ADMIT", 
                                covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                                data = hosp_dat, 
                                family = "binomial", 
                                miss_method = "best", 
                                folds = 5)

## View AUC from 5 folds 
kfold_log_best$all_fold_auc
```

    ## [1] 0.6221863 0.6410798 0.6253312 0.6310886 0.5959373

``` r
median(kfold_log_best$all_fold_auc) ### summarized by median
```

    ## [1] 0.6253312

``` r
## Extract ROC curve from first fold 
kfold_log_best |> 
  kfold_roc(plot_folds = 1, 
            overlay_average = FALSE, 
            color_by_fold = FALSE) 
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
## Plot individual ROC curves from 5 folds with median over top 
kfold_log_best |> 
  kfold_roc(plot_folds = 1:5, 
            overlay_average = TRUE, 
            color_by_fold = FALSE) 
```

![](README_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->
