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

- **Two types of outcome:** binary (logistic regression) or count
  (Poisson regression),
- **Two types of ALI components:** binary (healthy/unhealthy) or
  numeric, and
- **Five approaches to handle missing components:** missingness
  indicators (only for binary ALI components),

Example code for each of these options follows.

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
following count frequency table.

``` r
# Count outcome: Number of hospitalizations (0, 1, 2,...)
table(hosp_dat$NUM_ADMIT)
```

    ## 
    ##   0   1   2   3   4   5   6   7   8   9  12  13  14 
    ## 783 132  42  18  12   3   1   1   3   2   1   1   1

In the `missALI` package, there are functions for the various missing
data approaches. Each of these functions can handle *either* a binary or
count outcome. The user simply specifies which type of model they want
through the `family` argument, as they would with the built-in `glm()`
function in R.

For a count outcome, we let `family = "poisson"` in all of the
situations that follow.

## Using Different Missing Data Approaches

Each of the following missing data approaches is demonstrated for the
binary outcome, but can be applied with count outcomes instead by
replacing the `outcome` and `family` arguments as outlined above.

Most of the missing data approaches are specifically for when we use the
**binary** versions of the ALI components, rather than the numeric ones.
However, imputation can also be done on the original **numeric**
measurements from which the ALI components were derived.

#### Missingness Indicators

For each of the 10 ALI components, we can go from two levels
(unhealthy/healthy) to three levels (unhealthy/healthy/missing). Then,
we fit the model using a 3-level categorical variable for each
component, further controlling for age and sex.

``` r
# Allow each ALI component to be either healthy, unhealthy, or missing 
## and fit a model with each component separately as predictors (+ other covariates)
mod_log_ind = miss_ind_approach(outcome = "ANY_ADMIT", 
                                covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                                data = hosp_dat, 
                                family = "binomial") 

# View the fitted model coefficients
mod_log_ind$fit |> 
  coefficients()
```

    ##             (Intercept)          A1C_FUnhealthy            A1C_FMissing 
    ##           -15.873789092             0.382855076            -0.183276712 
    ##          ALB_FUnhealthy            ALB_FMissing          BMI_FUnhealthy 
    ##            -0.858847380            -2.744298395             0.429215274 
    ##            BMI_FMissing         CHOL_FUnhealthy           CHOL_FMissing 
    ##           -12.408278876            -0.108450557             1.246828153 
    ##          CRP_FUnhealthy            CRP_FMissing      CREAT_C_FUnhealthy 
    ##             0.402441396            -0.809959596            30.888433717 
    ##        CREAT_C_FMissing           HCST_FMissing         TRIG_FUnhealthy 
    ##            14.790745470            -0.273152588             0.208069585 
    ##           TRIG_FMissing BP_DIASTOLIC_FUnhealthy  BP_SYSTOLIC_FUnhealthy 
    ##                      NA            -0.150939481             0.105122329 
    ##                 SEXMale        AGE_AT_ENCOUNTER 
    ##            -0.007033514             0.029290626

``` r
# View the fitted model summary
mod_log_ind$fit |> 
  summary()
```

    ## 
    ## Call:
    ## glm(formula = as.formula(paste(outcome, "~", paste(c(factor_ALI_comp, 
    ##     covar), collapse = "+"))), family = family, data = data)
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                           Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             -1.587e+01  8.292e+02  -0.019 0.984726    
    ## A1C_FUnhealthy           3.829e-01  2.391e-01   1.601 0.109370    
    ## A1C_FMissing            -1.833e-01  1.929e-01  -0.950 0.342109    
    ## ALB_FUnhealthy          -8.588e-01  6.757e-01  -1.271 0.203743    
    ## ALB_FMissing            -2.744e+00  7.623e-01  -3.600 0.000318 ***
    ## BMI_FUnhealthy           4.292e-01  1.749e-01   2.454 0.014124 *  
    ## BMI_FMissing            -1.241e+01  1.026e+03  -0.012 0.990355    
    ## CHOL_FUnhealthy         -1.085e-01  1.997e-01  -0.543 0.587087    
    ## CHOL_FMissing            1.247e+00  2.899e-01   4.301 1.70e-05 ***
    ## CRP_FUnhealthy           4.024e-01  6.923e-01   0.581 0.561057    
    ## CRP_FMissing            -8.100e-01  3.920e-01  -2.066 0.038783 *  
    ## CREAT_C_FUnhealthy       3.089e+01  1.675e+03   0.018 0.985287    
    ## CREAT_C_FMissing         1.479e+01  8.292e+02   0.018 0.985768    
    ## HCST_FMissing           -2.732e-01  5.627e-01  -0.485 0.627347    
    ## TRIG_FUnhealthy          2.081e-01  1.956e-01   1.064 0.287548    
    ## TRIG_FMissing                   NA         NA      NA       NA    
    ## BP_DIASTOLIC_FUnhealthy -1.509e-01  3.725e-01  -0.405 0.685296    
    ## BP_SYSTOLIC_FUnhealthy   1.051e-01  2.430e-01   0.433 0.665362    
    ## SEXMale                 -7.034e-03  1.696e-01  -0.041 0.966926    
    ## AGE_AT_ENCOUNTER         2.929e-02  7.282e-03   4.022 5.77e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1046.17  on 999  degrees of freedom
    ## Residual deviance:  947.79  on 981  degrees of freedom
    ## AIC: 985.79
    ## 
    ## Number of Fisher Scoring iterations: 14

The resulting `mod_log_ind` contains two named slots.

1.  If you call `mod_log_ind$data`, you get the `hosp_dat` object back
    *but* with the missingness indicators applied to the 10 ALI
    components. (The data used to fit the model.)
2.  If you call `mod_log_ind$fit`, you get the `glm` fitted model
    object, which you can then use with the usual functions like
    `coefficients()` and `summary()`.

All of the approach functions return a list with these two slots! For
simplicity, only code fitting models with the following approaches is
shown below.

#### Sum of Missingness Indicators

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
```

#### Proportion of Non-Missing

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
```

#### Best/Worst Case Scenario

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
```

The code above fits the model for the “best” case scenario. To instead
fit the model for the “worst” case scenario, rather than best, simply
switch the last argument in the call to the `case_approach()` function
to be `best = FALSE` instead.

#### Multiple Imputation
