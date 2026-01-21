`missALI`: Overcoming missing data to predict hospitalization from the
ALI
================

## Package Installation

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

## Illustrative Example

``` r
# Read in data on hospitalizations (suppressed for privacy)
hosp_dat = read.csv("~/Documents/missALI_prediction/data/deidentified_ali_hospitalizations.csv")
```

## Modeling Binary Outcomes

``` r
# Binary outcome: Any hospitalization (yes/no)
table(hosp_dat$ANY_ADMIT)
```

    ## 
    ##   0   1 
    ## 783 217

### Missingness Indicators for the Binary ALI Components

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

## Modeling Count Outcomes for

### Missingness Indicators for the Binary ALI Components

``` r
# Allow each ALI component to be either healthy, unhealthy, or missing 
## and fit a model with each component separately as predictors (+ other covariates)
mod_pois_ind = miss_ind_approach(outcome = "NUM_ADMIT", 
                                 covar = c("SEX", "AGE_AT_ENCOUNTER"), 
                                 data = hosp_dat, 
                                 family = "poisson") 

# View the fitted model coefficients
mod_pois_ind$fit |> 
  coefficients()
```

    ##             (Intercept)          A1C_FUnhealthy            A1C_FMissing 
    ##            -14.23787683              0.28488287             -0.32748600 
    ##          ALB_FUnhealthy            ALB_FMissing          BMI_FUnhealthy 
    ##             -0.29868170             -1.98031325              0.22589528 
    ##            BMI_FMissing         CHOL_FUnhealthy           CHOL_FMissing 
    ##            -11.78803927             -0.12998232              0.83046958 
    ##          CRP_FUnhealthy            CRP_FMissing      CREAT_C_FUnhealthy 
    ##              0.96617031             -0.48282779             15.04630057 
    ##        CREAT_C_FMissing           HCST_FMissing         TRIG_FUnhealthy 
    ##             13.21866215             -0.28856626              0.20993574 
    ##           TRIG_FMissing BP_DIASTOLIC_FUnhealthy  BP_SYSTOLIC_FUnhealthy 
    ##                      NA             -0.29995475              0.01916067 
    ##                 SEXMale        AGE_AT_ENCOUNTER 
    ##              0.29089633              0.01957805

``` r
# View the fitted model summary
mod_pois_ind$fit |> 
  summary()
```

    ## 
    ## Call:
    ## glm(formula = as.formula(paste(outcome, "~", paste(c(factor_ALI_comp, 
    ##     covar), collapse = "+"))), family = family, data = data)
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##                           Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             -14.237877 387.994535  -0.037 0.970727    
    ## A1C_FUnhealthy            0.284883   0.131291   2.170 0.030018 *  
    ## A1C_FMissing             -0.327486   0.123296  -2.656 0.007905 ** 
    ## ALB_FUnhealthy           -0.298682   0.342538  -0.872 0.383227    
    ## ALB_FMissing             -1.980313   0.445767  -4.442 8.89e-06 ***
    ## BMI_FUnhealthy            0.225895   0.107874   2.094 0.036253 *  
    ## BMI_FMissing            -11.788039 546.225554  -0.022 0.982782    
    ## CHOL_FUnhealthy          -0.129982   0.122710  -1.059 0.289478    
    ## CHOL_FMissing             0.830470   0.166547   4.986 6.15e-07 ***
    ## CRP_FUnhealthy            0.966170   0.281950   3.427 0.000611 ***
    ## CRP_FMissing             -0.482828   0.219743  -2.197 0.028003 *  
    ## CREAT_C_FUnhealthy       15.046301 387.994576   0.039 0.969066    
    ## CREAT_C_FMissing         13.218662 387.994226   0.034 0.972822    
    ## HCST_FMissing            -0.288566   0.311005  -0.928 0.353484    
    ## TRIG_FUnhealthy           0.209936   0.115466   1.818 0.069040 .  
    ## TRIG_FMissing                   NA         NA      NA       NA    
    ## BP_DIASTOLIC_FUnhealthy  -0.299955   0.246327  -1.218 0.223334    
    ## BP_SYSTOLIC_FUnhealthy    0.019161   0.138993   0.138 0.890356    
    ## SEXMale                   0.290896   0.100454   2.896 0.003782 ** 
    ## AGE_AT_ENCOUNTER          0.019578   0.004533   4.319 1.57e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for poisson family taken to be 1)
    ## 
    ##     Null deviance: 1571.1  on 999  degrees of freedom
    ## Residual deviance: 1330.1  on 981  degrees of freedom
    ## AIC: 1886
    ## 
    ## Number of Fisher Scoring iterations: 12
