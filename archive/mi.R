### Multiple Imputation

Multiple imputation is the only approach that applies to both the binary and numeric versions of the 10 ALI components. First, we try imputing the **binary ALI components** directly.

```{r, cache = TRUE}
# Be reproducible, since multiple imputation is a random process
set.seed(124)

# Replace missing ALI components with imputations of either "healthy" or "unhealthy"
## and fit a model with each component separately as predictors (+ other covariates)
mod_log_mi = mult_imp_approach(outcome = "ANY_ADMIT",
                               covar = c("SEX", "AGE_AT_ENCOUNTER"),
                               data = hosp_dat,
                               family = "binomial",
                               components = "binary",
                               m = 100,
                               post_imputation = "none")

# View the fitted model coefficients (from mice)
mod_log_mi$fit
```

There were 2 of 10 ALI components that caused the warnings/logged events. These can be viewed from the returned object as follows.

```{r}
# View the loggedEvents (from mice)
mod_log_mi$data$loggedEvents |>
  head()
```

Essentially, homocysteine was imputed to be constant/the same for all patients, and creatinine clearance was collinear/redundant for hemoglobin A1C. Thus, the imputed models above were fit using a subset of 8 ALI components. (This can be seen from the `term` column in the output above.)

To instead impute the **numeric ALI components** and then categorize them post-imputation, the only change to the code above would be switching to `components = "numeric"`.

```{r, cache = TRUE}
# Be reproducible, since multiple imputation is a random process
set.seed(124)

# Impute numeric measurements and then re-define binary ALI components
mod_log_mi_num = mult_imp_approach(outcome = "ANY_ADMIT",
                                   covar = c("SEX", "AGE_AT_ENCOUNTER"),
                                   data = hosp_dat,
                                   family = "binomial",
                                   components = "numeric",
                                   m = 100,
                                   post_imputation = "none")

# View the fitted model coefficients (from mice)
mod_log_mi_num$fit

# View the loggedEvents (from mice)
mod_log_mi_num$data$loggedEvents |>
  head()
```

#### Handling Post-Imputation Residual Missingness

We could also apply one of the other missing data approaches to the post-imputation data using the `post_imputation` argument to the `mult_imp_approach()` function. For example, we could re-calculate the **complete-case proportion** ALI after some of the missing components have been filled in with imputed values.

```{r, cache = TRUE, eval = FALSE}
# Be reproducible, since multiple imputation is a random process
set.seed(124)

# Replace missing ALI components with imputations of either "healthy" or "unhealthy",
## re-calculate the complete-case proportion ALI after reducing the amount of missingness,
## and fit a model with this proportion as the primary predictor (+ other covariates)
mod_log_mi_cc_prop = mult_imp_approach(outcome = "ANY_ADMIT",
                                       covar = c("SEX", "AGE_AT_ENCOUNTER"),
                                       data = hosp_dat,
                                       family = "binomial",
                                       components = "binary",
                                       m = 100,
                                       post_imputation = "cc_prop")

# View the fitted model coefficients (from mice)
mod_log_mi_cc_prop$fit
```

Other options for post-imputation residual missing data handling are...

Assigning **missingness indicators** to any variables that couldn't be imputed:

```{r, cache = TRUE, eval = FALSE}
# Be reproducible, since multiple imputation is a random process
set.seed(124)

# Replace missing ALI components with imputations of either "healthy" or "unhealthy",
## create missingness indicators for ALI components that couldn't be imputed,
## and fit a model with each component separately as predictors (+ other covariates)
mod_log_mi_miss_ind = mult_imp_approach(outcome = "ANY_ADMIT",
                                        covar = c("SEX", "AGE_AT_ENCOUNTER"),
                                        data = hosp_dat,
                                        family = "binomial",
                                        components = "binary",
                                        m = 100,
                                        post_imputation = "miss_ind")

# View the fitted model coefficients (from mice)
mod_log_mi_miss_ind$fit
```

Including the traditional count ALI (number of unhealthy components) while controlling for the **number of missing** ones, in addition to the other covariates:

  ```{r, cache = TRUE, eval = FALSE}
# Be reproducible, since multiple imputation is a random process
set.seed(124)

# Replace missing ALI components with imputations of either "healthy" or "unhealthy",
## sum up numbers of unhealthy and missing ALI components,
## and fit a model with these counts as predictors (+ other covariates)
mod_log_mi_num_miss = mult_imp_approach(outcome = "ANY_ADMIT",
                                        covar = c("SEX", "AGE_AT_ENCOUNTER"),
                                        data = hosp_dat,
                                        family = "binomial",
                                        components = "binary",
                                        m = 100,
                                        post_imputation = "num_miss")

# View the fitted model coefficients (from mice)
mod_log_mi_num_miss$fit
```

And filling them in with the **best (healthy) or worst (unhealthy) case** scenarios:

  ```{r, cache = TRUE, eval = FALSE}
# Be reproducible, since multiple imputation is a random process
set.seed(124)

# Replace missing ALI components with imputations of either "healthy" or "unhealthy",
## assume remaining missing values are "healthy",
## and fit a model with these counts as predictors (+ other covariates)
mod_log_mi_best = mult_imp_approach(outcome = "ANY_ADMIT",
                                    covar = c("SEX", "AGE_AT_ENCOUNTER"),
                                    data = hosp_dat,
                                    family = "binomial",
                                    components = "binary",
                                    m = 100,
                                    post_imputation = "best")

# View the fitted model coefficients (from mice)
mod_log_mi_best$fit
```

```{r, cache = TRUE, eval = FALSE}
# Be reproducible, since multiple imputation is a random process
set.seed(124)

# Replace missing ALI components with imputations of either "healthy" or "unhealthy",
## assume remaining missing values are "unhealthy",
## and fit a model with these counts as predictors (+ other covariates)
mod_log_mi_worst = mult_imp_approach(outcome = "ANY_ADMIT",
                                     covar = c("SEX", "AGE_AT_ENCOUNTER"),
                                     data = hosp_dat,
                                     family = "binomial",
                                     components = "binary",
                                     m = 100,
                                     post_imputation = "worst")

# View the fitted model coefficients (from mice)
mod_log_mi_worst$fit
```
When predicting after **multiple imputation**, we take the pooled coefficients across all models (returned in the `$fit` slot by the functions above) and use them to predict for each based based on each imputed dataset. Ultimately, model performance is based on the *average prediction* for each patient across all imputed datasets.

```{r}
# Calculate predicted probabilities from logistic regression w/ multiple imputation
pred_prob_mi = mod_log_mi |>
  predict_imp()
## View the first few rows/columns of per-imputation predictions
pred_prob_mi$imp_pred |>
  head()
## View the first few average predictions (across imputations)
pred_prob_mi$pooled_pred |>
  head()

# Make the ROC curve
roc_curve = pROC::roc(hosp_dat$ANY_ADMIT, pred_prob_mi$pooled_pred)
plot(roc_curve,
     col = "#DF536B",
     main = "ROC Curve",
     print.auc = TRUE)
```

The `predict_imp()` function takes in the return list from `mult_imp_approach()`, which means it inherits the pooled model coefficients, the `mids` object returned by `mice` with all imputed datasets, and a reminder about whether any `post_imputation` transformations were performed. It returns a list with both the predictions per imputed dataset, `imp_pred`, and the average predictions per observation using the pooled coefficient estimates, `pooled_pred`.

```{r, eval = FALSE}
# Calculate predicted probabilities from logistic regression w/ multiple imputation
## followed by the complete-case proportion ALI calculation
pred_prob_mi_cc_prop = mod_log_mi_cc_prop |>
  predict_imp()
## View the first few
pred_prob_mi_cc_prop$imp_pred |>
  head()
## View the first few average predictions (across imputations)
pred_prob_mi_cc_prop$pooled_pred |>
  head()

# Make the ROC curve
roc_curve = pROC::roc(hosp_dat$ANY_ADMIT, pred_prob_mi_cc_prop$pooled_pred)
plot(roc_curve,
     col = "#2297E6",
     main = "ROC Curve",
     print.auc = TRUE)
```

We can also multiply impute the missing ALI components before applying the random forest classifier. The following code imputes the **binary ALI components** directly.

```{r, cache = TRUE}
# Be reproducible, since multiple imputation is a random process
set.seed(124)

# Replace missing ALI components with imputations of either "healthy" or "unhealthy"
## and fit a model with each component separately as predictors (+ other covariates)
mod_rf_mi = mult_imp_approach(outcome = "ANY_ADMIT",
                              covar = c("SEX", "AGE_AT_ENCOUNTER"),
                              data = hosp_dat,
                              family = "binomial",
                              components = "binary",
                              m = 100,
                              post_imputation = "none",
                              use_glm = FALSE)

# View the average variable importance across all random forests (from ranger)
mod_rf_mi$fit
```

```{r}
# Calculate predicted probabilities from random forest w/ multiple imputation
pred_prob_rf_mi = mod_rf_mi |>
  predict_imp()
## View the first few
pred_prob_rf_mi$imp_pred |>
  head()
## View the first few average predictions (across imputations)
pred_prob_rf_mi$pooled_pred |>
  head()

# Make the ROC curve
roc_curve = pROC::roc(hosp_dat$ANY_ADMIT, pred_prob_rf_mi$pooled_pred)
plot(roc_curve,
     col = "#DF7A53",
     main = "ROC Curve",
     print.auc = TRUE)
```

As with the statistical models, we can choose from various post-imputation options to handle residual missingness in the two ALI components (homocysteine and CRP).

```{r, cache = TRUE}
# Be reproducible, since multiple imputation is a random process
set.seed(124)

# Replace missing ALI components with imputations of either "healthy" or "unhealthy"
## and fit a model with each component separately as predictors (+ other covariates)
## use missingness indicators for residual missingness
mod_rf_mi_miss_ind = mult_imp_approach(outcome = "ANY_ADMIT",
                                       covar = c("SEX", "AGE_AT_ENCOUNTER"),
                                       data = hosp_dat,
                                       family = "binomial",
                                       components = "binary",
                                       m = 100,
                                       post_imputation = "miss_ind",
                                       use_glm = FALSE)

# Calculate predicted probabilities from logistic regression w/ multiple imputation
pred_prob_rf_mi_miss_ind = mod_rf_mi_miss_ind |>
  predict_imp()

# Make the ROC curve
roc_curve = pROC::roc(hosp_dat$ANY_ADMIT, pred_prob_rf_mi_miss_ind$pooled_pred)
plot(roc_curve,
     col = "#2A9D8F",
     main = "ROC Curve",
     print.auc = TRUE)
```
