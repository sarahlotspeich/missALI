# Load packages
library(dplyr) ## for mutate, bind_cols, if_else, rowwise

# Be reproducible
set.seed(11422)

# Set sample size
n = 1000

# Simulate demographics
PAT_MRN_ID = 1:n
SEX = sample(x = c("Female", "Male"), size = n, replace = TRUE, prob = c(0.605, 0.395))
AGE_AT_ENCOUNTER = (rpois(n = n, lambda = ifelse(test = SEX == "Female", yes = 45, no = 47)) - 18) / 10
demo = data.frame(PAT_MRN_ID, SEX, AGE_AT_ENCOUNTER)

# Simulate ALI components (gold standard / complete data)
## For simplicity, they are assumed to be independent normal random variables
## with mean/standard deviation based on real data
### Start by simulating numeric components...
ali = data.frame(
  NUM_A1C = rnorm(n = n, mean = 6.3, sd = 1.5),
  NUM_ALB = rnorm(n = n, mean = 4.3, sd = 0.3),
  NUM_BMI = rnorm(n = n, mean = 30.7, sd = 7.7),
  NUM_CHOL = rnorm(n = n, mean = 186.1, sd = 38.3),
  NUM_CRP = rnorm(n = n, mean = 31.2, sd = 53.3),
  NUM_CREAT_C = rnorm(n = n, mean = 194.4, sd = 53.2),
  NUM_HCST = rnorm(n = n, mean = 10.5, sd = 2.8),
  NUM_TRIG = rnorm(n = n, mean = 146.6, sd = 107.4),
  NUM_BP_DIASTOLIC = rnorm(n = n, mean = 77.0, sd = 8.4),
  NUM_BP_SYSTOLIC = rnorm(n = n, mean = 126.5, sd = 12.7)
  ) |>
  ### Bring in the demographics (for sex-specific thresholds)
  bind_cols(
    demo
  ) |>
  ### Then, define the binary ones based on thresholds.
  mutate(
    A1C = as.numeric(NUM_A1C >= 6.5),
    ALB = as.numeric(NUM_ALB >= 3.5),
    BMI = as.numeric(NUM_BMI > 30),
    CHOL = as.numeric(NUM_CHOL >= 200),
    CRP = as.numeric(NUM_CRP >= 10),
    CREAT_C = if_else(condition = SEX == "Male",
                      true = as.numeric(NUM_CREAT_C < 110),
                      false = as.numeric(NUM_CREAT_C < 100)),
    HCST = as.numeric(NUM_HCST > 50),
    TRIG = as.numeric(NUM_TRIG >= 150),
    BP_DIASTOLIC = as.numeric(NUM_BP_DIASTOLIC > 90),
    BP_SYSTOLIC = as.numeric(NUM_BP_SYSTOLIC > 140)
  ) |>
  ### And calculate complete-data ALI (gold standard)
  rowwise() |>
  mutate(
    GS_ALI = sum(c_across(A1C:BP_SYSTOLIC))
  )

# Simulate hospitalizations from demographics and gold standard/complete ALI components
## Start with counts of hospitalizations from a negative binomial
MEAN_ADMIT = exp(-1.32 + 0.05 * ali$GS_ALI + 0.11 * ali$AGE_AT_ENCOUNTER + 0.36 * as.numeric(ali$SEX == "Male"))
DISP_ADMIT = 0.34
NUM_ADMIT = rnbinom(n = n,
                    size = DISP_ADMIT,
                    prob = (DISP_ADMIT / (MEAN_ADMIT + DISP_ADMIT)))
## Simulate who are structural zeros (never going to be admitted)
PI_NEVER_ADMIT = 1 /
  (1 + exp(- (- 0.1 * ali$GS_ALI - 0.5 * ali$AGE_AT_ENCOUNTER + 0.4 * as.numeric(ali$SEX == "Male"))))
NEVER_ADMIT = rbinom(n = n,
                     size = 1,
                     prob = PI_NEVER_ADMIT)
## Put these pieces together for zero-inflated negative binomial
NUM_ADMIT[NEVER_ADMIT == 1] = 0 ### force structural zeros
## Add to ali
ali$NUM_ADMIT = NUM_ADMIT
# Simulate missingness in the ALI components
## To be realistic, make it more likely that healthy components are missing
## And set the percent missing per component to match real data
MISS_A1C = rbinom(n = n,
                  size = 1,
                  prob = ifelse(test = ali$A1C == 0,
                                yes = 0.53,
                                no = 0.13))
ali$NUM_A1C[MISS_A1C == 1] = ali$A1C[MISS_A1C == 1] = NA
MISS_ALB = rbinom(n = n,
                  size = 1,
                  prob = ifelse(test = ali$ALB == 0,
                                yes = 0.19,
                                no = 0.05))
ali$NUM_ALB[MISS_ALB == 1] = ali$ALB[MISS_ALB == 1] = NA
MISS_BMI = rbinom(n = n,
                  size = 1,
                  prob = ifelse(test = ali$BMI == 0,
                                yes = 0.01,
                                no = 0.001))
ali$NUM_BMI[MISS_BMI == 1] = ali$BMI[MISS_BMI == 1] = NA
MISS_CHOL = rbinom(n = n,
                   size = 1,
                   prob = ifelse(test = ali$CHOL == 0,
                                 yes = 0.26,
                                 no = 0.07))
ali$NUM_CHOL[MISS_CHOL == 1] = ali$CHOL[MISS_CHOL == 1] = NA
MISS_CRP = rbinom(n = n,
                  size = 1,
                  prob = ifelse(test = ali$CRP == 0,
                                yes = 0.98,
                                no = 0.02))
ali$NUM_CRP[MISS_CRP == 1] = ali$CRP[MISS_CRP == 1] = NA
MISS_CREAT_C = rbinom(n = n,
                      size = 1,
                      prob = ifelse(test = ali$CREAT_C == 0,
                                    yes = 0.99,
                                    no = 0.01))
ali$NUM_CREAT_C[MISS_CREAT_C == 1] = ali$CREAT_C[MISS_CREAT_C == 1] = NA
MISS_HCST = rbinom(n = n,
                   size = 1,
                   prob = ifelse(test = ali$HCST == 0,
                                 yes = 0.97,
                                 no = 0.03))
ali$NUM_HCST[MISS_HCST == 1] = ali$HCST[MISS_HCST == 1] = NA
MISS_TRIG = rbinom(n = n,
                   size = 1,
                   prob = ifelse(test = ali$TRIG == 0,
                                 yes = 0.26,
                                 no = 0.05))
ali$NUM_TRIG[MISS_TRIG == 1] = ali$TRIG[MISS_TRIG == 1] = NA

# Save data
ali |>
  dplyr::select(PAT_MRN_ID, AGE_AT_ENCOUNTER, SEX, NUM_ADMIT, everything(), -GS_ALI) |>
  write.csv("~/Documents/missALI_prediction/data/simulated_hospital_data.csv",
            row.names = FALSE)
