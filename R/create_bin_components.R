create_bin_components = function(data) {
  data = data |>
    mutate(
      A1C = ifelse(NUM_A1C >= 6.5, 1, 0),
      ALB = ifelse(NUM_ALB < 3.5, 1, 0),
      BMI = ifelse(NUM_BMI > 30, 1, 0),
      CHOL = ifelse(NUM_CHOL >= 200, 1, 0),
      CRP = ifelse(NUM_CRP >= 10, 1, 0),
      CREAT_C = ifelse(test = SEX == "MALE" & NUM_CREAT_C > 1.2,
                       yes = 1,
                       no = ifelse(test = SEX == "FEMALE" & NUM_CREAT_C > 1.1,
                                   yes = 1,
                                   no = 0)),
      HCST = ifelse(NUM_HCST > 50, 1, 0),
      TRIG = ifelse(NUM_TRIG >= 150, 1, 0),
      BP_DIASTOLIC = ifelse(NUM_BP_DIASTOLIC > 90, 1, 0),
      BP_SYSTOLIC = ifelse(NUM_BP_SYSTOLIC > 140, 1, 0)
    )
  return(data)
}
