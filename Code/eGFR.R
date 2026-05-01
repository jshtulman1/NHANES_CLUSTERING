# Load Packages

library(dplyr)

# Calculate eGFR (2021 iCKD-EPI Creatinine Equation

NHANES2015_2016eGFR <- NHANES2015_2016FLI %>%
  mutate(
    kappa = ifelse(RIAGENDR == 2, 0.7, 0.9),
    alpha = ifelse(RIAGENDR == 2, -0.241, -0.302),
    sex_mult = ifelse(RIAGENDR == 2, 1.012, 1),
    
    scr_k = LBXSCR / kappa,
    
    eGFR = 142 *
      (pmin(scr_k, 1, na.rm = TRUE) ^ alpha) *
      (pmax(scr_k, 1, na.rm = TRUE) ^ -1.200) *
      (0.9938 ^ RIDAGEYR) *
      sex_mult
  )




