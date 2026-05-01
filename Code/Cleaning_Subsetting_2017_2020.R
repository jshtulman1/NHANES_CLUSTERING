# Load Packages

library(dplyr)


NHANES2017_2020_Processed <- NHANES2017_2020Microalb


# Creating Mean Blood Pressure Variables

NHANES2017_2020_Processed <- NHANES2017_2020_Processed %>%
  mutate(
    SBP_mean = rowMeans(select(., BPXOSY1, BPXOSY2, BPXOSY3), na.rm = TRUE),
    DBP_mean = rowMeans(select(., BPXODI1, BPXODI2, BPXODI3), na.rm = TRUE)
  )

# Log Transform Skewed Variables

NHANES2017_2020_Processed <-
  NHANES2017_2020_Processed %>%
  mutate(
    ln_LBXTR   = ifelse(LBXTR   > 0, log(LBXTR),   NA),
    ln_GGT      = ifelse(LBXSGTSI > 0, log(LBXSGTSI), NA),
    ln_LBXHSCRP = ifelse(LBXHSCRP > 0, log(LBXHSCRP), NA),
    ln_URDACT   = ifelse(URDACT   > 0, log(URDACT),   NA)
  )

# Subset Prediabetics:

NHANES2017_2020_Processed <-
  NHANES2017_2020_Processed %>%
  filter(
    (LBXGH >= 5.7 & LBXGH <= 6.4) |
      (LBXGLU >= 100 & LBXGLU <= 125)
  )

# Apply Exlcusion Criteria

NHANES2017_2020_Processed %>%
  filter(RIDAGEYR >= 18) %>%
  summarise(
    total = n(),
    any_diabetes = sum(DIQ070 == 1 | DIQ050 == 1 | DIQ010 == 1, na.rm = TRUE)
  )

NHANES2017_2020_Processed <- 
  NHANES2017_2020_Processed %>%
  filter(
    RIDAGEYR >= 18,
    rowSums(across(c(DIQ070, DIQ050, DIQ010), ~ . == 1), na.rm = TRUE) == 0
  )

# Changing Column Names for Harmonization



NHANES2017_2020_Processed <- NHANES2017_2020_Processed %>%
  rename(
    WTAF2YR  = WTSAFPRP,
    WTMEC2YR = WTMECPRP,
    WTINT2YR = WTINTPRP
  )

# Adding Year for Weighting

NHANES2017_2020_Processed <- NHANES2017_2020_Processed %>%
  mutate(Cycle = 2017)