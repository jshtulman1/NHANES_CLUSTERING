# Load Packages

library(dplyr)


NHANES2015_2016_Processed <- NHANES2015_2016Microalb 

# Creating Mean Blood Pressure Variables

NHANES2015_2016_Processed <- NHANES2015_2016_Processed %>%
  mutate(
    SBP_mean = rowMeans(select(., BPXSY1, BPXSY2, BPXSY3, BPXSY4), na.rm = TRUE),
    DBP_mean = rowMeans(select(., BPXDI1, BPXDI2, BPXDI3, BPXDI4), na.rm = TRUE)
  )

# Log Transform Skewed Variables

NHANES2015_2016_Processed <-
  NHANES2015_2016_Processed %>%
  mutate(
    ln_LBXTR   = ifelse(LBXTR   > 0, log(LBXTR),   NA),
    ln_GGT      = ifelse(LBXSGTSI > 0, log(LBXSGTSI), NA),
    ln_LBXHSCRP = ifelse(LBXHSCRP > 0, log(LBXHSCRP), NA),
    ln_URDACT   = ifelse(URDACT   > 0, log(URDACT),   NA)
  )

# Subset Prediabetics:

NHANES2015_2016_Processed <-
  NHANES2015_2016_Processed %>%
  filter(
    (LBXGH >= 5.7 & LBXGH <= 6.4) |
      (LBXGLU >= 100 & LBXGLU <= 125)
  )

# Apply Exlcusion Criteria

NHANES2015_2016_Processed %>%
  filter(RIDAGEYR >= 18) %>%
  summarise(
    total = n(),
    any_diabetes = sum(DIQ070 == 1 | DIQ050 == 1 | DIQ010 == 1, na.rm = TRUE)
  )

NHANES2015_2016_Processed <- 
  NHANES2015_2016_Processed %>%
  filter(
    RIDAGEYR >= 18,
    rowSums(across(c(DIQ070, DIQ050, DIQ010), ~ . == 1), na.rm = TRUE) == 0
  )

# Renaming Columns for Harmonization

NHANES2015_2016_Processed <- NHANES2015_2016_Processed %>%
  rename(DMDMARTZ = DMDMARTL)

# Adding Year for Weighting

NHANES2015_2016_Processed <- NHANES2015_2016_Processed %>%
  mutate(Cycle = 2015)