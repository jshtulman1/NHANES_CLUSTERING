# Load Packages

library(dplyr)
library(CVrisk)

# Prep Data Set

NHANES2015_2016CVRisk <- NHANES2015_2016eGFR %>%
  mutate(
    ## Gender
    gender_chr = case_when(
      RIAGENDR == 1 ~ "male",
      RIAGENDR == 2 ~ "female",
      TRUE ~ NA_character_
    ),
    
    ## Race
    race_chr = case_when(
      RIDRETH3 == 4 ~ "aa",
      RIDRETH3 == 3 ~ "white",
      RIDRETH3 %in% c(1, 2, 6, 7) ~ "other",
      TRUE ~ NA_character_
    ),
    
    ## Mean SBP
    
    sbp_mean = rowMeans(
      cbind(BPXSY1, BPXSY2, BPXSY3, BPXSY4),
      na.rm = TRUE
    ),
    
    ## BP medication
    bp_med = ifelse(BPQ050A == 1, 1,
                    ifelse(BPQ050A == 2, 0, NA)),
    
    ## Smoker (ever smoked 100 cigarettes)
    smoker = ifelse(SMQ020 == 1, 1,
                    ifelse(SMQ020 == 2, 0, NA)),
    
    ## Diabetes
    diabetes = ifelse(DIQ010 == 1, 1,
                      ifelse(DIQ010 %in% c(2, 3), 0, NA))
  )

# Calculate ASCVD 10 Year (ACC/AHA 2013)

NHANES2015_2016CVRisk$ASCVD_10yr <- with(
  NHANES2015_2016CVRisk,
  mapply(
    FUN = ascvd_10y_accaha,
    race = race_chr,
    gender = gender_chr,
    age = RIDAGEYR,
    totchol = LBXTC,
    hdl = LBDHDD,
    sbp = sbp_mean,
    bp_med = bp_med,
    smoker = smoker,
    diabetes = diabetes
  )
)


