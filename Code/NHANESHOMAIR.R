# Load Packages

library(dplyr)

# Calculate HOMA-IR (Lee et al., 2022 Formula)

NHANES2015_2016HOMAIR <- NHANES2015_2016 %>%
  mutate(HOMA_IR = (LBXIN * LBDGLUSI) / 22.5)