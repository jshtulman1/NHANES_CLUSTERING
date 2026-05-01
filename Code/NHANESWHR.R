# Load Packages

library(dplyr)

# Calculate Waist-to-Height Ratio

NHANES2015_2016WHR <- NHANES2015_2016HOMAIR %>%
  mutate(WHtR = BMXWAIST / BMXHT)

    