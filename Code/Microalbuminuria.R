# Loading Packages

library(dplyr)

# Calculating Microalbuminuria (Prasad et al., 2023)

NHANES2015_2016Microalb <- NHANES2015_2016CVRisk %>%
  mutate(
    microalbuminuria = ifelse(
      URDACT >= 2 & URDACT <= 20,
      1,
      0
    )
  )

saveRDS(NHANES2015_2016Microalb,
        file = "/Users/jacob/Desktop/Grad School/NHANES Prediabetes Metabolic Profiles/Data/Processed Dataframes/NHANES2015_2016Microalb.rds")

