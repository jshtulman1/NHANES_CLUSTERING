# Load Packages

library(dplyr)

# Merging Data Frames

NHANES_Combined_Processed <- bind_rows(
  NHANES2015_2016_Processed,
  NHANES2017_2020_Processed,
  NHANES2021_2023_Processed
)

# Saving Merged Data Frame

saveRDS(NHANES_Combined_Processed,
        file = "/Users/jacob/Desktop/Grad School/NHANES Prediabetes Metabolic Profiles/Data/Processed Dataframes/NHANES_Combined_Processed.rds")

