# Packages

library(dplyr)

# Adding Sugar Intake and Alcohol Consumption to Data Set

combined_all <- left_join(NHANES_Combined_I, sugars_alc, by = "SEQN")

# Creating Combined Survey Weight

combined_all <- combined_all %>%
  mutate(
    WTDRD1_adj = case_when(
      Cycle == 2015 ~ WTDRD1 * (2 / 7.2),
      Cycle == 2017 ~ WTDRD1 * (3.2 / 7.2),
      Cycle == 2021 ~ WTDRD1 * (2 / 7.2),
      TRUE ~ WTDRD1
    )
  )

# Check

summary(combined_all$WTDRD1_adj)

# Save

saveRDS(combined_all, file = "Data/Processed Dataframes/combined_all.rds")


