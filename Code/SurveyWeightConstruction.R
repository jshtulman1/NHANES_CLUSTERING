# Loading Packages

library(dplyr)

# Checking Most Restrictive Weight

# List of weights
weights <- c("WTAF2YR", "WTMEC2YR", "WTINT2YR")

# Count non-missing values for each weight
weight_n <- sapply(weights, function(w) {
  sum(!is.na(NHANES_Combined_Processed[[w]]))
})

# Sort from smallest to largest
weight_n <- sort(weight_n)
weight_n

# New Weight Variable

NHANES_Combined_I <- NHANES_Combined_I %>%
  mutate(
    WTAF2YR_adj  = case_when(
      Cycle == 2015 ~ WTAF2YR  * (2 / 7.2),
      Cycle == 2017 ~ WTAF2YR  * (3.2 / 7.2),
      Cycle == 2021 ~ WTAF2YR  * (2 / 7.2),
      TRUE ~ WTAF2YR
    ),
    WTMEC2YR_adj = case_when(
      Cycle == 2015 ~ WTMEC2YR * (2 / 7.2),
      Cycle == 2017 ~ WTMEC2YR * (3.2 / 7.2),
      Cycle == 2021 ~ WTMEC2YR * (2 / 7.2),
      TRUE ~ WTMEC2YR
    ),
    WTINT2YR_adj = case_when(
      Cycle == 2015 ~ WTINT2YR * (2 / 7.2),
      Cycle == 2017 ~ WTINT2YR * (3.2 / 7.2),
      Cycle == 2021 ~ WTINT2YR * (2 / 7.2),
      TRUE ~ WTINT2YR
    )
  )
