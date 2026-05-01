# Load Packages
library(dplyr)
library(survey)
library(gtsummary)
library(gt)
options(survey.lonely.psu = "adjust")

# Analytic dataset — variables already recoded in combined_all
dat_ana <- combined_all %>%
  filter(!is.na(cluster),
         !is.na(SDMVPSU), !is.na(SDMVSTRA),
         !is.na(WTAF2YR_adj), WTAF2YR_adj > 0) %>%
  mutate(
    age      = RIDAGEYR,
    pir      = INDFMPIR,
    sex      = gender,
    race_eth = race,
    educ = factor(
      ifelse(DMDEDUC2 %in% 1:5, DMDEDUC2, NA),
      levels = 1:5,
      labels = c("<9th Grade", "9-11th Grade", "HS Graduate", "Some College", "College Degree")
    ),
    insured = insurance,
    marital = factor(
      ifelse(DMDMARTZ %in% 1:3, DMDMARTZ, NA),
      levels = 1:3,
      labels = c("Married/Living with Partner", "Widowed/Divorced/Separated", "Never Married")
    ),
    nativity = factor(
      ifelse(DMDBORN4 %in% c(1, 2), DMDBORN4, NA),
      levels = c(1, 2),
      labels = c("USA", "Other")
    ),
    food_sec = factor(
      ifelse(FSDAD %in% 1:4, FSDAD, NA),
      levels = 1:4,
      labels = c("Full Food Security", "Marginal Security", "Low Food Security", "Very Low Food Security")
    )
  )

# Survey design
des_ana <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTAF2YR_adj,
  nest    = TRUE,
  data    = dat_ana
)

# Weighted demographic table
tbl_demo <- tbl_svysummary(
  des_ana,
  by = cluster,
  include = c(age, sex, race_eth, educ, pir, insured, marital, nativity, food_sec),
  statistic = list(
    age ~ "{mean} ({sd})",
    pir ~ "{mean} ({sd})",
    all_categorical() ~ "{p}%"
  ),
  label = list(
    age      ~ "Age, years",
    sex      ~ "Sex, %",
    race_eth ~ "Race/Ethnicity, %",
    educ     ~ "Education, %",
    pir      ~ "Income-to-Poverty Ratio",
    insured  ~ "Health Insurance, %",
    marital  ~ "Marital Status, %",
    nativity ~ "Nativity, %",
    food_sec ~ "Food Security, %"
  ),
  missing = "no",
  digits = list(
    all_continuous()  ~ 1,
    all_categorical() ~ 1
  )
) %>%
  add_p() %>%
  modify_header(
    stat_1 ~ "**Cluster 4 (Ref)**\n n = 916",
    stat_2 ~ "**Cluster 1**\n n = 612",
    stat_3 ~ "**Cluster 2**\n n = 1,184",
    stat_4 ~ "**Cluster 3**\n n = 871",
    stat_5 ~ "**Cluster 5**\n n = 438"
  ) %>%
  modify_caption("**Table X. Demographic Characteristics by Prediabetes Phenotype Cluster**")

tbl_demo

# Save
tbl_demo %>%
  as_flex_table() %>%
  save_as_docx(path = "Data/Output/demographic_characteristics_table.docx")