# Load Packages
library(dplyr)
library(survey)
library(gtsummary)
library(gt)
options(survey.lonely.psu = "adjust")

# Analytic dataset
dat_ana_bhev <- combined_all %>%
  filter(!is.na(cluster),
         !is.na(SDMVPSU), !is.na(SDMVSTRA),
         !is.na(WTAF2YR_adj), WTAF2YR_adj > 0)

# Survey design
des_ana_bhev <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTAF2YR_adj,
  nest    = TRUE,
  data    = dat_ana_bhev
)

# Weighted behavior table
tbl_bhev <- tbl_svysummary(
  des_ana_bhev,
  by = cluster,
  include = c(smoking, ALQ151_binary, physical_activity, PAD680, SLD012, depression),
  statistic = list(
    PAD680 ~ "{mean} ({sd})",
    SLD012 ~ "{mean} ({sd})",
    all_categorical() ~ "{p}%"
  ),
  label = list(
    smoking           ~ "Ever smoked 100+ cigarettes, %",
    ALQ151_binary     ~ "Ever heavy drinker (4/5+ drinks daily), %",
    physical_activity ~ "Physically active (≥150 min/week), %",
    PAD680            ~ "Sedentary time, min/day",
    SLD012            ~ "Weekday sleep, hours",
    depression        ~ "Depressive symptoms (PHQ-9), %"
  ),
  missing = "no",
  digits = list(
    PAD680 ~ 1,
    SLD012 ~ 1,
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
  modify_caption("**Table X. Behavioral Characteristics by Prediabetes Phenotype Cluster**")

tbl_bhev

# Dietary analytic dataset
dat_ana_dr <- combined_all %>%
  filter(!is.na(cluster),
         !is.na(SDMVPSU), !is.na(SDMVSTRA),
         !is.na(WTDRD1_adj), WTDRD1_adj > 0)

# Dietary survey design
des_ana_dr <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTDRD1_adj,
  nest    = TRUE,
  data    = dat_ana_dr
)

# Sugar table
tbl_sugar <- tbl_svysummary(
  des_ana_dr,
  by = cluster,
  include = DR1TSUGR,
  statistic = list(DR1TSUGR ~ "{mean} ({sd})"),
  label = list(DR1TSUGR ~ "Daily sugar intake, g"),
  missing = "no",
  digits = list(DR1TSUGR ~ 1)
) %>%
  add_p() %>% modify_header(
    stat_1 ~ "**Cluster 4 (Ref)**\n n = 916",
    stat_2 ~ "**Cluster 1**\n n = 612",
    stat_3 ~ "**Cluster 2**\n n = 1,184",
    stat_4 ~ "**Cluster 3**\n n = 871",
    stat_5 ~ "**Cluster 5**\n n = 438"
  ) %>%
  modify_caption("**Table X. Sugar Intake by Prediabetes Phenotype Cluster (Dietary Recall Weight)**")

tbl_sugar

# Save main behavior table
tbl_bhev %>%
  as_flex_table() %>%
  save_as_docx(path = "Data/Output/behavioral_characteristics_table.docx")

# Save sugar table
tbl_sugar %>%
  as_flex_table() %>%
  save_as_docx(path = "Data/Output/sugar_intake_table.docx")