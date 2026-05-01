# Load Packages
library(dplyr)
library(survey)
library(gtsummary)
library(gt)
options(survey.lonely.psu = "adjust")

# Recode clinical variables not already in combined_all
combined_all <- combined_all %>%
  mutate(
    hypertension = factor(
      ifelse(BPQ020 %in% c(1, 2), BPQ020, NA),
      levels = c(1, 2), labels = c("Yes", "No")
    ),
    hyperlipidemia = factor(
      ifelse(BPQ080 %in% c(1, 2), BPQ080, NA),
      levels = c(1, 2), labels = c("Yes", "No")
    ),
    bp_meds = factor(
      ifelse(BPQ050A %in% c(1, 2), BPQ050A, NA),
      levels = c(1, 2), labels = c("Yes", "No")
    ),
    statins = factor(
      ifelse(BPQ100D %in% c(1, 2), BPQ100D, NA),
      levels = c(1, 2), labels = c("Yes", "No")
    ),
    ASCVD_10yr = suppressWarnings(as.numeric(ASCVD_10yr)),
    microalbuminuria = factor(
      ifelse(microalbuminuria %in% c(1, 0), microalbuminuria, NA),
      levels = c(1, 0), labels = c("Yes", "No")
    )
  )

# Analytic dataset
dat_ana_clin <- combined_all %>%
  filter(!is.na(cluster),
         !is.na(SDMVPSU), !is.na(SDMVSTRA),
         !is.na(WTAF2YR_adj), WTAF2YR_adj > 0)

# Survey design
des_ana_clin <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTAF2YR_adj,
  nest    = TRUE,
  data    = dat_ana_clin
)

# Weighted clinical table
tbl_clin <- tbl_svysummary(
  des_ana_clin,
  by = cluster,
  include = c(hypertension, hyperlipidemia, bp_meds, statins, ASCVD_10yr, microalbuminuria),
  statistic = list(
    ASCVD_10yr      ~ "{mean} ({sd})",
    all_categorical() ~ "{p}%"
  ),
  label = list(
    hypertension     ~ "Hypertension, %",
    hyperlipidemia   ~ "Hyperlipidemia, %",
    bp_meds          ~ "Blood pressure medication, %",
    statins          ~ "Statin use, %",
    ASCVD_10yr       ~ "10-year ASCVD risk, %",
    microalbuminuria ~ "Microalbuminuria, %"
  ),
  missing = "no",
  digits = list(
    ASCVD_10yr        ~ 1,
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
  modify_caption("**Table X. Clinical Characteristics by Prediabetes Phenotype Cluster**")

tbl_clin

# Save
tbl_clin %>%
  as_flex_table() %>%
  save_as_docx(path = "Data/Output/clinical_characteristics_table.docx")