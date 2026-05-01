# Load Packages

library(srvyr)
library(dplyr)
library(gtsummary)
library(gt)

# Filter Out Incomplete Data

nhanes_for_tbl1 <- combined_all %>%
  filter(!is.na(cluster) & !is.na(WTAF2YR_adj))

# Define weighted design (weights only)

nhanes_svy <- nhanes_for_tbl1 %>%
  as_survey_design(
    ids = 1,
    weights = WTAF2YR_adj
  )

# Quick Cluster Distribution Check

nhanes_svy %>%
  group_by(cluster) %>%
  summarise(n = survey_total())

# Define Cont. and Cat. Variables
cont_vars <- c("RIDAGEYR", "INDFMPIR")  # continuous variables
cat_vars  <- c("RIAGENDR", "RIDRETH1", "DMDEDUC2", "HIQ011", "DMDMARTZ", "DMDBORN4")

# Weighted means (continuous) by cluster
cont_summary <- nhanes_svy %>%
  group_by(cluster) %>%
  summarise(
    across(
      all_of(cont_vars),
      list(mean = ~survey_mean(., na.rm = TRUE),
           sd   = ~sqrt(survey_var(., na.rm = TRUE)))
    )
  ) %>%
  ungroup() %>%

# Combine mean (SD) into one column for each variable
  mutate(
    RIDAGEYR = paste0(round(RIDAGEYR_mean,1), " (", round(RIDAGEYR_sd,1), ")"),
    INDFMPIR = paste0(round(INDFMPIR_mean,2), " (", round(INDFMPIR_sd,2), ")")
  ) %>%
  select(cluster, all_of(cont_vars))

# Weighted proportions (categorical) by cluster
cat_summary <- nhanes_svy %>%
  group_by(cluster) %>%
  summarise(
    across(
      all_of(cat_vars),
      ~survey_mean(as.factor(.), vartype = "ci", na.rm = TRUE)
    )
  )

# Combine continuous and categorical summaries

# Continuous table
cont_long <- cont_summary %>%
  tidyr::pivot_longer(-cluster, names_to = "Variable", values_to = "Value")

# Categorical table
cat_long <- cat_summary %>%
  tidyr::pivot_longer(-cluster, names_to = "Variable", values_to = "Value") %>%
  mutate(Value = paste0(round(Value*100,1), "%"))

# Combine
tbl_long <- bind_rows(cont_long, cat_long)

# Create a publication-ready table with gt
tbl_gt <- tbl_long %>%
  gt() %>%
  tab_header(
    title = "SDOH Characteristics by Cluster"
  ) %>%
  cols_label(
    cluster = "Cluster",
    Variable = "Variable",
    Value = "Weighted Mean (SD) / %"
  ) %>%
  cols_align(align = "center", everything()) %>%
  opt_row_striping()

# Export as PNG
gtsave(tbl_gt, "SDOH_Table1.png", zoom = 2, vwidth = 2500)