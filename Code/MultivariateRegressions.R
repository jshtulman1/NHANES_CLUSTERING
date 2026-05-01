# Load Packages

library(dplyr)

library(survey)

library(haven)

library(MASS)

library(flextable)

library(officer)

library(nnet)

# Variables Needed to Recode

# Age, Sex/Gender, Race/Ethnicity, BMI, Income/Poverty ratio, insurance status, smoking, drinking, and physical activity

# Age <- No need to recode

# Gender

combined_all <- combined_all %>%
  mutate(
    gender = factor(
      ifelse(RIAGENDR %in% c(1, 2), RIAGENDR, NA),
      levels = c(1, 2),
      labels = c("Male", "Female")
    )
  )

# Race and Ethnicity

combined_all <- combined_all %>%
  mutate(
    race = factor(
      ifelse(RIDRETH1 %in% 1:5, RIDRETH1, NA),
      levels = 1:5,
      labels = c("Mexican American", "Other Hispanic", "Non-Hispanic White",
                 "Non-Hispanic Black", "Other Race - Including Multi-Racial")
    )
  )

# BMI - no need to recode

# Income-to-Poverty Ratio - no need to recode

# Insurance Status

combined_all <- combined_all %>%
  mutate(
    insurance = factor(
      ifelse(HIQ011 %in% c(1, 2), HIQ011, NA),
      levels = c(1, 2),
      labels = c("Yes", "No")
    )
  )

# Smoking

combined_all <- combined_all %>%
  mutate(
    smoking = factor(
      ifelse(SMQ020 %in% c(1, 2), SMQ020, NA),
      levels = c(2, 1),
      labels = c("No", "Yes")
    )
  )

# drinking - already factored

# physical activity <- using PAD680

combined_all <- combined_all %>%
  mutate(
    PAD680 = na_if(na_if(as.numeric(PAD680), 7777), 9999)
  )

# Fix Weight

combined_all <- combined_all %>%
  mutate(
    WTSAF2YR_combined = coalesce(WTSAF2YR, WTAF2YR),
    WTAF2YR_adj = case_when(
      Cycle == 2015 ~ WTSAF2YR_combined * (2 / 7.2),
      Cycle == 2017 ~ WTSAF2YR_combined * (3.2 / 7.2),
      Cycle == 2021 ~ WTSAF2YR_combined * (2 / 7.2),
      TRUE ~ WTAF2YR_adj
    )
  )

# Regressions:

# Smoking

# Relevel cluster
combined_all$cluster <- relevel(factor(combined_all$cluster), ref = "4")

nhanes_design_fast <- svydesign(
  ids     = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTAF2YR_adj,
  data    = combined_all %>% filter(!is.na(WTAF2YR_adj) & WTAF2YR_adj > 0),
  nest    = TRUE
)

# Unadjusted

model_smoking_unadj <- svyglm(
  smoking ~ cluster,
  design = nhanes_design_fast,
  family = quasibinomial()
)

exp(coef(model_smoking_unadj))
exp(confint(model_smoking_unadj))

# Adjusted

model_smoking <- svyglm(
  smoking ~ cluster + RIDAGEYR + gender + race + INDFMPIR + insurance + ALQ151_binary + PAD680,
  design = nhanes_design_fast,
  family = quasibinomial()
)

summary(model_smoking)
exp(coef(model_smoking))      # odds ratios
exp(confint(model_smoking))   # 95% 


# Sugar Intake

# Set up survey design with dietary weight

nhanes_design_dr <- svydesign(
  ids     = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTDRD1_adj,
  data    = combined_all,
  nest    = TRUE
)

#Unadjusted

model_sugar_unadj <- svyglm(
  DR1TSUGR ~ cluster,
  design = nhanes_design_dr,
  family = gaussian()
)

coef(model_sugar_unadj)
confint(model_sugar_unadj)

# Adjusted

model_sugar <- svyglm(
  DR1TSUGR ~ cluster + RIDAGEYR + gender + race + INDFMPIR + insurance + smoking + ALQ151_binary + PAD680,
  design = nhanes_design_dr,
  family = gaussian()
)

summary(model_sugar)
coef(model_sugar)         # betas
confint(model_sugar)      # 95% CIs

# Sleep

# Load sleep data for each cycle
slq_2015 <- read_xpt("Data/NHANES 2015_2016/SLQ_I.xpt")
slq_2017 <- read_xpt("Data/NHANES 2017_2020/P_SLQ.xpt")
slq_2021 <- read_xpt("Data/NHANES 2021_2023/SLQ_L.xpt")

# Select only SEQN and SLD012 from each
slq_2015 <- slq_2015 %>% select(SEQN, SLD012)
slq_2017 <- slq_2017 %>% select(SEQN, SLD012)
slq_2021 <- slq_2021 %>% select(SEQN, SLD012)

# Bind and join to combined_all
slq_combined <- bind_rows(slq_2015, slq_2017, slq_2021)

combined_all <- combined_all %>%
  left_join(slq_combined, by = "SEQN")

# Unadjusted Model

model_sleep_unadj <- svyglm(
  SLD012 ~ cluster,
  design = nhanes_design_fast,
  family = gaussian()
)

coef(model_sleep_unadj)
confint(model_sleep_unadj)

# Adjusted Model

model_sleep_adj <- svyglm(
  SLD012 ~ cluster + RIDAGEYR + gender + race + INDFMPIR + insurance + smoking + ALQ151_binary + PAD680,
  design = nhanes_design_fast,
  family = gaussian()
)

coef(model_sleep_adj)
confint(model_sleep_adj)

# Alcohol

# Unadjusted

model_alcohol_unadj <- svyglm(
  ALQ151_binary ~ cluster,
  design = nhanes_design_fast,
  family = quasibinomial()
)

exp(coef(model_alcohol_unadj))
exp(confint(model_alcohol_unadj))

# Adjusted

model_alcohol_adj <- svyglm(
  ALQ151_binary ~ cluster + RIDAGEYR + gender + race + INDFMPIR + insurance + smoking + PAD680,
  design = nhanes_design_fast,
  family = quasibinomial()
)

exp(coef(model_alcohol_adj))
exp(confint(model_alcohol_adj))

# MVPA

# Unadjusted

model_pa_unadj <- svyglm(
  physical_activity ~ cluster,
  design = nhanes_design_fast,
  family = quasibinomial()
)

exp(coef(model_pa_unadj))
exp(confint(model_pa_unadj))

# Adjusted

model_pa_adj <- svyglm(
  physical_activity ~ cluster + RIDAGEYR + gender + race + INDFMPIR + insurance + smoking + ALQ151_binary + PAD680,
  design = nhanes_design_fast,
  family = quasibinomial()
)

exp(coef(model_pa_adj))
exp(confint(model_pa_adj))

# Depression

# Recode Variable

combined_all <- combined_all %>%
  mutate(
    depression = factor(
      case_when(
        DPQ020 == 0 ~ 0,
        DPQ020 == 1 ~ 1,
        DPQ020 %in% c(2, 3) ~ 2,
        TRUE ~ NA_real_
      ),
      levels = 0:2,
      labels = c("Not at all", "Several days", "More than half the days or more"),
      ordered = TRUE
    )
  )

# Unadjusted

model_depression_unadj <- svyolr(
  depression ~ cluster,
  design = nhanes_design_fast
)

exp(coef(model_depression_unadj))
exp(confint(model_depression_unadj))

# Adjusted

model_depression_adj <- svyolr(
  depression ~ cluster + RIDAGEYR + gender + race + INDFMPIR + insurance + smoking + ALQ151_binary + PAD680,
  design = nhanes_design_fast
)

exp(coef(model_depression_adj))
exp(confint(model_depression_adj))

# Binary depression variable
combined_all <- combined_all %>%
  mutate(
    depression_binary = factor(
      case_when(
        DPQ020 == 0 ~ 0,
        DPQ020 %in% 1:3 ~ 1,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1),
      labels = c("No symptoms", "Any symptoms")
    )
  )

# Update survey design
nhanes_design_fast <- svydesign(
  ids     = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTAF2YR_adj,
  data    = combined_all %>% filter(!is.na(WTAF2YR_adj) & WTAF2YR_adj > 0),
  nest    = TRUE
)

# Unadjusted
model_depression_bin_unadj <- svyglm(
  depression_binary ~ cluster,
  design = nhanes_design_fast,
  family = quasibinomial()
)

exp(coef(model_depression_bin_unadj))
exp(confint(model_depression_bin_unadj))

# Adjusted
model_depression_bin_adj <- svyglm(
  depression_binary ~ cluster + RIDAGEYR + gender + race + INDFMPIR + 
    insurance + smoking + ALQ151_binary + PAD680,
  design = nhanes_design_fast,
  family = quasibinomial()
)

exp(coef(model_depression_bin_adj))
exp(confint(model_depression_bin_adj))

# Table

extract_results <- function(model, type = "or") {
  est <- if (type == "or") exp(coef(model)) else coef(model)
  ci  <- if (type == "or") exp(confint(model)) else confint(model)
  
  data.frame(
    term     = names(est),
    estimate = round(est, 2),
    lower    = round(ci[, 1], 2),
    upper    = round(ci[, 2], 2)
  ) %>%
    filter(grepl("cluster", term)) %>%
    mutate(
      label = paste0(estimate, " (", lower, ", ", upper, ")"),
      sig   = !(lower <= ifelse(type == "or", 1, 0) & upper >= ifelse(type == "or", 1, 0))
    )
}

# Extract results from all models 
smoking_unadj <- extract_results(model_smoking_unadj, "or")
smoking_adj   <- extract_results(model_smoking,       "or")

sugar_unadj   <- extract_results(model_sugar_unadj,   "beta")
sugar_adj     <- extract_results(model_sugar,         "beta")

sleep_unadj   <- extract_results(model_sleep_unadj,   "beta")
sleep_adj     <- extract_results(model_sleep_adj,     "beta")

alcohol_unadj <- extract_results(model_alcohol_unadj, "or")
alcohol_adj   <- extract_results(model_alcohol_adj,   "or")

pa_unadj      <- extract_results(model_pa_unadj,      "or")
pa_adj        <- extract_results(model_pa_adj,        "or")

dep_unadj     <- extract_results(model_depression_unadj, "or")
dep_adj       <- extract_results(model_depression_adj,   "or")

# Build combined table
cluster_labels <- c("cluster1" = "Cluster 1", "cluster2" = "Cluster 2",
                    "cluster3" = "Cluster 3", "cluster5" = "Cluster 5")

build_col <- function(unadj, adj) {
  unadj <- unadj %>% mutate(term = gsub("cluster", "cluster", term))
  adj   <- adj   %>% mutate(term = gsub("cluster", "cluster", term))
  
  left_join(
    unadj %>% select(term, unadj_label = label, unadj_sig = sig),
    adj   %>% select(term, adj_label   = label, adj_sig   = sig),
    by = "term"
  )
}

results <- build_col(smoking_unadj, smoking_adj) %>%
  rename(sm_unadj = unadj_label, sm_adj = adj_label,
         sm_unadj_sig = unadj_sig, sm_adj_sig = adj_sig) %>%
  left_join(build_col(sugar_unadj,   sugar_adj)   %>% rename(su_unadj = unadj_label, su_adj = adj_label, su_unadj_sig = unadj_sig, su_adj_sig = adj_sig),   by = "term") %>%
  left_join(build_col(sleep_unadj,   sleep_adj)   %>% rename(sl_unadj = unadj_label, sl_adj = adj_label, sl_unadj_sig = unadj_sig, sl_adj_sig = adj_sig),   by = "term") %>%
  left_join(build_col(alcohol_unadj, alcohol_adj) %>% rename(al_unadj = unadj_label, al_adj = adj_label, al_unadj_sig = unadj_sig, al_adj_sig = adj_sig), by = "term") %>%
  left_join(build_col(pa_unadj,      pa_adj)      %>% rename(pa_unadj = unadj_label, pa_adj = adj_label, pa_unadj_sig = unadj_sig, pa_adj_sig = adj_sig),      by = "term") %>%
  left_join(build_col(dep_unadj,     dep_adj)     %>% rename(de_unadj = unadj_label, de_adj = adj_label, de_unadj_sig = unadj_sig, de_adj_sig = adj_sig),     by = "term") %>%
  mutate(Cluster = recode(term, !!!cluster_labels)) %>%
  select(Cluster,
         sm_unadj, sm_unadj_sig, sm_adj, sm_adj_sig,
         su_unadj, su_unadj_sig, su_adj, su_adj_sig,
         sl_unadj, sl_unadj_sig, sl_adj, sl_adj_sig,
         al_unadj, al_unadj_sig, al_adj, al_adj_sig,
         pa_unadj, pa_unadj_sig, pa_adj, pa_adj_sig,
         de_unadj, de_unadj_sig, de_adj, de_adj_sig)

# Build flextable
display_cols <- c("Cluster",
                  "sm_unadj", "sm_adj",
                  "su_unadj", "su_adj",
                  "sl_unadj", "sl_adj",
                  "al_unadj", "al_adj",
                  "pa_unadj", "pa_adj",
                  "de_unadj", "de_adj")

sig_cols <- c("sm_unadj_sig", "sm_adj_sig",
              "su_unadj_sig", "su_adj_sig",
              "sl_unadj_sig", "sl_adj_sig",
              "al_unadj_sig", "al_adj_sig",
              "pa_unadj_sig", "pa_adj_sig",
              "de_unadj_sig", "de_adj_sig")

ft <- flextable(results %>% select(all_of(display_cols))) %>%
  # Column headers
  set_header_labels(
    Cluster  = "Cluster",
    sm_unadj = "Unadjusted", sm_adj = "Adjusted",
    su_unadj = "Unadjusted", su_adj = "Adjusted",
    sl_unadj = "Unadjusted", sl_adj = "Adjusted",
    al_unadj = "Unadjusted", al_adj = "Adjusted",
    pa_unadj = "Unadjusted", pa_adj = "Adjusted",
    de_unadj = "Unadjusted", de_adj = "Adjusted"
  ) %>%
  # Outcome group headers
  add_header_row(
    values = c("", "Smoking\n(OR, 95% CI)", "Sugar Intake\n(β, 95% CI)",
               "Sleep Hours\n(β, 95% CI)", "Alcohol\n(OR, 95% CI)",
               "Physical Activity\n(OR, 95% CI)", "Depression\n(OR, 95% CI)"),
    colwidths = c(1, 2, 2, 2, 2, 2, 2)
  ) %>%
  # Bold significant cells
  {
    ft_tmp <- .
    for (i in seq_along(sig_cols)) {
      data_col  <- display_cols[i + 1]   # +1 to skip "Cluster"
      sig_col   <- sig_cols[i]
      sig_rows  <- which(results[[sig_col]])
      if (length(sig_rows) > 0) {
        ft_tmp <- bold(ft_tmp, i = sig_rows, j = data_col)
      }
    }
    ft_tmp
  } %>%
  # Styling
  bold(part = "header") %>%
  bg(part = "header", bg = "#2E4057") %>%
  color(part = "header", color = "white") %>%
  bg(i = seq(1, nrow(results), 2), bg = "#F5F8FA") %>%
  align(align = "center", part = "all") %>%
  align(j = "Cluster", align = "left", part = "body") %>%
  fontsize(size = 9, part = "all") %>%
  font(fontname = "Arial", part = "all") %>%
  set_table_properties(layout = "autofit") %>%
  add_footer_lines(
    "Note: Bold = statistically significant (95% CI does not cross null). Reference = Cluster 4. OR = odds ratio; β = regression coefficient. Adjusted models include age, gender, race/ethnicity, income-to-poverty ratio, insurance, and relevant behavioral covariates. BMI excluded from physical activity model. Survey weights: WTAF2YR_adj (all models except sugar: WTDRD1_adj)."
  ) %>%
  fontsize(size = 8, part = "footer") %>%
  italic(part = "footer")

# Save to Word

doc <- read_docx() %>%
  body_add_par("Table X. Association of Prediabetes Phenotype Clusters with Behavioral Outcomes",
               style = "heading 1") %>%
  body_add_par("NHANES 2015–2023 | Survey-Weighted Regression | Reference: Cluster 4",
               style = "Normal") %>%
  body_add_par("") %>%
  body_add_flextable(ft) %>%
  body_end_section_landscape()

print(doc, target = "posthoc_results_table.docx")
cat("Saved to posthoc_results_table.docx\n")


# Demographic Variable Regressions:

# Age

# Unadjusted
model_age_unadj <- svyglm(
  RIDAGEYR ~ cluster,
  design = nhanes_design_fast,
  family = gaussian()
)

coef(model_age_unadj)
confint(model_age_unadj)

# Adjusted 
model_age_adj <- svyglm(
  RIDAGEYR ~ cluster + gender + race + INDFMPIR + insurance + smoking + ALQ151_binary + PAD680,
  design = nhanes_design_fast,
  family = gaussian()
)

coef(model_age_adj)
confint(model_age_adj)

# Gender

# Unadjusted
model_gender_unadj <- svyglm(
  gender ~ cluster,
  design = nhanes_design_fast,
  family = quasibinomial()
)

exp(coef(model_gender_unadj))
exp(confint(model_gender_unadj))

# Adjusted
model_gender_adj <- svyglm(
  gender ~ cluster + RIDAGEYR + race + INDFMPIR + insurance + smoking + ALQ151_binary + PAD680,
  design = nhanes_design_fast,
  family = quasibinomial()
)

exp(coef(model_gender_adj))
exp(confint(model_gender_adj))

# Race

# Create binary race indicators
combined_all <- combined_all %>%
  mutate(
    race_mexican     = factor(ifelse(race == "Mexican American", 1, 0),                   levels = c(0, 1)),
    race_hispanic    = factor(ifelse(race == "Other Hispanic", 1, 0),                     levels = c(0, 1)),
    race_black       = factor(ifelse(race == "Non-Hispanic Black", 1, 0),                 levels = c(0, 1)),
    race_other       = factor(ifelse(race == "Other Race - Including Multi-Racial", 1, 0), levels = c(0, 1))
  )
# Non-Hispanic White is the reference (no binary needed — it's the absence of all others)

# Rebuild design
nhanes_design_fast <- svydesign(
  ids     = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTAF2YR_adj,
  data    = combined_all %>% filter(!is.na(WTAF2YR_adj) & WTAF2YR_adj > 0),
  nest    = TRUE
)

# --- Mexican American ---
model_race_mexican_unadj <- svyglm(race_mexican ~ cluster,
                                   design = nhanes_design_fast, family = quasibinomial())
model_race_mexican_adj <- svyglm(race_mexican ~ cluster + RIDAGEYR + gender + INDFMPIR + insurance + smoking + ALQ151_binary + PAD680,
                                 design = nhanes_design_fast, family = quasibinomial())

# --- Other Hispanic ---
model_race_hispanic_unadj <- svyglm(race_hispanic ~ cluster,
                                    design = nhanes_design_fast, family = quasibinomial())
model_race_hispanic_adj <- svyglm(race_hispanic ~ cluster + RIDAGEYR + gender + INDFMPIR + insurance + smoking + ALQ151_binary + PAD680,
                                  design = nhanes_design_fast, family = quasibinomial())

# --- Non-Hispanic Black ---
model_race_black_unadj <- svyglm(race_black ~ cluster,
                                 design = nhanes_design_fast, family = quasibinomial())
model_race_black_adj <- svyglm(race_black ~ cluster + RIDAGEYR + gender + INDFMPIR + insurance + smoking + ALQ151_binary + PAD680,
                               design = nhanes_design_fast, family = quasibinomial())

# --- Other Race ---
model_race_other_unadj <- svyglm(race_other ~ cluster,
                                 design = nhanes_design_fast, family = quasibinomial())
model_race_other_adj <- svyglm(race_other ~ cluster + RIDAGEYR + gender + INDFMPIR + insurance + smoking + ALQ151_binary + PAD680,
                               design = nhanes_design_fast, family = quasibinomial())

# Extract results
race_mexican_unadj  <- extract_results(model_race_mexican_unadj,  "or")
race_mexican_adj    <- extract_results(model_race_mexican_adj,     "or")

race_hispanic_unadj <- extract_results(model_race_hispanic_unadj, "or")
race_hispanic_adj   <- extract_results(model_race_hispanic_adj,   "or")

race_black_unadj    <- extract_results(model_race_black_unadj,    "or")
race_black_adj      <- extract_results(model_race_black_adj,      "or")

race_other_unadj    <- extract_results(model_race_other_unadj,    "or")
race_other_adj      <- extract_results(model_race_other_adj,      "or")

# Display Results
race_results <- bind_rows(
  bind_cols(group = "Mexican American",    extract_results(model_race_mexican_unadj,  "or") %>% rename(unadj = label), extract_results(model_race_mexican_adj,  "or") %>% select(adj = label)),
  bind_cols(group = "Other Hispanic",      extract_results(model_race_hispanic_unadj, "or") %>% rename(unadj = label), extract_results(model_race_hispanic_adj, "or") %>% select(adj = label)),
  bind_cols(group = "Non-Hispanic Black",  extract_results(model_race_black_unadj,    "or") %>% rename(unadj = label), extract_results(model_race_black_adj,    "or") %>% select(adj = label)),
  bind_cols(group = "Other Race",          extract_results(model_race_other_unadj,    "or") %>% rename(unadj = label), extract_results(model_race_other_adj,    "or") %>% select(adj = label))
)

print(race_results)

