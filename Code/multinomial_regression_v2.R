# ============================================================
# MULTINOMIAL LOGISTIC REGRESSION — V2 CLUSTERS
# Survey-weighted using svyVGAM
#
# Model 2: Age, sex, race/ethnicity
# Model 3: Model 2 + SES (education, PIR, insurance, food security)
# Model 4: Model 2 + Behavioral/Treatment (smoking, alcohol,
#           physical activity, sleep, BP meds, statins)
# Sensitivity: Model 2 + Dietary (WTDRD1 subsample)
#
# Reference cluster: Cluster 4 (youngest, metabolically early)
# ============================================================

library(dplyr)
library(survey)
library(VGAM)
library(svyVGAM)
options(survey.lonely.psu = "adjust")


# ============================================================
# STEP 1: PREPARE ANALYTIC DATASET
# ============================================================

cat("╔══════════════════════════════════════════════╗\n")
cat("║  STEP 1: PREPARE ANALYTIC DATASET             ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

dat_reg <- combined_all_v2 %>%
  filter(
    !is.na(cluster_v2),
    !is.na(SDMVPSU), !is.na(SDMVSTRA_unique),
    !is.na(WTAF2YR_adj), WTAF2YR_adj > 0
  ) %>%
  mutate(
    # Outcome: cluster with Cluster 4 as reference
    cluster_v2 = relevel(factor(cluster_v2), ref = "4"),

    # ---- Demographics (Model 2) ----
    age = RIDAGEYR,
    sex = factor(RIAGENDR, levels = c(1, 2), labels = c("Male", "Female")),
    race_eth = factor(
      ifelse(RIDRETH1 %in% 1:5, RIDRETH1, NA),
      levels = c(3, 1, 2, 4, 5),
      labels = c("NH White", "Mexican American", "Other Hispanic",
                 "NH Black", "Other/Multi")
    ),

    # ---- SES (Model 3) ----
    educ = factor(
      ifelse(DMDEDUC2 %in% 1:5, DMDEDUC2, NA),
      levels = 1:5,
      labels = c("<9th", "9-11th", "HS Grad", "Some College", "College+")
    ),
    pir = INDFMPIR,
    insured = factor(
      ifelse(HIQ011 %in% c(1, 2), HIQ011, NA),
      levels = c(1, 2), labels = c("Yes", "No")
    ),
    food_sec = factor(
      ifelse(FSDAD %in% 1:4, FSDAD, NA),
      levels = 1:4,
      labels = c("Full", "Marginal", "Low", "Very Low")
    ),
    # Scale dietary variables for interpretable ORs
    DR1TKCAL_100 = DR1TKCAL / 100,    # per 100 kcal
    DR1TSUGR_10  = DR1TSUGR / 10,     # per 10g sugar
    DR1TFIBE_5   = DR1TFIBE / 5,      # per 5g fiber
    DR1TSFAT_10  = DR1TSFAT / 10,     # per 10g saturated fat

    # ---- Behavioral/Treatment (Model 4) ----
    current_drinker = factor(
      ifelse(current_drinker %in% c(0, 1), current_drinker, NA),
      levels = c(0, 1), labels = c("No", "Yes")
    ),
    sleep_hrs = SLD012,
    bp_meds = factor(
      ifelse(bp_med_corrected %in% c(0, 1), bp_med_corrected, NA),
      levels = c(0, 1), labels = c("No", "Yes")
    ),
    statin_use = relevel(statin_use, ref = "No")
  )

cat("Full analytic N:", nrow(dat_reg), "\n")
cat("Cluster distribution:\n")
print(table(dat_reg$cluster_v2))
cat("\nFactor level order (index 1-4 maps to these):\n")
cat(levels(dat_reg$cluster_v2), "\n")
cat("  Reference = Cluster 4\n")
cat("  Index 1 = Cluster 1, Index 2 = Cluster 2,",
    "Index 3 = Cluster 3, Index 4 = Cluster 5\n\n")


# ============================================================
# STEP 2: SURVEY DESIGNS
# ============================================================

cat("╔══════════════════════════════════════════════╗\n")
cat("║  STEP 2: SURVEY DESIGNS                       ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

des_reg <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA_unique,
  weights = ~WTAF2YR_adj,
  nest    = TRUE,
  data    = dat_reg
)

dat_diet <- dat_reg %>%
  filter(!is.na(WTDRD1_adj), WTDRD1_adj > 0)

des_diet <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA_unique,
  weights = ~WTDRD1_adj,
  nest    = TRUE,
  data    = dat_diet
)

cat("Fasting design N:", nrow(dat_reg), "\n")
cat("Dietary design N:", nrow(dat_diet), "\n\n")


# ============================================================
# HELPER: Extract ORs from svy_vglm
# ============================================================

# Cluster label mapping: svy_vglm indices → actual cluster labels
cluster_map <- c("1" = "Cluster 1", "2" = "Cluster 2",
                 "3" = "Cluster 3", "4" = "Cluster 5")

extract_or <- function(model, model_name) {
  coefs <- summary(model)$coeftable
  ci_low  <- coefs[, "Coef"] - 1.96 * coefs[, "SE"]
  ci_high <- coefs[, "Coef"] + 1.96 * coefs[, "SE"]

  results <- data.frame(
    term       = rownames(coefs),
    estimate   = coefs[, "Coef"],
    se         = coefs[, "SE"],
    z          = coefs[, "z"],
    p_value    = coefs[, "p"],
    ci_low     = ci_low,
    ci_high    = ci_high,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      OR      = round(exp(estimate), 2),
      OR_low  = round(exp(ci_low), 2),
      OR_high = round(exp(ci_high), 2),
      OR_CI   = paste0(OR, " (", OR_low, "-", OR_high, ")"),
      p_fmt   = case_when(
        p_value < 0.001 ~ "<0.001",
        TRUE            ~ formatC(p_value, format = "f", digits = 3)
      ),
      model = model_name
    ) %>%
    mutate(
      predictor   = gsub(":.*", "", term),
      cluster_idx = gsub(".*:", "", term),
      cluster     = cluster_map[cluster_idx]
    ) %>%
    filter(!grepl("Intercept", term))

  return(results)
}


# ============================================================
# STEP 3: FIT MODELS
# ============================================================

cat("╔══════════════════════════════════════════════╗\n")
cat("║  STEP 3: FIT MULTINOMIAL MODELS               ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

cat("Fitting Model 2 (demographics)...\n")
m2 <- svy_vglm(
  cluster_v2 ~ age + sex + race_eth,
  design = des_reg,
  family = multinomial(refLevel = "4")
)
cat("  ✓ Model 2 complete\n")

cat("Fitting Model 3 (demographics + SES)...\n")
m3 <- svy_vglm(
  cluster_v2 ~ age + sex + race_eth + educ + pir + insured + food_sec,
  design = des_reg,
  family = multinomial(refLevel = "4")
)
cat("  ✓ Model 3 complete\n")

cat("Fitting Model 4 (demographics + behavioral)...\n")
m4 <- svy_vglm(
  cluster_v2 ~ age + sex + race_eth + smoking_status + current_drinker +
    physically_active + sleep_hrs + bp_meds + statin_use,
  design = des_reg,
  family = multinomial(refLevel = "4")
)
cat("  ✓ Model 4 complete\n")

cat("Fitting Sensitivity model (demographics + dietary)...\n")
m_diet <- svy_vglm(
  cluster_v2 ~ age + sex + race_eth + DR1TKCAL_100 + DR1TSUGR_10 + DR1TFIBE_5 + DR1TSFAT_10,
  design = des_diet,
  family = multinomial(refLevel = "4")
)
cat("  ✓ Sensitivity model complete\n\n")


# ============================================================
# STEP 4: EXTRACT AND COMBINE RESULTS
# ============================================================

cat("╔══════════════════════════════════════════════╗\n")
cat("║  STEP 4: EXTRACT RESULTS                      ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

results_m2   <- extract_or(m2, "Model 2: Demographics")
results_m3   <- extract_or(m3, "Model 3: Demographics + SES")
results_m4   <- extract_or(m4, "Model 4: Demographics + Behavioral")
results_diet <- extract_or(m_diet, "Sensitivity: Demographics + Dietary")

all_results <- bind_rows(results_m2, results_m3, results_m4, results_diet)

cat("Total coefficients extracted:", nrow(all_results), "\n\n")


# ============================================================
# STEP 5: DISPLAY RESULTS
# ============================================================

cat("╔══════════════════════════════════════════════╗\n")
cat("║  STEP 5: KEY RESULTS                          ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

# ---- Model 2 ----
cat("=== MODEL 2: Core Demographics ===\n")
cat("(Reference: Cluster 4, Male, Mexican American)\n\n")
results_m2 %>%
  select(cluster, predictor, OR_CI, p_fmt) %>%
  as.data.frame() %>%
  print()

# ---- Model 3 ----
cat("\n=== MODEL 3: Demographics + SES ===\n")
cat("(Reference: Cluster 4, Male, Mexican American,\n")
cat(" College+, Insured, Full Food Security)\n\n")
results_m3 %>%
  select(cluster, predictor, OR_CI, p_fmt) %>%
  as.data.frame() %>%
  print()

# ---- Model 4 ----
cat("\n=== MODEL 4: Demographics + Behavioral/Treatment ===\n")
cat("(Reference: Cluster 4, Male, Mexican American,\n")
cat(" Never smoker, Non-drinker, Not active, No BP meds, No statins)\n\n")
results_m4 %>%
  select(cluster, predictor, OR_CI, p_fmt) %>%
  as.data.frame() %>%
  print()

# ---- Sensitivity ----
cat("\n=== SENSITIVITY: Demographics + Dietary (WTDRD1 subsample) ===\n\n")
results_diet %>%
  select(cluster, predictor, OR_CI, p_fmt) %>%
  as.data.frame() %>%
  print()


# ============================================================
# STEP 6: FORMATTED TABLES BY MODEL
# ============================================================

cat("\n╔══════════════════════════════════════════════╗\n")
cat("║  STEP 6: WIDE-FORMAT TABLES                   ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

# Reshape each model to wide: predictor rows × cluster columns
format_wide <- function(results_df, model_label) {
  wide <- results_df %>%
    select(cluster, predictor, OR_CI, p_fmt) %>%
    mutate(display = paste0(OR_CI, " (p=", p_fmt, ")")) %>%
    select(predictor, cluster, display) %>%
    tidyr::pivot_wider(names_from = cluster, values_from = display)
  
  cat(paste0("--- ", model_label, " ---\n"))
  cat("Reference: Cluster 4 (Young, Metabolically Early)\n\n")
  as.data.frame(wide) %>% print(right = FALSE)
  cat("\n")
  
  return(wide)
}

wide_m2   <- format_wide(results_m2, "Model 2: Demographics")
wide_m3   <- format_wide(results_m3, "Model 3: Demographics + SES")
wide_m4   <- format_wide(results_m4, "Model 4: Demographics + Behavioral")
wide_diet <- format_wide(results_diet, "Sensitivity: Dietary")

# ============================================================
# SAVE MODEL TABLES TO WORD
# ============================================================

library(flextable)
library(officer)

make_model_ft <- function(results_df, model_label, ref_note) {
  wide <- results_df %>%
    select(cluster, predictor, OR_CI, p_fmt) %>%
    tidyr::pivot_wider(
      names_from = cluster,
      values_from = c(OR_CI, p_fmt),
      names_glue = "{cluster}_{.value}"
    )
  
  # Reorder columns: predictor, then each cluster's OR and p
  clusters <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 5")
  col_order <- c("predictor",
                 unlist(lapply(clusters, function(cl) paste0(cl, c("_OR_CI", "_p_fmt")))))
  col_order <- col_order[col_order %in% names(wide)]
  wide <- wide[, col_order]
  
  # Rename columns for display
  names(wide) <- gsub("_OR_CI", "\nOR (95% CI)", names(wide))
  names(wide) <- gsub("_p_fmt", "\np-value", names(wide))
  names(wide)[1] <- "Predictor"
  
  ft <- flextable(wide) %>%
    font(fontname = "Arial", part = "all") %>%
    fontsize(size = 8, part = "body") %>%
    fontsize(size = 9, part = "header") %>%
    bold(part = "header") %>%
    bg(part = "header", bg = "#D9E2F3") %>%
    align(align = "center", part = "all") %>%
    align(j = 1, align = "left", part = "all") %>%
    width(j = 1, width = 1.8) %>%
    border_outer(border = fp_border(color = "#000000", width = 1)) %>%
    border_inner_h(border = fp_border(color = "#DDDDDD", width = 0.5)) %>%
    add_header_lines(model_label) %>%
    add_footer_lines(ref_note) %>%
    fontsize(size = 7, part = "footer") %>%
    italic(part = "footer") %>%
    autofit()
  
  return(ft)
}

ft_m2 <- make_model_ft(results_m2, 
                       "Model 2: Core Demographics",
                       "Reference: Cluster 4 (Young, Metabolically Early), Male, White. OR = odds ratio; CI = confidence interval.")

ft_m3 <- make_model_ft(results_m3,
                       "Model 3: Demographics + Socioeconomic",
                       "Reference: Cluster 4 (Young, Metabolically Early), Male, White, College+, Insured, Full Food Security. OR = odds ratio; CI = confidence interval.")

ft_m4 <- make_model_ft(results_m4,
                       "Model 4: Demographics + Behavioral/Treatment",
                       "Reference: Cluster 4 (Young, Metabolically Early), Male, White, Never smoker, Non-drinker, Not physically active, No BP meds, No statins. OR = odds ratio; CI = confidence interval.")

ft_diet <- make_model_ft(results_diet,
                         "Sensitivity: Demographics + Dietary (WTDRD1 subsample)",
                         "Reference: Cluster 4 (Young, Metabolically Early), Male, White. Dietary ORs: per 100 kcal (energy), per 10g (sugars, saturated fat), per 5g (fiber). OR = odds ratio; CI = confidence interval.")
# Save all models to a single Word doc with page breaks
save_as_docx(
  ft_m2, ft_m3, ft_m4, ft_diet,
  path = "Data/Output/multinomial_regression_tables_v2.docx",
  pr_section = prop_section(type = "nextPage")
)
cat("✓ Saved: Data/Output/multinomial_regression_tables_v2.docx\n")

# ============================================================
# STEP 7: SAVE
# ============================================================

cat("╔══════════════════════════════════════════════╗\n")
cat("║  STEP 7: SAVE                                 ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

# Full long-format results
write.csv(
  all_results %>%
    select(model, cluster, predictor, OR, OR_low, OR_high, OR_CI, p_value, p_fmt),
  file = "Data/Output/multinomial_regression_results_v2.csv",
  row.names = FALSE
)
cat("✓ Saved: Data/Output/multinomial_regression_results_v2.csv\n")

# Save models for later use
saveRDS(
  list(m2 = m2, m3 = m3, m4 = m4, m_diet = m_diet),
  file = "Data/Processed Dataframes/multinomial_models_v2.rds"
)
cat("✓ Saved: Data/Processed Dataframes/multinomial_models_v2.rds\n")

# Save updated dataset
saveRDS(combined_all_v2, file = "Data/Processed Dataframes/combined_all_v2.rds")
cat("✓ Saved: Data/Processed Dataframes/combined_all_v2.rds\n")

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║  COMPLETE                                                  ║\n")
cat("║  Model 2: age + sex + race/ethnicity                       ║\n")
cat("║  Model 3: Model 2 + education + PIR + insurance + food sec ║\n")
cat("║  Model 4: Model 2 + smoking + alcohol + PA + sleep +       ║\n")
cat("║           BP meds + statins                                ║\n")
cat("║  Sensitivity: Model 2 + dietary (WTDRD1 weight)            ║\n")
cat("║  Reference: Cluster 4 (youngest, metabolically early)      ║\n")
cat("║                                                            ║\n")
cat("║  Cluster mapping (svy_vglm index → label):                 ║\n")
cat("║    Index 1 = Cluster 1 (Hepatic/Dyslipidemic)              ║\n")
cat("║    Index 2 = Cluster 2 (Older, Lean)                       ║\n")
cat("║    Index 3 = Cluster 3 (Obese/Inflammatory)                ║\n")
cat("║    Index 4 = Cluster 5 (Older, Moderate/Renal)             ║\n")
cat("║    Reference = Cluster 4 (Young, Metabolically Early)      ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")

# Secondary Analysis of Clinical Outcomes Predicted by Cluster Membership

# ---- Step 1: Prepare dataset ----
dat_secondary <- combined_all_v2 %>%
  filter(
    !is.na(cluster_v2),
    !is.na(SDMVPSU), !is.na(SDMVSTRA_unique),
    !is.na(WTAF2YR_adj), WTAF2YR_adj > 0
  ) %>%
  mutate(
    cluster_v2 = relevel(factor(cluster_v2), ref = "4"),
    age = RIDAGEYR,
    sex = factor(RIAGENDR, levels = c(1, 2), labels = c("Male", "Female")),
    race_eth = factor(
      RIDRETH3,
      levels = c(3, 1, 2, 4, 6, 7),
      labels = c("NH White", "Mexican American", "Other Hispanic",
                 "NH Black", "Other/Multi", "Other/Multi")
    ),
    # Binary outcomes
    elevated_alb  = ifelse(microalbuminuria == "Yes", 1, 0),
    hypert_bin    = ifelse(hypertension == "Yes", 1, 0),
    hyperl_bin    = ifelse(hyperlipidemia == "Yes", 1, 0),
    depress_bin   = ifelse(depression == "Yes (PHQ-9 >= 10)", 1, 0),
    high_ascvd    = ifelse(ASCVD_10yr >= 7.5, 1, 0)
  )

# ---- Step 2: Survey design ----
des_secondary <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA_unique,
  weights = ~WTAF2YR_adj,
  nest    = TRUE,
  data    = dat_secondary
)

# ---- Step 3: Fit models ----
cat("Fitting models...\n")

m_alb <- svyglm(elevated_alb ~ cluster_v2 + age + sex + race_eth + bp_meds + statin_use,
                design = des_secondary, family = quasibinomial())

m_ascvd <- svyglm(high_ascvd ~ cluster_v2 + age + sex + race_eth + bp_meds + statin_use,
                  design = des_secondary, family = quasibinomial())

m_htn <- svyglm(hypert_bin ~ cluster_v2 + age + sex + race_eth+ bp_meds + statin_use,
                design = des_secondary, family = quasibinomial())

m_hyperl <- svyglm(hyperl_bin ~ cluster_v2 + age + sex + race_eth+ bp_meds + statin_use,
                   design = des_secondary, family = quasibinomial())

m_dep <- svyglm(depress_bin ~ cluster_v2 + age + sex + race_eth+ bp_meds + statin_use,
                design = des_secondary, family = quasibinomial())

cat("  ✓ All models complete\n")

# ---- Step 4: Extract ORs ----
extract_or <- function(model, label) {
  coefs <- summary(model)$coefficients
  ci <- confint(model)
  cluster_rows <- grep("cluster_v2", rownames(coefs))
  
  data.frame(
    outcome = label,
    cluster = gsub("cluster_v2", "Cluster ", rownames(coefs)[cluster_rows]),
    OR = round(exp(coefs[cluster_rows, "Estimate"]), 2),
    lower = round(exp(ci[cluster_rows, 1]), 2),
    upper = round(exp(ci[cluster_rows, 2]), 2),
    p = round(coefs[cluster_rows, "Pr(>|t|)"], 4)
  )
}

results <- rbind(
  extract_or(m_alb,   "Elevated Albuminuria"),
  extract_or(m_ascvd, "High ASCVD Risk (≥7.5%)"),
  extract_or(m_htn,   "Hypertension"),
  extract_or(m_hyperl,"Hyperlipidemia"),
  extract_or(m_dep,   "Moderate+ Depression")
)

results$OR_CI <- paste0(results$OR, " (", results$lower, "-", results$upper, ")")
results$sig <- ifelse(results$p < 0.05, "✓", "")

# ---- Step 5: Display ----
print(results[, c("outcome", "cluster", "OR_CI", "p", "sig")])

