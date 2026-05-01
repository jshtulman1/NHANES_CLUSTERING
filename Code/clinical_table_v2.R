# ============================================================
# CLINICAL CHARACTERISTICS TABLE — V2 CLUSTERS
# Key fixes: harmonized BP meds (already done) + statin use
# across cycles with different skip patterns
# ============================================================

library(dplyr)
library(survey)
library(gtsummary)
library(gt)
library(flextable)
options(survey.lonely.psu = "adjust")


# ============================================================
# STEP 1: DERIVE / RECODE CLINICAL VARIABLES
# ============================================================

cat("╔══════════════════════════════════════════════╗\n")
cat("║  STEP 1: DERIVE CLINICAL VARIABLES           ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

# ---- Audit raw medication variables by cycle ----
cat("--- BP medication raw variables by cycle ---\n")
combined_all_v2 %>%
  group_by(Cycle) %>%
  summarise(
    n_BPQ050A_1 = sum(BPQ050A == 1, na.rm = TRUE),
    n_BPQ050A_2 = sum(BPQ050A == 2, na.rm = TRUE),
    n_BPQ050A_NA = sum(is.na(BPQ050A)),
    n_BPQ150_1  = sum(BPQ150 == 1, na.rm = TRUE),
    n_BPQ150_2  = sum(BPQ150 == 2, na.rm = TRUE),
    n_BPQ150_NA = sum(is.na(BPQ150)),
    .groups = "drop"
  ) %>% print()

cat("\n--- Statin raw variables by cycle ---\n")
combined_all_v2 %>%
  group_by(Cycle) %>%
  summarise(
    n_BPQ100D_1 = sum(BPQ100D == 1, na.rm = TRUE),
    n_BPQ100D_2 = sum(BPQ100D == 2, na.rm = TRUE),
    n_BPQ100D_NA = sum(is.na(BPQ100D)),
    n_BPQ101D_1  = sum(BPQ101D == 1, na.rm = TRUE),
    n_BPQ101D_2  = sum(BPQ101D == 2, na.rm = TRUE),
    n_BPQ101D_NA = sum(is.na(BPQ101D)),
    .groups = "drop"
  ) %>% print()

# ---- Derive variables ----
combined_all_v2 <- combined_all_v2 %>%
  mutate(

    # ---- Hypertension diagnosis (BPQ020, consistent across cycles) ----
    hypertension = factor(
      ifelse(BPQ020 %in% c(1, 2), BPQ020, NA),
      levels = c(1, 2),
      labels = c("Yes", "No")
    ),

    # ---- Hyperlipidemia diagnosis (BPQ080, consistent across cycles) ----
    hyperlipidemia = factor(
      ifelse(BPQ080 %in% c(1, 2), BPQ080, NA),
      levels = c(1, 2),
      labels = c("Yes", "No")
    ),

    # ---- BP medication (use bp_med_corrected from clustering pipeline) ----
    # Already handles: BPQ050A for 2015/2017, BPQ150 for 2021,
    # and codes non-hypertensives (BPQ020 == 2) as 0
    bp_meds = factor(
      ifelse(bp_med_corrected %in% c(0, 1), bp_med_corrected, NA),
      levels = c(1, 0),
      labels = c("Yes", "No")
    ),

    # ---- Statin / cholesterol medication (harmonized) ----
    # 2015-16 & 2017-20: BPQ100D (asked only if BPQ090D == 1,
    #   i.e., told to take cholesterol meds). Skip pattern:
    #   BPQ080 == 2 → never asked → NA (should be 0)
    #   BPQ080 == 1 & BPQ090D == 2 → told not to take meds → 0
    #   BPQ080 == 1 & BPQ090D == 1 → BPQ100D
    #
    # 2021-23: BPQ101D (asked of ALL respondents regardless of
    #   BPQ080). No skip pattern inflation.
    statin_use = case_when(
      # 2015-16 & 2017-20
      Cycle %in% c(2015, 2017) & BPQ100D == 1         ~ 1L,
      Cycle %in% c(2015, 2017) & BPQ100D == 2         ~ 0L,
      Cycle %in% c(2015, 2017) & BPQ080 == 1
                               & BPQ090D == 2          ~ 0L,  # told not to take meds
      Cycle %in% c(2015, 2017) & BPQ080 == 2          ~ 0L,  # no hyperlipidemia
      # 2021-23
      Cycle == 2021 & BPQ101D == 1                     ~ 1L,
      Cycle == 2021 & BPQ101D == 2                     ~ 0L,
      # Refused/don't know or missing
      TRUE                                             ~ NA_integer_
    ),
    statin_use = factor(
      statin_use,
      levels = c(1, 0),
      labels = c("Yes", "No")
    ),

    # ---- ASCVD 10-year risk (ensure numeric) ----
    ASCVD_10yr = suppressWarnings(as.numeric(ASCVD_10yr)))

    # ---- Microalbuminuria (ensure factor) ----

combined_all_v2 <- combined_all_v2 %>%
  mutate(
    microalbuminuria = factor(
      case_when(
        URDACT >= 30  ~ 1L,
        URDACT < 30   ~ 0L,
        TRUE          ~ NA_integer_
      ),
      levels = c(1, 0),
      labels = c("Yes", "No")
    )
  )


# ---- Verify ----
cat("\n--- Hypertension ---\n")
print(table(combined_all_v2$hypertension, useNA = "always"))

cat("\n--- Hyperlipidemia ---\n")
print(table(combined_all_v2$hyperlipidemia, useNA = "always"))

cat("\n--- BP meds (from bp_med_corrected) ---\n")
print(table(combined_all_v2$bp_meds, useNA = "always"))

cat("\n--- Statin use (harmonized) ---\n")
print(table(combined_all_v2$statin_use, useNA = "always"))

cat("\n--- Statin use by cycle (check for skip pattern inflation) ---\n")
combined_all_v2 %>%
  group_by(Cycle) %>%
  summarise(
    n = n(),
    pct_statin = round(mean(statin_use == "Yes", na.rm = TRUE) * 100, 1),
    pct_missing = round(mean(is.na(statin_use)) * 100, 1),
    .groups = "drop"
  ) %>% print()
cat("✓ Expected: ~20-35% in prediabetes population\n")

cat("\n--- ASCVD 10yr risk ---\n")
print(summary(combined_all_v2$ASCVD_10yr))

cat("\n--- Microalbuminuria ---\n")
print(table(combined_all_v2$microalbuminuria, useNA = "always"))


# ============================================================
# STEP 2: CLINICAL TABLE (FASTING WEIGHT)
# ============================================================

cat("\n╔══════════════════════════════════════════════╗\n")
cat("║  STEP 2: CLINICAL TABLE                       ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

dat_clin <- combined_all_v2 %>%
  filter(
    !is.na(cluster_v2),
    !is.na(SDMVPSU), !is.na(SDMVSTRA_unique),
    !is.na(WTAF2YR_adj), WTAF2YR_adj > 0
  ) %>%
  mutate(cluster_v2 = factor(cluster_v2))

cat("Clinical analytic N:", nrow(dat_clin), "\n")

des_clin <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA_unique,
  weights = ~WTAF2YR_adj,
  nest    = TRUE,
  data    = dat_clin
)

# Dynamic headers
clin_counts <- as.data.frame(table(dat_clin$cluster_v2))
names(clin_counts) <- c("cluster_v2", "n")
clin_headers <- setNames(
  lapply(seq_len(nrow(clin_counts)), function(i) {
    sprintf("**Cluster %s**\n n = %s",
            clin_counts$cluster_v2[i],
            format(clin_counts$n[i], big.mark = ","))
  }),
  paste0("stat_", seq_len(nrow(clin_counts)))
)

tbl_clin <- tbl_svysummary(
  des_clin,
  by = cluster_v2,
  include = c(hypertension, hyperlipidemia, bp_meds, statin_use,
              ASCVD_10yr, microalbuminuria),
  statistic = list(
    ASCVD_10yr        ~ "{mean} ({sd})",
    all_categorical() ~ "{p}%"
  ),
  label = list(
    hypertension     ~ "Hypertension, %",
    hyperlipidemia   ~ "Hyperlipidemia, %",
    bp_meds          ~ "BP Medication Use, %",
    statin_use       ~ "Cholesterol Medication Use, %",
    ASCVD_10yr       ~ "10-Year ASCVD Risk, %",
    microalbuminuria ~ "Elevated Albuminuria (ACR ≥ 30 mg/g), %"
  ),
  missing = "no",
  digits = list(
    ASCVD_10yr        ~ 1,
    all_categorical() ~ 1
  )
) %>%
  add_p() %>%
  modify_header(!!!clin_headers) %>%
  modify_caption(
    "**Table X. Clinical Characteristics by Prediabetes Phenotype Cluster (V2)**"
  )

tbl_clin


# ============================================================
# STEP 3: SAVE
# ============================================================

cat("\n╔══════════════════════════════════════════════╗\n")
cat("║  STEP 3: SAVE                                 ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

tbl_clin %>%
  as_flex_table() %>%
  save_as_docx(path = "Data/Output/clinical_characteristics_table_v2.docx")
cat("✓ Saved: Data/Output/clinical_characteristics_table_v2.docx\n")

# Save updated combined_all_v2
saveRDS(combined_all_v2, file = "Data/Processed Dataframes/combined_all_v2.rds")
cat("✓ Saved: Data/Processed Dataframes/combined_all_v2.rds\n")
