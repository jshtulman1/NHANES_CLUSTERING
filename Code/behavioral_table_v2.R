# ============================================================
# BEHAVIORAL CHARACTERISTICS TABLE — V2 CLUSTERS
# 1. Merge SLD012 (sleep) and dietary variables
# 2. Derive: smoking, depression, physical activity
# 3. Weighted behavioral table (fasting weight)
# 4. Weighted dietary table (dietary day-1 weight)
# ============================================================

library(haven)
library(dplyr)
library(survey)
library(gtsummary)
library(gt)
library(flextable)
options(survey.lonely.psu = "adjust")

base_path <- "/Users/jacob/Desktop/Grad School/NHANES Prediabetes Metabolic Profiles/Data"


# ============================================================
# STEP 1: MERGE SLEEP (SLD012)
# ============================================================

cat("╔══════════════════════════════════════════════╗\n")
cat("║  STEP 1: MERGE SLEEP (SLD012)                ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

slq_2015 <- read_xpt(file.path(base_path, "NHANES 2015_2016", "SLQ_I.xpt"))
slq_2017 <- read_xpt(file.path(base_path, "NHANES 2017_2020", "P_SLQ.xpt"))
slq_2021 <- read_xpt(file.path(base_path, "NHANES 2021_2023", "SLQ_L.xpt"))

slq_combined <- bind_rows(
  slq_2015 %>% select(SEQN, SLD012),
  slq_2017 %>% select(SEQN, SLD012),
  slq_2021 %>% select(SEQN, SLD012)
)

cat("SLQ rows:", nrow(slq_combined), "\n")
cat("Duplicate SEQNs:", sum(duplicated(slq_combined$SEQN)), "\n")
cat("SLD012 summary:\n")
print(summary(slq_combined$SLD012))

n_before <- nrow(combined_all_v2)
combined_all_v2 <- combined_all_v2 %>%
  left_join(slq_combined, by = "SEQN")
stopifnot(nrow(combined_all_v2) == n_before)
cat("\n✓ SLD012 merged. Missing in prediabetes sample:",
    sum(is.na(combined_all_v2$SLD012)), "\n")


# ============================================================
# STEP 2: MERGE DIETARY (DR1TFIBE, DR1TSFAT, DR1TKCAL)
# DR1TSUGR already exists in combined_all_v2
# ============================================================

cat("\n╔══════════════════════════════════════════════╗\n")
cat("║  STEP 2: MERGE DIETARY VARIABLES              ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

dr_2015 <- read_xpt(file.path(base_path, "NHANES 2015_2016", "DR1TOT_I.xpt"))
dr_2017 <- read_xpt(file.path(base_path, "NHANES 2017_2020", "P_DR1TOT.xpt"))
dr_2021 <- read_xpt(file.path(base_path, "NHANES 2021_2023", "DR1TOT_L.xpt"))

dr_combined <- bind_rows(
  dr_2015 %>% select(SEQN, DR1TFIBE, DR1TSFAT, DR1TKCAL),
  dr_2017 %>% select(SEQN, DR1TFIBE, DR1TSFAT, DR1TKCAL),
  dr_2021 %>% select(SEQN, DR1TFIBE, DR1TSFAT, DR1TKCAL)
)

cat("DR1TOT rows:", nrow(dr_combined), "\n")
cat("Duplicate SEQNs:", sum(duplicated(dr_combined$SEQN)), "\n\n")

n_before <- nrow(combined_all_v2)
combined_all_v2 <- combined_all_v2 %>%
  left_join(dr_combined, by = "SEQN")
stopifnot(nrow(combined_all_v2) == n_before)

cat("✓ Dietary variables merged.\n")
cat("  DR1TFIBE missing:", sum(is.na(combined_all_v2$DR1TFIBE)), "\n")
cat("  DR1TSFAT missing:", sum(is.na(combined_all_v2$DR1TSFAT)), "\n")
cat("  DR1TKCAL missing:", sum(is.na(combined_all_v2$DR1TKCAL)), "\n")


# ============================================================
# STEP 3: DERIVE BEHAVIORAL VARIABLES
# ============================================================

cat("\n╔══════════════════════════════════════════════╗\n")
cat("║  STEP 3: DERIVE BEHAVIORAL VARIABLES          ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

combined_all_v2 <- combined_all_v2 %>%
  mutate(

    # ---- Smoking: ever smoked 100+ cigarettes (SMQ020) ----
    # 1 = Yes, 2 = No, 7 = Refused, 9 = Don't know
    ever_smoker = factor(
      ifelse(SMQ020 %in% c(1, 2), SMQ020, NA),
      levels = c(1, 2),
      labels = c("Yes", "No")
    ),

    # ---- Current smoking status (3-level) ----
    # SMQ020 == 2 → Never; SMQ040 == 3 → Former; SMQ040 in 1,2 → Current
    smoking_status = factor(
      case_when(
        SMQ020 == 2                    ~ 1L,  # Never
        SMQ020 == 1 & SMQ040 == 3      ~ 2L,  # Former
        SMQ020 == 1 & SMQ040 %in% 1:2  ~ 3L,  # Current
        TRUE                           ~ NA_integer_
      ),
      levels = 1:3,
      labels = c("Never", "Former", "Current")
    ),

    # ---- Depression: PHQ-9 total score ----
    # DPQ010-DPQ090 each scored 0-3; 7/9 = refused/dk → NA
    across_dpq = across(DPQ010:DPQ090,
      ~ ifelse(. %in% 0:3, ., NA_real_)),

    PHQ9_score = rowSums(across(DPQ010:DPQ090,
      ~ ifelse(. %in% 0:3, ., NA_real_)), na.rm = FALSE),

    # Binary: moderate+ depression (PHQ-9 >= 10)
    depression = factor(
      ifelse(!is.na(PHQ9_score),
             ifelse(PHQ9_score >= 10, 1L, 0L),
             NA_integer_),
      levels = c(0, 1),
      labels = c("No (PHQ-9 < 10)", "Yes (PHQ-9 >= 10)")
    ),

    # ---- Physical activity: leisure-time ≥150 min/week ----
    # 2015-16 & 2017-20 (GPAQ recreational domain):
    #   Moderate rec: PAQ665 (yes/no) → PAQ670 (days/wk) × PAD675 (min)
    #   Vigorous rec: PAQ650 (yes/no) → PAQ655 (days/wk) × PAD660 (min)
    #   Vigorous counted double per WHO convention
    #
    # 2021-23 (LTPA questions):
    #   Moderate: PAD790Q × PAD800, convert to weekly
    #   Vigorous: PAD810Q × PAD820, convert to weekly

    # -- GPAQ recreational (2015-16, 2017-20) --
    gpaq_mod_min = case_when(
      PAQ665 == 2  ~ 0,                                          # no moderate rec
      PAQ665 == 1 & PAQ670 %in% 1:7 & PAD675 %in% 1:1440
                   ~ as.numeric(PAQ670) * as.numeric(PAD675),    # days × min
      TRUE         ~ NA_real_
    ),
    gpaq_vig_min = case_when(
      PAQ650 == 2  ~ 0,                                          # no vigorous rec
      PAQ650 == 1 & PAQ655 %in% 1:7 & PAD660 %in% 1:1440
                   ~ as.numeric(PAQ655) * as.numeric(PAD660),    # days × min
      TRUE         ~ NA_real_
    ),

    # -- LTPA (2021-23): convert frequency to weekly --
    ltpa_mod_freq_wk = case_when(
      PAD790Q == 0                 ~ 0,
      PAD790U == "D"               ~ PAD790Q * 7,
      PAD790U == "W"               ~ PAD790Q,
      PAD790U == "M"               ~ PAD790Q / 4.345,
      PAD790U == "Y"               ~ PAD790Q / 52,
      TRUE                         ~ NA_real_
    ),
    ltpa_vig_freq_wk = case_when(
      PAD810Q == 0                 ~ 0,
      PAD810U == "D"               ~ PAD810Q * 7,
      PAD810U == "W"               ~ PAD810Q,
      PAD810U == "M"               ~ PAD810Q / 4.345,
      PAD810U == "Y"               ~ PAD810Q / 52,
      TRUE                         ~ NA_real_
    ),
    ltpa_mod_min = case_when(
      PAD790Q == 0                                    ~ 0,
      !is.na(ltpa_mod_freq_wk) & PAD800 %in% 1:1440  ~ ltpa_mod_freq_wk * PAD800,
      TRUE                                            ~ NA_real_
    ),
    ltpa_vig_min = case_when(
      PAD810Q == 0                                    ~ 0,
      !is.na(ltpa_vig_freq_wk) & PAD820 %in% 1:1440  ~ ltpa_vig_freq_wk * PAD820,
      TRUE                                            ~ NA_real_
    ),

    # -- Combine: use GPAQ rec for 2015/2017, LTPA for 2021 --
    mod_min_wk = case_when(
      Cycle %in% c(2015, 2017) ~ gpaq_mod_min,
      Cycle == 2021            ~ ltpa_mod_min
    ),
    vig_min_wk = case_when(
      Cycle %in% c(2015, 2017) ~ gpaq_vig_min,
      Cycle == 2021            ~ ltpa_vig_min
    ),

    # WHO-equivalent minutes: moderate + 2 × vigorous
    total_ltpa_min_wk = mod_min_wk + 2 * vig_min_wk,

    # Binary: meets ≥150 equivalent min/week
    physically_active = factor(
      case_when(
        !is.na(total_ltpa_min_wk) & total_ltpa_min_wk >= 150 ~ 1L,
        !is.na(total_ltpa_min_wk) & total_ltpa_min_wk <  150 ~ 0L,
        TRUE ~ NA_integer_
      ),
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

# Clean up intermediate columns used in across()
combined_all_v2 <- combined_all_v2 %>%
  select(-starts_with("across_dpq"))

# ---- Verify derived variables ----
cat("--- Ever smoker ---\n")
print(table(combined_all_v2$ever_smoker, useNA = "always"))

cat("\n--- Smoking status ---\n")
print(table(combined_all_v2$smoking_status, useNA = "always"))

cat("\n--- PHQ-9 score ---\n")
print(summary(combined_all_v2$PHQ9_score))

cat("\n--- Depression (PHQ-9 >= 10) ---\n")
print(table(combined_all_v2$depression, useNA = "always"))

cat("\n--- Physical activity (≥150 min/wk leisure-time) ---\n")
print(table(combined_all_v2$physically_active, useNA = "always"))

cat("\n--- Physical activity by cycle ---\n")
combined_all_v2 %>%
  filter(!is.na(cluster_v2)) %>%
  group_by(Cycle) %>%
  summarise(
    n = n(),
    pct_active  = round(mean(physically_active == "Yes", na.rm = TRUE) * 100, 1),
    pct_missing = round(mean(is.na(physically_active)) * 100, 1),
    .groups = "drop"
  ) %>%
  print()
cat("⚠️  Check that prevalence is reasonably consistent across cycles\n")
cat("   (2021 uses LTPA-only instrument; slight differences expected)\n")


# ============================================================
# STEP 4: BEHAVIORAL TABLE (FASTING WEIGHT)
# ============================================================

cat("\n╔══════════════════════════════════════════════╗\n")
cat("║  STEP 4: BEHAVIORAL TABLE                     ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

dat_bhev <- combined_all_v2 %>%
  filter(
    !is.na(cluster_v2),
    !is.na(SDMVPSU), !is.na(SDMVSTRA_unique),
    !is.na(WTAF2YR_adj), WTAF2YR_adj > 0
  ) %>%
  mutate(cluster_v2 = factor(cluster_v2))

cat("Behavioral analytic N:", nrow(dat_bhev), "\n")

des_bhev <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA_unique,
  weights = ~WTAF2YR_adj,
  nest    = TRUE,
  data    = dat_bhev
)

# Dynamic headers
bhev_counts <- as.data.frame(table(dat_bhev$cluster_v2))
names(bhev_counts) <- c("cluster_v2", "n")
bhev_headers <- setNames(
  lapply(seq_len(nrow(bhev_counts)), function(i) {
    sprintf("**Cluster %s**\n n = %s",
            bhev_counts$cluster_v2[i],
            format(bhev_counts$n[i], big.mark = ","))
  }),
  paste0("stat_", seq_len(nrow(bhev_counts)))
)

tbl_bhev <- tbl_svysummary(
  des_bhev,
  by = cluster_v2,
  include = c(smoking_status, current_drinker, physically_active,
              PAD680, SLD012, PHQ9_score, depression),
  statistic = list(
    PAD680    ~ "{mean} ({sd})",
    SLD012    ~ "{mean} ({sd})",
    PHQ9_score ~ "{mean} ({sd})",
    all_categorical() ~ "{p}%"
  ),
  label = list(
    smoking_status   ~ "Smoking Status, %",
    current_drinker  ~ "Current Drinker (past 12 mo), %",
    physically_active ~ "Physically Active (≥150 LTPA min/wk), %",
    PAD680           ~ "Sedentary Time, min/day",
    SLD012           ~ "Weekday Sleep, hours",
    PHQ9_score       ~ "PHQ-9 Depression Score",
    depression       ~ "Moderate+ Depression (PHQ-9 ≥ 10), %"
  ),
  missing = "no",
  digits = list(
    PAD680     ~ 1,
    SLD012     ~ 1,
    PHQ9_score ~ 1,
    all_categorical() ~ 1
  )
) %>%
  add_p() %>%
  modify_header(!!!bhev_headers) %>%
  modify_caption(
    "**Table X. Behavioral Characteristics by Prediabetes Phenotype Cluster (V2)**"
  )

tbl_bhev


# ============================================================
# STEP 5: DIETARY TABLE (DIETARY DAY-1 WEIGHT)
# ============================================================

cat("\n╔══════════════════════════════════════════════╗\n")
cat("║  STEP 5: DIETARY TABLE                        ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

dat_diet <- combined_all_v2 %>%
  filter(
    !is.na(cluster_v2),
    !is.na(SDMVPSU), !is.na(SDMVSTRA_unique),
    !is.na(WTDRD1_adj), WTDRD1_adj > 0
  ) %>%
  mutate(cluster_v2 = factor(cluster_v2))

cat("Dietary analytic N:", nrow(dat_diet), "\n")

des_diet <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA_unique,
  weights = ~WTDRD1_adj,
  nest    = TRUE,
  data    = dat_diet
)

# Dynamic headers for dietary sample (different N)
diet_counts <- as.data.frame(table(dat_diet$cluster_v2))
names(diet_counts) <- c("cluster_v2", "n")
diet_headers <- setNames(
  lapply(seq_len(nrow(diet_counts)), function(i) {
    sprintf("**Cluster %s**\n n = %s",
            diet_counts$cluster_v2[i],
            format(diet_counts$n[i], big.mark = ","))
  }),
  paste0("stat_", seq_len(nrow(diet_counts)))
)

tbl_diet <- tbl_svysummary(
  des_diet,
  by = cluster_v2,
  include = c(DR1TKCAL, DR1TSUGR, DR1TFIBE, DR1TSFAT),
  statistic = list(
    all_continuous() ~ "{mean} ({sd})"
  ),
  label = list(
    DR1TKCAL ~ "Total Energy, kcal",
    DR1TSUGR ~ "Total Sugars, g",
    DR1TFIBE ~ "Dietary Fiber, g",
    DR1TSFAT ~ "Saturated Fat, g"
  ),
  missing = "no",
  digits = list(
    DR1TKCAL ~ 0,
    DR1TSUGR ~ 1,
    DR1TFIBE ~ 1,
    DR1TSFAT ~ 1
  )
) %>%
  add_p() %>%
  modify_header(!!!diet_headers) %>%
  modify_caption(
    "**Table X. Dietary Intake by Prediabetes Phenotype Cluster (V2, Day-1 Recall Weight)**"
  )

tbl_diet


# ============================================================
# STEP 6: SAVE
# ============================================================

cat("\n╔══════════════════════════════════════════════╗\n")
cat("║  STEP 6: SAVE                                 ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

tbl_bhev %>%
  as_flex_table() %>%
  save_as_docx(path = "Data/Output/behavioral_characteristics_table_v2.docx")
cat("✓ Saved: Data/Output/behavioral_characteristics_table_v2.docx\n")

tbl_diet %>%
  as_flex_table() %>%
  save_as_docx(path = "Data/Output/dietary_intake_table_v2.docx")
cat("✓ Saved: Data/Output/dietary_intake_table_v2.docx\n")

# Save updated combined_all_v2 with new variables
saveRDS(combined_all_v2, file = "Data/Processed Dataframes/combined_all_v2.rds")
cat("✓ Saved: Data/Processed Dataframes/combined_all_v2.rds\n")

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║  COMPLETE                                                  ║\n")
cat("║  New variables: ever_smoker, smoking_status, PHQ9_score,   ║\n")
cat("║    depression, physically_active, total_ltpa_min_wk,       ║\n")
cat("║    SLD012, DR1TFIBE, DR1TSFAT, DR1TKCAL                   ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")
