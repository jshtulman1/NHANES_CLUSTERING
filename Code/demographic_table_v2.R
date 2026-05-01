# ============================================================
# DEMOGRAPHIC CHARACTERISTICS TABLE — V2 CLUSTERS
# Input:  combined_all_v2 (with cluster_v2, current_drinker)
# Output: Weighted Table 1 by prediabetes phenotype cluster
# ============================================================

library(dplyr)
library(survey)
library(gtsummary)
library(gt)
library(flextable)
options(survey.lonely.psu = "adjust")


# ============================================================
# STEP 1: CONSTRUCT ADJUSTED FASTING SUBSAMPLE WEIGHT
# ============================================================

# Clustering used fasting-dependent variables (HOMA-IR, HOMA-B,
# FPG, triglycerides, FLI), so the fasting subsample weight is
# required for all post-hoc analyses of cluster membership.
# WTAF2YR = harmonized fasting weight (WTSAFPRP renamed for 2021-23)

# WTAF2YR is populated for 2017 only; WTSAF2YR covers 2015 and 2021
# Coalesce to create a single raw fasting weight, then adjust
combined_all_v2 <- combined_all_v2 %>%
  mutate(
    fasting_wt_raw = coalesce(WTAF2YR, WTSAF2YR),
    WTAF2YR_adj = case_when(
      Cycle == 2015 ~ fasting_wt_raw * (2   / 7.2),
      Cycle == 2017 ~ fasting_wt_raw * (3.2 / 7.2),
      Cycle == 2021 ~ fasting_wt_raw * (2   / 7.2)
    )
  )


# ============================================================
# STEP 2: PREPARE ANALYTIC DATASET WITH RECODED VARIABLES
# ============================================================

dat_ana <- combined_all_v2 %>%
  filter(
    !is.na(cluster_v2),
    !is.na(SDMVPSU), !is.na(SDMVSTRA_unique),
    !is.na(WTAF2YR_adj), WTAF2YR_adj > 0
  ) %>%
  mutate(
    # ---- Continuous ----
    age = RIDAGEYR,
    pir = INDFMPIR,

    # ---- Sex (RIAGENDR: 1 = Male, 2 = Female) ----
    sex = factor(
      ifelse(RIAGENDR %in% c(1, 2), RIAGENDR, NA),
      levels = c(1, 2),
      labels = c("Male", "Female")
    ),

    # ---- Race/ethnicity ----
    # Use the existing 'race' variable (character: aa, white, other)
    race_eth = factor(
      ifelse(RIDRETH1 %in% 1:5, RIDRETH1, NA),
      levels = 1:5,
      labels = c("Mexican American", "Other Hispanic", "Non-Hispanic White",
                 "Non-Hispanic Black", "Other/Multiracial")
    ),
    # ---- Education (DMDEDUC2: 1-5, adults 20+) ----
    educ = factor(
      ifelse(DMDEDUC2 %in% 1:5, DMDEDUC2, NA),
      levels = 1:5,
      labels = c("<9th Grade", "9-11th Grade", "HS Graduate",
                  "Some College", "College Degree")
    ),

    # ---- Health insurance (HIQ011: 1 = Yes, 2 = No) ----
    insured = factor(
      ifelse(HIQ011 %in% c(1, 2), HIQ011, NA),
      levels = c(1, 2),
      labels = c("Insured", "Uninsured")
    ),

    # ---- Marital status (DMDMARTZ: 1-3) ----
    marital = factor(
      ifelse(DMDMARTZ %in% 1:3, DMDMARTZ, NA),
      levels = 1:3,
      labels = c("Married/Living with Partner",
                  "Widowed/Divorced/Separated",
                  "Never Married")
    ),

    # ---- Nativity (DMDBORN4: 1 = USA, 2 = Other) ----
    nativity = factor(
      ifelse(DMDBORN4 %in% c(1, 2), DMDBORN4, NA),
      levels = c(1, 2),
      labels = c("USA", "Other")
    ),

    # ---- Food security (FSDAD: 1-4) ----
    food_sec = factor(
      ifelse(FSDAD %in% 1:4, FSDAD, NA),
      levels = 1:4,
      labels = c("Full Food Security", "Marginal Security",
                  "Low Food Security", "Very Low Food Security")
    ),

    # ---- Cluster as factor ----
    cluster_v2 = factor(cluster_v2)
  )

cat("Analytic N:", nrow(dat_ana), "\n")
cat("Cluster distribution:\n")
print(table(dat_ana$cluster_v2))


# ============================================================
# STEP 3: BUILD SURVEY DESIGN
# ============================================================

des_ana <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA_unique,
  weights = ~WTAF2YR_adj,
  nest    = TRUE,
  data    = dat_ana
)


# ============================================================
# STEP 4: BUILD DYNAMIC COLUMN HEADERS
# ============================================================

cluster_counts <- as.data.frame(table(dat_ana$cluster_v2))
names(cluster_counts) <- c("cluster_v2", "n")

# Format cluster sizes with commas (e.g., "1,184")
header_list <- setNames(
  lapply(seq_len(nrow(cluster_counts)), function(i) {
    sprintf("**Cluster %s**\n n = %s",
            cluster_counts$cluster_v2[i],
            format(cluster_counts$n[i], big.mark = ","))
  }),
  paste0("stat_", seq_len(nrow(cluster_counts)))
)

cat("\nColumn headers:\n")
for (h in names(header_list)) cat(" ", h, ":", header_list[[h]], "\n")


# ============================================================
# STEP 5: WEIGHTED DEMOGRAPHIC TABLE
# ============================================================

tbl_demo <- tbl_svysummary(
  des_ana,
  by = cluster_v2,
  include = c(age, sex, race_eth, educ, pir, insured,
              marital, nativity, food_sec),
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
  modify_header(!!!header_list) %>%
  modify_caption(
    "**Table X. Demographic Characteristics by Prediabetes Phenotype Cluster (V2)**"
  )

tbl_demo


# ============================================================
# STEP 6: SAVE
# ============================================================

tbl_demo %>%
  as_flex_table() %>%
  save_as_docx(path = "Data/Output/demographic_characteristics_table_v2.docx")

cat("\n✓ Saved: Data/Output/demographic_characteristics_table_v2.docx\n")
