# ============================================================
# CLUSTERING VARIABLE TABLE — V2 CLUSTERS
# Weighted means (SD) for all 20 clustering variables
# Used to identify/name the phenotype clusters
# ============================================================

library(dplyr)
library(survey)
library(gtsummary)
library(gt)
library(flextable)
options(survey.lonely.psu = "adjust")


# ============================================================
# STEP 1: ANALYTIC DATASET + SURVEY DESIGN
# ============================================================

dat_clust <- combined_all_v2 %>%
  filter(
    !is.na(cluster_v2),
    !is.na(SDMVPSU), !is.na(SDMVSTRA_unique),
    !is.na(WTAF2YR_adj), WTAF2YR_adj > 0
  ) %>%
  mutate(cluster_v2 = factor(cluster_v2))

cat("Analytic N:", nrow(dat_clust), "\n")
print(table(dat_clust$cluster_v2))

des_clust <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA_unique,
  weights = ~WTAF2YR_adj,
  nest    = TRUE,
  data    = dat_clust
)


# ============================================================
# STEP 2: CLUSTERING VARIABLE TABLE
# ============================================================

# Dynamic headers
clust_counts <- as.data.frame(table(dat_clust$cluster_v2))
names(clust_counts) <- c("cluster_v2", "n")
clust_headers <- setNames(
  lapply(seq_len(nrow(clust_counts)), function(i) {
    sprintf("**Cluster %s**\n n = %s",
            clust_counts$cluster_v2[i],
            format(clust_counts$n[i], big.mark = ","))
  }),
  paste0("stat_", seq_len(nrow(clust_counts)))
)

tbl_clust <- tbl_svysummary(
  des_clust,
  by = cluster_v2,
  include = c(RIDAGEYR, BMXBMI, LBXGH, LBXGLU,
              HOMA_IR, HOMA_B, LBDHDD,
              LBXSASSI, LBXSATSI, LBXSGTSI,
              WHtR, LBXTR, tg_hdl,
              SBP_mean, DBP_mean, FLI,
              LBXHSCRP, LBXWBCSI, URDACT, eGFR),
  statistic = list(
    all_continuous() ~ "{mean} ({sd})"
  ),
  label = list(
    RIDAGEYR ~ "Age, years",
    BMXBMI   ~ "BMI, kg/m²",
    LBXGH    ~ "HbA1c, %",
    LBXGLU   ~ "Fasting Glucose, mg/dL",
    HOMA_IR  ~ "HOMA-IR",
    HOMA_B   ~ "HOMA-β",
    LBDHDD   ~ "HDL-C, mg/dL",
    LBXSASSI ~ "AST, IU/L",
    LBXSATSI ~ "ALT, IU/L",
    LBXSGTSI ~ "GGT, IU/L",
    WHtR     ~ "Waist-to-Height Ratio",
    LBXTR    ~ "Triglycerides, mg/dL",
    tg_hdl   ~ "TG/HDL Ratio",
    SBP_mean ~ "Systolic BP, mmHg",
    DBP_mean ~ "Diastolic BP, mmHg",
    FLI      ~ "Fatty Liver Index",
    LBXHSCRP ~ "hs-CRP, mg/L",
    LBXWBCSI ~ "WBC, 1000 cells/µL",
    URDACT   ~ "Albumin-Creatinine Ratio, mg/g",
    eGFR     ~ "eGFR, mL/min/1.73m²"
  ),
  missing = "no",
  digits = list(
    RIDAGEYR ~ 1,
    BMXBMI   ~ 1,
    LBXGH    ~ 2,
    LBXGLU   ~ 1,
    HOMA_IR  ~ 1,
    HOMA_B   ~ 1,
    LBDHDD   ~ 1,
    LBXSASSI ~ 1,
    LBXSATSI ~ 1,
    LBXSGTSI ~ 1,
    WHtR     ~ 3,
    LBXTR    ~ 1,
    tg_hdl   ~ 2,
    SBP_mean ~ 1,
    DBP_mean ~ 1,
    FLI      ~ 1,
    LBXHSCRP ~ 2,
    LBXWBCSI ~ 1,
    URDACT   ~ 1,
    eGFR     ~ 1
  )
) %>%
  add_p() %>%
  modify_header(!!!clust_headers) %>%
  modify_caption(
    "**Table X. Clustering Variable Profiles by Prediabetes Phenotype Cluster (V2)**"
  )

tbl_clust


# ============================================================
# STEP 3: SAVE
# ============================================================

tbl_clust %>%
  as_flex_table() %>%
  save_as_docx(path = "Data/Output/clustering_variables_table_v2.docx")
cat("\n✓ Saved: Data/Output/clustering_variables_table_v2.docx\n")

#=============================================================
# Prevalence Estimates
#=============================================================

prev <- svymean(~cluster_v2, design = des_clust, na.rm = TRUE)
data.frame(
  cluster = levels(dat_clust$cluster_v2),
  pct     = round(coef(prev) * 100, 1),
  se      = round(SE(prev) * 100, 1)
)
