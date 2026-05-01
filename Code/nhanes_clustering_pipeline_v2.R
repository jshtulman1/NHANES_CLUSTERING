# ============================================================
# NHANES PREDIABETES CLUSTERING PIPELINE
# Starting from: NHANES_Combined_Processed
# Output:        combined_all_v2 (full dataset + cluster_v2)
#
# Note: Clustering is performed unweighted on complete cases.
# Survey weights are constructed at the end for post-hoc
# population-level estimates only.
# ============================================================

library(dplyr)
library(tidyverse)
library(survey)
library(ConsensusClusterPlus)
library(mclust)
library(gt)


# ============================================================
# SECTION 1: INPUT DATA INTEGRITY CHECKS
# ============================================================

cat("╔══════════════════════════════════════════╗\n")
cat("║  SECTION 1: INPUT DATA INTEGRITY CHECKS ║\n")
cat("╚══════════════════════════════════════════╝\n\n")

# ---- 1A: Cycle distribution ----
cat("--- 1A: Cycle distribution ---\n")
print(table(NHANES_Combined_Processed$Cycle, useNA = "always"))

# ---- 1B: SEQN integrity ----
cat("\n--- 1B: SEQN integrity ---\n")
cat("Duplicate SEQNs:", sum(duplicated(NHANES_Combined_Processed$SEQN)), "\n")
NHANES_Combined_Processed %>%
  group_by(Cycle) %>%
  summarise(
    n        = n(),
    min_SEQN = min(SEQN),
    max_SEQN = max(SEQN),
    .groups  = "drop"
  ) %>%
  print()

# ---- 1C: PSU/Strata counts per cycle ----
cat("\n--- 1C: PSU/Strata counts per cycle ---\n")
NHANES_Combined_Processed %>%
  group_by(Cycle) %>%
  summarise(
    n_PSU    = n_distinct(SDMVPSU),
    n_strata = n_distinct(SDMVSTRA),
    .groups  = "drop"
  ) %>%
  print()


# ============================================================
# SECTION 2: BP MEDICATION SKIP PATTERN AUDIT
# ============================================================

cat("\n╔══════════════════════════════════════════╗\n")
cat("║  SECTION 2: BP MEDICATION AUDIT          ║\n")
cat("╚══════════════════════════════════════════╝\n\n")

cat("--- 2A: Raw BP variable presence ---\n")
for (v in c("BPQ020", "BPQ050A", "BPQ150")) {
  cat(sprintf("  %s: %s\n", v,
      ifelse(v %in% names(NHANES_Combined_Processed),
             "✓ present", "✗ NOT FOUND")))
}

cat("\n--- 2B: BPQ050A by cycle (expect 0 in 2021) ---\n")
NHANES_Combined_Processed %>%
  group_by(Cycle) %>%
  summarise(
    n_1       = sum(BPQ050A == 1, na.rm = TRUE),
    n_2       = sum(BPQ050A == 2, na.rm = TRUE),
    n_missing = sum(is.na(BPQ050A)),
    .groups   = "drop"
  ) %>% print()

cat("\n--- 2B: BPQ150 by cycle (expect 0 in 2015 & 2017) ---\n")
NHANES_Combined_Processed %>%
  group_by(Cycle) %>%
  summarise(
    n_1       = sum(BPQ150 == 1, na.rm = TRUE),
    n_2       = sum(BPQ150 == 2, na.rm = TRUE),
    n_missing = sum(is.na(BPQ150)),
    .groups   = "drop"
  ) %>% print()

cat("\n--- 2C: Original bp_med prevalence (inflated by skip pattern) ---\n")
NHANES_Combined_Processed %>%
  group_by(Cycle) %>%
  summarise(
    n_1       = sum(`bp_med` == 1, na.rm = TRUE),
    n_0       = sum(`bp_med` == 0, na.rm = TRUE),
    n_missing = sum(is.na(`bp_med`)),
    pct_wrong = round(mean(`bp_med` == 1, na.rm = TRUE) * 100, 1),
    .groups   = "drop"
  ) %>% print()
cat("⚠️  NAs are non-hypertensives excluded by skip pattern — inflating pct\n")


# ============================================================
# SECTION 3: DERIVE VARIABLES + CORRECTIONS
# ============================================================

cat("\n╔══════════════════════════════════════════╗\n")
cat("║  SECTION 3: VARIABLE DERIVATION          ║\n")
cat("╚══════════════════════════════════════════╝\n\n")

NHANES_Combined_I <- NHANES_Combined_Processed %>%

  # ---- HOMA-B (standard formula, NA-guarded) ----
  mutate(
    HOMA_B = 360 * LBXIN / (LBXGLU - 63),
    HOMA_B = ifelse(is.finite(HOMA_B), HOMA_B, NA_real_)
  ) %>%

  # ---- Corrected BP medication (skip pattern fix) ----
  # Non-hypertensives (BPQ020 == 2) were never routed to
  # BPQ050A/BPQ150 and must be coded 0, not left as NA
  mutate(
    bp_med_corrected = case_when(
      Cycle %in% c(2015, 2017) & BPQ050A == 1 ~ 1,
      Cycle %in% c(2015, 2017) & BPQ050A == 2 ~ 0,
      Cycle == 2021            & BPQ150  == 1 ~ 1,
      Cycle == 2021            & BPQ150  == 2 ~ 0,
      BPQ020 == 2                             ~ 0,
      TRUE                                    ~ NA_real_
    )
  ) %>%

  # ---- Unique strata identifier (for post-hoc survey design only) ----
  mutate(SDMVSTRA_unique = paste(Cycle, SDMVSTRA, sep = "_"))

# ---- 3A: Verify corrected BP med prevalence ----
cat("--- 3A: bp_med_corrected prevalence after skip pattern fix ---\n")
NHANES_Combined_I %>%
  group_by(Cycle) %>%
  summarise(
    n_1         = sum(bp_med_corrected == 1, na.rm = TRUE),
    n_0         = sum(bp_med_corrected == 0, na.rm = TRUE),
    n_missing   = sum(is.na(bp_med_corrected)),
    pct_correct = round(mean(bp_med_corrected == 1, na.rm = TRUE) * 100, 1),
    .groups     = "drop"
  ) %>% print()
cat("✓ Expected: ~20-30% in prediabetes population\n")

# ---- 3B: Derived variable spot checks ----
cat("\n--- 3B: Derived variable formula verification ---\n")
NHANES_Combined_I %>%
  summarise(
    homa_ir_diff = round(abs(
      mean((LBXGLU / 18 * LBXIN) / 22.5, na.rm = TRUE) -
      mean(HOMA_IR, na.rm = TRUE)), 4),
    homa_b_diff  = round(abs(
      mean((360 * LBXIN) / (LBXGLU - 63), na.rm = TRUE) -
      mean(HOMA_B, na.rm = TRUE)), 4),
    whtr_diff    = round(abs(
      mean(BMXWAIST / BMXHT, na.rm = TRUE) -
      mean(WHtR, na.rm = TRUE)), 4),
    tghdl_diff   = round(abs(
      mean(LBXTR / LBDHDD, na.rm = TRUE) -
      mean(tg_hdl, na.rm = TRUE)), 4)
  ) %>%
  pivot_longer(everything(), names_to = "check", values_to = "diff") %>%
  mutate(flag = ifelse(diff < 0.01, "✓ OK", "⚠️  MISMATCH — review formula")) %>%
  print()

# Fixed HOMA-B verification — apply the same is.finite() guard to the manual calculation
NHANES_Combined_I %>%
  mutate(
    homa_b_manual = 360 * LBXIN / (LBXGLU - 63),
    homa_b_manual = ifelse(is.finite(homa_b_manual), homa_b_manual, NA_real_)
  ) %>%
  summarise(
    homa_b_diff = round(abs(
      mean(homa_b_manual, na.rm = TRUE) -
        mean(HOMA_B,        na.rm = TRUE)), 4),
    n_invalid_glucose = sum(LBXGLU <= 63, na.rm = TRUE)  # how many triggered the guard
  ) %>%
  mutate(flag = ifelse(homa_b_diff < 0.01, "✓ OK", "⚠️  MISMATCH — review formula")) %>%
  print()

# ============================================================
# SECTION 4: CLUSTERING VARIABLE CHECKS
# ============================================================

cat("\n╔══════════════════════════════════════════╗\n")
cat("║  SECTION 4: CLUSTERING VARIABLE CHECKS  ║\n")
cat("╚══════════════════════════════════════════╝\n\n")

vars <- c(
  "RIDAGEYR", "BMXBMI",   "LBXGH",    "LBXGLU",
  "HOMA_IR",  "HOMA_B",   "LBDHDD",
  "LBXSASSI", "LBXSATSI", "LBXSGTSI",
  "WHtR",     "LBXTR",    "tg_hdl",
  "SBP_mean", "DBP_mean", "FLI",
  "LBXHSCRP", "LBXWBCSI", "URDACT",   "eGFR"
)

# ---- 4A: Missingness by variable ----
cat("--- 4A: Missingness by variable ---\n")
data.frame(
  variable = vars,
  n        = sapply(vars, function(v) sum(!is.na(NHANES_Combined_I[[v]]))),
  n_miss   = sapply(vars, function(v) sum(is.na(NHANES_Combined_I[[v]]))),
  pct_miss = sapply(vars, function(v) round(mean(is.na(NHANES_Combined_I[[v]])) * 100, 1))
) %>%
  arrange(desc(pct_miss)) %>%
  mutate(flag = case_when(
    pct_miss > 20 ~ "⚠️  HIGH — fasting required",
    pct_miss > 5  ~ "⚠️  MODERATE",
    TRUE          ~ "✓ OK"
  )) %>%
  print(row.names = FALSE)

# ---- 4B: Missingness by cycle ----
cat("\n--- 4B: Missingness by cycle ---\n")
do.call(rbind, lapply(vars, function(v) {
  NHANES_Combined_I %>%
    group_by(Cycle) %>%
    summarise(pct_missing = round(mean(is.na(.data[[v]])) * 100, 1),
              .groups = "drop") %>%
    mutate(variable = v)
})) %>%
  pivot_wider(names_from = Cycle, values_from = pct_missing) %>%
  mutate(
    max_miss = pmax(`2015`, `2017`, `2021`),
    flag = case_when(
      (`2015` == 100 | `2017` == 100 | `2021` == 100) & max_miss < 100
                    ~ "⚠️  CYCLE COLLECTION GAP",
      max_miss > 20 ~ "⚠️  HIGH MISSINGNESS",
      max_miss > 5  ~ "⚠️  CHECK",
      TRUE          ~ "✓ OK"
    )
  ) %>%
  arrange(desc(max_miss)) %>%
  print(row.names = FALSE)

# ---- 4C: Complete case analysis ----
cat("\n--- 4C: Complete case analysis ---\n")
n_total    <- nrow(NHANES_Combined_I)
n_complete <- sum(complete.cases(NHANES_Combined_I[, vars]))
cat("Total N:       ", n_total, "\n")
cat("Complete cases:", n_complete, "\n")
cat("Dropped:       ", n_total - n_complete,
    sprintf("(%.1f%%)\n", (1 - n_complete / n_total) * 100))

cat("\nCompleters vs non-completers:\n")
NHANES_Combined_I %>%
  mutate(complete = complete.cases(across(all_of(vars)))) %>%
  group_by(complete) %>%
  summarise(
    n          = n(),
    mean_age   = round(mean(RIDAGEYR, na.rm = TRUE), 1),
    mean_bmi   = round(mean(BMXBMI,   na.rm = TRUE), 1),
    mean_hba1c = round(mean(LBXGH,    na.rm = TRUE), 2),
    pct_female = round(mean(RIAGENDR == 2, na.rm = TRUE) * 100, 1),
    .groups    = "drop"
  ) %>% print()
cat("⚠️  Systematic differences = informative missingness — state as limitation\n")

# ---- 4D: Primary drivers of exclusion ----
cat("\n--- 4D: Variables driving exclusion ---\n")
baseline <- sum(complete.cases(NHANES_Combined_I[, vars]))
data.frame(
  variable        = vars,
  cases_recovered = sapply(vars, function(v) {
    sum(complete.cases(NHANES_Combined_I[, setdiff(vars, v)])) - baseline
  })
) %>%
  arrange(desc(cases_recovered)) %>%
  mutate(
    pct_recovered = round(cases_recovered / n_total * 100, 1),
    flag = case_when(
      cases_recovered > 500 ~ "⚠️  PRIMARY DRIVER of exclusion",
      cases_recovered > 100 ~ "⚠️  MEANINGFUL contributor",
      TRUE                  ~ "✓ Minor"
    )
  ) %>%
  print(row.names = FALSE)


# ============================================================
# SECTION 5: CORRECTED CLUSTERING (V2)
# Key fix: log-transform ALL 10 skewed variables
# Original only transformed 6 — missing LBXSASSI, LBXSATSI,
# tg_hdl, LBXWBCSI (max=400 vs median=6.7)
# ============================================================

cat("\n╔══════════════════════════════════════════╗\n")
cat("║  SECTION 5: CORRECTED CLUSTERING (V2)   ║\n")
cat("╚══════════════════════════════════════════╝\n\n")

log_vars <- c(
  "HOMA_IR",   # ✓ in original pipeline
  "HOMA_B",    # ✓ in original pipeline
  "LBXTR",     # ✓ in original pipeline
  "LBXHSCRP",  # ✓ in original pipeline
  "LBXSGTSI",  # ✓ in original pipeline
  "URDACT",    # ✓ in original pipeline
  "LBXSASSI",  # ⚠️  NEW — skewed but untransformed in original
  "LBXSATSI",  # ⚠️  NEW — skewed but untransformed in original
  "tg_hdl",    # ⚠️  NEW — skewed but untransformed in original
  "LBXWBCSI"   # ⚠️  NEW — max=400 vs median=6.7, severe skew
)

cat("--- 5A: Log-transforming all skewed variables ---\n")
cat("Variables:", paste(log_vars, collapse = ", "), "\n\n")

nhanes_sub_v2 <- NHANES_Combined_I %>%
  select(SEQN, all_of(vars)) %>%
  mutate(across(
    all_of(log_vars),
    ~ dplyr::if_else(. <= 0 | is.na(.), NA_real_, log(.))
  ))

# ---- 5B: Verify log transformation resolved skew (fixed) ----
cat("--- 5B: Skew check after log transformation ---\n")
data.frame(
  variable      = log_vars,
  median        = sapply(log_vars, function(v) round(median(nhanes_sub_v2[[v]], na.rm = TRUE), 2)),
  max           = sapply(log_vars, function(v) round(max(nhanes_sub_v2[[v]],    na.rm = TRUE), 2)),
  max_med_ratio = sapply(log_vars, function(v) round(
    max(nhanes_sub_v2[[v]], na.rm = TRUE) /
      median(nhanes_sub_v2[[v]], na.rm = TRUE), 1))
) %>%
  mutate(flag = case_when(
    max_med_ratio > 10 ~ "⚠️  STILL SKEWED — check outliers",
    max_med_ratio > 5  ~ "⚠️  MODERATE",
    TRUE               ~ "✓ Resolved"
  )) %>%
  arrange(desc(max_med_ratio)) %>%
  print(row.names = FALSE)

# ---- 5C: Complete cases, scale, transpose ----
nhanes_cc_v2     <- nhanes_sub_v2 %>% drop_na()
seqn_vector_v2   <- nhanes_cc_v2$SEQN
nhanes_cc_mat_v2 <- nhanes_cc_v2 %>% select(-SEQN)
rownames(nhanes_cc_mat_v2) <- seqn_vector_v2

cat(sprintf("\n--- 5C: N for clustering: %d ---\n", nrow(nhanes_cc_v2)))

nhanes_scaled_v2 <- scale(nhanes_cc_mat_v2)
nhanes_ccp_v2    <- t(nhanes_scaled_v2)
colnames(nhanes_ccp_v2) <- seqn_vector_v2

# ---- 5D: Consensus clustering ----
cat("\n--- 5D: Running consensus clustering (v2, this may take a few minutes) ---\n")
set.seed(2026)
cc_results_v2 <- ConsensusClusterPlus(
  nhanes_ccp_v2,
  maxK        = 9,
  reps        = 100,
  pItem       = 0.8,
  pFeature    = 1,
  clusterAlg  = "km",
  distance    = "euclidean",
  seed        = 2026,
  plot        = "png"
)

for (k in 2:9) {
  names(cc_results_v2[[k]]$consensusClass) <- colnames(nhanes_ccp_v2)
}

# ---- 5E: ICL scores ----
cat("\n--- 5E: Cluster consensus scores ---\n")
icl_v2 <- calcICL(cc_results_v2,
                  title = "NHANES Consensus Clustering V2",
                  plot  = "png")
print(icl_v2$clusterConsensus)

avg_consensus_v2 <- tapply(
  icl_v2$clusterConsensus[, "clusterConsensus"],
  icl_v2$clusterConsensus[, "k"],
  mean
)
cat("\nAverage consensus score by K:\n")
print(round(avg_consensus_v2, 3))
cat("(Review this + CDF plots to confirm optimal K)\n")


# ============================================================
# SECTION 6: CLUSTER ASSIGNMENT + COMPARE V1 vs V2
# Update K below to match your chosen optimal cluster number
# ============================================================

cat("\n╔══════════════════════════════════════════╗\n")
cat("║  SECTION 6: CLUSTER ASSIGNMENT + COMPARE ║\n")
cat("╚══════════════════════════════════════════╝\n\n")

K <- 5   # <- update if optimal K changes after reviewing ICL plots

# ---- V2 final clusters via HC on consensus matrix ----
cm_v2             <- cc_results_v2[[K]]$consensusMatrix
dist_cm_v2        <- as.dist(1 - cm_v2)
hc_v2             <- hclust(dist_cm_v2, method = "ward.D2")
final_clusters_v2 <- cutree(hc_v2, k = K)

cluster_df_v2 <- data.frame(
  SEQN       = as.numeric(colnames(nhanes_ccp_v2)),
  cluster_v2 = as.integer(final_clusters_v2)
)

cat(sprintf("--- V2 cluster sizes (K=%d) ---\n", K))
print(table(cluster_df_v2$cluster_v2))

# ---- Compare V1 vs V2 if original cluster column exists ----
if ("cluster" %in% names(NHANES_Combined_I)) {
  cat("\n--- V1 vs V2 comparison ---\n")
  comparison <- combined_all %>%
    select(SEQN, cluster_v1 = cluster) %>%
    filter(!is.na(cluster_v1)) %>%
    inner_join(cluster_df_v2, by = "SEQN")

  cat(sprintf("N in both versions: %d\n\n", nrow(comparison)))
  cat("Cross-tabulation (rows = V1, cols = V2):\n")
  print(table(V1 = comparison$cluster_v1, V2 = comparison$cluster_v2))

  ari <- adjustedRandIndex(comparison$cluster_v1, comparison$cluster_v2)
  cat(sprintf("\nAdjusted Rand Index: %.3f\n", ari))
  cat(ifelse(ari > 0.80,
    "✓ >0.80 — Highly similar; original defensible, note log-transform limitation\n",
    ifelse(ari > 0.60,
    "⚠️  0.60-0.80 — Moderate disagreement; review phenotype interpretation\n",
    "🔴 <0.60 — Meaningfully different; use V2 results\n")))
} else {
  cat("(No V1 cluster column found on NHANES_Combined_I — skipping comparison)\n")
}


# ============================================================
# SECTION 7: BUILD FINAL DATASET
# Survey weights added here for post-hoc analyses only
# ============================================================

cat("\n╔══════════════════════════════════════════╗\n")
cat("║  SECTION 7: FINAL DATASET CONSTRUCTION  ║\n")
cat("╚══════════════════════════════════════════╝\n\n")

combined_all_v2 <- NHANES_Combined_I %>%
  left_join(cluster_df_v2, by = "SEQN") %>%
  left_join(sugars_alc, by = "SEQN") %>%
  mutate(
    in_clustering = !is.na(cluster_v2),
    # Adjusted survey weights for post-hoc analyses only
    WTMEC2YR_adj = case_when(
      Cycle == 2015 ~ WTMEC2YR * (2   / 7.2),
      Cycle == 2017 ~ WTMEC2YR * (3.2 / 7.2),
      Cycle == 2021 ~ WTMEC2YR * (2   / 7.2)
    ),
    WTINT2YR_adj = case_when(
      Cycle == 2015 ~ WTINT2YR * (2   / 7.2),
      Cycle == 2017 ~ WTINT2YR * (3.2 / 7.2),
      Cycle == 2021 ~ WTINT2YR * (2   / 7.2)
    ),
    WTDRD1_adj = case_when(
      Cycle == 2015 ~ WTDRD1 * (2   / 7.2),
      Cycle == 2017 ~ WTDRD1 * (3.2 / 7.2),
      Cycle == 2021 ~ WTDRD1 * (2   / 7.2)
    )
  )

# ---- Verify join didn't inflate rows ----
cat("Total N:          ", nrow(combined_all_v2), "\n")
cat("In clustering:    ", sum(combined_all_v2$in_clustering), "\n")
cat("Not in clustering:", sum(!combined_all_v2$in_clustering), "\n\n")

# ---- Verify sugars_alc joined correctly ----
cat("--- sugars_alc join check ---\n")
cat("Sugar missing:   ", sum(is.na(combined_all_v2$DR1TSUGR)), "\n")
cat("Alcohol missing: ", sum(is.na(combined_all_v2$ALQ151_binary)), "\n")

# ---- Verify diet weight ----
cat("\n--- WTDRD1_adj summary ---\n")
print(summary(combined_all_v2$WTDRD1_adj))

# ---- Cluster sizes ----
cat("\nCluster sizes (V2):\n")
print(table(combined_all_v2$cluster_v2, useNA = "always"))

# ---- Unweighted cluster means (raw scale for interpretability) ----
cat("\n--- Cluster means (unweighted, raw scale) ---\n")
combined_all_v2 %>%
  filter(in_clustering) %>%
  group_by(cluster_v2) %>%
  summarise(
    n = n(),
    across(all_of(vars), ~round(mean(.x, na.rm = TRUE), 2)),
    .groups = "drop"
  ) %>%
  print()

# ---- Save ----
saveRDS(combined_all_v2, file = "Data/Processed Dataframes/combined_all_v2.rds")
cat("\n✓ Saved to Data/Processed Dataframes/combined_all_v2.rds\n")

cat("\n╔════════════════════════════════════════════════╗\n")
cat("║  PIPELINE COMPLETE                             ║\n")
cat("║  Input:          NHANES_Combined_Processed     ║\n")
cat("║  Working df:     NHANES_Combined_I             ║\n")
cat("║  Output dataset: combined_all_v2               ║\n")
cat("║  Survey design:  nhanes_design_v2 (post-hoc)   ║\n")
cat("║  Cluster col:    cluster_v2                    ║\n")
cat("║  BP med col:     bp_med_corrected              ║\n")
cat("╚════════════════════════════════════════════════╝\n")
