# Loading Packages
library(dplyr)
library(ConsensusClusterPlus)
library(tidyverse)
library(gt)

# (Optional but commonly needed for saving gt tables as PNG)
 install.packages("webshot2")
 library(webshot2)

# ---- Create HOMA-B ----
NHANES_Combined_I <- NHANES_Combined_Processed %>%
  mutate(
    HOMA_B = 360 * LBXIN / (LBXGLU - 63)
  ) %>%
  mutate(
    HOMA_B = ifelse(is.finite(HOMA_B), HOMA_B, NA_real_)
  )

# ---- Variables to cluster on ----
vars <- c(
  "RIDAGEYR",   # age
  "BMXBMI",     # BMI
  "LBXGH",      # HbA1c
  "LBXGLU",     # fasting glucose
  "HOMA_IR",
  "HOMA_B",
  "LBDHDD",     # HDL-C
  "LBXSASSI",   # AST
  "LBXSATSI",   # ALT
  "LBXSGTSI",   # GGT
  "WHtR",       # Waist to Height
  "LBXTR",      # Trig
  "tg_hdl",     # Trig/HDL
  "SBP_mean",   # Systolic BP
  "DBP_mean",   # Diastolic BP
  "FLI",        # Fatty Liver Index
  "LBXHSCRP",   # C-Reactive Protein
  "LBXWBCSI",   # White Blood Cell Count
  "URDACT",     # Albumin-Creatinine Ratio
  "eGFR"        # Glomerular Filtration Rate
)

# ---- Subset and keep SEQN attached ----
nhanes_sub <- NHANES_Combined_I %>%
  select(SEQN, all_of(vars))

# ---- Log-transform skewed variables (safer typing than base ifelse) ----
nhanes_sub <- nhanes_sub %>%
  mutate(across(
    c(HOMA_IR, HOMA_B, LBXTR, LBXHSCRP, LBXSGTSI, URDACT),
    ~ dplyr::if_else(. <= 0 | is.na(.), NA_real_, log(.))
  ))

# ---- Complete cases for clustering ----
nhanes_cc <- nhanes_sub %>%
  drop_na()

# Keep SEQN vector aligned to clustering rows
seqn_vector <- nhanes_cc$SEQN

# Drop SEQN column for scaling/clustering
nhanes_cc_mat <- nhanes_cc %>%
  select(-SEQN)

# Set rownames to SEQN so everything stays aligned/traceable
rownames(nhanes_cc_mat) <- seqn_vector

# ---- Scaling ----
nhanes_scaled <- scale(nhanes_cc_mat)

# ---- Transpose for ConsensusClusterPlus (features in rows, samples in cols) ----
nhanes_ccp <- t(nhanes_scaled)

# Explicitly carry sample IDs as column names
colnames(nhanes_ccp) <- seqn_vector

# ---- Consensus Clustering ----
set.seed(2026)

cc_results <- ConsensusClusterPlus(
  nhanes_ccp,
  maxK = 9,
  reps = 100,
  pItem = 0.8,
  pFeature = 1,
  clusterAlg = "km",
  distance = "euclidean",
  seed = 2026,
  plot = "png"
)

# ---- Label consensusClass with SEQN (these are sample assignments) ----
for (k in 2:9) {
  names(cc_results[[k]]$consensusClass) <- colnames(nhanes_ccp)
}

# ---- Individual Cluster Consensus Scores ----
title <- "NHANES Consensus Clustering"
icl <- calcICL(cc_results, title = title, plot = "png")

print(icl$clusterConsensus)

# ---- Average K Consensus Score ----
cc_mat <- icl[["clusterConsensus"]]
avg_consensus_by_k <- tapply(cc_mat[, "clusterConsensus"], cc_mat[, "k"], mean)
avg_consensus_by_k

# ---- HC to confirm groups ----
K <- 5
cm <- cc_results[[K]]$consensusMatrix
dist_cm <- as.dist(1 - cm)
hc <- hclust(dist_cm, method = "ward.D2")
final_clusters <- cutree(hc, k = K)

# ---- Combine clusters with SEQN (aligned to consensus matrix order) ----
cluster_df <- data.frame(
  SEQN = as.numeric(colnames(nhanes_ccp)),
  cluster = as.integer(final_clusters)
)

# ---- Merge into full dataset ----
NHANES_Combined_I <- NHANES_Combined_I %>%
  left_join(cluster_df, by = "SEQN")

# ---- Obtain Cluster Means ----
cluster_means <- NHANES_Combined_I %>%
  filter(!is.na(cluster)) %>%
  group_by(cluster) %>%
  summarise(across(all_of(vars), ~ mean(.x, na.rm = TRUE)))

cluster_means

# ---- Cluster Means Table ----
clt <- cluster_means %>%
  gt() %>%
  tab_header(title = "Cluster Characteristics") %>%
  cols_label(
    RIDAGEYR = "Age at Screening (Years)",
    BMXBMI   = "Body Mass Index (kg/m²)",
    LBXGH    = "Glycohemoglobin (%)",
    LBXGLU   = "Fasting Glucose (mg/dL)",
    HOMA_IR  = "HOMA-IR",
    HOMA_B   = "HOMA-B",
    LBDHDD   = "Direct HDL-Cholesterol (mg/dL)",
    LBXSASSI = "AST (IU/L)",
    LBXSATSI = "ALT (IU/L)",
    LBXSGTSI = "GGT (IU/L)",
    WHtR     = "Waist-to-Height Ratio",
    LBXTR    = "Triglyceride (mg/dL)",
    tg_hdl   = "TG/HDL Ratio",
    SBP_mean = "Mean Systolic Blood Pressure (mmHg)",
    DBP_mean = "Mean Diastolic Blood Pressure (mmHg)",
    FLI      = "Fatty Liver Index",
    LBXHSCRP = "HS C-Reactive Protein (mg/L)",
    LBXWBCSI = "White blood cell count (1000 cells/uL)",
    URDACT   = "Albumin creatinine ratio (mg/g)",
    eGFR     = "eGFR"
  ) %>%
  fmt_number(columns = -cluster, decimals = 2) %>%
  cols_align(align = "center", everything()) %>%
  cols_width(everything() ~ px(120))

# ---- Save table ----
gtsave(
  clt,
  "Cluster_Characteristics.png",
  zoom = 2,
  vwidth = 3000
)