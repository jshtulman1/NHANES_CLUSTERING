# Load Packages

library(dplyr)
library(mice)
library(cluster)
library(mclust)
library(tidyr)
library(formattable)

# Merge Dataframes

NHANES_Combined_Clustering_Subset1 <- bind_rows(
  NHANES2015_2016_ClusteringSubset,
  NHANES2017_2020_ClusteringSubset,
  NHANES2021_2023_ClusteringSubset
)

# Drop Extra Variables

NHANES_Combined_Clustering_Subset1 <- NHANES_Combined_Clustering_Subset1 %>%
  select(
    -c(
      BPXSY1, BPXSY2, BPXSY3, BPXSY4,   # raw systolic readings
      BPXDI1, BPXDI2, BPXDI3, BPXDI4,   # raw diastolic readings
      LBXTR, LBXHSCRP, URDACT, # raw labs
      BPXOSY1, BPXOSY2, BPXOSY3,         # NHANES alternative BP vars
      BPXODI1, BPXODI2, BPXODI3          # NHANES alternative BP vars
    )
  )

# Multiple Imputation

# imputed <- mice(NHANES_Combined_Clustering_Subset, m = 5, method = "pmm", seed = 123)

# NHANES_Combined_Imputed_Clustering_Subset <- complete(imputed, 1)

# Z-Score Standardization

z_vars <- c(
  "RIDAGEYR", "LBXGH", "LBXGLU", "HOMA_IR", "BMXBMI", "WHtR",
  "ln_LBXTR", "LBDHDD", "tg_hdl", "SBP_mean", "DBP_mean",
  "LBXSATSI", "LBXSASSI", "ln_GGT", "FLI",
  "ln_LBXHSCRP", "LBXWBCSI", "ln_URDACT", "eGFR"
)

NHANES_Combined_Imputed_Clustering_Subset <- NHANES_Combined_Imputed_Clustering_Subset %>%
  mutate(across(all_of(z_vars), ~ scale(.)[,1], .names = "{.col}_z"))

# Isolating Clustering Variables

Standardized_Clustering_Subset <- NHANES_Combined_Imputed_Clustering_Subset %>%
  select(ends_with("_z"))

# Elbow

set.seed(123)  
max_k <- 15    

wss <- sapply(1:max_k, function(k){
  kmeans(Standardized_Clustering_Subset, centers = k, nstart = 25, iter.max = 100)$tot.withinss
})

plot(1:max_k, wss, type = "b", pch = 19,
     xlab = "Number of clusters (k)",
     ylab = "Total within-cluster sum of squares",
     main = "Elbow Method with Extended Iterations")

# Silhouette

avg_sil <- sapply(2:12, function(k){
  km <- kmeans(Standardized_Clustering_Subset, centers = k, nstart = 50, iter.max = 100)
  sil <- silhouette(km$cluster, dist(Standardized_Clustering_Subset))
  mean(sil[, 3])
})

plot(2:12, avg_sil, type = "b", pch = 19,
     xlab = "Number of clusters", ylab = "Average silhouette width",
     main = "Silhouette Method for NHANES Clustering")

# LPA (Gaussian Mixture Models)

LPA_Data <- NHANES_Combined_Imputed_Clustering_Subset %>%
  select(ends_with("_z")) %>%
  as.matrix()

set.seed(123)
gmm_result <- Mclust(LPA_Data, G = 1:10)

summary(gmm_result)

gmm_result$G

gmm_result$BIC

# Running With Ideal Number of Clusters

gmm_8 <- Mclust(LPA_Data, G = 8, modelNames = "VVV")

# Assigning Cluster

NHANES_Combined_Imputed_Clustering_Subset$Cluster_GMM8 <- gmm_8$classification
table(NHANES_Combined_Imputed_Clustering_Subset$Cluster_GMM8)

# Mean Score for Variables in Each Cluster

# Retrieve Original Variable Values

NHANES_Combined_Imputed_Clustering_Subset <- NHANES_Combined_Imputed_Clustering_Subset %>%
  mutate(
    LBXTR = exp(ln_LBXTR),               
    LBXSGTSI = exp(ln_GGT),             
    LBXHSCRP = exp(ln_LBXHSCRP),         
    URDACT = exp(ln_URDACT)              
  )

# Display Cluster Means

original_vars <- c("RIDAGEYR", "LBXGH", "LBXGLU", "HOMA_IR", "BMXBMI", "WHtR",
                   "LBXTR", "LBDHDD", "tg_hdl", "SBP_mean", "DBP_mean",
                   "LBXSATSI", "LBXSASSI", "LBXSGTSI", "FLI", 
                   "LBXHSCRP", "LBXWBCSI", "URDACT", "eGFR")


cluster_means <- aggregate(
  NHANES_Combined_Imputed_Clustering_Subset[, original_vars],
  by = list(Cluster = NHANES_Combined_Imputed_Clustering_Subset$Cluster_GMM8),
  FUN = mean, na.rm = TRUE
)

cluster_means

# Creating Table

# Select original variables and cluster
cluster_data <- NHANES_Combined_Imputed_Clustering_Subset %>%
  select(Cluster_GMM8, RIDAGEYR, LBXGH, LBXGLU, HOMA_IR, BMXBMI, WHtR,
         LBXTR, LBDHDD, tg_hdl, SBP_mean, DBP_mean, LBXSATSI, LBXSASSI,
         LBXSGTSI, FLI, LBXHSCRP, LBXWBCSI, URDACT, eGFR)

# Compute means per cluster
cluster_summary <- cluster_data %>%
  group_by(Cluster_GMM8) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
  ungroup()

# Long Format

cluster_long <- cluster_summary %>%
  pivot_longer(-Cluster_GMM8, names_to = "Variable", values_to = "Mean")

# Standardize Across Clusters

cluster_long <- cluster_long %>%
  group_by(Variable) %>%
  mutate(Relative = scale(Mean)[,1]) %>%
  ungroup()

# Generate Table

cluster_wide <- cluster_long %>%
  select(Cluster_GMM8, Variable, Mean) %>%
  pivot_wider(names_from = Variable, values_from = Mean)

# Color code: blue = low, red = high
formattable(cluster_wide, list(
  area(col = 2:ncol(cluster_wide)) ~ color_tile("lightblue", "salmon")
))








