# -------------------------------------------------------
# SANITY CHECKS
# -------------------------------------------------------

cat("---- SANITY CHECKS ----\n")

# 1. Length alignment check
cat("Length of final_clusters:", length(final_clusters), "\n")
cat("Number of clustered individuals:", ncol(nhanes_ccp), "\n")

stopifnot(length(final_clusters) == ncol(nhanes_ccp))

cat("✔ Cluster length matches number of individuals\n\n")

# 2. SEQN alignment check
cat("First 5 SEQNs in clustering matrix:\n")
print(head(colnames(nhanes_ccp), 5))

cat("First 5 names of final_clusters:\n")
print(head(names(final_clusters), 5))

cat("\n✔ If these match, alignment is correct\n\n")

# 3. Cluster size distribution
cluster_sizes <- table(final_clusters)

cat("Cluster sizes:\n")
print(cluster_sizes)

cat("\nProportion per cluster:\n")
print(round(prop.table(cluster_sizes), 3))

# 4. Check for empty clusters
if(any(cluster_sizes == 0)){
  warning("⚠ One or more clusters are empty.")
} else {
  cat("\n✔ No empty clusters detected\n")
}

# 5. Basic separation diagnostic
cm <- cc_results[[K]]$consensusMatrix
cl <- final_clusters

within_means <- sapply(sort(unique(cl)), function(g) {
  idx <- which(cl == g)
  if (length(idx) < 2) return(NA_real_)
  m <- cm[idx, idx]
  mean(m[upper.tri(m)], na.rm = TRUE)  # within-cluster pairwise consensus
})

within_means
mean(within_means, na.rm = TRUE)

# Check Sequence Integrity

truth_df <- data.frame(
  SEQN = as.numeric(colnames(nhanes_ccp)),
  cluster_truth = final_clusters
)

check_df <- NHANES_Combined_I %>%
  select(SEQN, cluster)

comparison <- truth_df %>%
  left_join(check_df, by = "SEQN")

sum(comparison$cluster_truth != comparison$cluster, na.rm = TRUE)
