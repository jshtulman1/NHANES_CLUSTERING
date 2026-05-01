# ============================================================
# PAIRWISE DUNN COMPARISONS — 20 CLUSTERING VARIABLES
# Method: Dunn's test with Benjamini-Hochberg correction
# Output: Formatted Word table + CSV
# ============================================================

library(dplyr)
library(tidyr)
library(dunn.test)
library(flextable)
library(officer)
options(survey.lonely.psu = "adjust")


# ============================================================
# STEP 1: ANALYTIC DATASET
# ============================================================

cat("╔══════════════════════════════════════════════╗\n")
cat("║  PAIRWISE DUNN COMPARISONS (BH-corrected)    ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

vars <- c(
  "RIDAGEYR", "BMXBMI",   "LBXGH",    "LBXGLU",
  "HOMA_IR",  "HOMA_B",   "LBDHDD",
  "LBXSASSI", "LBXSATSI", "LBXSGTSI",
  "WHtR",     "LBXTR",    "tg_hdl",
  "SBP_mean", "DBP_mean", "FLI",
  "LBXHSCRP", "LBXWBCSI", "URDACT",   "eGFR"
)

var_labels <- c(
  "Age (years)", "BMI (kg/m\u00B2)", "HbA1c (%)", "Fasting Glucose (mg/dL)",
  "HOMA-IR", "HOMA-\u03B2", "HDL-C (mg/dL)",
  "AST (IU/L)", "ALT (IU/L)", "GGT (IU/L)",
  "Waist-to-Height Ratio", "Triglycerides (mg/dL)", "TG/HDL Ratio",
  "Systolic BP (mmHg)", "Diastolic BP (mmHg)", "Fatty Liver Index",
  "hs-CRP (mg/L)", "WBC (1000 cells/\u00B5L)", "ACR (mg/g)", "eGFR (mL/min/1.73m\u00B2)"
)

dat_pw <- combined_all_v2 %>%
  filter(
    !is.na(cluster_v2),
    !is.na(SDMVPSU), !is.na(SDMVSTRA_unique),
    !is.na(WTAF2YR_adj), WTAF2YR_adj > 0
  ) %>%
  mutate(cluster_v2 = factor(cluster_v2))

cat("Analytic N:", nrow(dat_pw), "\n")
cat("Cluster sizes:\n")
print(table(dat_pw$cluster_v2))


# ============================================================
# STEP 2: RUN DUNN'S TEST FOR EACH VARIABLE
# ============================================================

all_results <- list()

for (i in seq_along(vars)) {
  v <- vars[i]
  label <- var_labels[i]

  dat_v <- dat_pw %>% filter(!is.na(.data[[v]]))

  dunn_out <- invisible(
    dunn.test(
      x       = dat_v[[v]],
      g       = dat_v$cluster_v2,
      method  = "bh",
      kw      = TRUE,
      label   = FALSE,
      table   = FALSE,
      list    = TRUE,
      altp    = TRUE
    )
  )

  res <- data.frame(
    Variable    = label,
    Comparison  = dunn_out$comparisons,
    Z           = round(dunn_out$Z, 3),
    p_adj       = dunn_out$altP.adjusted,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      Significant = ifelse(p_adj < 0.05, "\u2713", "")
    )

  all_results[[i]] <- res
}

dunn_results <- bind_rows(all_results)

cat("\nTotal pairwise tests:", nrow(dunn_results), "\n")
cat("Significant (BH p < 0.05):", sum(dunn_results$p_adj < 0.05), "\n")
cat("Non-significant:", sum(dunn_results$p_adj >= 0.05), "\n\n")


# ============================================================
# STEP 3: FORMAT P-VALUES FOR DISPLAY
# ============================================================

dunn_results <- dunn_results %>%
  mutate(
    `Adjusted p-value` = case_when(
      p_adj < 0.0001 ~ formatC(p_adj, format = "e", digits = 2),
      p_adj < 0.001  ~ formatC(p_adj, format = "f", digits = 4),
      TRUE           ~ formatC(p_adj, format = "f", digits = 4)
    )
  )

# Only show variable name on first row of each group
dunn_display <- dunn_results %>%
  group_by(Variable) %>%
  mutate(
    Variable = ifelse(row_number() == 1, Variable, "")
  ) %>%
  ungroup() %>%
  select(Variable, Comparison, `Z Statistic` = Z,
         `Adjusted p-value`, Significant)


# ============================================================
# STEP 4: BUILD FORMATTED FLEXTABLE
# ============================================================

ft <- flextable(dunn_display) %>%
  # Column widths
  width(j = "Variable", width = 2.2) %>%
  width(j = "Comparison", width = 0.9) %>%
  width(j = "Z Statistic", width = 1.0) %>%
  width(j = "Adjusted p-value", width = 1.3) %>%
  width(j = "Significant", width = 0.8) %>%
  # Alignment
  align(j = c("Comparison", "Z Statistic", "Adjusted p-value", "Significant"),
        align = "center", part = "all") %>%
  align(j = "Variable", align = "left", part = "all") %>%
  # Header styling
  bold(part = "header") %>%
  bg(part = "header", bg = "#D9E2F3") %>%
  # Font
  font(fontname = "Arial", part = "all") %>%
  fontsize(size = 9, part = "body") %>%
  fontsize(size = 10, part = "header") %>%
  # Borders — add horizontal line between variable groups
  border_inner_h(border = fp_border(color = "#DDDDDD", width = 0.5)) %>%
  border_outer(border = fp_border(color = "#000000", width = 1)) %>%
  # Add heavier border between variable groups
  hline(i = which(dunn_display$Variable != "")[-1] - 1,
        border = fp_border(color = "#000000", width = 1)) %>%
  # Bold the variable name rows
  bold(i = which(dunn_display$Variable != ""), j = "Variable") %>%
  # Footer
  add_footer_lines(
    "Benjamini-Hochberg adjusted p-values. Significant = p < 0.05. \u2713 = significant."
  ) %>%
  fontsize(size = 8, part = "footer") %>%
  italic(part = "footer")

ft


# ============================================================
# STEP 5: SAVE
# ============================================================

# Word table
save_as_docx(ft, path = "Data/Output/dunn_pairwise_results_v2.docx")
cat("\u2713 Saved: Data/Output/dunn_pairwise_results_v2.docx\n")

# CSV (for reference / supplementary)
dunn_results %>%
  select(Variable, Comparison, Z, p_adj, Significant) %>%
  write.csv(file = "Data/Output/dunn_pairwise_results_v2.csv", row.names = FALSE)
cat("\u2713 Saved: Data/Output/dunn_pairwise_results_v2.csv\n")

# Non-significant pairs summary
ns_pairs <- dunn_results %>%
  filter(p_adj >= 0.05) %>%
  select(Variable, Comparison, Z, p_adj)

cat("\n--- Non-significant pairs (BH p >= 0.05) ---\n")
if (nrow(ns_pairs) > 0) {
  print(as.data.frame(ns_pairs))
} else {
  cat("All pairwise comparisons are significant\n")
}

cat("\n\u2713 Complete\n")
