# Load Packages

library(dplyr)

library(dunn.test)

library(flextable)

# Filter Data

combined_dunn <- combined_all %>%
  filter(!is.na(WTAF2YR_adj), WTAF2YR_adj > 0,
         !is.na(SDMVPSU), !is.na(SDMVSTRA),
         !is.na(cluster),
         !is.na(RIDAGEYR), !is.na(BMXBMI), !is.na(LBXGH), !is.na(LBXGLU),
         !is.na(HOMA_IR), !is.na(HOMA_B), !is.na(LBDHDD), !is.na(LBXSASSI),
         !is.na(LBXSATSI), !is.na(LBXSGTSI), !is.na(WHtR), !is.na(LBXTR),
         !is.na(tg_hdl), !is.na(SBP_mean), !is.na(DBP_mean), !is.na(FLI),
         !is.na(LBXHSCRP), !is.na(LBXWBCSI), !is.na(URDACT), !is.na(eGFR))

# Define your variables

vars <- c("RIDAGEYR", "BMXBMI", "LBXGH", "LBXGLU", "HOMA_IR", "HOMA_B",
          "LBDHDD", "LBXSASSI", "LBXSATSI", "LBXSGTSI", "WHtR", "LBXTR",
          "tg_hdl", "SBP_mean", "DBP_mean", "FLI", "LBXHSCRP", "LBXWBCSI",
          "URDACT", "eGFR")

# Run Dunn's test for each variable

dunn_results <- lapply(vars, function(var) {
  test <- dunn.test(combined_dunn[[var]], 
                    combined_dunn$cluster, 
                    method = "bonferroni", 
                    altp = TRUE)
  data.frame(
    Variable   = var,
    Comparison = test$comparisons,
    Z          = round(test$Z, 3),
    P_adj      = round(test$altP.adjusted, 4)
  )
})

# Combine into one table
dunn_table <- bind_rows(dunn_results) %>%
  mutate(
    Variable = dplyr::recode(Variable,
                             "RIDAGEYR"  = "Age at Screening (Years)",
                             "BMXBMI"    = "Body Mass Index (kg/m²)",
                             "LBXGH"     = "Glycohemoglobin (%)",
                             "LBXGLU"    = "Fasting Glucose (mg/dL)",
                             "HOMA_IR"   = "HOMA-IR",
                             "HOMA_B"    = "HOMA-B",
                             "LBDHDD"    = "Direct HDL-Cholesterol (mg/dL)",
                             "LBXSASSI"  = "AST (IU/L)",
                             "LBXSATSI"  = "ALT (IU/L)",
                             "LBXSGTSI"  = "GGT (IU/L)",
                             "WHtR"      = "Waist-to-Height Ratio",
                             "LBXTR"     = "Triglyceride (mg/dL)",
                             "tg_hdl"    = "TG/HDL Ratio",
                             "SBP_mean"  = "Mean Systolic Blood Pressure (mmHg)",
                             "DBP_mean"  = "Mean Diastolic Blood Pressure (mmHg)",
                             "FLI"       = "Fatty Liver Index",
                             "LBXHSCRP"  = "HS C-Reactive Protein (mg/L)",
                             "LBXWBCSI"  = "White blood cell count (1000 cells/uL)",
                             "URDACT"    = "Albumin creatinine ratio (mg/g)",
                             "eGFR"      = "eGFR"
    ),
    Significant = ifelse(P_adj < 0.05, "✓", "")
  )
print(dunn_table)

# Save as Word Doc

dunn_flex <- dunn_table %>%
  flextable() %>%
  set_header_labels(
    Variable    = "Variable",
    Comparison  = "Comparison",
    Z           = "Z Statistic",
    P_adj       = "Adjusted p-value",
    Significant = "Significant"
  ) %>%
  bold(part = "header") %>%
  bg(i = ~ P_adj < 0.05, bg = "lightyellow") %>%
  merge_v(j = "Variable") %>%
  valign(j = "Variable", valign = "top") %>%
  autofit() %>%
  add_footer_lines("Bonferroni-adjusted p-values. Significant = p < 0.05")

dunn_flex

save_as_docx(dunn_flex, path = "Data/Output/dunn_pairwise_results.docx")