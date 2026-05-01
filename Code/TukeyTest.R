# Load Packages

library(dplyr)

library(gt)

library(flextable)

library(officer)

library(moments)

library(car)

# Filtering Data

combined_tukey <- combined_all %>%
  filter(!is.na(WTAF2YR_adj), WTAF2YR_adj > 0,
         !is.na(SDMVPSU), !is.na(SDMVSTRA),
         !is.na(cluster),
         !is.na(RIDAGEYR), !is.na(BMXBMI), !is.na(LBXGH), !is.na(LBXGLU),
         !is.na(HOMA_IR), !is.na(HOMA_B), !is.na(LBDHDD), !is.na(LBXSASSI),
         !is.na(LBXSATSI), !is.na(LBXSGTSI), !is.na(WHtR), !is.na(LBXTR),
         !is.na(tg_hdl), !is.na(SBP_mean), !is.na(DBP_mean), !is.na(FLI),
         !is.na(LBXHSCRP), !is.na(LBXWBCSI), !is.na(URDACT), !is.na(eGFR))

# Tukey Test

# Define your variables
vars <- c("RIDAGEYR", "BMXBMI", "LBXGH", "LBXGLU", "HOMA_IR", "HOMA_B",
          "LBDHDD", "LBXSASSI", "LBXSATSI", "LBXSGTSI", "WHtR", "LBXTR",
          "tg_hdl", "SBP_mean", "DBP_mean", "FLI", "LBXHSCRP", "LBXWBCSI",
          "URDACT", "eGFR")

# Run Tukey HSD for each variable
tukey_results <- lapply(vars, function(var) {
  aov_fit <- aov(combined_tukey[[var]] ~ factor(combined_tukey$cluster))
  tukey   <- TukeyHSD(aov_fit)
  df      <- as.data.frame(tukey[[1]])
  df$Variable   <- var
  df$Comparison <- rownames(df)
  rownames(df)  <- NULL
  df
})

# Combine into one table
tukey_table <- bind_rows(tukey_results) %>%
  select(Variable, Comparison, diff, lwr, upr, `p adj`)

# Format and save as gt object
tukey_gt <- tukey_table %>%
  mutate(
    diff    = round(diff, 3),
    lwr     = round(lwr, 3),
    upr     = round(upr, 3),
    `p adj` = round(`p adj`, 4),
    sig     = ifelse(`p adj` < 0.05, "✓", "")
  ) %>%
  gt(groupname_col = "Variable") %>%
  tab_header(
    title    = "Tukey HSD Pairwise Comparisons",
    subtitle = "All cluster comparisons across 20 variables"
  ) %>%
  cols_label(
    Comparison = "Comparison",
    diff       = "Mean Difference",
    lwr        = "Lower 95% CI",
    upr        = "Upper 95% CI",
    `p adj`    = "Adjusted p-value",
    sig        = "Significant"
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) %>%
  tab_style(
    style     = cell_fill(color = "lightyellow"),
    locations = cells_body(rows = `p adj` < 0.05)
  ) %>%
  opt_row_striping() %>%
  tab_footnote("Significant = p < 0.05 after Tukey adjustment")

# View it
tukey_gt

# Save it
gtsave(tukey_gt, "Data/Output/tukey_all_comparisons.html")

# Word Doc Table

tukey_flex <- tukey_table %>%
  mutate(
    diff    = round(diff, 3),
    lwr     = round(lwr, 3),
    upr     = round(upr, 3),
    `p adj` = round(`p adj`, 4),
    sig     = ifelse(`p adj` < 0.05, "✓", "")
  ) %>%
  flextable() %>%
  set_header_labels(
    Variable   = "Variable",
    Comparison = "Comparison",
    diff       = "Mean Difference",
    lwr        = "Lower 95% CI",
    upr        = "Upper 95% CI",
    `p adj`    = "Adjusted p-value",
    sig        = "Significant"
  ) %>%
  bold(part = "header") %>%
  bg(i = ~ `p adj` < 0.05, bg = "lightyellow") %>%
  merge_v(j = "Variable") %>%   # groups rows by variable like gt did
  valign(j = "Variable", valign = "top") %>%
  autofit() %>%
  add_footer_lines("Significant = p < 0.05 after Tukey adjustment")

# Save as Word doc
save_as_docx(tukey_flex, path = "Data/Output/tukey_all_comparisons.docx")

# Check Assumptions for Tukey's

# Check skewness

sapply(combined_tukey[, vars], skewness, na.rm = TRUE)

# Check variance homogeneity (Levene's test)

lapply(vars, function(var) {
  leveneTest(combined_tukey[[var]] ~ factor(combined_tukey$cluster))
})
