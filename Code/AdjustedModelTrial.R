library(survey)
library(dplyr)
library(broom)

# ------------------------------------------------------------
# 0) Prep: outcomes + factor cleanup
# ------------------------------------------------------------
DAT2 <- dat %>%
  mutate(
    # cluster: make factor; cluster 1 reference
    cluster = factor(cluster),
    cluster = relevel(cluster, ref = "1"),
    
    # meds flags as 0/1
    bp_meds = as.integer(bp_meds == 1),
    statins = as.integer(statins == 1),
    
    # outcomes (Supplementary Table 3 style)
    high_sbp_or_meds = as.integer(sbp_mean >= 140 | bp_meds == 1),
    high_dbp_or_meds = as.integer(DBP_mean >=  90 | bp_meds == 1),
    hypertension     = as.integer((sbp_mean >= 140) | (DBP_mean >= 90) | (bp_meds == 1)),
    
    # covariates
    sex       = factor(sex),
    smoking   = factor(smoking),
    education = factor(educ),
    race_eth  = factor(race_eth)
    # pir stays numeric
  ) %>%
  # drop NAs for vars used in prevalence/models
  filter(
    !is.na(cluster),
    !is.na(WTAF2YR_adj),
    !is.na(SDMVPSU),
    !is.na(SDMVSTRA),
    !is.na(sbp_mean),
    !is.na(DBP_mean),
    !is.na(bp_meds),
    !is.na(statins),
    !is.na(sex),
    !is.na(smoking),
    !is.na(educ),
    !is.na(pir),
    !is.na(race_eth)
  )

# ------------------------------------------------------------
# 1) Survey design object
# ------------------------------------------------------------
des <- svydesign(
  id      = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTAF2YR_adj,
  nest    = TRUE,
  data    = DAT2
)

# ------------------------------------------------------------
# 2) Weighted prevalence by cluster (with 95% CI)
# ------------------------------------------------------------
prev_by_cluster <- function(outcome_var, design) {
  f <- as.formula(paste0("~", outcome_var))
  svyby(
    formula = f,
    by = ~cluster,
    design = design,
    FUN = svymean,
    vartype = c("ci"),
    na.rm = TRUE,
    keep.names = FALSE
  ) %>%
    as_tibble() %>%
    transmute(
      cluster,
      prevalence = .data[[outcome_var]],
      ci_l = ci_l,
      ci_u = ci_u,
      prev_ci = sprintf("%.2f (%.2f - %.2f)", prevalence, ci_l, ci_u)
    )
}

prev_htn <- prev_by_cluster("hypertension", des)

# ------------------------------------------------------------
# 3) Survey-weighted logistic regression ORs (unadj + adj)
#    NOTE: adjusted model does NOT include bp_meds
# ------------------------------------------------------------
fit_or <- function(outcome_var, design) {
  
  f_unadj <- as.formula(paste0(outcome_var, " ~ cluster"))
  
  f_adj <- as.formula(paste0(
    outcome_var,
    " ~ cluster + age + sex + smoking + educ + statins"
  ))
  
  m0 <- svyglm(f_unadj, design = design, family = quasibinomial())
  m1 <- svyglm(f_adj,   design = design, family = quasibinomial())
  
  tidy_or <- function(model, label) {
    broom::tidy(model) %>%
      filter(grepl("^cluster", term)) %>%
      mutate(
        model = label,
        cluster = sub("^cluster", "", term),
        OR  = exp(estimate),
        lo  = exp(estimate - 1.96 * std.error),
        hi  = exp(estimate + 1.96 * std.error),
        or_ci = sprintf("%.2f (%.2f - %.2f)", OR, lo, hi)
      ) %>%
      select(model, cluster, or_ci, p.value)
  }
  
  bind_rows(
    tidy_or(m0, "Unadjusted"),
    tidy_or(m1, "Adjusted")
  )
}

ors_htn <- fit_or("hypertension", des)

# ------------------------------------------------------------
# 4) Assemble final “table-style” output (cluster 1 is reference)
# ------------------------------------------------------------
final_htn <- prev_htn %>%
  left_join(ors_htn %>% filter(model == "Unadjusted"),
            by = "cluster") %>%
  rename(unadjusted_or = or_ci, unadjusted_p = p.value) %>%
  left_join(ors_htn %>% filter(model == "Adjusted"),
            by = "cluster") %>%
  rename(adjusted_or = or_ci, adjusted_p = p.value) %>%
  arrange(as.integer(as.character(cluster)))

final_htn



library(gt)

final_htn %>%
  mutate(
    cluster = paste0("Cluster ", cluster)
  ) %>%
  select(
    cluster,
    prev_ci,
    unadjusted_or,
    unadjusted_p,
    adjusted_or,
    adjusted_p
  ) %>%
  gt() %>%
  tab_header(
    title = "Hypertension by Cluster",
    subtitle = "Weighted prevalence and survey-weighted logistic regression"
  ) %>%
  cols_label(
    cluster = "Cluster",
    prev_ci = "Prevalence (95% CI)",
    unadjusted_or = "Unadjusted OR (95% CI)",
    unadjusted_p = "Unadjusted p",
    adjusted_or = "Adjusted OR (95% CI)",
    adjusted_p = "Adjusted p"
  ) %>%
  fmt_number(
    columns = c(unadjusted_p, adjusted_p),
    decimals = 3
  ) %>%
  cols_align("center") %>%
  tab_options(
    table.font.size = 13,
    heading.title.font.size = 16
  )


