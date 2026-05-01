# Loading Packages
library(dplyr)
library(ggplot2)
library(patchwork)

# -------------------------
# 1) Variables (log versions already created)
# -------------------------
vars_named <- c(
  RIDAGEYR = "Age at Screening (Years)",
  BMXBMI   = "Body Mass Index (kg/m²)",
  LBXGH    = "Glycohemoglobin (%)",
  LBXGLU   = "Fasting Glucose (mg/dL)",
  HOMA_IR  = "HOMA-IR",
  HOMA_B   = "HOMA-B",
  LBDHDD   = "Direct HDL-Cholesterol (mg/dL)",
  LBXSASSI = "AST (IU/L)",
  LBXSATSI = "ALT (IU/L)",
  ln_GGT      = "Log(GGT)",
  WHtR     = "Waist-to-Height Ratio",
  ln_LBXTR    = "Log(Triglycerides)",
  tg_hdl   = "TG/HDL Ratio",
  SBP_mean = "Mean Systolic Blood Pressure (mmHg)",
  DBP_mean = "Mean Diastolic Blood Pressure (mmHg)",
  FLI      = "Fatty Liver Index",
  ln_LBXHSCRP = "Log(hsCRP)",
  LBXWBCSI = "WBC (1000 cells/uL)",
  ln_URDACT   = "Log(Albumin/Creatinine Ratio)",
  eGFR     = "eGFR"
)

# -------------------------
# 2) Cluster factor
# -------------------------
dfp <- NHANES_Combined_I %>%
  mutate(cluster_f = factor(cluster))  # change if cluster column named differently

# -------------------------
# 3) Panel function
# -------------------------
panel <- function(v, lab) {
  
  # Remove NAs for THIS variable only
  temp <- dfp %>%
    filter(!is.na(.data[[v]]))
  
  # Trim extreme outliers (1st–99th percentile)
  upper <- quantile(temp[[v]], 0.99, na.rm = TRUE)
  lower <- quantile(temp[[v]], 0.01, na.rm = TRUE)
  
  ggplot(temp, aes(x = cluster_f, y = .data[[v]], fill = cluster_f)) +
    geom_boxplot(width = 0.65, outlier.size = 1) +
    coord_cartesian(ylim = c(lower, upper)) +
    labs(x = NULL, y = lab) +
    guides(fill = "none") +
    theme_classic(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.margin = margin(6, 10, 6, 6)
    )
}

# -------------------------
# 4) Build + stitch
# -------------------------
plots <- Map(panel, names(vars_named), unname(vars_named))

fig <- wrap_plots(plots, ncol = 3) +
  plot_annotation(tag_levels = "A")

fig

# -------------------------
# 5) Save large to avoid smushing
# -------------------------
ggsave("cluster_panels.png", fig, width = 14, height = 18, dpi = 300)
ggsave("cluster_panels.pdf", fig, width = 14, height = 18)