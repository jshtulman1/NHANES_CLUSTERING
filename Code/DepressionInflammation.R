# Load Packages

library(dplyr)       
library(tidyverse)   
library(haven)       
library(survey)      
library(MASS)        

# Depression Inflammation Indicators Modeling

nhanes_design_mec <- svydesign(
  ids     = ~SDMVPSU,
  strata  = ~SDMVSTRA,
  weights = ~WTMEC2YR_adj,
  data    = combined_all %>% filter(!is.na(WTMEC2YR_adj) & WTMEC2YR_adj > 0),
  nest    = TRUE
)

model_crp <- svyglm(
  ln_LBXHSCRP ~ smoking + ALQ151_binary + physical_activity + DR1TSUGR + 
    SLD012 + depression + gender + race + INDFMPIR + 
    insurance + PAD680,
  design = nhanes_design_mec,
  family = gaussian()
)

coef(model_crp)
confint(model_crp)

combined_all <- combined_all %>%
  mutate(depression_num = as.numeric(depression) - 1)  # 0, 1, 2

model_crp2 <- svyglm(
  ln_LBXHSCRP ~ depression_num + smoking + ALQ151_binary + physical_activity + DR1TSUGR + 
    SLD012 + gender + race + INDFMPIR + 
    insurance + PAD680,
  design = nhanes_design_mec,
  family = gaussian()
)

exp(coef(model_crp2))
exp(confint(model_crp2))

