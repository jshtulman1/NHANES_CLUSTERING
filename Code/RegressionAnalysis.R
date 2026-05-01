# Load Packages

library(dplyr)
library(survey)
library(svyVGAM)

options(survey.lonely.psu = "adjust")

dat <- NHANES_Combined_I %>%
  filter(!is.na(WTAF2YR_adj), WTAF2YR_adj > 0,
         !is.na(SDMVPSU), !is.na(SDMVSTRA)) %>%
  mutate(
    # Outcome: most-likely class membership
    class = factor(cluster),
    
    # Demographics
    age = RIDAGEYR,
    sex = factor(ifelse(RIAGENDR %in% c(1,2), RIAGENDR, NA),
                 levels = c(1,2), labels = c("Male","Female")),
    race_eth = factor(ifelse(RIDRETH1 %in% 1:5, RIDRETH1, NA),
                      levels = 1:5,
                      labels = c("Mexican American","Other Hispanic","White","Black","Other")),
    educ = factor(ifelse(DMDEDUC2 %in% 1:5, DMDEDUC2, NA),
                  levels = 1:5,
                  labels = c("<9th Grade","9-11th Grade","HS Graduate","Some College","College Degree")),
    pir = INDFMPIR,
    insured = factor(ifelse(HIQ011 %in% c(1,2), HIQ011, NA),
                     levels = c(1,2), labels = c("Yes","No")),
    nativity = factor(ifelse(DMDBORN4 %in% c(1,2), DMDBORN4, NA),
                      levels = c(1,2), labels = c("USA","Other")),
    food_sec = factor(ifelse(FSDAD %in% 1:4, FSDAD, NA),
                      levels = 1:4,
                      labels = c("Full Food Security","Marginal Security","Low Food Security","Very Low Food Security")),
    
    # Smoking
    smoking = factor(ifelse(SMQ020 %in% c(1,2), SMQ020, NA),
                     levels = c(1,2), labels = c("Yes","No")),
    
    # Recode special missing codes for MVPA components (7777 refused, 9999 DK)
    across(
      any_of(c("PAD660","PAD675","PAD800","PAD820")),
      ~ dplyr::na_if(dplyr::na_if(as.numeric(.x), 7777), 9999)
    ),
    
    # MVPA uses whichever pair is available for that survey cycle
    MVPA = dplyr::coalesce(PAD660 + PAD675, PAD800 + PAD820),
    
    # Sleep - DPQ030
    sleep = factor(ifelse(DPQ030 %in% 0:3, DPQ030, NA),
                   levels = 0:3,
                   labels = c("None", "Several days", "More than half the days", "Nearly every day")),
    
    # Depression - DPQ020
    depression = factor(ifelse(DPQ020 %in% 0:3, DPQ020, NA),
                        levels = 0:3,
                        labels = c("Not at all", "Several days", "More than half the days", "Nearly every day")),
    
    # Med indicators (Yes/No)
    hypertension = factor(ifelse(BPQ020 %in% c(1,2), BPQ020, NA),
                          levels = c(1,2), labels = c("Yes","No")),
    hyperlipidemia = factor(ifelse(BPQ080 %in% c(1,2), BPQ080, NA),
                            levels = c(1,2), labels = c("Yes","No")),
    bp_meds = factor(ifelse(BPQ050A %in% c(1,2), BPQ050A, NA),
                     levels = c(1,2), labels = c("Yes","No")),
    statins = factor(ifelse(BPQ100D %in% c(1,2), BPQ100D, NA),
                     levels = c(1,2), labels = c("Yes","No")),
    
    # Stratifier: BMI category
    bmi = suppressWarnings(as.numeric(BMXBMI)),
    bmi_cat = cut(bmi,
                  breaks = c(-Inf, 18.5, 25, 30, Inf),
                  labels = c("Underweight","Normal","Overweight","Obese"),
                  right = FALSE),
    
    # Secondary outcomes
    ASCVD_10yr = suppressWarnings(as.numeric(ASCVD_10yr)),
    
    # Microalbuminuria is coded 1=yes, 2=no (per your earlier message)
    microalbuminuria = factor(ifelse(microalbuminuria %in% c(1,0), microalbuminuria, NA),
                              levels = c(1,0), labels = c("Yes","No")),
    
    # uncontrolled BP is designated if BPQ050A = No (2)
    uncontrolled_bp = dplyr::case_when(
      BPQ050A == 2 ~ 1,
      BPQ050A == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Prediabetes awareness: DIQ160 (1 yes, 2 no, 7/9 missing)
    prediab_awareness = dplyr::case_when(
      DIQ160 == 1 ~ 1,
      DIQ160 == 2 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(class))  # need outcome observed; model will drop remaining NAs itself







