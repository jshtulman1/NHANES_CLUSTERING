# Loading Packages

library(haven)
library(here)
library(dplyr)

# Reading in XPT Files

folder <- here::here("data/NHANES 2015_2016")

xpt_files <- list.files(folder, pattern = "\\.xpt$", full.names = TRUE)

for (file in xpt_files) {
  df_name <- tools::file_path_sans_ext(basename(file))
  assign(df_name, read_xpt(file))
}

# Merging Data Frames

NHANES2015_2016 <- merge(DEMO_I, ALB_CR_I, by = "SEQN", all = TRUE)

NHANES2015_2016 <- merge(NHANES2015_2016, BIOPRO_I, by = "SEQN", all = TRUE)

NHANES2015_2016 <- merge(NHANES2015_2016, BMX_I, by = "SEQN", all = TRUE)

NHANES2015_2016 <- merge(NHANES2015_2016, BPQ_I, by = "SEQN", all = TRUE)

NHANES2015_2016 <- merge(NHANES2015_2016, BPX_I, by = "SEQN", all = TRUE)

NHANES2015_2016 <- merge(NHANES2015_2016, CBC_I, by = "SEQN", all = TRUE)

NHANES2015_2016 <- merge(NHANES2015_2016, DIQ_I, by = "SEQN", all = TRUE)

NHANES2015_2016 <- merge(NHANES2015_2016, DPQ_I, by = "SEQN", all = TRUE)

NHANES2015_2016 <- merge(NHANES2015_2016, GHB_I, by = "SEQN", all = TRUE)

NHANES2015_2016 <- merge(NHANES2015_2016, GLU_I, by = "SEQN", all = TRUE)

NHANES2015_2016 <- merge(NHANES2015_2016, HDL_I, by = "SEQN", all = TRUE)

NHANES2015_2016 <- merge(NHANES2015_2016, HIQ_I, by = "SEQN", all = TRUE)

NHANES2015_2016 <- merge(NHANES2015_2016, HSCRP_I, by = "SEQN", all = TRUE)

NHANES2015_2016 <- merge(NHANES2015_2016, INS_I, by = "SEQN", all = TRUE)

NHANES2015_2016 <- merge(NHANES2015_2016, PAQ_I, by = "SEQN", all = TRUE)

NHANES2015_2016 <- merge(NHANES2015_2016, TRIGLY_I, by = "SEQN", all = TRUE)

NHANES2015_2016 <- merge(NHANES2015_2016, SMQ_I, by = "SEQN", all = TRUE)

NHANES2015_2016 <- merge(NHANES2015_2016, TCHOL_I, by = "SEQN", all = TRUE)

NHANES2015_2016 <- merge(NHANES2015_2016, FSQ_I, by = "SEQN", all = TRUE)

# Removing Duplicate Weights

names(NHANES2015_2016) <- make.names(names(NHANES2015_2016), unique = TRUE)

grep("WT", names(NHANES2015_2016), value = TRUE)

NHANES2015_2016 <- NHANES2015_2016 %>%
  select(-matches("WTSAFPRP\\."))

NHANES2015_2016 <- NHANES2015_2016 %>%
  select(-ends_with(".x"), -ends_with(".y"))


