# Loading Packages

library(haven)
library(here)
library(dplyr)

# Reading in XPT Files

folder <- here::here("data/NHANES 2017_2020")

xpt_files <- list.files(folder, pattern = "\\.xpt$", full.names = TRUE)

for (file in xpt_files) {
  df_name <- tools::file_path_sans_ext(basename(file))
  assign(df_name, read_xpt(file))
}

# Merging Data Frames

NHANES2017_2020 <- merge(P_DEMO, P_ALB_CR, by = "SEQN", all = TRUE)

NHANES2017_2020 <- merge(NHANES2017_2020, P_BIOPRO, by = "SEQN", all = TRUE)

NHANES2017_2020 <- merge(NHANES2017_2020, P_BMX, by = "SEQN", all = TRUE)

NHANES2017_2020 <- merge(NHANES2017_2020, P_BPQ, by = "SEQN", all = TRUE)

NHANES2017_2020 <- merge(NHANES2017_2020, P_BPXO, by = "SEQN", all = TRUE)

NHANES2017_2020 <- merge(NHANES2017_2020, P_CBC, by = "SEQN", all = TRUE)

NHANES2017_2020 <- merge(NHANES2017_2020, P_DIQ, by = "SEQN", all = TRUE)

NHANES2017_2020 <- merge(NHANES2017_2020, P_DPQ, by = "SEQN", all = TRUE)

NHANES2017_2020 <- merge(NHANES2017_2020, P_GHB, by = "SEQN", all = TRUE)

NHANES2017_2020 <- merge(NHANES2017_2020, P_GLU, by = "SEQN", all = TRUE)

NHANES2017_2020 <- merge(NHANES2017_2020, P_HDL, by = "SEQN", all = TRUE)

NHANES2017_2020 <- merge(NHANES2017_2020, P_HIQ, by = "SEQN", all = TRUE)

NHANES2017_2020 <- merge(NHANES2017_2020, P_HSCRP, by = "SEQN", all = TRUE)

NHANES2017_2020 <- merge(NHANES2017_2020, P_INS, by = "SEQN", all = TRUE)

NHANES2017_2020 <- merge(NHANES2017_2020, P_PAQ, by = "SEQN", all = TRUE)

NHANES2017_2020 <- merge(NHANES2017_2020, P_TRIGLY, by = "SEQN", all = TRUE)

NHANES2017_2020 <- merge(NHANES2017_2020, P_SMQ, by = "SEQN", all = TRUE)

NHANES2017_2020 <- merge(NHANES2017_2020, P_TCHOL, by = "SEQN", all = TRUE)

NHANES2017_2020 <- merge(NHANES2017_2020, P_FSQ, by = "SEQN", all = TRUE)

# Removing Duplicate Weights

names(NHANES2017_2020) <- make.names(names(NHANES2017_2020), unique = TRUE)

grep("WT", names(NHANES2017_2020), value = TRUE)

NHANES2017_2020 <- NHANES2017_2020 %>%
  select(-matches("WTSAFPRP\\."))

NHANES2017_2020 <- NHANES2017_2020 %>%
  select(-ends_with(".x"), -ends_with(".y"))


