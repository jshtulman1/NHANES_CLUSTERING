# Loading Packages

library(haven)
library(here)
library(dplyr)

# Reading in XPT Files

folder <- here::here("data/NHANES 2021_2023")

xpt_files <- list.files(folder, pattern = "\\.xpt$", full.names = TRUE)

for (file in xpt_files) {
  df_name <- tools::file_path_sans_ext(basename(file))
  assign(df_name, read_xpt(file))
}

# Merging Data Frames

NHANES2021_2023 <- merge(DEMO_L, ALB_CR_L, by = "SEQN", all = TRUE)

NHANES2021_2023 <- merge(NHANES2021_2023, BIOPRO_L, by = "SEQN", all = TRUE)

NHANES2021_2023 <- merge(NHANES2021_2023, BMX_L, by = "SEQN", all = TRUE)

NHANES2021_2023 <- merge(NHANES2021_2023, BPQ_L, by = "SEQN", all = TRUE)

NHANES2021_2023 <- merge(NHANES2021_2023, BPXO_L, by = "SEQN", all = TRUE)

NHANES2021_2023 <- merge(NHANES2021_2023, CBC_L, by = "SEQN", all = TRUE)

NHANES2021_2023 <- merge(NHANES2021_2023, DIQ_L, by = "SEQN", all = TRUE)

NHANES2021_2023 <- merge(NHANES2021_2023, DPQ_L, by = "SEQN", all = TRUE)

NHANES2021_2023 <- merge(NHANES2021_2023, GHB_L, by = "SEQN", all = TRUE)

NHANES2021_2023 <- merge(NHANES2021_2023, GLU_L, by = "SEQN", all = TRUE)

NHANES2021_2023 <- merge(NHANES2021_2023, HDL_L, by = "SEQN", all = TRUE)

NHANES2021_2023 <- merge(NHANES2021_2023, HIQ_L, by = "SEQN", all = TRUE)

NHANES2021_2023 <- merge(NHANES2021_2023, HSCRP_L, by = "SEQN", all = TRUE)

NHANES2021_2023 <- merge(NHANES2021_2023, INS_L, by = "SEQN", all = TRUE)

NHANES2021_2023 <- merge(NHANES2021_2023, PAQ_L, by = "SEQN", all = TRUE)

NHANES2021_2023 <- merge(NHANES2021_2023, TRIGLY_L, by = "SEQN", all = TRUE)

NHANES2021_2023 <- merge(NHANES2021_2023, SMQ_L, by = "SEQN", all = TRUE)

NHANES2021_2023 <- merge(NHANES2021_2023, TCHOL_L, by = "SEQN", all = TRUE)

NHANES2021_2023 <- merge(NHANES2021_2023, FSQ_L, by = "SEQN", all = TRUE)

# Removing Duplicate Weights

names(NHANES2021_2023) <- make.names(names(NHANES2021_2023), unique = TRUE)

grep("WTPH2YR", names(NHANES2021_2023), value = TRUE)

NHANES2021_2023 <- NHANES2021_2023 %>%
  mutate(WTPH2YR = coalesce(WTPH2YR.x, WTPH2YR.y, WTPH2YR.x.1, WTPH2YR.y.1))

NHANES2021_2023 <- NHANES2021_2023 %>%
  select(-matches("WTPH2YR\\."))

NHANES2021_2023 <- NHANES2021_2023 %>%
  select(-ends_with(".x"), -ends_with(".y"))



