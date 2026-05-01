# ============================================================
# DERIVE HARMONIZED BINARY ALCOHOL VARIABLE
# Input:  ALQ_I.xpt (2015-16), P_ALQ.xpt (2017-20), ALQ_L.xpt (2021-23)
# Output: Adds 'current_drinker' to combined_all_v2
#
# Definition (harmonizable across all 3 cycles):
#   current_drinker = 1  if drank any alcohol in the past 12 months
#   current_drinker = 0  if eligible for the ALQ and did NOT drink
#                        in the past 12 months (including never-drinkers
#                        and lifetime drinkers who abstained this year)
#   current_drinker = NA if refused/don't know, or missing the ALQ
#
# WHY THIS DEFINITION:
#   The gateway questions changed across cycles:
#     2015-16:  ALQ101 ("12+ drinks in any one year?")
#               → if No → ALQ110 ("12+ drinks in lifetime?")
#     2017-20:  ALQ111 ("ever had at least 1 drink in lifetime?")
#     2021-23:  ALQ111 (same)
#
#   The 2015-16 gateway has a HIGHER threshold (12 drinks/year)
#   than the 2017+ gateway (1 drink ever), so "ever drinker" is
#   not comparable across cycles.
#
#   However, ALL cycles ask about past-12-month drinking frequency
#   for those routed past the gateway, and all cycles route
#   non-drinkers to "end of section" before those questions.
#   This lets us construct a clean binary:
#     - Confirmed past-12-month drinker  → 1
#     - Confirmed non-drinker / abstainer → 0
#
# SKIP PATTERN LOGIC (cycle-specific):
#
#   2015-16 (ALQ_I):
#     ALQ101 == 1 (yes 12+ drinks/yr) → skip to ALQ120Q (frequency)
#       ALQ120Q == 0   → drank in past years, not past 12 mo → 0
#       ALQ120Q >= 1   → current drinker → 1
#     ALQ101 == 2 (no) → ALQ110 (lifetime)
#       ALQ110 == 1    → lifetime drinker, but <12/yr → routed to ALQ120Q
#         ALQ120Q == 0 → abstainer this year → 0
#         ALQ120Q >= 1 → current drinker → 1
#       ALQ110 == 2    → never drinker → 0
#     ALQ101 missing   → NA (not administered, e.g. <18 proxy)
#
#   2017-20 (P_ALQ) & 2021-23 (ALQ_L):
#     ALQ111 == 1 (yes, ever had a drink) → ALQ121 (frequency)
#       ALQ121 == 0    → drank before, not past 12 mo → 0
#       ALQ121 >= 1    → current drinker → 1
#     ALQ111 == 2 (no, never) → end of section → 0
#     ALQ111 missing   → NA
# ============================================================

library(haven)
library(dplyr)


# ============================================================
# STEP 1: LOAD RAW ALQ FILES
# ============================================================

cat("╔══════════════════════════════════════════════╗\n")
cat("║  STEP 1: LOAD RAW ALQ FILES                 ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

base_path <- "/Users/jacob/Desktop/Grad School/NHANES Prediabetes Metabolic Profiles/Data"

alq_2015 <- read_xpt(file.path(base_path, "NHANES 2015_2016", "ALQ_I.xpt"))
alq_2017 <- read_xpt(file.path(base_path, "NHANES 2017_2020", "P_ALQ.xpt"))
alq_2021 <- read_xpt(file.path(base_path, "NHANES 2021_2023", "ALQ_L.xpt"))

cat("ALQ_I  (2015-16):", nrow(alq_2015), "rows\n")
cat("P_ALQ  (2017-20):", nrow(alq_2017), "rows\n")
cat("ALQ_L  (2021-23):", nrow(alq_2021), "rows\n\n")


# ============================================================
# STEP 2: AUDIT RAW VARIABLES BEFORE DERIVATION
# ============================================================

cat("╔══════════════════════════════════════════════╗\n")
cat("║  STEP 2: AUDIT RAW GATEWAY + FREQUENCY VARS ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

cat("--- 2015-16: ALQ101 (gateway) ---\n")
print(table(alq_2015$ALQ101, useNA = "always"))

cat("\n--- 2015-16: ALQ110 (lifetime, asked only if ALQ101 != 1) ---\n")
print(table(alq_2015$ALQ110, useNA = "always"))

cat("\n--- 2015-16: ALQ120Q (past-12-mo frequency) ---\n")
cat("  N non-missing:", sum(!is.na(alq_2015$ALQ120Q)), "\n")
cat("  N == 0 (none):", sum(alq_2015$ALQ120Q == 0, na.rm = TRUE), "\n")
cat("  N >= 1 (some): ", sum(alq_2015$ALQ120Q >= 1 & alq_2015$ALQ120Q <= 366, na.rm = TRUE), "\n")

cat("\n--- 2017-20: ALQ111 (gateway) ---\n")
print(table(alq_2017$ALQ111, useNA = "always"))

cat("\n--- 2017-20: ALQ121 (past-12-mo frequency, categorical) ---\n")
print(table(alq_2017$ALQ121, useNA = "always"))

cat("\n--- 2021-23: ALQ111 (gateway) ---\n")
print(table(alq_2021$ALQ111, useNA = "always"))

cat("\n--- 2021-23: ALQ121 (past-12-mo frequency, categorical) ---\n")
print(table(alq_2021$ALQ121, useNA = "always"))


# ============================================================
# STEP 3: DERIVE current_drinker BY CYCLE
# ============================================================

cat("\n╔══════════════════════════════════════════════╗\n")
cat("║  STEP 3: DERIVE current_drinker BY CYCLE     ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

# ---- 2015-16 ----
# ALQ120Q is numeric (0 = none in past year, 1-365 = days, 777/999 = ref/dk)
alq_2015_derived <- alq_2015 %>%
  select(SEQN, ALQ101, ALQ110, ALQ120Q) %>%
  mutate(
    current_drinker = case_when(
      # Route 1: ALQ101 == 1 → skip to ALQ120Q
      ALQ101 == 1 & ALQ120Q >= 1 & ALQ120Q <= 366  ~ 1L,
      ALQ101 == 1 & ALQ120Q == 0                    ~ 0L,

      # Route 2: ALQ101 == 2 → ALQ110
      ALQ101 == 2 & ALQ110 == 2                     ~ 0L,  # never drinker
      ALQ101 == 2 & ALQ110 == 1 & ALQ120Q >= 1
                                & ALQ120Q <= 366    ~ 1L,  # lifetime + current
      ALQ101 == 2 & ALQ110 == 1 & ALQ120Q == 0     ~ 0L,  # lifetime, abstained

      # Refused / don't know at gateway → NA
      ALQ101 %in% c(7, 9)                           ~ NA_integer_,
      ALQ110 %in% c(7, 9)                           ~ NA_integer_,
      ALQ120Q %in% c(777, 999)                      ~ NA_integer_,

      # Missing entirely (not administered) → NA
      TRUE                                          ~ NA_integer_
    ),
    Cycle = 2015
  )

cat("--- 2015-16 current_drinker ---\n")
print(table(alq_2015_derived$current_drinker, useNA = "always"))


# ---- 2017-20 ----
# ALQ121 is categorical: 0 = never in last year, 1-10 = frequency codes,
#                         77/99 = refused/dk
alq_2017_derived <- alq_2017 %>%
  select(SEQN, ALQ111, ALQ121) %>%
  mutate(
    current_drinker = case_when(
      ALQ111 == 2                                   ~ 0L,  # never drank
      ALQ111 == 1 & ALQ121 == 0                     ~ 0L,  # drank before, not this yr
      ALQ111 == 1 & ALQ121 >= 1 & ALQ121 <= 10      ~ 1L,  # current drinker

      ALQ111 %in% c(7, 9)                           ~ NA_integer_,
      ALQ121 %in% c(77, 99)                         ~ NA_integer_,
      TRUE                                          ~ NA_integer_
    ),
    Cycle = 2017
  )

cat("\n--- 2017-20 current_drinker ---\n")
print(table(alq_2017_derived$current_drinker, useNA = "always"))


# ---- 2021-23 ----
# Same instrument structure as 2017-20
alq_2021_derived <- alq_2021 %>%
  select(SEQN, ALQ111, ALQ121) %>%
  mutate(
    current_drinker = case_when(
      ALQ111 == 2                                   ~ 0L,  # never drank
      ALQ111 == 1 & ALQ121 == 0                     ~ 0L,  # drank before, not this yr
      ALQ111 == 1 & ALQ121 >= 1 & ALQ121 <= 10      ~ 1L,  # current drinker

      ALQ111 %in% c(7, 9)                           ~ NA_integer_,
      ALQ121 %in% c(77, 99)                         ~ NA_integer_,
      TRUE                                          ~ NA_integer_
    ),
    Cycle = 2021
  )

cat("\n--- 2021-23 current_drinker ---\n")
print(table(alq_2021_derived$current_drinker, useNA = "always"))


# ============================================================
# STEP 4: STACK + SANITY CHECKS
# ============================================================

cat("\n╔══════════════════════════════════════════════╗\n")
cat("║  STEP 4: STACK + SANITY CHECKS               ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

alq_harmonized <- bind_rows(
  alq_2015_derived %>% select(SEQN, Cycle, current_drinker),
  alq_2017_derived %>% select(SEQN, Cycle, current_drinker),
  alq_2021_derived %>% select(SEQN, Cycle, current_drinker)
)

cat("Total harmonized rows:", nrow(alq_harmonized), "\n")
cat("Duplicate SEQNs:      ", sum(duplicated(alq_harmonized$SEQN)), "\n\n")

cat("--- Prevalence by cycle ---\n")
alq_harmonized %>%
  group_by(Cycle) %>%
  summarise(
    n         = n(),
    n_1       = sum(current_drinker == 1, na.rm = TRUE),
    n_0       = sum(current_drinker == 0, na.rm = TRUE),
    n_NA      = sum(is.na(current_drinker)),
    pct_drink = round(mean(current_drinker, na.rm = TRUE) * 100, 1),
    .groups   = "drop"
  ) %>%
  print()

cat("\n✓ Expected: ~55-70% current drinkers in adult NHANES sample\n")
cat("  (Varies by cycle; prediabetes subsample may differ)\n")


# ============================================================
# STEP 5: MERGE INTO combined_all_v2
# ============================================================

cat("\n╔══════════════════════════════════════════════╗\n")
cat("║  STEP 5: MERGE INTO combined_all_v2          ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

n_before <- nrow(combined_all_v2)

combined_all_v2 <- combined_all_v2 %>%
  left_join(
    alq_harmonized %>% select(SEQN, current_drinker),
    by = "SEQN"
  )

# ---- Verify join didn't inflate rows ----
n_after <- nrow(combined_all_v2)
cat("Rows before join:", n_before, "\n")
cat("Rows after join: ", n_after, "\n")
stopifnot(n_before == n_after)
cat("✓ Row count preserved\n\n")

# ---- Check merge rate ----
cat("--- Merge diagnostics ---\n")
cat("current_drinker == 1: ", sum(combined_all_v2$current_drinker == 1, na.rm = TRUE), "\n")
cat("current_drinker == 0: ", sum(combined_all_v2$current_drinker == 0, na.rm = TRUE), "\n")
cat("current_drinker == NA:", sum(is.na(combined_all_v2$current_drinker)), "\n\n")

cat("--- Prevalence by cycle (in prediabetes sample) ---\n")
combined_all_v2 %>%
  group_by(Cycle) %>%
  summarise(
    n             = n(),
    pct_drinker   = round(mean(current_drinker, na.rm = TRUE) * 100, 1),
    pct_missing   = round(mean(is.na(current_drinker)) * 100, 1),
    .groups       = "drop"
  ) %>%
  print()

# ---- Check within clustering subsample ----
cat("\n--- Among clustered subjects ---\n")
combined_all_v2 %>%
  filter(in_clustering) %>%
  group_by(Cycle) %>%
  summarise(
    n             = n(),
    pct_drinker   = round(mean(current_drinker, na.rm = TRUE) * 100, 1),
    pct_missing   = round(mean(is.na(current_drinker)) * 100, 1),
    .groups       = "drop"
  ) %>%
  print()


# ============================================================
# STEP 6: SAVE UPDATED DATASET
# ============================================================

cat("\n╔══════════════════════════════════════════════╗\n")
cat("║  STEP 6: SAVE                                ║\n")
cat("╚══════════════════════════════════════════════╝\n\n")

saveRDS(combined_all_v2, file = "Data/Processed Dataframes/combined_all_v2.rds")
cat("✓ Saved updated combined_all_v2 with 'current_drinker' column\n")
cat("  → Data/Processed Dataframes/combined_all_v2.rds\n")

cat("\n╔════════════════════════════════════════════════════════════╗\n")
cat("║  COMPLETE                                                  ║\n")
cat("║  New variable: current_drinker (1 = drank past 12 mo,      ║\n")
cat("║                                 0 = did not, NA = missing)  ║\n")
cat("║  Harmonized across: 2015-16, 2017-20, 2021-23              ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n")
