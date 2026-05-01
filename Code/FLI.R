# Load Packages

library(dplyr)

# Calculate Fatty Liver Index (Haung et al., 2015)

NHANES2015_2016FLI <- NHANES2015_2016TG_HDL %>%
  mutate(
    fli_linear = 0.953 * log(LBXTR) +
      0.139 * BMXBMI +
      0.718 * log(LBXSGTSI) +
      0.053 * BMXWAIST -
      15.745,
    FLI = (exp(fli_linear) / (1 + exp(fli_linear))) * 100
  )


