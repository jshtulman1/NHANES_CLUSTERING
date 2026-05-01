# Load Packages
library(dplyr)

# Calculate TG/HDL Ratio
NHANES2015_2016TG_HDL <- NHANES2015_2016WHR %>%
  mutate(tg_hdl = LBXTR / LBDHDD)

