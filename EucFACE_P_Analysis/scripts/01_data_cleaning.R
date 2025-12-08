# ==============================================================================
# SCRIPT 01: DATA LOADING & CLEANING
# ==============================================================================
library(tidyverse)
library(lubridate)

# Helper function to map rings to treatment
get_trt <- function(ring) { ifelse(ring %in% c(1,4,5), "eCO2", "Ambient") }

# 1. LOAD SOIL P (The Resource)
# We filter to 0-10cm because the paper states this is the most active zone
df_soil <- read_csv("data/soil_phosphate_pool.csv") %>%
  filter(Depth == "0_10") %>%
  mutate(Year = year(Date), Treatment = get_trt(Ring)) %>%
  group_by(Year, Ring, Treatment) %>%
  summarise(Soil_P = mean(soil_phosphate_p_g_m2, na.rm=TRUE), .groups="drop")

# 2. LOAD MICROBIAL P (The Competitor)
df_micro <- read_csv("data/microbial_p_pool.csv") %>%
  filter(Depth == "0_10") %>%
  mutate(Year = year(Date), Treatment = get_trt(Ring)) %>%
  group_by(Year, Ring, Treatment) %>%
  summarise(Micro_P = mean(microbial_p_g_m2, na.rm=TRUE), .groups="drop")

# 3. LOAD WOOD CARBON (The Victim)
df_wood <- read_csv("data/wood_c_pool.csv") %>%
  mutate(Year = year(Date), Treatment = get_trt(Ring)) %>%
  group_by(Year, Ring, Treatment) %>%
  summarise(Wood_C = mean(wood_pool, na.rm=TRUE), .groups="drop")

# 4. LOAD GPP (For Alternative Analysis)
df_gpp <- read_csv("data/overstorey_gpp_flux.csv") %>%
  rename(Year = year) %>%
  mutate(Treatment = get_trt(Ring)) %>%
  select(Year, Ring, Treatment, GPP)

# 5. MERGE INTO MASTER DATASET
df_master <- df_soil %>%
  left_join(df_micro, by=c("Year", "Ring", "Treatment")) %>%
  left_join(df_wood, by=c("Year", "Ring", "Treatment")) %>%
  left_join(df_gpp, by=c("Year", "Ring", "Treatment"))

# Save for the next scripts
write_csv(df_master, file = "EucFACE_P_Analysis/data/master_data_eucface.csv")
print("Data Cleaned and Saved as 'master_data_eucface.csv' in the data folder of default directory.")
