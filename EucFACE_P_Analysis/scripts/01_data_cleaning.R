# ==============================================================================
# SCRIPT 01: DATA CLEANING
# ==============================================================================
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
library(tidyverse)
library(lubridate)

# Define Treatment Mapping
get_trt <- function(ring) { ifelse(ring %in% c(1,4,5), "eCO2", "Ambient") }

# --- 1. LOAD INVENTORY (POOLS) ---
# We use parse_date_time to handle both YYYY-MM-DD and DD/MM/YYYY formats
print("Loading Pools...")

soil_p <- read_csv("data/soil_phosphate_pool.csv") %>% 
  filter(Depth == "0_10") %>% 
  mutate(Date = parse_date_time(Date, orders = c("ymd", "dmy", "mdy"))) %>% 
  mutate(Year = year(Date), Trt = get_trt(Ring)) %>% 
  group_by(Year, Ring, Trt) %>% 
  summarise(Soil_P = mean(soil_phosphate_p_g_m2, na.rm=TRUE), .groups="drop")

micro_p <- read_csv("data/microbial_p_pool.csv") %>% 
  filter(Depth == "0_10") %>% 
  mutate(Date = parse_date_time(Date, orders = c("ymd", "dmy", "mdy"))) %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Year, Ring) %>% 
  summarise(Micro_P = mean(microbial_p_g_m2, na.rm=TRUE), .groups="drop")

wood_c <- read_csv("data/wood_c_pool.csv") %>% 
  mutate(Date = parse_date_time(Date, orders = c("ymd", "dmy", "mdy"))) %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Year, Ring) %>% 
  summarise(Wood_C = mean(wood_pool, na.rm=TRUE), .groups="drop")

root_c <- read_csv("data/fineroot_c_pool.csv") %>% 
  mutate(Date = parse_date_time(Date, orders = c("ymd", "dmy", "mdy"))) %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Year, Ring) %>% 
  summarise(Root_C = mean(fineroot_pool, na.rm=TRUE), .groups="drop")

# --- 2. LOAD FLUXES ---
print("Loading Fluxes...")

min_p <- read_csv("data/soil_p_mineralization_flux.csv") %>% 
  filter(Depth == "0_10") %>% 
  mutate(Date = parse_date_time(Date, orders = c("ymd", "dmy", "mdy"))) %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Year, Ring) %>% 
  summarise(Min_Flux = sum(p_mineralization_mg_m2_d, na.rm=TRUE)/1000, .groups="drop")

gpp <- read_csv("data/overstorey_gpp_flux.csv") %>% 
  rename(Year = year) %>% 
  select(Year, Ring, GPP)

# --- 3. LITTER PROCESSING ---
process_litter <- function(file_path) {
  df <- read_csv(file_path)
  
  # Identify the column that actually contains data numbers (contains "flux" or "mg")
  val_col <- names(df)[grep("flux|mg", names(df), ignore.case = T)]
  val_col <- val_col[!grepl("Date", val_col, ignore.case = T)] # Exclude Date columns
  
  df %>%
    mutate(Date = parse_date_time(Date, orders = c("ymd", "dmy", "mdy"))) %>% 
    mutate(Year = year(Date)) %>%
    rename(Value = all_of(val_col[1])) %>% # Use the first matching numeric column
    group_by(Year, Ring) %>%
    summarise(Annual_Flux = mean(Value, na.rm=TRUE) * 365 / 1000, .groups="drop")
}

leaf_l <- process_litter("data/leaflitter_p_flux.csv")
twig_l <- process_litter("data/twig_litter_p_flux.csv")
bark_l <- process_litter("data/bark_litter_p_flux.csv")
seed_l <- process_litter("data/seed_litter_p_flux.csv")

total_litter <- bind_rows(leaf_l, twig_l, bark_l, seed_l) %>%
  group_by(Year, Ring) %>%
  summarise(Total_P_Lost = sum(Annual_Flux, na.rm=TRUE), .groups="drop")

# --- 4. MERGE & CALCULATE ---
print("Merging...")

df_master <- soil_p %>%
  full_join(micro_p, by=c("Year", "Ring")) %>%
  full_join(wood_c, by=c("Year", "Ring")) %>%
  full_join(root_c, by=c("Year", "Ring")) %>%
  full_join(min_p, by=c("Year", "Ring")) %>%
  full_join(gpp, by=c("Year", "Ring")) %>%
  full_join(total_litter, by=c("Year", "Ring")) %>%
  mutate(Trt = get_trt(Ring)) %>%
  # Filter to Core Years where we have Plant Data
  filter(Year >= 2013 & Year <= 2016) %>%
  mutate(
    # Key Alternative Metric: GPP gained per P lost
    PUE_Total = GPP / Total_P_Lost,
    # Key Alternative Metric: Investment strategy
    Root_Shoot_Ratio = Root_C / Wood_C
  )

write_csv(df_master, file = "data/master_data.csv")
print("DONE: master_data.csv created successfully.")
