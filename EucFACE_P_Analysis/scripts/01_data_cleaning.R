# ==============================================================================
# SCRIPT 01: DATA CLEANING & INTEGRATION
# Author: Person 1
# ==============================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate)

# Helper: Map Rings to Treatments
get_trt <- function(ring) { ifelse(ring %in% c(1,4,5), "eCO2", "Ambient") }

print(">>> 1. Processing Pools (Inventory)...")
# Soil & Microbes (Filter to 0-10cm topsoil)
soil_p <- read_csv("data/soil_phosphate_pool.csv") %>% 
  filter(Depth == "0_10") %>% 
  mutate(Date = parse_date_time(Date, orders = c("ymd", "dmy", "mdy"))) %>% 
  mutate(Year = year(Date), Trt = get_trt(Ring)) %>% 
  group_by(Year, Ring, Trt) %>% summarise(Soil_P = mean(soil_phosphate_p_g_m2, na.rm=T), .groups="drop")

micro_p <- read_csv("data/microbial_p_pool.csv") %>% 
  filter(Depth == "0_10") %>% 
  mutate(Date = parse_date_time(Date, orders = c("ymd", "dmy", "mdy"))) %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Year, Ring) %>% summarise(Micro_P = mean(microbial_p_g_m2, na.rm=T), .groups="drop")

# Plant Carbon Pools
wood_c <- read_csv("data/wood_c_pool.csv") %>% 
  mutate(Date = parse_date_time(Date, orders = c("ymd", "dmy", "mdy"))) %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Year, Ring) %>% summarise(Wood_C = mean(wood_pool, na.rm=T), .groups="drop")

root_c <- read_csv("data/fineroot_c_pool.csv") %>% 
  mutate(Date = parse_date_time(Date, orders = c("ymd", "dmy", "mdy"))) %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Year, Ring) %>% summarise(Root_C = mean(fineroot_pool, na.rm=T), .groups="drop")

print(">>> 2. Processing Fluxes (Rates)...")
# P Mineralization (Supply)
min_p <- read_csv("data/soil_p_mineralization_flux.csv") %>% 
  filter(Depth == "0_10") %>% 
  mutate(Date = parse_date_time(Date, orders = c("ymd", "dmy", "mdy"))) %>% 
  mutate(Year = year(Date)) %>% 
  group_by(Year, Ring) %>% 
  summarise(Min_Flux = sum(p_mineralization_mg_m2_d, na.rm=T)/1000, .groups="drop")

# Carbon Gain
gpp <- read_csv("data/overstorey_gpp_flux.csv") %>% 
  rename(Year = year) %>% select(Year, Ring, GPP)

# Total P Cost (Summing all litter types)
process_litter <- function(file_path) {
  df <- read_csv(file_path)
  val_col <- names(df)[grep("flux|mg", names(df), ignore.case = T)] # Dynamic column find
  val_col <- val_col[!grepl("Date", val_col, ignore.case = T)]
  
  df %>%
    mutate(Date = parse_date_time(Date, orders = c("ymd", "dmy", "mdy"))) %>% 
    mutate(Year = year(Date)) %>%
    rename(Value = all_of(val_col[1])) %>%
    group_by(Year, Ring) %>%
    summarise(Ann_Flux = mean(Value, na.rm=T)*365/1000, .groups="drop") # mg/d to g/yr
}

# Run safely in case a file is missing
l_list <- list()
try(l_list[[1]] <- process_litter("data/leaflitter_p_flux.csv"), silent=T)
try(l_list[[2]] <- process_litter("data/twiglitter_p_flux.csv"), silent=T)
try(l_list[[3]] <- process_litter("data/bark_litter_p_flux.csv"), silent=T)
try(l_list[[4]] <- process_litter("data/seed_litter_p_flux.csv"), silent=T)

total_litter <- bind_rows(l_list) %>%
  group_by(Year, Ring) %>% summarise(Total_P_Lost = sum(Ann_Flux, na.rm=T), .groups="drop")

print(">>> 3. Merging and Filtering...")
# Merge all
df_master <- soil_p %>%
  full_join(micro_p, by=c("Year", "Ring")) %>%
  full_join(wood_c, by=c("Year", "Ring")) %>%
  full_join(root_c, by=c("Year", "Ring")) %>%
  full_join(min_p, by=c("Year", "Ring")) %>%
  full_join(gpp, by=c("Year", "Ring")) %>%
  full_join(total_litter, by=c("Year", "Ring")) %>%
  mutate(Trt = get_trt(Ring)) %>%
  # FILTER: Only keep years where we have plant data to calculate ratios
  filter(Year >= 2013 & Year <= 2016) %>% 
  mutate(
    PUE_Total = GPP / Total_P_Lost,      # Efficiency Metric
    Root_Shoot = Root_C / Wood_C         # Allocation Metric
  )

write_csv(df_master, "data/master_data.csv")
print("SUCCESS: 'master_data.csv' created.")
