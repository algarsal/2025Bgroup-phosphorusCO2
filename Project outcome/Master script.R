# ==============================================================================
# MASTER SCRIPT: EucFACE PHOSPHORUS LIMITATION (Merged & Cleaned)
# ==============================================================================
# This script integrates Carbon Growth, P Fluxes, and P Pools.
# It performs data cleaning, robust merging, statistical testing, and plotting.

# 1. SETUP & LIBRARIES ----------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, lubridate, lme4, lmerTest, patchwork, ggplot2)

# --- DEFINE PATHS ---
# Update these if your folder location changes
path_flux <- "C:/Users/augre/Desktop/UPV S1/Data/2025Bgroup-phosphorusCO2/Flux_data"
path_pool <- "C:/Users/augre/Desktop/UPV S1/Data/2025Bgroup-phosphorusCO2/Pool data"

# 2. HELPER FUNCTIONS (The Engine) ----------------------------------------

# Robust Treatment Mapping Function
add_treatment <- function(df) {
  if(is.null(df)) return(NULL)
  df %>%
    mutate(Ring_Char = as.character(Ring)) %>%
    mutate(Trt = case_when(
      Ring_Char %in% c("1", "4", "5") ~ "eCO2",
      Ring_Char %in% c("2", "3", "6") ~ "aCO2",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(Trt)) # Clean data: remove rows without valid rings
}

# Process Rate Files (mg/day -> g/year)
process_rate <- function(filename, col, folder) {
  f <- file.path(folder, filename)
  if(!file.exists(f)) { message(paste("⚠️ MISSING FILE:", filename)); return(NULL) }
  
  d <- read_csv(f, show_col_types = FALSE)
  if(nrow(d) == 0) return(NULL)
  
  d %>%
    mutate(Year = year(ymd(Date)), Ring = factor(Ring)) %>%
    filter(Year >= 2013 & Year <= 2016) %>%
    mutate(Grams = (get(col) * Days) / 1000) %>%
    group_by(Year, Ring) %>%
    summarise(Value = sum(Grams, na.rm=TRUE), .groups="drop")
}

# Process Sum Files (Totals)
process_sum <- function(filename, col, folder) {
  f <- file.path(folder, filename)
  if(!file.exists(f)) { message(paste("⚠️ MISSING FILE:", filename)); return(NULL) }
  
  d <- read_csv(f, show_col_types = FALSE)
  if(nrow(d) == 0) return(NULL)
  
  d %>%
    mutate(Year = year(ymd(Date)), Ring = factor(Ring)) %>%
    filter(Year >= 2013 & Year <= 2016) %>%
    group_by(Year, Ring) %>%
    summarise(Value = sum(get(col), na.rm=TRUE), .groups="drop")
}

# Process Pool Files (Snapshots)
process_pool <- function(filename, col, folder) {
  f <- file.path(folder, filename)
  if(!file.exists(f)) { message(paste("⚠️ MISSING FILE:", filename)); return(NULL) }
  
  df <- read_csv(f, show_col_types = FALSE)
  # Fix Date
  date_col <- names(df)[grep("Date", names(df), ignore.case = TRUE)][1]
  df$Date <- ymd(df[[date_col]])
  
  df <- df %>%
    mutate(Year = year(Date), Ring = factor(Ring)) %>%
    filter(Year >= 2013 & Year <= 2016)
  
  # Handle Depths (Sum 0-30cm)
  if("Depth" %in% names(df)) {
    df <- df %>% 
      filter(Depth %in% c("0_10", "10_30")) %>%
      group_by(Date, Year, Ring) %>%
      summarise(Step1 = sum(get(col), na.rm=TRUE), .groups="drop") %>%
      rename(Target = Step1)
  } else {
    df <- df %>% mutate(Target = get(col))
  }
  
  df %>%
    group_by(Year, Ring) %>%
    summarise(Value = mean(Target, na.rm=TRUE), .groups="drop")
}

# 3. DATA LOADING & CLEANING ----------------------------------------------
print(">>> Loading Data...")

# --- 3A. Carbon Data ---
c_wood <- process_sum("wood_c_production_flux.csv", "wood_production_flux", path_flux)
c_root <- process_rate("fineroot_c_production_flux.csv", "fineroot_production_flux", path_flux)
c_gpp  <- read_csv(file.path(path_flux, "overstorey_gpp_flux.csv"), show_col_types=F) %>% 
  rename(Year=year, Value=GPP) %>% mutate(Ring=factor(Ring)) %>% 
  filter(Year >= 2013 & Year <= 2016) %>% select(Year, Ring, Value)

# --- 3B. Phosphorus Fluxes ---
p_wood_dem <- process_sum("wood_p_flux.csv", "wood_p_flux", path_flux)
p_leaf_dem <- process_sum("canopy_p_flux.csv", "canopy_p_flux", path_flux)
p_root_dem <- process_rate("fineroot_p_production_flux.csv", "fineroot_p_flux_mg_m2_d", path_flux)

p_wood_ret <- process_sum("sapwood_P_retranslocation_flux.csv", "sapwood_p_retrans_flux", path_flux)
p_leaf_ret <- process_sum("canopy_P_retranslocation_flux.csv", "canopy_p_retrans_flux", path_flux)
p_root_ret <- process_rate("fineroot_P_retranslocation_flux.csv", "fineroot_p_retrans_flux", path_flux)

# --- 3C. Soil Supply ---
raw_soil <- read_csv(file.path(path_flux, "soil_p_mineralization_flux.csv"), show_col_types=F)
p_supply <- raw_soil %>%
  mutate(Year=year(ymd(Date)), Ring=factor(Ring)) %>%
  filter(Year >= 2013 & Year <= 2016) %>%
  group_by(Date, Year, Ring, Days) %>%
  summarise(Daily = sum(p_mineralization_mg_m2_d, na.rm=TRUE), .groups="drop") %>%
  mutate(Grams = (Daily * Days)/1000) %>%
  group_by(Year, Ring) %>%
  summarise(Value = sum(Grams, na.rm=TRUE), .groups="drop")

# --- 3D. Pools ---
pool_mic <- process_pool("microbial_p_pool.csv", "microbial_p_g_m2", path_pool)
# Combine plant pools
pool_wood_p <- process_pool("wood_p_pool.csv", "wood_p_pool", path_pool)
pool_leaf_p <- process_pool("canopy_p_pool.csv", "leaf_p_pool", path_pool)
pool_root_p <- process_pool("fineroot_p_pool.csv", "fineroot_p_pool", path_pool)

# Variable Check before merging
if(any(sapply(list(c_wood, c_root, p_wood_dem, pool_mic), is.null))) {
  stop("CRITICAL ERROR: One or more key files failed to load. Check paths.")
}

# 4. PROCESSING & MERGING (Dataframes creation) ---------------------------

# --- DF1: Carbon Growth ---
df_carbon <- bind_rows(
  c_wood %>% mutate(Type = "Wood Growth"),
  c_root %>% mutate(Type = "Root Growth")
) %>% add_treatment() %>% filter(!is.na(Value))

# --- DF2: P Flux Budget (Uptake Calculation) ---
# Join all P components
df_uptake_raw <- p_wood_dem %>% rename(WD=Value) %>%
  left_join(p_leaf_dem, by=c("Year","Ring")) %>% rename(LD=Value) %>%
  left_join(p_root_dem, by=c("Year","Ring")) %>% rename(RD=Value) %>%
  left_join(p_wood_ret, by=c("Year","Ring")) %>% rename(WR=Value) %>%
  left_join(p_leaf_ret, by=c("Year","Ring")) %>% rename(LR=Value) %>%
  left_join(p_root_ret, by=c("Year","Ring")) %>% rename(RR=Value)

# Calc Uptake (Demand - Recycling)
df_uptake_final <- df_uptake_raw %>%
  mutate(Uptake = (WD + LD + RD) - (WR + LR + RR)) %>%
  select(Year, Ring, Uptake) %>%
  rename(Value = Uptake) %>%
  mutate(Type = "Tree Uptake")

# Combine Supply and Uptake
df_budget <- bind_rows(
  p_supply %>% mutate(Type = "Soil Supply"),
  df_uptake_final
) %>% add_treatment() %>% filter(!is.na(Value))

# --- DF3: P Pools ---
pool_plant_total <- bind_rows(pool_wood_p, pool_leaf_p, pool_root_p) %>%
  group_by(Year, Ring) %>%
  summarise(Value = sum(Value, na.rm=TRUE), .groups="drop")

df_pools <- bind_rows(
  pool_mic %>% mutate(Type = "Microbial P"),
  pool_plant_total %>% mutate(Type = "Total Plant P")
) %>% add_treatment() %>% filter(!is.na(Value))

# --- DF4: Efficiency (PUE) ---
df_pue <- c_gpp %>% 
  left_join(p_leaf_dem, by=c("Year","Ring")) %>% 
  rename(GPP = Value.x, LeafP = Value.y) %>%
  mutate(PUE = GPP / LeafP) %>%
  add_treatment() %>% filter(!is.na(PUE))

# 5. STATISTICS & PLOTTING ------------------------------------------------

print(">>> Running Statistics...")

# Helper for stats printing
run_stat <- function(data, variable_name) {
  # Simple T-test (Aggregated by Ring to avoid pseudo-replication issues in simple output)
  agg <- data %>% group_by(Ring, Trt) %>% summarise(MeanVal = mean(Value, na.rm=T), .groups="drop")
  res <- t.test(MeanVal ~ Trt, data = agg)
  print(paste("--- T-Test for:", variable_name, "---"))
  print(res)
}

# Stats Calls
run_stat(df_carbon %>% filter(Type=="Wood Growth"), "Wood Growth")
run_stat(df_carbon %>% filter(Type=="Root Growth"), "Root Growth")
# For Budget, compare Uptake vs Supply
run_stat(df_budget %>% filter(Type=="Tree Uptake"), "Tree P Uptake")
# For Pools
run_stat(df_pools %>% filter(Type=="Total Plant P"), "Total Plant P Pool")

print(">>> Generating Plots...")

# Themes
common_theme <- theme_bw() + 
  theme(legend.position="none", plot.title = element_text(size=11, face="bold"))
colors <- scale_fill_manual(values=c("aCO2"="#F8766D", "eCO2"="#00BFC4"))

# P1: Carbon Stagnation
p1 <- ggplot(df_carbon, aes(x=Trt, y=Value, fill=Trt)) +
  geom_boxplot(alpha=0.6, outlier.shape = NA) + geom_jitter(width=0.1, alpha=0.5) +
  facet_wrap(~Type, scales="free") +
  labs(title="1. Biomass Stagnation", y="g C m-2 yr-1", x="") +
  common_theme + colors

# P2: The Heist
p2 <- ggplot(df_budget, aes(x=Trt, y=Value, fill=Type)) +
  geom_boxplot(alpha=0.6, outlier.shape = NA) + geom_jitter(position=position_jitterdodge(), alpha=0.4) +
  labs(title="2. Supply vs Uptake", y="g P m-2 yr-1", x="") +
  theme_bw() + theme(legend.position="bottom") + 
  scale_fill_manual(values=c("orange", "forestgreen"))

# P3: The Inventory
p3 <- ggplot(df_pools, aes(x=Type, y=Value, fill=Trt)) +
  geom_boxplot(alpha=0.6, outlier.shape = NA) +
  labs(title="3. Phosphorus Inventory", y="g P m-2", x="") +
  common_theme + colors

# P4: Efficiency
p4 <- ggplot(df_pue, aes(x=Trt, y=PUE, fill=Trt)) +
  geom_boxplot(alpha=0.6, outlier.shape = NA) + geom_jitter(width=0.1) +
  labs(title="4. Efficiency (PUE)", y="g C / g P", x="") +
  common_theme + colors

# 6. SAVE FINAL OUTPUT ----------------------------------------------------
final_layout <- (p1 + p2) / (p3 + p4)
ggsave("EucFACE_Final_Analysis.png", final_layout, width=12, height=10, dpi=300)

print("Analysis Complete! Output saved as 'EucFACE_Final_Analysis.png'")
