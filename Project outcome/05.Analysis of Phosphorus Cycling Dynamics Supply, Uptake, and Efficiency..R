setwd("C:/Users/augre/Desktop/UPV S1/Data/2025Bgroup-phosphorusCO2/Flux_data")

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(lme4)
library(emmeans)
library(ggplot2)

# ==============================================================================
# 1. SETUP AND DATA LOADING
# ==============================================================================

# Define Treatments
treatments <- data.frame(
  Ring = factor(c(1, 2, 3, 4, 5, 6)),
  Trt = c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2")
)

# Helper function to process Rate-based files (mg P m-2 day-1)
process_rate_file <- function(filename, col_name) {
  df <- read.csv(filename)
  # Standardize Date
  if("Date" %in% names(df)) { df$Date <- ymd(df$Date) }
  
  # Calculate Total for that period (Rate * Days) / 1000 to get g m-2
  # Note: The files provided sometimes have 'Days' column. 
  # If flux is mg/m2/day, multiply by Days and divide by 1000 for g.
  
  df <- df %>%
    mutate(Year = year(Date)) %>%
    filter(Year >= 2013 & Year <= 2016) %>% # Limit to core study years
    mutate(g_P_flux = (df[[col_name]] * Days) / 1000) %>%
    group_by(Year, Ring) %>%
    summarise(Total_P = sum(g_P_flux, na.rm = TRUE), .groups = "drop")
  
  return(df)
}

# Helper for Total Flux files (already in g m-2 or similar, need summing)
process_total_file <- function(filename, col_name) {
  df <- read.csv(filename)
  # Try to find date column
  date_col <- names(df)[grep("Date", names(df), ignore.case = TRUE)][1]
  df$Date <- ymd(df[[date_col]])
  
  df <- df %>%
    mutate(Year = year(Date)) %>%
    filter(Year >= 2013 & Year <= 2016) %>%
    group_by(Year, Ring) %>%
    summarise(Total_P = sum(df[[col_name]], na.rm = TRUE), .groups = "drop")
  
  return(df)
}

# ==============================================================================
# 2. LOAD SPECIFIC P FLUXES
# ==============================================================================

# A. Canopy P Flux (This is P used to build leaves)
# File: canopy_p_flux.csv (Cols: canopy_p_flux)
df_canopy_p <- process_total_file("canopy_p_flux.csv", "canopy_p_flux") %>%
  rename(Canopy_P = Total_P)

# B. Wood P Flux (P used to build wood)
# File: wood_p_flux.csv
df_wood_p <- process_total_file("wood_p_flux.csv", "wood_p_flux") %>%
  rename(Wood_P = Total_P)

# C. Fine Root P Production (Rate based)
# File: fineroot_p_production_flux.csv (Col: fineroot_p_flux_mg_m2_d)
df_froot_p <- process_rate_file("fineroot_p_production_flux.csv", "fineroot_p_flux_mg_m2_d") %>%
  rename(FRoot_P = Total_P)

# D. Soil P Mineralization (The Supply)
# File: soil_p_mineralization_flux.csv (Col: p_mineralization_mg_m2_d)
# Note: Mineralization is depth specific. We sum all depths for total soil supply.
raw_min <- read.csv("soil_p_mineralization_flux.csv")
raw_min$Date <- ymd(raw_min$Date)
df_min_p <- raw_min %>%
  mutate(Year = year(Date)) %>%
  filter(Year >= 2013 & Year <= 2016) %>%
  # Sum depths first for each date/ring
  group_by(Date, Year, Ring, Days) %>%
  summarise(Daily_Rate_Sum = sum(p_mineralization_mg_m2_d, na.rm=TRUE), .groups="drop") %>%
  # Convert to Grams: (mg/day * days) / 1000
  mutate(Period_Flux_g = (Daily_Rate_Sum * Days) / 1000) %>%
  group_by(Year, Ring) %>%
  summarise(Mineralization_P = sum(Period_Flux_g, na.rm=TRUE), .groups="drop")

# E. GPP (For Efficiency Calculation)
# File: overstorey_gpp_flux.csv
# This file seems to be annual already
raw_gpp <- read.csv("overstorey_gpp_flux.csv")
df_gpp <- raw_gpp %>%
  rename(Year = year) %>% # Ensure capitalization matches
  select(Year, Ring, GPP) %>%
  filter(Year >= 2013 & Year <= 2016)

# ==============================================================================
# 3. MERGE DATA INTO P BUDGET
# ==============================================================================

# Merge all P metrics
p_budget <- df_canopy_p %>%
  left_join(df_wood_p, by = c("Year", "Ring")) %>%
  left_join(df_froot_p, by = c("Year", "Ring")) %>%
  left_join(df_min_p, by = c("Year", "Ring")) %>%
  left_join(df_gpp, by = c("Year", "Ring")) %>%
  mutate(Ring = factor(Ring)) %>%
  left_join(treatments, by = "Ring")

# Calculate Derived Metrics
p_budget <- p_budget %>%
  # 1. Total Plant Demand (Approximate Uptake for this analysis)
  # We assume Demand ≈ Uptake + Retranslocation. 
  # For specific "Uptake" from soil, we ideally subtract retranslocation, 
  # but Total Production P is a good proxy for "Invested P".
  mutate(Total_Plant_P_Flux = Canopy_P + Wood_P + FRoot_P) %>%
  
  # 2. Phosphorus Use Efficiency (Canopy Level)
  # How much Carbon (GPP) do they get per gram of P invested in leaves?
  mutate(Canopy_PUE = GPP / Canopy_P)

# ==============================================================================
# 4. STATISTICAL ANALYSIS (LME)
# ==============================================================================

# Model 1: Soil P Supply (Mineralization)
# Did eCO2 increase the supply of P?
mod_min <- lmer(Mineralization_P ~ Trt + (1|Year) + (1|Ring), data = p_budget)
print("--- TEST 1: Soil P Mineralization (Supply) ---")
print(summary(mod_min))
print(anova(mod_min))

# Model 2: Plant P Uptake (Total Flux into Biomass)
# Did the plants actually GET more P?
mod_uptake <- lmer(Total_Plant_P_Flux ~ Trt + (1|Year) + (1|Ring), data = p_budget)
print("--- TEST 2: Total Plant P Investment (Proxy for Uptake) ---")
print(summary(mod_uptake))
print(anova(mod_uptake))

# Model 3: Phosphorus Use Efficiency (PUE)
# Did plants become more efficient?
mod_pue <- lmer(Canopy_PUE ~ Trt + (1|Year) + (1|Ring), data = p_budget)
print("--- TEST 3: Canopy P Use Efficiency (PUE) ---")
print(summary(mod_pue))
print(anova(mod_pue))

# ==============================================================================
# 5. VISUALIZATION
# ==============================================================================

# Plot A: The "Heist" - Supply vs Uptake
# If Supply (Min) goes up but Uptake (Plant) stays flat, Microbes took it.
plot_data_long <- p_budget %>%
  select(Year, Ring, Trt, Mineralization_P, Total_Plant_P_Flux) %>%
  pivot_longer(cols = c(Mineralization_P, Total_Plant_P_Flux), 
               names_to = "Metric", values_to = "Value")

p1 <- ggplot(plot_data_long, aes(x = Trt, y = Value, fill = Metric)) +
  geom_boxplot() +
  geom_jitter(position = position_jitterdodge(), alpha=0.3) +
  labs(title = "The Microbial Heist: P Supply vs Plant Uptake",
       y = "Phosphorus Flux (g P m-2 yr-1)",
       x = "Treatment") +
  theme_bw() +
  scale_fill_manual(values = c("orange", "forestgreen"), 
                    labels = c("Soil Supply (Mineralization)", "Plant Production P"))

# Plot B: P Efficiency
p2 <- ggplot(p_budget, aes(x = Trt, y = Canopy_PUE, fill = Trt)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha=0.5) +
  labs(title = "Phosphorus Use Efficiency (PUE)",
       subtitle = "Does eCO2 make trees more efficient?",
       y = "g C Fixed per g P Invested (GPP / Canopy P)",
       x = "Treatment") +
  theme_bw()

print(p1)
print(p2)




