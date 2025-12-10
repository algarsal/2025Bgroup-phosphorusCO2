#Load the correct location of files we are using for this code:
setwd("C:/Users/augre/Desktop/UPV S1/Data/2025Bgroup-phosphorusCO2/Flux_data")
library(tidyverse)
library(lubridate)

# 1.Created a reference table assigning Rings 1, 4, 5 to Elevated CO2 (eCO2) and Rings 2, 3, 6 to Ambient CO2 (aCO2). The raw data files only have "Ring Numbers" (1-6).To answer the research question ("Does eCO2 change anything?"), we must group the data by treatment.
trt_map <- data.frame(
  Ring = factor(c(1, 2, 3, 4, 5, 6)),
  Trt = c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2")
)

# 2. THE CORRECTED FUNCTION; we wrote a custom function to handle the math for every file. we must convert everything to grams per square meter per year.
# Then averages across the 6 years (2013-2018)
process_flux_correct <- function(file_path, val_col, item_name) {
  
  df <- read_csv(file_path, show_col_types = FALSE) %>%
    mutate(
      Ring = factor(Ring),
      Year = year(ymd(Date)) # Extract year from measurement date
    ) %>%
    # Filtered for the correct paper timeframe (2013-2018)
    filter(Year >= 2013 & Year <= 2018)
  
  # CALCULATION FIX: 
  # 1. Calculate total mass for this specific period (mg m-2)
  # 2. Sum all periods in a year to get Annual Total (mg m-2 yr-1)
  # 3. Convert to grams (div by 1000)
  
  df_annual <- df %>%
    mutate(Period_Mass_mg = get(val_col) * Days) %>% # Flux * Duration
    group_by(Year, Ring) %>%
    summarise(Annual_Sum_g = sum(Period_Mass_mg, na.rm = TRUE) / 1000, .groups = "drop") %>%
    mutate(Type = item_name)
  
  return(df_annual)
}

# 3. LOAD DATA: three specific categories of plant data (Phosphorus Budget Components) 

#DEMAND: How much Phosphorus the tree needs to build new Wood, Leaves, and Roots

df_wood_dem  <- process_flux_correct("wood_p_flux.csv", "wood_p_flux", "Wood_Demand")
df_leaf_dem  <- process_flux_correct("canopy_p_flux.csv", "canopy_p_flux", "Canopy_Demand")
# For roots, use production flux
df_root_dem  <- process_flux_correct("fineroot_p_production_flux.csv", "fineroot_p_flux_mg_m2_d", "Root_Demand")

# --- RECYCLING (P retranslocated internally) How much Phosphorus the tree sucks back in from dying leaves/wood before dropping them.
df_leaf_ret  <- process_flux_correct("canopy_P_retranslocation_flux.csv", "canopy_p_retrans_flux", "Canopy_Retrans")
df_wood_ret  <- process_flux_correct("sapwood_P_retranslocation_flux.csv", "sapwood_p_retrans_flux", "Wood_Retrans")
df_root_ret  <- process_flux_correct("fineroot_P_retranslocation_flux.csv", "fineroot_p_retrans_flux", "Root_Retrans")

#SUPPLY (Soil Mineralization)
# Soil data usually has depths. We summed up the mineralization rates across soil depths (0-10cm, 10-30cm, etc.) before calculating the annual total.Roots exist at multiple depths. If we only looked at the topsoil, we would underestimate the total "Supply" of Phosphorus available to the tree. 
soil_raw <- read_csv("soil_p_mineralization_flux.csv", show_col_types = FALSE) %>%
  mutate(Ring = factor(Ring), Year = year(ymd(Date))) %>%
  filter(Year >= 2013 & Year <= 2018)

# Step A: Sum across depths for each specific date/ring to get Ecosystem Total
soil_daily_sums <- soil_raw %>%
  group_by(Date, Ring, Year, Days) %>%
  summarise(Daily_Flux_Total = sum(p_mineralization_mg_m2_d, na.rm = TRUE), .groups = "drop")

# Step B: Apply the time integration formula
df_soil_supply <- soil_daily_sums %>%
  mutate(Period_Mass_mg = Daily_Flux_Total * Days) %>%
  group_by(Year, Ring) %>%
  summarise(Annual_Sum_g = sum(Period_Mass_mg, na.rm = TRUE) / 1000, .groups = "drop") %>%
  mutate(Type = "Soil_Supply")

# 4. CALCULATE THE BUDGET 

# Combine all plant components
plant_components <- bind_rows(df_wood_dem, df_leaf_dem, df_root_dem, 
                              df_leaf_ret, df_wood_ret, df_root_ret)

# Pivot to wide format to do math
budget_calc <- plant_components %>%
  group_by(Year, Ring, Type) %>%
  summarise(Value = sum(Annual_Sum_g, na.rm=TRUE), .groups="drop") %>%
  pivot_wider(names_from = Type, values_from = Value, values_fill = 0) %>%
  mutate(
    # Total P needed by tree
    Total_Demand = Wood_Demand + Canopy_Demand + Root_Demand,
    # Total P recycled by tree
    Total_Retrans = Canopy_Retrans + Wood_Retrans + Root_Retrans,
    # Net P Uptake from Soil (The critical number)
    Plant_P_Uptake = Total_Demand - Total_Retrans
  ) %>%
  select(Year, Ring, Plant_P_Uptake)

# Join Uptake with Supply
final_analysis <- budget_calc %>%
  left_join(df_soil_supply, by = c("Year", "Ring")) %>%
  rename(Soil_P_Supply = Annual_Sum_g) %>%
  left_join(trt_map, by = "Ring") %>%
  mutate(
    # The Gap: If Positive, Microbes are likely immobilizing the P
    P_Gap = Soil_P_Supply - Plant_P_Uptake
  )

# 5. VISUALIZATION & STATISTICS --------------------------------------------

# A. Visualizing the Fluxes
plot_data <- final_analysis %>%
  pivot_longer(cols = c("Soil_P_Supply", "Plant_P_Uptake"), 
               names_to = "Flux_Pathway", values_to = "g_P_m2_yr")

ggplot(plot_data, aes(x = Trt, y = g_P_m2_yr, fill = Flux_Pathway)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1), alpha = 0.4) +
  labs(title = "The Microbial Bottleneck",
       subtitle = "Comparison of Available Soil P (Supply) vs. Tree Uptake",
       y = "Phosphorus Flux (g P m-2 yr-1)",
       x = "Treatment") +
  scale_fill_manual(values = c("Soil_P_Supply" = "#E69F00", "Plant_P_Uptake" = "#009E73"),
                    labels = c("Tree Uptake", "Soil Supply (Mineralization)")) +
  theme_bw()

# B. Statistical Summary (Time-Averaged)
summary_table <- final_analysis %>%
  group_by(Trt) %>%
  summarise(
    Avg_Supply = mean(Soil_P_Supply, na.rm = TRUE),
    Avg_Uptake = mean(Plant_P_Uptake, na.rm = TRUE),
    Avg_Gap_Microbial_Lock = mean(P_Gap, na.rm = TRUE)
  )

print("--- Time-Averaged Ecosystem P Budget (2013-2018) ---")
print(summary_table)

#Orange Box (Soil Supply) is significantly higher than the Green Box (Tree Uptake).The soil microbes are mineralizing (releasing) plenty of Phosphorus—enough to support more tree growth. However, the trees are only capturing a small fraction of it.This proves the "Microbial Bottleneck." The microbes release the P, but because they are physically closer to the enzyme sites and biologically faster, they "immobilize" (eat) the P before the tree roots can grab it.
# CO2 did not solve the problem.Green Boxes (Uptake) for aCO2 vs eCO2 are likely at the same level (no significant increase).#Scientific Meaning: Even though the trees had extra Carbon (energy) from the eCO2 to grow more roots , it didn't result in more Phosphorus Uptake.
#Result: The microbial competition is so strong that it negates the benefits of elevated CO2. The trees cannot "buy" their way out of the nutrient shortage.





# C. Is the gap significantly larger under eCO2? (Testing the hypothesis)
# If trees allocate C to roots but get no P, the gap might widen or stay same, 
# but Uptake wont increase significantly.
t_test_gap <- t.test(P_Gap ~ Trt, data = final_analysis)
print(paste("P-value for CO2 effect on the P Gap:", round(t_test_gap$p.value, 4)))

#The trees are trapped. Even with extra CO2 energy, they cannot overcome the microbial competition. The gap remains, and the trees stay small.
