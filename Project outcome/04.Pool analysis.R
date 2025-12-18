# ==============================================================================
# PHOSPHORUS POOL ANALYSIS: THE ECOSYSTEM INVENTORY
# ==============================================================================
##It tests if the P is locked up in Microbes (Hoarding) or Deep Soil (Unavailable), explaining why the trees cannot access it.
setwd("C:/Users/augre/Desktop/UPV S1/Data/2025Bgroup-phosphorusCO2/Pool data")
# 1. SETUP & LIBRARIES
library(tidyverse)
library(lubridate)
library(lme4)
library(lmerTest)
library(patchwork)

# Define Treatment Map
trt_map <- data.frame(
  Ring = factor(c(1, 2, 3, 4, 5, 6)),
  Trt = c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2")
)

# ==============================================================================
# 2. DATA PROCESSING FUNCTIONS
# ==============================================================================

# FUNCTION A: Process Simple Plant Pools (No Depth)
# Logic: Read -> Extract Year -> Filter 2013-2016 -> Average per Year
process_plant_pool <- function(filename, col_name, label) {
  read_csv(filename, show_col_types = FALSE) %>%
    mutate(Ring = factor(Ring), Date = ymd(Date)) %>%
    mutate(Year = year(Date)) %>%
    filter(Year >= 2013 & Year <= 2016) %>%
    group_by(Year, Ring) %>%
    summarise(Value = mean(get(col_name), na.rm=TRUE), .groups="drop") %>%
    mutate(Component = label)
}

# FUNCTION B: Process Soil/Microbial Pools (With Depth)
# Logic: Read -> Filter Depths (0-30cm) -> Sum Depths (to get Total m2) -> Average per Year
process_soil_pool <- function(filename, col_name, label) {
  read_csv(filename, show_col_types = FALSE) %>%
    mutate(Ring = factor(Ring), Date = ymd(Date)) %>%
    mutate(Year = year(Date)) %>%
    filter(Year >= 2013 & Year <= 2016) %>%
    # We only want the active root zone (0-10 and 10-30)
    # "transition" is usually deeper/variable, often excluded for standard 0-30cm comparisons
    filter(Depth %in% c("0_10", "10_30")) %>%
    # Step 1: Sum the depths for each specific date
    group_by(Date, Year, Ring) %>%
    summarise(Total_Depth_Sum = sum(get(col_name), na.rm=TRUE), .groups="drop") %>%
    # Step 2: Average the dates within a year
    group_by(Year, Ring) %>%
    summarise(Value = mean(Total_Depth_Sum, na.rm=TRUE), .groups="drop") %>%
    mutate(Component = label)
}

# ==============================================================================
# 3. LOAD DATA
# ==============================================================================

# --- VEGETATION POOLS ---
# 1. Canopy (Leaves)
df_leaf <- process_plant_pool("canopy_p_pool.csv", "leaf_p_pool", "Leaves")
# 2. Wood (Stems)
df_wood <- process_plant_pool("wood_p_pool.csv", "wood_p_pool", "Wood")
# 3. Fine Roots
df_froot <- process_plant_pool("fineroot_p_pool.csv", "fineroot_p_pool", "Fine Roots")
# 4. Coarse Roots
df_croot <- process_plant_pool("coarse_root_p_pool.csv", "coarse_root_p_pool", "Coarse Roots")
# 5. Understorey
df_under <- process_plant_pool("understorey_p_pool.csv", "understorey_p_pool", "Understorey")

# --- SOIL POOLS ---
# 6. Microbial Biomass P (The Competitors)
df_mic <- process_soil_pool("microbial_p_pool.csv", "microbial_p_g_m2", "Microbes")

# 7. Available Phosphate (The "Free" Cash - usually tiny)
df_avail <- process_soil_pool("soil_phosphate_pool.csv", "soil_phosphate_p_g_m2", "Available Soil P")

# 8. Total Soil P (The Warehouse)
df_total_soil <- process_soil_pool("soil_p_pool.csv", "soil_p_g_m2", "Total Soil P")

# ==============================================================================
# 4. CALCULATE AGGREGATE METRICS
# ==============================================================================

# Calculate TOTAL PLANT P (Sum of all plant parts)
df_total_plant <- bind_rows(df_leaf, df_wood, df_froot, df_croot, df_under) %>%
  group_by(Year, Ring) %>%
  summarise(Value = sum(Value, na.rm=TRUE), .groups="drop") %>%
  mutate(Component = "Total Plant P")

# Combine everything for plotting
all_pools <- bind_rows(df_total_plant, df_mic, df_avail) %>%
  left_join(trt_map, by="Ring")

# ==============================================================================
# 5. VISUALIZATION
# ==============================================================================

# Plot A: The Inventory (Distribution of P)
# Shows Mean sizes of Plant vs Microbe vs Available
means_for_plot <- all_pools %>%
  group_by(Trt, Component) %>%
  summarise(Mean_Val = mean(Value, na.rm=TRUE), .groups="drop")

p1 <- ggplot(means_for_plot, aes(x=Component, y=Mean_Val, fill=Trt)) +
  geom_bar(stat="identity", position=position_dodge()) +
  labs(title = "Where is the Phosphorus?",
       subtitle = "Comparison of P Stocks (0-30cm Soil)",
       y = "Phosphorus Pool (g P m-2)",
       x = "") +
  theme_bw() +
  scale_fill_manual(values=c("aCO2"="#F8766D", "eCO2"="#00BFC4")) +
  theme(legend.position = "bottom")

# Plot B: The "Stagnation" Check (Boxplot of Total Plant P)
p2 <- ggplot(df_total_plant %>% left_join(trt_map, by="Ring"), 
             aes(x=Trt, y=Value, fill=Trt)) +
  geom_boxplot(alpha=0.6, width=0.5, outlier.shape = NA) +
  geom_jitter(width=0.1, size=2, alpha=0.6) +
  labs(title = "Did Trees Store More P?",
       subtitle = "Total Plant P Biomass (Wood+Root+Leaf)",
       y = "Total Plant P (g m-2)",
       x = "Treatment") +
  theme_bw() +
  scale_fill_manual(values=c("aCO2"="#F8766D", "eCO2"="#00BFC4")) +
  theme(legend.position = "none")

# Combine and Save
final_plot <- p1 + p2
print(final_plot)
ggsave("Phosphorus_Pools_Analysis.png", plot=final_plot, width=12, height=6)

# ==============================================================================
# 6. STATISTICS
# ==============================================================================

print("--- STATS: Did eCO2 increase Total Plant P? (Stagnation Check) ---")
mod_plant <- lmer(Value ~ Trt + as.factor(Year) + (1|Ring), 
                  data = df_total_plant %>% left_join(trt_map, by="Ring"))
print(anova(mod_plant))

print("--- STATS: Did Microbes hoard more P under eCO2? ---")
mod_mic <- lmer(Value ~ Trt + as.factor(Year) + (1|Ring), 
                data = df_mic %>% left_join(trt_map, by="Ring"))
print(anova(mod_mic)) 


#Chemical Limitation: "The pool of Available Soil P (Phosphate) was negligible compared to biological pools, confirming that the ecosystem is severely P-limited.
#"Microbial Dominance:"The Microbial P pool was substantial, holding a quantity of Phosphorus comparable to the entire standing biomass of the forest vegetation. This supports the hypothesis of strong microbial competition.
#"Biomass Stagnation: "There was no significant increase in Total Plant P stocks under elevated CO2 (p>0.05p>0.05). This confirms that the physiological constraints imposed by P limitation prevented the conversion of atmospheric CO2 into long-term biomass storage."