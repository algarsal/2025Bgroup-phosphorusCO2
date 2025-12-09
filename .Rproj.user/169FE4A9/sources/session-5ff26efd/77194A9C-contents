#The Hypothesis: The paper argues that because Phosphorus (P) is limited, trees under eCO2 cannot grow more wood ("Stagnation"). Instead, they should send that extra carbon underground to build roots to find more P ("Adaptability"). The Problem: Looking at just "wood" or just "roots" is not enough. We need to see the trade-off.The Solution: We calculated the total Carbon budget for every ring for every year (2013-2018) and compared the ratio of Root Investment vs. Wood Investment.

setwd("C:/Users/augre/Desktop/UPV S1/Data/2025Bgroup-phosphorusCO2/Flux_data")

# 1. LIBRARIES
library(tidyverse)
library(lubridate)
library(lme4)      # For Linear Mixed-Effects Models
library(lmerTest)  # To get p-values from LME models
library(patchwork) # For plotting

# 2. SETUP & HELPER FUNCTION 
#We create a "Lookup Table." Later, we will join this to our data so every number gets assigned a label: eCO2 or aCO2.
trt_map <- data.frame(
  Ring = factor(c(1, 2, 3, 4, 5, 6)),
  Trt = c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2")
) 

# Corrected Integration Function (Flux * Days / 1000) The data comes in Rates (grams per day). We cannot analyze "growth" using a daily rate; we need the Total Amount produced in a year.The measurements happen at irregular intervals (sometimes 14 days, sometimes 30 days).
process_c_flux <- function(file_path, val_col, item_name) {
  read_csv(file_path, show_col_types = FALSE) %>%
    mutate(Ring = factor(Ring), Year = year(ymd(Date))) %>%
    filter(Year >= 2013 & Year <= 2018) %>%
    mutate(Period_Mass_g = get(val_col) * Days) %>% # Integrate over time
    group_by(Year, Ring) %>%
    summarise(Annual_Sum_g = sum(Period_Mass_g, na.rm=TRUE), .groups="drop") %>%
    mutate(Component = item_name)
}
#We cannot compare a daily rate of twig fall to a yearly growth of wood. We normalized everything to Grams of Carbon per square meter per Year (g C m⁻² yr⁻¹).

# 3. LOAD ALL COMPREHENSIVE DATA COMPONENTS 

# --- BELOWGROUND (Total Root Investment) ---
# 1. Fine Roots (Foraging)
df_fr <- process_c_flux("fineroot_c_production_flux.csv", "fineroot_production_flux", "FineRoot")
# 2. Coarse Roots (Structural)
df_cr <- process_c_flux("coarse_root_c_production_flux.csv", "coarse_root_production_flux", "CoarseRoot")

# --- ABOVEGROUND (Total Woody Investment) ---
# 3. Wood (Stem Increment)
df_wood <- process_c_flux("wood_c_production_flux.csv", "wood_production_flux", "Wood")
# 4. Bark Litter
df_bark <- process_c_flux("barklitter_c_production_flux.csv", "bark_flux", "Bark")
# 5. Twig Litter
df_twig <- process_c_flux("twiglitter_c_production_flux.csv", "twig_flux", "Twig")
# 6. Seed/Reproduction
df_seed <- process_c_flux("seedlitter_c_production_flux.csv", "seed_flux", "Seed")


# 4. AGGREGATE THE BUDGET 

# Combine all dataframes
all_c_fluxes <- bind_rows(df_fr, df_cr, df_wood, df_bark, df_twig, df_seed)

# Pivot and Calculate Allocations
c_allocation_full <- all_c_fluxes %>%
  pivot_wider(names_from = Component, values_from = Annual_Sum_g, values_fill = 0) %>%
  mutate(
    # The Correct Denominator: Total Woody C
    Total_Woody_C = Wood + Bark + Twig + Seed,
    
    # The Correct Numerator: Total Root C
    Total_Root_C = FineRoot + CoarseRoot,
    
    # The Ecological Metric: Shift in Allocation
    Root_to_Wood_Ratio = Total_Root_C / Total_Woody_C
  ) %>%
  left_join(trt_map, by = "Ring")
#The tree doesn't just grow wood. It grows leaves, bark, twigs, seeds, and roots. If we only look at one part, we miss the "Strategy." the Solution: We sum the components into two main categories: Total Woody C: This represents "Growth" (Aboveground biomass). Total Root C: This represents "Foraging" (Searching for nutrients underground).


# 5. VISUALIZATION (Comprehensive Budget) 
library(ggplot2)
library(patchwork)

# Common theme settings to apply to all plots (prevents repetition)
# making text smaller prevents overlap
my_theme <- theme_bw() + 
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11, face = "bold"), # Smaller, bold title
    plot.subtitle = element_text(size = 9),              # Smaller subtitle
    axis.title.y = element_text(size = 9),
    axis.text = element_text(size = 9)
  )

# 1. Define the theme to keep things clean (optional, but helps readability)
my_theme <- theme_bw() + 
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    legend.position = "none" # Remove legend since x-axis labels are clear
  )

# Plot A: Total Woody Production
p1 <- ggplot(c_allocation_full, aes(x=Trt, y=Total_Woody_C, fill=Trt)) +
  # CHANGE: width = 0.8 makes the box occupy 80% of the category space
  geom_boxplot(width = 0.8, alpha=0.5, outlier.shape = NA) + 
  geom_jitter(width = 0.1, alpha=0.6, size=2) +
  labs(title="1. Total Woody Output", 
       subtitle="(Stem + Bark + Twig + Seed)", 
       y="g C m-2 yr-1",
       x="") + # Remove x label for top plots to save space
  scale_fill_manual(values = c("aCO2"="#F8766D", "eCO2"="#00BFC4")) + # Explicit colors
  my_theme

# Plot B: Total Root Production
p2 <- ggplot(c_allocation_full, aes(x=Trt, y=Total_Root_C, fill=Trt)) +
  geom_boxplot(width = 0.8, alpha=0.5, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha=0.6, size=2) +
  labs(title="2. Total Root Output", 
       subtitle="(Fine + Coarse)", 
       y="g C m-2 yr-1",
       x="") +
  scale_fill_manual(values = c("aCO2"="#F8766D", "eCO2"="#00BFC4")) +
  my_theme

# Plot C: The Allocation Ratio
p3 <- ggplot(c_allocation_full, aes(x=Trt, y=Root_to_Wood_Ratio, fill=Trt)) +
  geom_boxplot(width = 0.8, alpha=0.5, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha=0.6, size=2) +
  labs(title="3. Adaptability Strategy", 
       subtitle="Ratio: Total Roots / Total Wood", 
       y="Ratio",
       x="Treatment") +
  scale_fill_manual(values = c("aCO2"="#F8766D", "eCO2"="#00BFC4")) +
  my_theme

# Combine vertically
library(patchwork)
final_plot <- p1 / p2 / p3



# Display
print(final_plot)


ggsave("C_Allocation_Results.png", plot = final_plot, width = 6, height = 12, dpi = 300)

# Display
final_plot


# CRITICAL STEP: Save the file to ensure it looks good
# Making the height larger (e.g., 10) prevents the titles from crunching together
ggsave("Analysis_Results_Stacked.png", final_plot, width = 6, height = 10)
# 6. CORRECT STATISTICS (Linear Mixed-Effects Models) ---------------------

# We use lmer() to account for:
# 1. Fixed Effect: Trt (eCO2 vs aCO2)
# 2. Fixed Effect: Year (Accounting for inter-annual climate variability)
# 3. Random Effect: (1|Ring) (Accounting for repeated measures on the same plot)

run_lme <- function(formula, data, metric_name) {
  print(paste("--- LME Results for:", metric_name, "---"))
  model <- lmer(formula, data = data)
  print(anova(model)) # Returns the F-statistics and P-values
}

# Test 1: Did Wood Production Stagnate? 
run_lme(Total_Woody_C ~ Trt + as.factor(Year) + (1|Ring), c_allocation_full, "Woody Production")

# Test 2: Did Root Production Increase? 
run_lme(Total_Root_C ~ Trt + as.factor(Year) + (1|Ring), c_allocation_full, "Root Production")

# Test 3: Did the Allocation Strategy Shift? 
run_lme(Root_to_Wood_Ratio ~ Trt + as.factor(Year) + (1|Ring), c_allocation_full, "Root:Wood Ratio")



#Conclusion 1 (Woody Production): Result: Not Significant."Elevated CO2 did not result in a significant increase in woody biomass production (p>0.05) This confirms the hypothesis of biomass stagnation due to nutrient limitation."

#Conclusion 2 (Root Production): Result: Not Significant. Contrary to the hypothesis that trees would increase root biomass to forage for Phosphorus, we found no significant increase in total root production under eCO2."

#Conclusion 3 (Allocation Strategy): Result: Not Significant. "The Root-to-Wood allocation ratio remained constant between treatments. This indicates that the trees did not structurally adapt their biomass partitioning in response to the CO2 fertilization."

