# 1. LIBRARIES ------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(lme4)      # For Linear Mixed-Effects Models
library(lmerTest)  # To get p-values from LME models
library(patchwork) # For plotting

# 2. SETUP & HELPER FUNCTION ----------------------------------------------

trt_map <- data.frame(
  Ring = factor(c(1, 2, 3, 4, 5, 6)),
  Trt = c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2")
)

# Corrected Integration Function (Flux * Days / 1000)
process_c_flux <- function(file_path, val_col, item_name) {
  read_csv(file_path, show_col_types = FALSE) %>%
    mutate(Ring = factor(Ring), Year = year(ymd(Date))) %>%
    filter(Year >= 2013 & Year <= 2018) %>%
    mutate(Period_Mass_g = get(val_col) * Days) %>% # Integrate over time
    group_by(Year, Ring) %>%
    summarise(Annual_Sum_g = sum(Period_Mass_g, na.rm=TRUE), .groups="drop") %>%
    mutate(Component = item_name)
}

# 3. LOAD ALL COMPREHENSIVE DATA COMPONENTS -------------------------------

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

# 4. AGGREGATE THE BUDGET -------------------------------------------------

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

# 5. VISUALIZATION (Comprehensive Budget) ---------------------------------
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

# Plot A: Total Woody Production
p1 <- ggplot(c_allocation_full, aes(x=Trt, y=Total_Woody_C, fill=Trt)) +
  # CHANGE 1: width = 0.7 makes the box wider (default is usually 0.5 or 0.75)
  geom_boxplot(width = 0.7, alpha=0.5) + 
  geom_jitter(width=0.1, alpha=0.6) +
  labs(title="1. Total Woody Output", subtitle="(Stem+Bark+Twig+Seed)", y="g C m-2 yr-1") +
  my_theme

# Plot B: Total Root Production
p2 <- ggplot(c_allocation_full, aes(x=Trt, y=Total_Root_C, fill=Trt)) +
  geom_boxplot(width = 0.7, alpha=0.5) +
  geom_jitter(width=0.1, alpha=0.6) +
  labs(title="2. Total Root Output", subtitle="(Fine + Coarse)", y="g C m-2 yr-1") +
  my_theme

# Plot C: The Allocation Ratio
p3 <- ggplot(c_allocation_full, aes(x=Trt, y=Root_to_Wood_Ratio, fill=Trt)) +
  geom_boxplot(width = 0.7, alpha=0.5) +
  geom_jitter(width=0.1, alpha=0.6) +
  labs(title="3. Adaptability Strategy", subtitle="Ratio: Total Roots / Total Wood", y="Ratio") +
  my_theme

# Combine vertically
final_plot <- p1 / p2 / p3

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

# Test 1: Did Wood Production Stagnate? (Expect P > 0.05 for Trt)
run_lme(Total_Woody_C ~ Trt + as.factor(Year) + (1|Ring), c_allocation_full, "Woody Production")

# Test 2: Did Root Production Increase? (Expect P < 0.05 or trend)
run_lme(Total_Root_C ~ Trt + as.factor(Year) + (1|Ring), c_allocation_full, "Root Production")

# Test 3: Did the Allocation Strategy Shift? (Expect P < 0.05)
run_lme(Root_to_Wood_Ratio ~ Trt + as.factor(Year) + (1|Ring), c_allocation_full, "Root:Wood Ratio")
