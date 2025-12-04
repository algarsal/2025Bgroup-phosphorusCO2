



# Check your current working directory (optional)


# Set the working directory to the folder containing your files

setwd("C:/Users/augre/Desktop/UPV S1/Data/2025Bgroup-phosphorusCO2/New folder")
getwd()
list.files()
# --- Install and Load Packages ---
# install.packages("tidyverse")
library(tidyverse)

# ----------------------------------------------------------------------
# PART A: Microbial C:P Ratio Analysis
# ----------------------------------------------------------------------

# --- 1. Data Loading and Preparation ---

# Load the essential microbial C and P pool data
cmic <- read_csv("microbial_c_pool.csv")
pmic <- read_csv("microbial_p_pool.csv")

# Merge the data and calculate the Microbial C:P Ratio
microbial_data <- cmic %>%
  # Select only the columns needed from C data
  select(Date, Ring, Depth, Cmic_g_m2) %>%
  # Merge with P data by Date, Ring, and Depth
  full_join(pmic %>% select(Date, Ring, Depth, Pmic_g_m2 = microbial_p_g_m2),
            by = c("Date", "Ring", "Depth")) %>%
  mutate(
    # Convert 'Date' column to proper date format
    Date = as.Date(Date, format = "%Y-%m-%d"),
    # Calculate the Microbial C:P ratio (key metric)
    C_P_ratio = Cmic_g_m2 / Pmic_g_m2,
    # Convert 'Ring' and 'Depth' to categorical factors
    Ring = as.factor(Ring),
    Depth = as.factor(Depth)
  ) %>%
  # Remove rows missing the calculated ratio
  drop_na(C_P_ratio)

print("--- Data Structure Check: Microbial C:P Ratio ---")
print(head(microbial_data))


# --- 2. Visualization: C:P Ratio Over Time ---

p_cp_ratio <- ggplot(microbial_data,
                     # Map Date to x-axis, C:P Ratio to y-axis
                     aes(x = Date, y = C_P_ratio, color = Depth)) +
  # Add individual points
  geom_point(alpha = 0.5) +
  # Add a smoothed line to show the trend over time
  geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
  # Create separate panels (facets) for each Ring
  facet_wrap(~Ring, ncol = 3) +
  labs(
    title = "Microbial C:P Ratio Trends by Ring and Depth",
    x = "Sampling Date",
    y = "Microbial C:P Ratio (Cmic/Pmic)"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

# You can save the plot using: ggsave("C_P_ratio_trend.png", plot = p_cp_ratio)
print("Microbial C:P Ratio plot generated.")


# --- 3. Simple Statistical Test: Two-way ANOVA ---

# Calculate the average C:P ratio for each Ring and Depth combination over the entire study period
avg_cp_data <- microbial_data %>%
  group_by(Ring, Depth) %>%
  summarise(Mean_C_P = mean(C_P_ratio, na.rm = TRUE), .groups = 'drop') %>%
  # Filter out 'transition' depth for a cleaner, standard ANOVA
  filter(Depth != "transition")

# Perform a Two-way ANOVA to test for main effects of Ring and Depth
print("--- Statistical Analysis: Two-way ANOVA on Mean C:P Ratio ---")

anova_model <- aov(Mean_C_P ~ Ring * Depth, data = avg_cp_data)
summary(anova_model)


# ----------------------------------------------------------------------
# PART B: P Mineralization Flux Analysis (Rate of Cycling)
# ----------------------------------------------------------------------

# --- 4. Load and Visualize P Flux ---

p_min_flux <- read_csv("soil_p_mineralization_flux.csv") %>%
  mutate(
    Date = as.Date(Date, format = "%Y-%m-%d"),
    Ring = as.factor(Ring),
    Depth = as.factor(Depth)
  ) %>%
  # Focus only on the essential columns
  select(Date, Ring, Depth, p_mineralization_mg_m2_d)

p_flux_plot <- ggplot(p_min_flux,
                      aes(x = Date, y = p_mineralization_mg_m2_d, color = Depth)) +
  geom_line(aes(group = interaction(Ring, Depth)), alpha = 0.4) +
  geom_point(aes(shape = Ring), alpha = 0.6) +
  # Add a line at zero for reference (mineralization vs. immobilization)
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~Ring, scales = "free_y") +
  labs(
    title = "P Mineralization Flux Over Time",
    x = "Sampling Date",
    y = expression(paste("P Mineralization Flux (mg ", m^{-2}, " ", d^{-1}, ")"))
  ) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2")

# You can save the plot using: 
ggsave("p_flux_trend.png", plot = p_flux_plot)
print("P Mineralization Flux plot generated.")

getwd() 


# --- 1. Calculate Mean C:P Ratio ---

# Calculate the average C:P ratio for each Ring and Depth combination 
# over the entire study period, and filter out the 'transition' depth 
# for a cleaner ANOVA.
avg_cp_data <- microbial_data %>%
  group_by(Ring, Depth) %>%
  summarise(Mean_C_P = mean(C_P_ratio, na.rm = TRUE), .groups = 'drop') %>%
  filter(Depth != "transition")

# Check the resulting data table
print("--- Time-Averaged C:P Data ---")
print(avg_cp_data)

# --- 2. Perform Two-way ANOVA ---

# Test for the main effects of Ring, Depth, and their interaction on the mean C:P ratio.
print("--- Statistical Analysis: Two-way ANOVA on Mean C:P Ratio ---")

anova_model <- aov(Mean_C_P ~ Ring * Depth, data = avg_cp_data)

# Print the full ANOVA results
summary(anova_model)

# --- NEW Step 2: Perform Two-way ANOVA on RAW Data ---

# Filter the raw data first (assuming 'microbial_data' is loaded and cleaned)
microbial_data_filtered <- microbial_data %>%
  filter(Depth != "transition") %>%
  drop_na(C_P_ratio)

print("--- Statistical Analysis: Two-way ANOVA on Raw C:P Data ---")

# Run ANOVA on the raw data (microbial_data_filtered), which includes replication over time
anova_raw <- aov(C_P_ratio ~ Ring * Depth, data = microbial_data_filtered)

summary(anova_raw) 
# This will produce valid P-values!

# Ring - The Treatment (Ring) has no significant effect on the Microbial C:P ratio
#Depth - Depth has no significant effect on the Microbial C:P ratio
#The effect of ring does not depend on depth 
#Based on the analysis there is no statistically significant evidence that either the experimental treatment Ring or the soil layer Depth significantly altered the microbial nutrient limitation C:P ratio during the study period. 


# 1. VISUAL CHECK: Normality (Q-Q Plot) and Homogeneity (Residuals Plot)
# A window will pop up with four plots. Press [Enter] after each plot.
plot(anova_raw)  
# For Normality: Dots should closely follow the diagonal line on the "Normal Q-Q" plot.
# For Homogeneity: Dots should be scattered randomly around the horizontal line on the "Residuals vs Fitted" plot.

# 2. STATISTICAL CHECK: Levene's Test for Homogeneity of Variance
# You might need to install the 'car' package: install.packages("car")
library(car)

# Test if variance is equal across all Ring*Depth combinations
leveneTest(C_P_ratio ~ Ring * Depth, data = microbial_data_filtered)
# If the P-value of Levene's test is > 0.05, the assumption is met.

# 1. Filter the data again (for safety)
microbial_data_filtered <- microbial_data %>%
  filter(Depth != "transition") %>%
  drop_na(C_P_ratio)

# 2. Generate Boxplot
p_boxplot <- ggplot(microbial_data_filtered, 
                    aes(x = Ring, y = C_P_ratio, fill = Ring)) +
  geom_boxplot() +
  # Separate the boxes by Depth
  facet_wrap(~Depth) + 
  labs(title = "Microbial C:P Ratio by Ring and Depth",
       x = "Ring (Treatment)",
       y = "Microbial C:P Ratio (Cmic/Pmic)") +
  theme_bw() +
  theme(legend.position = "none") 


# 3. Save the plot
ggsave("final_cp_boxplot.png", plot = p_boxplot, width = 10, height = 6)
leveneTest(C_P_ratio ~ Ring * Depth, data = microbial_data_filtered)
# Filter and prepare P Mineralization Flux data
p_min_flux_filtered <- p_min_flux %>%
  filter(Depth != "transition") %>%
  drop_na(p_mineralization_mg_m2_d)

print("--- Running ANOVA on P Mineralization Flux ---")

# Run ANOVA on the P mineralization flux
anova_flux <- aov(p_mineralization_mg_m2_d ~ Ring * Depth, data = p_min_flux_filtered)

summary(anova_flux)
