
# --- 1. Setup and Package Installation ---

# Install necessary packages if you haven't already:
# install.packages(c("tidyverse", "car", "rstatix", "RVAideMemoire"))

library(tidyverse) # For data manipulation and plotting (ggplot2)
library(car)       # For MANOVA and Type III ANOVA
library(rstatix)   # For ANOVA helper functions (e.g., Anova_test)
library(RVAideMemoire) # For sphericity correction in MANOVA/ANOVA

# --- 2. Data Loading and Preparation ---

# NOTE: The provided file path (C:\Users\Oishanu...) is a local path and cannot 
# be used by the current execution environment. Please place your consolidated 
# data file in the same directory as this script, or replace 'path_to_data' 
# with the correct, accessible path.

# *** IMPORTANT: You need to create this file! ***
# It must contain the columns: Ring, Year, C_Pool, P_Pool, Microbe_Pool
data_file_name <- "consolidated_pool_data.csv"

# --- Placeholder Data Generation ---
# Since your actual data is not available, we create a mock dataset to ensure 
# the rest of the script runs and demonstrates the correct statistical logic.
# REPLACE this entire block with the actual data loading step below.
set.seed(42)
num_years <- 5
num_rings <- 6
mock_data <- expand.grid(Ring = 1:num_rings, Year = 2012:(2012 + num_years - 1)) %>%
  mutate(
    # Simulate data with a slight treatment difference and time trend
    Treatment = factor(ifelse(Ring %in% c(1, 4, 5), "Control", "Track")),
    C_Pool = 10 + 0.5 * (Year - 2012) + (Ring %% 2) * 1 + rnorm(n()),
    P_Pool = 5 + 0.3 * (Year - 2012) + (Ring %% 2) * 0.5 + rnorm(n()),
    Microbe_Pool = 2 + 0.2 * (Year - 2012) - (Ring %% 2) * 0.3 + rnorm(n())
  )
# mock_data$Ring <- factor(mock_data$Ring) # Keep Ring as a factor for ANOVA subject ID

# Use the mock data for demonstration
df <- mock_data 
# --- END Placeholder Data Generation ---

# --- Actual Data Loading (Uncomment and use when you have the consolidated file) ---
# file_path <- file.path("C:/Users/Oishanu/OneDrive/Desktop/EM Plant Health/Subject wise 1st sem/Data Acquisition/2025Bgroup-phosphorusCO2/New folder", data_file_name)
# df <- read.csv(file_path)
# --- End Actual Data Loading ---

# 3. Data Formatting for Analysis
df <- df %>%
  mutate(
    Ring = factor(Ring),
    Year = factor(Year), # Treat Year as a categorical variable for RM-ANOVA
    Treatment = factor(ifelse(Ring %in% c(1, 4, 5), "Control", "Track"))
  )

# --- 4. Multivariate Analysis (MANOVA) ---
# Tests if the combined mean vector of C_Pool, P_Pool, and Microbe_Pool 
# differs significantly between treatments and over time.

# Create the response matrix
pool_matrix <- cbind(df$C_Pool, df$P_Pool, df$Microbe_Pool)

# Fit the MANOVA model (Treatment and Year fixed effects, interaction)
manova_fit <- manova(pool_matrix ~ Treatment * Year, data = df)

# Summarize the MANOVA results using a robust test (Pillai is recommended)
cat("\n--- MANOVA Results (Overall effect on C, P, and Microbe Pools) ---\n")
print(summary(manova_fit, test = "Pillai"))


# --- 5. Repeated Measures ANOVA (RM-ANOVA) ---
# Tests the effect of Treatment and Year on each pool individually, 
# accounting for repeated measures on the same ring over time.

# Use aov with the Error term for RM-ANOVA (Requires balanced data)
cat("\n--- Repeated Measures ANOVA: Carbon Pool ---\n")
c_aov <- aov(C_Pool ~ Treatment * Year + Error(Ring/Year), data = df)
print(summary(c_aov))

cat("\n--- Repeated Measures ANOVA: Phosphorus Pool ---\n")
p_aov <- aov(P_Pool ~ Treatment * Year + Error(Ring/Year), data = df)
print(summary(p_aov))

cat("\n--- Repeated Measures ANOVA: Microbe Pool ---\n")
microbe_aov <- aov(Microbe_Pool ~ Treatment * Year + Error(Ring/Year), data = df)
print(summary(microbe_aov))

# --- 6. Visualization: Time Series Plots ---

# Prepare summary data (Mean and Standard Error)
summary_df <- df %>%
  group_by(Treatment, Year) %>%
  summarise(
    C_Mean = mean(C_Pool), C_SE = sd(C_Pool) / sqrt(n()),
    P_Mean = mean(P_Pool), P_SE = sd(P_Pool) / sqrt(n()),
    M_Mean = mean(Microbe_Pool), M_SE = sd(Microbe_Pool) / sqrt(n()),
    .groups = 'drop'
  )

# Plot 1: Carbon Pool Over Time
plot_c <- ggplot(summary_df, aes(x = Year, y = C_Mean, group = Treatment, color = Treatment)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = C_Mean - C_SE, ymax = C_Mean + C_SE), width = 0.2) +
  labs(
    title = "Carbon Pool Over Study Period (Mean ± SE)",
    y = "Carbon Pool Size (Units TBD)",
    x = "Year",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Control" = "#1b9e77", "Track" = "black"))

print(plot_c)

# Plot 2: Phosphorus Pool Over Time
plot_p <- ggplot(summary_df, aes(x = Year, y = P_Mean, group = Treatment, color = Treatment)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = P_Mean - P_SE, ymax = P_Mean + P_SE), width = 0.2) +
  labs(
    title = "Phosphorus Pool Over Study Period (Mean ± SE)",
    y = "Phosphorus Pool Size (Units TBD)",
    x = "Year",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Control" = "darkblue", "Track" = "#d95f02"))

print(plot_p)

# Plot 3: Microbe Pool Over Time
plot_m <- ggplot(summary_df, aes(x = Year, y = M_Mean, group = Treatment, color = Treatment)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = M_Mean - M_SE, ymax = M_Mean + M_SE), width = 0.2) +
  labs(
    title = "Microbial Pool Over Study Period (Mean ± SE)",
    y = "Microbial Pool Size (Units TBD)",
    x = "Year",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Control" = "orange", "Track" = "blue"))

print(plot_m)


###############Neww
# --- 1. Setup and Package Installation ---

# Install necessary packages if you haven't already:
# install.packages(c("tidyverse", "car", "rstatix", "RColorBrewer"))

library(tidyverse) # For data manipulation and plotting (ggplot2)
library(car)       # For MANOVA and Type III ANOVA
library(rstatix)   # For ANOVA helper functions
library(RColorBrewer) # For color palettes
library(ggrepel)   # To prevent labels from overlapping

# --- 2. Data Loading and Preparation (Requires Consolidation) ---

# *** IMPORTANT: You need to create this file! ***
# It must contain the columns: Ring, Year, C_Pool, P_Pool, Microbe_Pool
# NOTE: The file path provided (C:\Users\Oishanu...) is a local path and cannot 
# be accessed here. Please ensure your consolidated data is ready.

# --- Placeholder Data Generation ---
# REPLACE this entire block with the actual data loading step below.
set.seed(42)
num_years <- 5
num_rings <- 6
mock_data <- expand.grid(Ring = 1:num_rings, Year = 2012:(2012 + num_years - 1)) %>%
  mutate(
    # Simulate data with a slight treatment difference and time trend
    Treatment = factor(ifelse(Ring %in% c(1, 4, 5), "Control", "Track")),
    C_Pool = 100 + 15 * (Year - 2012) + (Ring %% 2) * 5 + rnorm(n(), sd=5), # Carbon
    P_Pool = 20 + 4 * (Year - 2012) + (Ring %% 2) * 2 + rnorm(n(), sd=2),   # Phosphorus
    Microbe_Pool = 5 + 1.5 * (Year - 2012) - (Ring %% 2) * 1 + rnorm(n(), sd=1) # Microbes
  )

df <- mock_data 
# --- END Placeholder Data Generation ---

# 3. Data Formatting for Analysis
df <- df %>%
  mutate(
    Ring = factor(Ring),
    Year = factor(Year), # Treat Year as a categorical variable for RM-ANOVA
    Treatment = factor(Treatment)
  )

# --- 4. Multivariate Analysis (MANOVA) ---
cat("\n--- MANOVA Results (Overall effect on C, P, and Microbe Pools) ---\n")

# Create the response matrix
pool_matrix <- cbind(df$C_Pool, df$P_Pool, df$Microbe_Pool)

# Fit the MANOVA model (Treatment and Year fixed effects, interaction)
# Note: For repeated measures (nested within Ring), the 'Ring' must be a subject ID.
# A simpler fixed effects model is run here, assuming annual mean data is used:
manova_fit <- manova(pool_matrix ~ Treatment * Year, data = df)

# Summarize the MANOVA results
print(summary(manova_fit, test = "Pillai"))

# --- 5. Repeated Measures ANOVA (RM-ANOVA) ---
# Tests the effect of Treatment and Year on each pool individually, 
# accounting for repeated measures on the same ring over time (via Error term).

cat("\n--- Repeated Measures ANOVA: Carbon Pool ---\n")
c_aov <- aov(C_Pool ~ Treatment * Year + Error(Ring/Year), data = df)
print(summary(c_aov))

cat("\n--- Repeated Measures ANOVA: Phosphorus Pool ---\n")
p_aov <- aov(P_Pool ~ Treatment * Year + Error(Ring/Year), data = df)
print(summary(p_aov))

cat("\n--- Repeated Measures ANOVA: Microbe Pool ---\n")
microbe_aov <- aov(Microbe_Pool ~ Treatment * Year + Error(Ring/Year), data = df)
print(summary(microbe_aov))

# --- 6. Visualization: Time Series Plots (Classic View) ---

# Prepare summary data (Mean and Standard Error)
summary_df <- df %>%
  group_by(Treatment, Year) %>%
  summarise(
    C_Mean = mean(C_Pool), C_SE = sd(C_Pool) / sqrt(n()),
    P_Mean = mean(P_Pool), P_SE = sd(P_Pool) / sqrt(n()),
    M_Mean = mean(Microbe_Pool), M_SE = sd(Microbe_Pool) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  # Convert Year back to numeric for line plots
  mutate(Year_Num = as.numeric(as.character(Year)))

# Plot 1: Carbon Pool Over Time (Line Plot)
plot_c <- ggplot(summary_df, aes(x = Year_Num, y = C_Mean, group = Treatment, color = Treatment)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = C_Mean - C_SE, ymax = C_Mean + C_SE), width = 0.2) +
  labs(
    title = "Carbon Pool Dynamics Over Time",
    y = expression("Mean Carbon Pool Size ("*g~C/m^2*")"), # Example unit
    x = "Year",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_brewer(palette = "Dark2")
print(plot_c)

# Plot 2: Phosphorus Pool Over Time (Line Plot)
plot_p <- ggplot(summary_df, aes(x = Year_Num, y = P_Mean, group = Treatment, color = Treatment)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = P_Mean - P_SE, ymax = P_Mean + P_SE), width = 0.2) +
  labs(
    title = "Phosphorus Pool Dynamics Over Time",
    y = expression("Mean Phosphorus Pool Size ("*g~P/m^2*")"), # Example unit
    x = "Year",
    color = "Treatment"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_brewer(palette = "Dark2")
print(plot_p)

# --- 7. Visualization: Combined C-P Scatter Plot (Mapping Year to Size) ---
# This plot mimics the requested aesthetic, showing how C and P pools change
# relative to each other over the study period, with year mapped to size.

plot_scatter_combined <- ggplot(summary_df, aes(x = C_Mean, y = P_Mean, color = Treatment, size = Year_Num)) +
  # Add circles for each mean (size reflects time progression)
  geom_point(alpha = 0.8, shape = 21, stroke = 1.5, fill = "white") +
  # Add lines connecting the points for each treatment to show the path over time
  geom_path(aes(group = Treatment), linewidth = 0.7, alpha = 0.6, linetype = "dashed") +
  # Label the points with the Year for clarity
  geom_text_repel(aes(label = Year), color = "black", size = 3, force = 2, 
                  box.padding = 0.5, point.padding = 0.5, 
                  min.segment.length = 0, max.overlaps = Inf) +
  
  labs(
    title = "Multivariate Trajectory of Carbon vs. Phosphorus Pools",
    subtitle = "Point size indicates year progression (smaller = earlier)",
    x = expression("Mean Carbon Pool Size ("*g~C/m^2*")"),
    y = expression("Mean Phosphorus Pool Size ("*g~P/m^2*")"),
    color = "Treatment",
    size = "Year"
  ) +
  # Customize scales and theme
  scale_size_continuous(range = c(3, 8)) + # Set size range for visual effect
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(plot_scatter_combined)

