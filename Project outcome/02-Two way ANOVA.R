# --- 2. Simple Statistical Test: Two-way ANOVA ---

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
