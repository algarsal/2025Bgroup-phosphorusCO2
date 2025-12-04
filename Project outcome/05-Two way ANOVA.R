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
