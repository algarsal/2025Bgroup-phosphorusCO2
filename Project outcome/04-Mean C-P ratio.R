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
