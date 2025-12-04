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
