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
