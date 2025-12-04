# --- 3. Load and Visualize P Flux ---

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
