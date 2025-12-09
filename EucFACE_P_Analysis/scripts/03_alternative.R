# ==============================================================================
# SCRIPT 03: ALTERNATIVE ANALYSIS (PHOSPHORUS USE EFFICIENCY)
# Hypothesis: While Biomass is stagnant, Physiology is adapting.
# ==============================================================================
library(tidyverse)
library(lme4)
#install.packages("lmerTest",dependencies=TRUE) ### Install this for the first time only if error occured
library(lmerTest)

df <- read_csv("EucFACE_P_Analysis/data/master_data_eucface.csv")

# 1. Calculate PUE (Phosphorus Use Efficiency)
# PUE = How much Carbon (GPP) produced per unit of Soil P available
df <- df %>%
  mutate(PUE_Soil = GPP / Soil_P) %>%
  filter(!is.na(PUE_Soil))

# 2. STATISTICAL TEST
# Does eCO2 increase Efficiency?
model_pue <- lmer(PUE_Soil ~ Treatment * as.factor(Year) + (1|Ring), data = df)
cat("--- PUE STATISTICAL RESULTS ---\n")
print(anova(model_pue))

# 3. FIGURE 4: FUNCTIONAL PLASTICITY
# We want to show the relationship: Soil P (X) vs GPP (Y)
# If eCO2 has a higher intercept/slope, they are more efficient.

Fig4 <- ggplot(df, aes(x = Soil_P, y = GPP, color = Treatment)) +
  geom_point(size = 3, alpha = 0.6) +
  # Add regression lines
  geom_smooth(method = "lm", se = TRUE, fill = "gray90") +
  scale_color_manual(values = c("Ambient" = "black", "eCO2" = "red")) +
  labs(
    title = "Figure 4: Alternative Model - Functional Plasticity",
    subtitle = "Trees under eCO2 (Red) extract more Carbon at low Soil P levels",
    x = "Available Soil Phosphorus (g / m2)",
    y = "Carbon Uptake (GPP)"
  ) +
  theme_classic() +
  theme(legend.position = "top")

ggsave("EucFACE_P_Analysis/output/Alternative_Analysis.png", Fig4, width = 8, height = 6)
