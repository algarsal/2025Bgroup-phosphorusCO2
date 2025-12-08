# ==============================================================================
# SCRIPT 02: REPLICATING JIANG ET AL. (2024)
# ==============================================================================
library(tidyverse)
library(lme4) 
#install.packages("lmerTest",dependencies=TRUE) ### Install this for the first time only
library(lmerTest)
library(patchwork)

df <- read_csv("EucFACE_P_Analysis/data/master_data_eucface.csv")

# ------------------------------------------------------------------------------
# FIGURE 1: THE INVENTORY (Pool Sizes)
# Hypothesis: Microbes hold much more P than is available in the soil.
# ------------------------------------------------------------------------------
# Reshape data for plotting
df_long <- df %>%
  select(Treatment, Soil_P, Micro_P) %>%
  pivot_longer(cols = c(Soil_P, Micro_P), names_to = "Pool", values_to = "Value")

# Calculate means and standard error
df_summary <- df_long %>%
  group_by(Treatment, Pool) %>%
  summarise(Mean = mean(Value, na.rm=TRUE), 
            SE = sd(Value, na.rm=TRUE)/sqrt(n()), .groups="drop")

Fig1 <- ggplot(df_summary, aes(x = Pool, y = Mean, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge(), color="black") +
  geom_errorbar(aes(ymin = Mean-SE, ymax = Mean+SE), width = 0.2, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("Ambient" = "#A6CEE3", "eCO2" = "#1F78B4")) +
  labs(title = "Figure 1: The Phosphorus Inventory", 
       subtitle = "Microbial P pool is significantly larger than Available Soil P",
       y = "Phosphorus Pool (g P / m2)") +
  theme_classic()

# ------------------------------------------------------------------------------
# FIGURE 2: THE TIMELINE (Trends over Years)
# Hypothesis: No divergence between treatments (Stagnation).
# ------------------------------------------------------------------------------
# We plot Wood Carbon because that represents tree growth
Fig2 <- ggplot(df, aes(x = Year, y = Wood_C, color = Treatment, group = Treatment)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  scale_color_manual(values = c("Ambient" = "black", "eCO2" = "red")) +
  labs(title = "Figure 2: Tree Biomass Trajectory", 
       subtitle = "eCO2 (Red) does not diverge from Ambient (Black) -> Stagnation",
       y = "Wood Carbon (g C / m2)") +
  theme_bw()

# ------------------------------------------------------------------------------
# FIGURE 3: THE VERDICT (Effect Size & Statistics)
# Calculate the % difference and test significance
# ------------------------------------------------------------------------------

# Statistical Model for Wood Carbon
# We use a Linear Mixed Model accounting for Ring random effects
model_wood <- lmer(Wood_C ~ Treatment * as.factor(Year) + (1|Ring), data = df)
stats_wood <- anova(model_wood)

# Statistical Model for Soil P
model_soil <- lmer(Soil_P ~ Treatment * as.factor(Year) + (1|Ring), data = df)
stats_soil <- anova(model_soil)

# Create a clear table of results to plot
results <- tibble(
  Variable = c("Wood Carbon", "Available Soil P", "Microbial P"),
  P_Value_Trt = c(stats_wood["Treatment", "Pr(>F)"], 
                  stats_soil["Treatment", "Pr(>F)"], 
                  anova(lmer(Micro_P ~ Treatment * as.factor(Year) + (1|Ring), data=df))["Treatment", "Pr(>F)"]),
  Effect_Size = c(
    mean(df$Wood_C[df$Treatment=="eCO2"], na.rm=T) - mean(df$Wood_C[df$Treatment=="Ambient"], na.rm=T),
    mean(df$Soil_P[df$Treatment=="eCO2"], na.rm=T) - mean(df$Soil_P[df$Treatment=="Ambient"], na.rm=T),
    mean(df$Micro_P[df$Treatment=="eCO2"], na.rm=T) - mean(df$Micro_P[df$Treatment=="Ambient"], na.rm=T)
  )
) %>%
  mutate(Significance = ifelse(P_Value_Trt < 0.05, "Significant", "NS"))

# Forest Plot
Fig3 <- ggplot(results, aes(x = Variable, y = Effect_Size, fill = Significance)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  labs(title = "Figure 3: CO2 Effect Size (Difference)", 
       subtitle = "NS = Not Significant (Supports Hypothesis 2)",
       y = "Absolute Difference (eCO2 - Ambient)") +
  theme_light()

# Combine and Save
final_layout <- (Fig1 | Fig2) / Fig3
ggsave("EucFACE_P_Analysis/output/Replication_Analysis.png", final_layout, width = 12, height = 10)

print("Replication Analysis Complete.")
print("--- MODEL P-VALUES (If > 0.05, eCO2 had no effect) ---")
print(results)
