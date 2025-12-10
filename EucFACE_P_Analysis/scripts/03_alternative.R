# ==============================================================================
# 03: ALTERNATIVE (The "Solution/Response")
# ==============================================================================
library(tidyverse)
library(lme4)
library(lmerTest)
library(patchwork)

df <- read_csv("data/master_data.csv") %>% filter(!is.na(GPP))

# 1. FORAGING STRATEGY (Root:Shoot Ratio)
# If eCO2 > Ambient, trees are shifting allocation to roots
p_alloc <- ggplot(df, aes(x=Year, y=Root_Shoot_Ratio, color=Trt)) +
  stat_summary(fun=mean, geom="line", size=1) +
  stat_summary(fun.data=mean_se, geom="errorbar", width=0.1) +
  labs(title="A. Allocation Shift (Foraging)", y="Fine Root : Wood Ratio") +
  scale_color_manual(values=c("black", "red")) + theme_classic()

# 2. EFFICIENCY (PUE)
# Functional response: Carbon Gain per Soil P
p_pue <- ggplot(df, aes(x=Soil_P, y=GPP, color=Trt)) +
  geom_point(alpha=0.6) +
  geom_smooth(method="lm", se=TRUE, fill="gray90") +
  labs(title="B. Functional Efficiency", x="Available Soil P", y="Carbon Gain (GPP)") +
  scale_color_manual(values=c("black", "red")) + theme_classic()

ggsave("output/Alternative_Fig.png", p_alloc / p_pue, width=8, height=8)

# Stats
print("--- ALLOCATION STATS ---")
print(anova(lmer(Root_Shoot_Ratio ~ Trt * as.factor(Year) + (1|Ring), data=df)))

print("--- PUE INTERACTION STATS ---")
print(summary(lmer(GPP ~ Soil_P * Trt + (1|Ring), data=df)))
