# ==============================================================================
# SCRIPT 02: REPLICATION (The "Problem")
# ==============================================================================
library(tidyverse)
library(lme4)
library(lmerTest)
library(patchwork)

df <- read_csv("data/master_data.csv")

# 1. THE INVENTORY (Fig 2 Replication)
# Show that Microbes > Soil P
df_pool <- df %>%
  pivot_longer(c(Soil_P, Micro_P), names_to="Pool", values_to="Val") %>%
  group_by(Trt, Pool) %>%
  summarise(Mean=mean(Val, na.rm=T), SE=sd(Val, na.rm=T)/sqrt(n()), .groups="drop")

p1 <- ggplot(df_pool, aes(x=Pool, y=Mean, fill=Trt)) +
  geom_bar(stat="identity", position=position_dodge(0.9), color="black") +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), position=position_dodge(0.9), width=0.2) +
  labs(title="A. The P Bottleneck", y="Phosphorus Pool (g/m2)") +
  scale_fill_manual(values=c("#A6CEE3", "#1F78B4")) + theme_classic()

# 2. THE STAGNATION (Fig 4 Replication)
# Show Wood C is not responding
p2 <- ggplot(df, aes(x=Year, y=Wood_C, color=Trt)) +
  stat_summary(fun=mean, geom="line", size=1) +
  stat_summary(fun=mean, geom="point", size=3) +
  stat_summary(fun.data=mean_se, geom="errorbar", width=0.1) +
  labs(title="B. Biomass Stagnation", y="Wood Carbon (g/m2)") +
  scale_color_manual(values=c("black", "red")) + theme_bw()

# Combine
ggsave("output/Replication_Fig.png", p1 / p2, width=8, height=8)

# Stats
print("--- REPLICATION STATS ---")
print(anova(lmer(Wood_C ~ Trt * as.factor(Year) + (1|Ring), data=df)))
