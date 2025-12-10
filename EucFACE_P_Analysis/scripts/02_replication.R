# ==============================================================================
# SCRIPT 02: REPLICATION (The "Problem")
# Author: Person 2
# ==============================================================================
library(tidyverse); library(lme4); library(lmerTest); library(patchwork)
df <- read_csv("data/master_data.csv")

# 1. THE INVENTORY (Fig 1)
# Hypothesis: Microbial P pool >> Soil P pool
pool_long <- df %>%
  pivot_longer(c(Soil_P, Micro_P), names_to="Pool", values_to="Val") %>%
  group_by(Trt, Pool) %>% summarise(Mean=mean(Val,na.rm=T), SE=sd(Val,na.rm=T)/sqrt(n()))

p1 <- ggplot(pool_long, aes(x=Pool, y=Mean, fill=Trt)) +
  geom_bar(stat="identity", position=position_dodge(0.9), color="black") +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), position=position_dodge(0.9), width=0.2) +
  labs(title="A. P Inventory (Pre-emption)", y="g P / m2") +
  scale_fill_manual(values=c("#A6CEE3", "#1F78B4")) + theme_classic()

# 2. THE STAGNATION (Fig 2)
# Hypothesis: Wood Carbon shows no response to eCO2
p2 <- ggplot(df, aes(x=Year, y=Wood_C, color=Trt)) +
  stat_summary(fun=mean, geom="line", size=1) +
  stat_summary(fun.data=mean_se, geom="errorbar", width=0.2) +
  labs(title="B. Biomass Stagnation", y="Wood C (g/m2)") +
  scale_color_manual(values=c("black", "red")) + theme_bw()

# 3. THE BOTTLENECK (Fig 3)
# Hypothesis: Supply (Mineralization) < Demand (Uptake/Loss)
p3 <- df %>% 
  pivot_longer(c(Min_Flux, Total_P_Lost), names_to="Flow", values_to="Val") %>%
  ggplot(aes(x=Flow, y=Val, fill=Trt)) + geom_boxplot() +
  labs(title="C. Flux Bottleneck", x="Process", y="g P / m2 / yr") + theme_classic()

ggsave("C:\Users\PC\OneDrive\Documents\GitHub\2025Bgroup-phosphorusCO2\EucFACE_P_Analysis\output/Fig_Replication.png", (p1 + p2) / p3, width=10, height=10)

# Statistical Proof (Look for Pr(>F) > 0.05)
print(anova(lmer(Wood_C ~ Trt * as.factor(Year) + (1|Ring), data=df)))
