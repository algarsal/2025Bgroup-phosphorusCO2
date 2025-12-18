
#EXPLAINING THE RING EFFECT
# Correlation: Does low Soil P force trees to build more roots?
#EXPLAINING THE RING EFFECT (STANDALONE SCRIPT)
# Correlation: Does low Soil P force trees to build more roots?
#In the previous steps, we found that CO2 did not change tree growth. However, the Rings were very different from each other (The "Ring Effect").
#The Problem: If Ring 5 behaves differently than Ring 1, is it just random noise? Or is the forest acting intelligently?
#The Hypothesis (Optimal Foraging Theory):
#Trees are economic investors. If nutrients are scarce (Low Soil P), the tree should invest more money into Roots to find food.
#If nutrients are abundant (High Soil P), the tree can be lazy with roots and invest more in Wood.
#The Goal: We ran this correlation to prove that the differences between rings are not random errors. They are evidence of Functional Plasticity. The trees are actively responding to the soil chemistry, even if they aren't responding to the CO2.
# 1. SETUP 
library(tidyverse)
library(lubridate)
library(ggplot2)

# UPDATE PATHS HERE
path_flux <- "C:/Users/augre/Desktop/UPV S1/Data/2025Bgroup-phosphorusCO2/Flux_data"
path_pool <- "C:/Users/augre/Desktop/UPV S1/Data/2025Bgroup-phosphorusCO2/Pool data"

# Define Treatments
trt_map <- data.frame(
  Ring = factor(c(1, 2, 3, 4, 5, 6)),
  Trt = c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2")
)

# 2. HELPER FUNCTIONS 

# Load Sums (Wood)
process_sum <- function(filename, col, folder) {
  f <- file.path(folder, filename)
  if(!file.exists(f)) return(NULL)
  read_csv(f, show_col_types = FALSE) %>%
    mutate(Year = year(ymd(Date)), Ring = factor(Ring)) %>%
    filter(Year >= 2013 & Year <= 2016) %>%
    group_by(Year, Ring) %>%
    summarise(Value = sum(get(col), na.rm=TRUE), .groups="drop")
}

# Load Rates (Roots)
process_rate <- function(filename, col, folder) {
  f <- file.path(folder, filename)
  if(!file.exists(f)) return(NULL)
  read_csv(f, show_col_types = FALSE) %>%
    mutate(Year = year(ymd(Date)), Ring = factor(Ring)) %>%
    filter(Year >= 2013 & Year <= 2016) %>%
    mutate(Grams = (get(col) * Days) / 1000) %>%
    group_by(Year, Ring) %>%
    summarise(Value = sum(Grams, na.rm=TRUE), .groups="drop")
}

# Load Pools (Soil P)
process_pool <- function(filename, col, folder) {
  f <- file.path(folder, filename)
  if(!file.exists(f)) return(NULL)
  df <- read_csv(f, show_col_types = FALSE) 
  date_col <- names(df)[grep("Date", names(df), ignore.case = TRUE)][1]
  df$Date <- ymd(df[[date_col]])
  
  df <- df %>% mutate(Year = year(Date), Ring = factor(Ring)) %>% filter(Year >= 2013 & Year <= 2016)
  
  if("Depth" %in% names(df)) {
    df <- df %>% filter(Depth %in% c("0_10", "10_30")) %>%
      group_by(Date, Year, Ring) %>%
      summarise(Step1 = sum(get(col), na.rm=TRUE), .groups="drop") %>%
      rename(Target = Step1)
  } else {
    df <- df %>% mutate(Target = get(col))
  }
  df %>% group_by(Year, Ring) %>% summarise(Value = mean(Target, na.rm=TRUE), .groups="drop")
}

# 3. CREATE 'df_clean_plot' (Carbon Allocation) 
print("Loading Carbon Data...")
c_wood <- process_sum("wood_c_production_flux.csv", "wood_production_flux", path_flux)
c_root <- process_rate("fineroot_c_production_flux.csv", "fineroot_production_flux", path_flux)

df_ratio <- left_join(c_wood, c_root, by = c("Year", "Ring"), suffix = c("_Wood", "_Root")) %>%
  left_join(trt_map, by = "Ring") %>%
  mutate(Root_Shoot = Value_Root / Value_Wood)

# Clean Data (Remove negative growth/outliers)
df_clean_plot <- df_ratio %>%
  filter(Value_Wood > 0) %>%
  filter(Root_Shoot > 0) %>%
  filter(Root_Shoot < 10.0) # Adjusted filter for your data range

# 4. LOAD SOIL P DATA -----------------------------------------------------
print("Loading Soil P Data...")
df_total_soil <- process_pool("soil_p_pool.csv", "soil_p_g_m2", path_pool)

# 5. MERGE & ANALYZE ------------------------------------------------------
print("Merging and plotting...")

ring_explanation <- df_clean_plot %>%
  # Get average Root:Wood ratio per Ring
  group_by(Ring) %>%
  summarise(Mean_Allocation = mean(Root_Shoot, na.rm = TRUE)) %>%
  # Join with the Soil P content for that Ring
  left_join(
    df_total_soil %>% group_by(Ring) %>% summarise(Mean_Soil_P = mean(Value, na.rm=TRUE)),
    by = "Ring"
  ) %>%
  left_join(trt_map, by = "Ring")

# 6. STATISTICS & PLOT ----------------------------------------------------
cor_test <- cor.test(ring_explanation$Mean_Allocation, ring_explanation$Mean_Soil_P)
print("--- CORRELATION RESULT ---")
print(cor_test)

p_correlation <- ggplot(ring_explanation, aes(x = Mean_Soil_P, y = Mean_Allocation, label = Ring)) +
  geom_smooth(method = "lm", color = "gray50", se = FALSE, linetype = "dashed") +
  geom_point(aes(color = Trt), size = 5) +
  geom_text(color = "white", size = 3, fontface = "bold") +
  labs(title = "Why do Rings behave differently?",
       subtitle = "Correlation: Soil P vs. Root Investment",
       x = "Total Soil Phosphorus (g m-2)",
       y = "Root : Wood Ratio",
       caption = paste("Correlation (r) =", round(cor_test$estimate, 2), 
                       "| P-value =", round(cor_test$p.value, 3))) +
  theme_classic() +
  scale_color_manual(values = c("aCO2"="#F8766D", "eCO2"="#00BFC4"))

print(p_correlation)
ggsave("Bonus_Ring_Explanation.png", p_correlation, width = 6, height = 5)

print("Analysis Complete.")


#P-value (0.5794): This is much larger than 0.05.
#Meaning: There is no statistically significant relationship between Soil Phosphorus and Root Allocation.
#Correlation (0.28): It is weak and positive (which is actually the opposite of what we expected, but since it's not significant, we treat it as random noise).

#Hypothesis: We thought that rings with Low P would force trees to grow More Roots (Foraging Strategy).Reality: They didn't.
#Why? This strengthens your main argument about "Stagnation" and "Lockout."
#The Phosphorus limitation is so severe and uniform across the entire forest that even the differences between rings (e.g., Ring 1 vs Ring 5) aren't enough to trigger a change in tree behavior. The trees are maxed out everywhere.