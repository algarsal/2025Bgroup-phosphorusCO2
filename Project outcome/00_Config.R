# ==============================================================================
# 00_Config.R - SHARED SETTINGS AND FUNCTIONS
# ==============================================================================

library(tidyverse)
library(lubridate)
library(lme4)

library(lmerTest)
library(patchwork)
library(ggplot2)

# --- 1. DEFINE PATHS (Edit these ONCE here) ---
path_flux <- "C:/Users/augre/Desktop/UPV S1/Data/2025Bgroup-phosphorusCO2/Flux_data"
path_pool <- "C:/Users/augre/Desktop/UPV S1/Data/2025Bgroup-phosphorusCO2/Pool data"

# --- 2. DEFINE TREATMENTS ---
trt_map <- data.frame(
  Ring = factor(c(1, 2, 3, 4, 5, 6)),
  Trt = c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2")
)

# --- 3. HELPER FUNCTIONS ---

# Function for Rate Files (mg/day -> g/year)
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

# Function for Total Files (Summing existing grams)
process_sum <- function(filename, col, folder) {
  f <- file.path(folder, filename)
  if(!file.exists(f)) return(NULL)
  read_csv(f, show_col_types = FALSE) %>%
    mutate(Year = year(ymd(Date)), Ring = factor(Ring)) %>%
    filter(Year >= 2013 & Year <= 2016) %>%
    group_by(Year, Ring) %>%
    summarise(Value = sum(get(col), na.rm=TRUE), .groups="drop")
}

# Function for Pools (Average Standing Stock)
process_pool <- function(filename, col, folder) {
  f <- file.path(folder, filename)
  if(!file.exists(f)) return(NULL)
  df <- read_csv(f, show_col_types = FALSE) %>%
    mutate(Date = ymd(Date), Year = year(Date), Ring = factor(Ring)) %>%
    filter(Year >= 2013 & Year <= 2016)
  
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

print(">>> Config Loaded: Paths and Functions ready.")