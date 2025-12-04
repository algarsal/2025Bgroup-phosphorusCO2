

# Set the working directory to the folder containing your files

setwd("C:/Users/augre/Desktop/UPV S1/Data/2025Bgroup-phosphorusCO2/New folder")
getwd()
list.files()
# --- Install and Load Packages ---
# install.packages("tidyverse")
library(tidyverse)

# ----------------------------------------------------------------------
# PART A: Microbial C:P Ratio Analysis
# ----------------------------------------------------------------------

# --- 0. Data Loading and Preparation ---

# Load the essential microbial C and P pool data
cmic <- read_csv("microbial_c_pool.csv")
pmic <- read_csv("microbial_p_pool.csv")

# Merge the data and calculate the Microbial C:P Ratio
microbial_data <- cmic %>%
  # Select only the columns needed from C data
  select(Date, Ring, Depth, Cmic_g_m2) %>%
  # Merge with P data by Date, Ring, and Depth
  full_join(pmic %>% select(Date, Ring, Depth, Pmic_g_m2 = microbial_p_g_m2),
            by = c("Date", "Ring", "Depth")) %>%
  mutate(
    # Convert 'Date' column to proper date format
    Date = as.Date(Date, format = "%Y-%m-%d"),
    # Calculate the Microbial C:P ratio (key metric)
    C_P_ratio = Cmic_g_m2 / Pmic_g_m2,
    # Convert 'Ring' and 'Depth' to categorical factors
    Ring = as.factor(Ring),
    Depth = as.factor(Depth)
  ) %>%
  # Remove rows missing the calculated ratio
  drop_na(C_P_ratio)

print("--- Data Structure Check: Microbial C:P Ratio ---")
print(head(microbial_data))






