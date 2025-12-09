# R Script for Trieu: Phosphate Data
# Goal: Consolidate all raw Phosphate data files into a single, standardized, wide-format CSV.

# --- 1. Setup and Configuration ---

# Install and load necessary packages
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse", dependencies = TRUE)
}
library(tidyverse)

# Define file paths
RAW_DATA_DIR <- "trieu_ecosystem_analysis/data/raw/"
CLEAN_DATA_PATH <- "trieu_ecosystem_analysis/data/clean/clean_phosphate_trieu.csv"

# Mapping of ring_id to treatment (Crucial assumption - must be verified by Trieu)
RING_TREATMENT_MAP <- c(
  "1" = "Control", "2" = "Track",
  "3" = "Control", "4" = "Track",
  "5" = "Control", "6" = "Track"
)

# --- 2. Data Processing Function ---

process_file <- function(filepath) {
  filename <- basename(filepath)
  cat(paste("Processing", filename, "...\n"))
  
  # 1. Load data - assuming no header row
  # We use read_csv with col_names=FALSE to read the file without a header
  df <- read_csv(filepath, col_names = FALSE, show_col_types = FALSE)
  
  # 2. Infer column structure and transform to long format
  if (ncol(df) < 3) {
    cat(paste("Skipping", filename, ": Unexpected number of columns (", ncol(df), ")\n"))
    return(NULL)
  }
  
  # Rename the first two columns which are assumed to be Date/Year and Ring_ID
  df <- df %>% rename(date_or_year = 1, ring_id = 2)
  
  # Transform to long format (pivot_longer equivalent to Python's melt)
  if (ncol(df) > 3) {
    # Multi-column file (e.g., soil data)
    df_long <- df %>%
      pivot_longer(
        cols = starts_with("X"), # Columns X3, X4, etc.
        names_to = "col_index",
        values_to = "value"
      ) %>%
      # Create a unique variable name from the filename and the column index
      mutate(
        base_var = str_replace(filename, "\\.csv$", ""),
        variable = paste0(base_var, "_", col_index)
      ) %>%
      select(-col_index, -base_var)
  } else {
    # Simple 3-column file
    df_long <- df %>%
      rename(value = 3) %>%
      mutate(
        variable = str_replace(filename, "\\.csv$", "")
      )
  }
  
  # 3. Standardize Core Columns
  df_long <- df_long %>%
    # Convert value to numeric, coercing errors to NA
    mutate(value = as.numeric(value)) %>%
    # Extract year from date_or_year
    mutate(
      year = case_when(
        # Try to parse as date and extract year
        !is.na(as.Date(date_or_year, format = "%Y-%m-%d")) ~ as.integer(format(as.Date(date_or_year, format = "%Y-%m-%d"), "%Y")),
        # Otherwise, assume it's already the year (numeric)
        TRUE ~ as.integer(date_or_year)
      ),
      # Clean and standardize ring_id
      ring_id = as.character(as.integer(ring_id))
    ) %>%
    # Remove rows with missing core data
    drop_na(year, ring_id, value) %>%
    # Add Treatment
    mutate(treatment = RING_TREATMENT_MAP[ring_id])
  
  # 4. Add Depth Category (Placeholder - MUST be corrected by Trieu)
  df_long <- df_long %>%
    mutate(
      depth_cat = case_when(
        str_detect(filename, "soil") ~ "0-10", # Placeholder for soil data
        str_detect(filename, "fineroot|coarse_root") ~ "0-30", # Assuming roots are in the top 30cm
        str_detect(filename, "canopy|wood|leaflitter") ~ "NA", # Above-ground components
        TRUE ~ "NA" # Default for unknown
      )
    ) %>%
    # Select and reorder final columns
    select(year, ring_id, treatment, depth_cat, variable, value)
  
  return(df_long)
}

# --- 3. Main Execution ---

main <- function() {
  # Find all P-related files in the raw data directory
  all_files <- list.files(
    path = RAW_DATA_DIR,
    pattern = "p_.*\\.csv$|P_.*\\.csv$|phosphate.*\\.csv$",
    ignore.case = TRUE,
    full.names = TRUE
  )
  
  if (length(all_files) == 0) {
    cat(paste("No Phosphate-related CSV files found in", RAW_DATA_DIR, ". Please check the directory.\n"))
    return()
  }
  
  # Process all files and combine into a single long data frame
  all_data <- map(all_files, process_file)
  master_df_long <- bind_rows(all_data)
  
  if (nrow(master_df_long) == 0) {
    cat("No data was successfully processed. Exiting.\n")
    return()
  }
  
  # Pivot the table to wide format (equivalent to Python's pivot_table)
  # Aggregating by mean to handle duplicates (simplification - review for data integrity)
  final_clean_df <- master_df_long %>%
    pivot_wider(
      names_from = variable,
      values_from = value,
      values_fn = mean # Use mean to aggregate duplicate entries
    ) %>%
    # Ensure the output directory exists
    {
      dir.create(dirname(CLEAN_DATA_PATH), recursive = TRUE, showWarnings = FALSE)
      .
    }
  
  # Save the final clean data
  cat("\n--- Final Clean Data Summary ---\n")
  cat(paste("Total rows:", nrow(final_clean_df), "\n"))
  cat(paste("Total columns (variables):", ncol(final_clean_df), "\n"))
  cat("First 5 rows of the final clean data:\n")
  print(head(final_clean_df))
  
  cat(paste("\nSaving final clean data to:", CLEAN_DATA_PATH, "\n"))
  write_csv(final_clean_df, CLEAN_DATA_PATH)
  cat("Success! Trieu's clean Phosphate data is ready.\n")
}

# Execute the main function
main()
