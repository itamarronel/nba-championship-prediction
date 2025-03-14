###############################################################################
# NBA Championship Prediction Project
# Script 2: Data Preprocessing
# 
# Purpose: Process and prepare NBA data for analysis 
# Author: Itamar Ronel
###############################################################################

###############################################################################
# 1. SETUP AND CONFIGURATION
###############################################################################
# Clear environment
rm(list = ls())
gc()

# Configure script behavior
options(stringsAsFactors = FALSE)

# Load required packages
required_packages <- c("dplyr", "readr", "stringr", "tidyr")
for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Define paths for data storage
base_path <- "/Users/itamar/Documents/GitHub/nba-championship-prediction/data"
raw_tables_dir <- file.path(base_path, "raw_tables")
playoffs_path <- "/Users/itamar/Documents/GitHub/nba-championship-prediction/playoffs"
champions_file <- file.path(base_path, "champions.csv")

# Ensure output directories exist
for(dir in c(base_path, playoffs_path)) {
  if(!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    message("Creating directory: ", dir)
  }
}

###############################################################################
# 2. DATA EXTRACTION FUNCTIONS
###############################################################################

#' Process NBA Advanced Stats from CSV files
#'
#' @param base_path Base directory containing the data files
#' @param start_year Starting year to process (default: 1974)
#' @param end_year Ending year to process (default: 2024)
#' @param stats_to_collect List of stats to collect with column indices
#' @param output_filename Name of the output file
#' @param convert_rds Whether to convert RDS files to CSV (default: TRUE)
#' @return Dataframe containing the processed stats
process_nba_stats <- function(
  base_path = "/Users/itamar/Documents/GitHub/nba-championship-prediction/data",
  start_year = 1974,
  end_year = 2024,
  stats_to_collect = list(
    "W" = 4,        # Wins
    "L" = 5,        # Losses
    "Age" = 3,     
    "ORtg" = 11,    # Offensive Rating
    "DRtg" = 12,    # Defensive Rating
    "NRtg" = 13,    # Net Rating
    # Four Factors - Offense:
    "eFG" = 19,
    "TOV_Pct" = 20,
    "ORB_Pct" = 21,
    "FTr" = 22,
    # Four Factors - Defense:
    "Opp_eFG" = 24,
    "Opp_TOV" = 25,
    "DRB_Pct" = 26,
    "Opp_FTr" = 27
  ),
  output_filename = "full_advanced_stats_combined.csv",
  convert_rds = TRUE
) {
  # Define paths
  raw_tables_dir <- file.path(base_path, "raw_tables")
  champions_file <- file.path(base_path, "champions.csv")
  output_file <- file.path(base_path, output_filename)

  # Check if champions file exists and load it
  if(!file.exists(champions_file)) {
    warning("Champions file not found: ", champions_file, 
            "\nWill continue processing without champion information.")
    champions_df <- NULL
  } else {
    champions_df <- read.csv(champions_file)
  }
  
  # Function to convert RDS files to CSV if needed
  convert_rds_to_csv <- function(year) {
    season_dir <- file.path(raw_tables_dir, paste0("season_", year))
    
    if(!dir.exists(season_dir)) {
      return(FALSE)
    }
    
    rds_files <- list.files(season_dir, pattern = "\\.rds$", full.names = TRUE)
    
    for(rds_file in rds_files) {
      csv_file <- gsub("\\.rds$", ".csv", rds_file)
      
      if(!file.exists(csv_file)) {
        tryCatch({
          df <- readRDS(rds_file)
          write.csv(df, csv_file, row.names = FALSE)
          message("  Created ", basename(csv_file))
        }, error = function(e) {
          message("  Error converting ", basename(rds_file), ": ", e$message)
        })
      }
    }
    
    return(TRUE)
  }
  
  # Function to process a single advanced stats CSV file
  process_advanced_csv <- function(csv_file, season_year, stats_columns) {
    # Read CSV with the first row as header
    df <- read.csv(csv_file, header = FALSE, stringsAsFactors = FALSE)
    
    # Check if we have enough rows
    if(nrow(df) < 2) {
      message("CSV file has insufficient rows: ", csv_file)
      return(NULL)
    }
    
    # Use first row as column names
    colnames(df) <- as.character(unlist(df[1,]))
    df <- df[-1, ]  # Remove the header row
    
    # Find the Team column (usually the second column)
    if("Team" %in% colnames(df)) {
      team_col <- "Team"
    } else {
      # Second column is typically Team
      colnames(df)[2] <- "Team"
      team_col <- "Team"
    }
    
    # Clean team names
    df$Team <- gsub("\\*$", "", df$Team)  # Remove asterisks
    df$Team <- gsub("\\([^)]*\\)", "", df$Team)  # Remove parentheses
    df$Team <- str_trim(df$Team)  # Trim whitespace
    
    # Create result dataframe
    result <- data.frame(
      Team = df$Team,
      SeasonYear = season_year,
      Team_Season = paste(df$Team, season_year)
    )
    
    # Add each statistic from the user-defined list
    for(stat_name in names(stats_columns)) {
      col_index <- stats_columns[[stat_name]]
      if(col_index <= ncol(df)) {
        result[[stat_name]] <- suppressWarnings(as.numeric(df[[col_index]]))
      }
    }
    
    # Calculate Win Percentage if W and L are included
    if(all(c("W", "L") %in% names(result))) {
      result$WinPct <- result$W / (result$W + result$L)
    }
    
    # Return processed data
    return(result)
  }
  
  # Function to process all seasons
  process_all_seasons <- function(start_yr, end_yr, stats_cols) {
    all_results <- list()
    failed_years <- c()
    
    for(year in start_yr:end_yr) {
      message("Processing season ", year)
      
      # Find CSV files for this season
      season_dir <- file.path(raw_tables_dir, paste0("season_", year))
      if(!dir.exists(season_dir)) {
        message("  Directory not found for season ", year)
        failed_years <- c(failed_years, year)
        next
      }
      
      # Convert RDS to CSV if needed
      if(convert_rds) {
        convert_rds_to_csv(year)
      }
      
      # Check for advanced-team.csv
      csv_file <- file.path(season_dir, "advanced-team.csv")
      
      # Process CSV if it exists
      if(file.exists(csv_file)) {
        result <- tryCatch({
          process_advanced_csv(csv_file, year, stats_cols)
        }, error = function(e) {
          message("  Error processing CSV: ", e$message)
          return(NULL)
        })
        
        if(!is.null(result) && nrow(result) > 0) {
          all_results[[as.character(year)]] <- result
          message("  Successfully processed season ", year, " (", nrow(result), " teams)")
        } else {
          message("  Failed to extract data for season ", year)
          failed_years <- c(failed_years, year)
        }
      } else {
        # If no CSV found, look for any advanced stats file
        advanced_files <- list.files(
          season_dir, 
          pattern = "advanced|team_stats|misc", 
          full.names = TRUE
        )
        
        if(length(advanced_files) > 0) {
          message("  No advanced-team.csv found, checking alternatives")
          for(alt_file in advanced_files) {
            if(grepl("\\.csv$", alt_file)) {
              result <- tryCatch({
                process_advanced_csv(alt_file, year, stats_cols)
              }, error = function(e) {
                message("  Error processing alternative file: ", e$message)
                return(NULL)
              })
              
              if(!is.null(result) && nrow(result) > 0) {
                all_results[[as.character(year)]] <- result
                message("  Successfully processed season ", year, " with ", basename(alt_file))
                break
              }
            }
          }
        } else {
          message("  No suitable files found for season ", year)
          failed_years <- c(failed_years, year)
        }
      }
    }
    
    return(list(
      data = all_results,
      failed = failed_years
    ))
  }
  
  # Process all seasons
  message("\nProcessing all seasons from ", start_year, " to ", end_year)
  results <- process_all_seasons(start_year, end_year, stats_to_collect)
  
  # Extract data and failed seasons
  all_data <- results$data
  failed_years <- results$failed
  
  # Check if we got any data
  if(length(all_data) == 0) {
    stop("No data could be extracted.")
  }
  
  # Combine all seasons into one dataframe
  combined_df <- bind_rows(all_data)
  
  # Add champion information if available
  if(!is.null(champions_df)) {
    # Make sure ChampionTeam column exists in champions_df
    if(!"ChampionTeam" %in% colnames(champions_df)) {
      warning("ChampionTeam column not found in champions.csv file. Skipping champion information.")
    } else {
      tryCatch({
        # Fix season year mapping issue
        # Basketball Reference uses the end year of the season (e.g., 2024 for 2023-24 season)
        # while champion data may use the start year (e.g., 2023 for 2023-24 season)
        message("Adjusting champion season year mapping...")
        
        # Create a copy of champions_df with adjusted SeasonYear
        champions_df_adjusted <- champions_df %>%
          # Add 1 to SeasonYear to match Basketball Reference's convention
          mutate(SeasonYear = SeasonYear + 1)
        
        # Join with champion information using adjusted years
        combined_df <- left_join(combined_df, champions_df_adjusted, by = "SeasonYear")
        
        # Create temporary columns for team name cleaning
        combined_df$Team_clean <- str_trim(gsub("\\*$", "", combined_df$Team))
        combined_df$Champ_clean <- str_trim(gsub("\\*$", "", combined_df$ChampionTeam))
        
        # Create Champion factor (1 for champion, 0 for others)
        combined_df$Champion <- ifelse(
          !is.na(combined_df$Team_clean) & !is.na(combined_df$Champ_clean) & 
          str_to_lower(combined_df$Team_clean) == str_to_lower(combined_df$Champ_clean), 
          1, 0
        )
        
        # Convert to factor for logistic regression
        combined_df$Champion <- factor(combined_df$Champion, levels = c(0, 1))
        
        # Remove temporary columns if they exist
        if("Team_clean" %in% colnames(combined_df)) {
          combined_df$Team_clean <- NULL
        }
        if("Champ_clean" %in% colnames(combined_df)) {
          combined_df$Champ_clean <- NULL
        }
        
        # Verify champion information
        champion_count <- sum(combined_df$Champion == 1, na.rm = TRUE)
        message("Champion information successfully added. Found ", champion_count, " champions.")
        
        # Additional verification
        if(champion_count > 0) {
          champions_by_year <- combined_df %>%
            filter(Champion == 1) %>%
            select(Team, SeasonYear) %>%
            arrange(SeasonYear)
          
          message("Most recent champions identified:")
          recent_champions <- tail(champions_by_year, 5)
          for(i in 1:nrow(recent_champions)) {
            message("  ", recent_champions$SeasonYear[i], ": ", recent_champions$Team[i])
          }
        }
      }, error = function(e) {
        warning("Error adding champion information: ", e$message)
      })
    }
  }
  
  # Save the result
  write.csv(combined_df, output_file, row.names = FALSE)
  message("\nCombined data saved to: ", output_file)
  
  # Return the final dataframe
  return(combined_df)
}

#' Function to process single current season
#' 
#' @param season_year The NBA season year to process (e.g., 2025)
#' @param base_path Base directory for the data
#' @param output_filename Filename for the processed data
#' @return Dataframe with the processed stats
process_nba_season <- function(
  season_year = 2025,
  base_path = "/Users/itamar/Documents/GitHub/nba-championship-prediction/data",
  output_filename = "current_season.csv"
) {
  message("Processing NBA season ", season_year)
  
  # Define the stats we want to collect based on the actual table structure
  stats_to_collect = list(
    "W" = 3,        # Wins
    "L" = 4,        # Losses
    "Age" = 2,      # Age
    "ORtg" = 10,    # Offensive Rating
    "DRtg" = 11,    # Defensive Rating
    "NRtg" = 12,    # Net Rating
    # Four Factors - Offense:
    "eFG" = 18,     # Offensive eFG%
    "TOV_Pct" = 19, # Offensive TOV%
    "ORB_Pct" = 20, # Offensive ORB%
    "FTr" = 21,     # Offensive FT rate
    # Four Factors - Defense:
    "Opp_eFG" = 23, # Defensive eFG%
    "Opp_TOV" = 24, # Defensive TOV%
    "DRB_Pct" = 25, # Defensive DRB%
    "Opp_FTr" = 26  # Defensive FT rate
  )
  
  # Construct path to the HTML file
  html_file <- file.path(raw_tables_dir, "..","raw_pages", paste0("nba_", season_year, ".html"))
  
  if(!file.exists(html_file)) {
    stop("HTML file not found: ", html_file)
  }
  
  # Read HTML content
  html_content <- readLines(html_file, warn = FALSE)
  html_content <- paste(html_content, collapse = "\n")
  page <- read_html(html_content)
  
  # Extract the advanced stats table
  message("Extracting advanced team stats table...")
  advanced_table <- page %>%
    html_element("#advanced-team") %>%
    html_table(fill = TRUE)
  
  if(is.null(advanced_table) || nrow(advanced_table) == 0) {
    stop("Could not find the advanced stats table. The website structure might have changed.")
  }
  
  # Display table dimensions
  message("Table dimensions: ", nrow(advanced_table), " x ", ncol(advanced_table))
  
  # Assign simple column names based on position (0-based indexing)
  names(advanced_table) <- paste0("Col", 0:(ncol(advanced_table)-1))
  
  # Remove header rows - look in the Rk column (Col0)
  # The header rows will have non-numeric values in the Rk column
  advanced_table <- advanced_table %>%
    filter(!is.na(suppressWarnings(as.numeric(Col0))))
  
  # Clean team names (Col1 is the Team column)
  advanced_table$Col1 <- gsub("\\*$", "", advanced_table$Col1)  # Remove asterisks
  advanced_table$Col1 <- gsub("\\([^)]*\\)", "", advanced_table$Col1)  # Remove parentheses
  advanced_table$Col1 <- str_trim(advanced_table$Col1)  # Trim whitespace
  
  # Create result dataframe
  result_df <- data.frame(
    Team = advanced_table$Col1,  # Team is in Col1, not Col0
    Season = season_year,
    Team_Season = paste(advanced_table$Col1, season_year, sep = "_")
  )
  
  # Add each requested stat to the dataframe
  for(stat_name in names(stats_to_collect)) {
    col_index <- stats_to_collect[[stat_name]]
    col_name <- paste0("Col", col_index)
    
    if(col_index < ncol(advanced_table)) {
      # Try to add column, handle errors gracefully
      tryCatch({
        col_data <- advanced_table[[col_name]]
        if(!is.null(col_data)) {
          # Convert data to numeric, handling percentages
          numeric_data <- suppressWarnings(as.numeric(gsub("%", "", col_data)))
          if(all(is.na(numeric_data))) {
            # If all converted to NA, try another approach
            numeric_data <- suppressWarnings(as.numeric(col_data))
          }
          
          result_df[[stat_name]] <- numeric_data
          message(paste0("Successfully extracted ", stat_name, " (column ", col_index, ")"))
        } else {
          message(paste0("Column ", col_index, " for ", stat_name, " is NULL. Setting to NA."))
          result_df[[stat_name]] <- NA
        }
      }, error = function(e) {
        message(paste0("Error extracting column ", col_index, " for ", stat_name, ": ", e$message))
        result_df[[stat_name]] <- NA
      })
    } else {
      message(paste0("Column index ", col_index, " for ", stat_name, " is out of bounds. Setting to NA."))
      result_df[[stat_name]] <- NA
    }
  }
  
  # Calculate win percentage
  if(all(c("W", "L") %in% names(result_df))) {
    result_df <- result_df %>%
      mutate(WinPct = as.numeric(W) / (as.numeric(W) + as.numeric(L)))
  }
  
  # Save the result
  output_file <- file.path(base_path, output_filename)
  
  write.csv(result_df, output_file, row.names = FALSE)
  message("Data saved to: ", output_file)
  
  # Return the result
  return(result_df)
}

###############################################################################
# 3. DATA CLEANING AND FILTERING FUNCTIONS
###############################################################################

#' Clean NBA data by removing non-team rows and handling missing values
#'
#' @param input_file Path to the input CSV file
#' @param output_prefix Prefix for the output files
#' @return Cleaned dataframe
clean_nba_data <- function(input_file, output_prefix) {
  # Load the data
  message("Loading data from ", basename(input_file))
  nba_data <- read.csv(input_file)
  
  # Display basic info
  message("Original dataset dimensions: ", nrow(nba_data), " x ", ncol(nba_data))
  
  # Remove rows containing "Team" or "League"
  nba_filtered <- nba_data %>%
    filter(!grepl("Team|League|Average|Total", Team_Season, ignore.case = TRUE))
  
  # Check for missing values
  missing_count <- colSums(is.na(nba_filtered))
  message("\nMissing values by column:")
  print(missing_count)
  
  # Calculate percentage of missing values
  message("\nPercentage of missing values by column:")
  missing_percent <- round(100 * missing_count / nrow(nba_filtered), 2)
  print(missing_percent)
  
  # Count how many complete rows we have
  complete_rows <- nba_filtered %>%
    filter(complete.cases(.)) %>%
    nrow()
  
  message("\nComplete rows: ", complete_rows, " out of ", nrow(nba_filtered), 
          " (", round(100 * complete_rows / nrow(nba_filtered), 2), "%)")
  
  # Keep only complete cases
  nba_complete <- nba_filtered %>%
    filter(complete.cases(.))
  
  message("\nFiltered dataset with complete cases: ", nrow(nba_complete), " x ", ncol(nba_complete))
  
  # Save complete cases
  output_file <- paste0(output_prefix, "_complete.csv")
  write.csv(nba_complete, output_file, row.names = FALSE)
  
  message("\nData cleaning complete.")
  message("Filtered dataset saved to: ", output_file)
  
  return(nba_complete)
}

###############################################################################
# 4. EXECUTE DATA PROCESSING
###############################################################################

# 4.1 Process the regular season data
message("\n=== PROCESSING REGULAR SEASON DATA ===")
regular_season_data <- process_nba_stats(
  base_path = base_path,
  start_year = 1974,
  end_year = 2024,
  output_filename = "full_advanced_stats_combined.csv"
)

# 4.2 Process playoff data
message("\n=== PROCESSING PLAYOFF DATA ===")
playoff_data <- process_nba_stats(
  base_path = playoffs_path,
  start_year = 1974,
  end_year = 2024,
  stats_to_collect = list(
    "W" = 4,        # Wins
    "L" = 5,        # Losses
    "Age" = 3,     
    "ORtg" = 9,     # Offensive Rating
    "DRtg" = 10,    # Defensive Rating
    "NRtg" = 11    # Net Rating
  ),
  output_filename = "playoffs_advanced_stats_combined.csv"
)

# 4.3 Process current season data
message("\n=== PROCESSING CURRENT SEASON DATA ===")
current_season_data <- process_nba_season(
  season_year = 2025,
  base_path = base_path,
  output_filename = "current_season.csv"
)

# 4.4 Clean the regular season data
message("\n=== CLEANING REGULAR SEASON DATA ===")
regular_season_clean <- clean_nba_data(
  input_file = file.path(base_path, "full_advanced_stats_combined.csv"),
  output_prefix = file.path(base_path, "full_advanced_stats_filtered")
)

# 4.5 Clean the playoff data
message("\n=== CLEANING PLAYOFF DATA ===")
playoff_clean <- clean_nba_data(
  input_file = file.path(playoffs_path, "playoffs_advanced_stats_combined.csv"),
  output_prefix = file.path(playoffs_path, "playoffs_advanced_stats_filtered")
)

###############################################################################
# 5. SUMMARY
###############################################################################

# Display final dataset summaries
message("\n=== FINAL DATASETS SUMMARY ===")
message("Regular season data: ", nrow(regular_season_clean), " rows, ", ncol(regular_season_clean), " columns")
message("Playoff data: ", nrow(playoff_clean), " rows, ", ncol(playoff_clean), " columns")
message("Current season data: ", nrow(current_season_data), " rows, ", ncol(current_season_data), " columns")

message("\nData preprocessing complete!")
