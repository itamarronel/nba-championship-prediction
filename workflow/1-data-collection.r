###############################################################################
# NBA Championship Prediction Project
# Script 1: Data Collection
# 
# Purpose: Download NBA data from Basketball Reference for multiple seasons
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
required_packages <- c("rvest", "dplyr", "stringr", "httr", "readr")
for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Define paths for data storage
base_path <- "/Users/itamar/Documents/GitHub/nba-championship-prediction/data"
playoffs_path <- "/Users/itamar/Documents/GitHub/nba-championship-prediction/playoffs"
raw_data_dir <- file.path(base_path, "raw_pages")
playoffs_raw_dir <- file.path(playoffs_path, "raw_pages")
raw_tables_dir <- file.path(base_path, "raw_tables")
playoffs_tables_dir <- file.path(playoffs_path, "raw_tables")
champions_file <- file.path(base_path, "champions.csv")

# Create directories if they don't exist
for(dir in c(base_path, playoffs_path, raw_data_dir, playoffs_raw_dir, raw_tables_dir, playoffs_tables_dir)) {
  if(!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    message("Creating directory: ", dir)
  }
}

###############################################################################
# 2. HELPER FUNCTIONS
###############################################################################

# Function to safely download web content with retry logic
safe_download <- function(url, max_retries = 3, wait_time = 2) {
  user_agents <- c(
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/16.0 Safari/605.1.15",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 Safari/537.36"
  )
  
  for(attempt in 1:max_retries) {
    # Use a random User-Agent
    chosen_ua <- sample(user_agents, 1)
    
    response <- tryCatch({
      GET(url, 
          add_headers(
            `User-Agent` = chosen_ua,
            `Accept` = "text/html,application/xhtml+xml,application/xml",
            `Accept-Language` = "en-US,en;q=0.9"
          ), 
          timeout(15))
    }, error = function(e) {
      message("Connection error: ", e$message)
      return(NULL)
    })
    
    if(!is.null(response) && status_code(response) == 200) {
      content <- content(response, "text", encoding = "UTF-8")
      if(nchar(content) > 0) {
        return(content)
      }
    }
    
    message("Attempt ", attempt, " failed. Waiting ", wait_time, " seconds...")
    Sys.sleep(wait_time)
  }
  
  stop("Failed to download from URL after ", max_retries, " attempts: ", url)
}

# Function to download HTML for a specific NBA season (regular or playoffs)
download_season_html <- function(year, is_playoffs = FALSE) {
  # Determine the appropriate directory and filename
  if(is_playoffs) {
    dir_path <- playoffs_raw_dir
    file_path <- file.path(dir_path, paste0("playoffs_", year, ".html"))
    url <- paste0("https://www.basketball-reference.com/playoffs/NBA_", year, ".html")
  } else {
    dir_path <- raw_data_dir
    file_path <- file.path(dir_path, paste0("nba_", year, ".html"))
    url <- paste0("https://www.basketball-reference.com/leagues/NBA_", year, ".html")
  }
  
  # Check if already downloaded
  if(file.exists(file_path)) {
    message("HTML for ", if(is_playoffs) "playoffs" else "regular season", " ", year, " already exists. Skipping download.")
    return(file_path)
  }
  
  message("Downloading ", if(is_playoffs) "playoffs" else "regular season", " ", year, " from ", url)
  
  tryCatch({
    html_content <- safe_download(url)
    writeLines(html_content, file_path)
    message("Saved raw HTML for ", if(is_playoffs) "playoffs" else "regular season", " year ", year)
    return(file_path)
  }, error = function(e) {
    message("Error downloading ", if(is_playoffs) "playoffs" else "regular season", " ", year, ": ", e$message)
    return(NULL)
  })
}

# Function to extract all tables from an HTML file
extract_all_tables <- function(html_file, year, is_playoffs = FALSE) {
  if(!file.exists(html_file)) {
    message("HTML file not found: ", html_file)
    return(FALSE)
  }
  
  # Read HTML content
  html_content <- readLines(html_file, warn = FALSE)
  html_content <- paste(html_content, collapse = "\n")
  page <- read_html(html_content)
  
  # Get all tables on the page
  all_tables <- page %>% html_nodes("table")
  
  if(length(all_tables) == 0) {
    message("No tables found for ", if(is_playoffs) "playoffs" else "season", " ", year)
    return(FALSE)
  }
  
  # Determine the appropriate directory
  if(is_playoffs) {
    tables_dir <- playoffs_tables_dir
  } else {
    tables_dir <- raw_tables_dir
  }
  
  # Create a directory for this year's tables
  year_dir <- file.path(tables_dir, paste0("season_", year))
  if(!dir.exists(year_dir)) {
    dir.create(year_dir, recursive = TRUE)
  }
  
  # Extract and save each table
  tables_saved <- 0
  for(i in seq_along(all_tables)) {
    table_id <- all_tables[[i]] %>% html_attr("id")
    table_id <- if(is.na(table_id) || table_id == "") paste0("table_", i) else table_id
    
    # Try to convert table to dataframe
    df <- tryCatch({
      html_table(all_tables[[i]], header = TRUE, fill = TRUE)
    }, error = function(e) {
      message("Error parsing table ", i, ": ", e$message)
      return(NULL)
    })
    
    if(!is.null(df) && ncol(df) > 1 && nrow(df) > 0) {
      # Save as RDS file
      file_path <- file.path(year_dir, paste0(table_id, ".rds"))
      saveRDS(df, file_path)
      tables_saved <- tables_saved + 1
    }
  }
  
  message("Saved ", tables_saved, " tables for ", if(is_playoffs) "playoffs" else "season", " ", year)
  return(tables_saved > 0)
}

# Function to download NBA champions list
download_champions_list <- function() {
  if(file.exists(champions_file)) {
    message("Champions list already exists. Skipping download.")
    return(read.csv(champions_file))
  }
  
  url_champs <- "https://www.nba.com/news/history-nba-champions"
  message("Downloading champions list from ", url_champs)
  
  content <- safe_download(url_champs)
  
  # Parse HTML
  page_champs <- read_html(content)
  
  # Try different selector patterns to find the champions list
  text_champs <- page_champs %>% html_element("article") %>% html_text()
  
  if(is.null(text_champs) || nchar(text_champs) == 0) {
    text_champs <- page_champs %>% 
      html_nodes("#main-content .Text") %>% 
      html_text() %>% 
      paste(collapse = "\n")
  }
  
  if(is.null(text_champs) || nchar(text_champs) == 0) {
    text_champs <- page_champs %>% 
      html_nodes("body") %>% 
      html_text()
  }
  
  if(is.null(text_champs) || nchar(text_champs) == 0) {
    stop("No text found for champions list from the page.")
  }
  
  # Split text into lines
  lines <- str_split(text_champs, "\\r?\\n+")[[1]]
  
  # Patterns to match different year-champion formats
  patterns <- c(
    "^(\\d{4})-(\\d{2,4})\\s*[-—]+\\s*(.*)$",
    "^(\\d{4})-(\\d{2,4}):\\s*(.*)$",
    "^(\\d{4})\\s+Champions?:\\s*(.*)$"
  )
  
  # Initialize results dataframe
  results <- data.frame(
    SeasonYear = integer(),
    ChampionTeam = character(),
    stringsAsFactors = FALSE
  )
  
  # Extract champions
  for(line in lines) {
    for(pattern in patterns) {
      match_result <- str_match(line, pattern)
      
      if(!is.na(match_result[1,1])) {
        if(grepl("^\\d{4}\\s+Champions?:", line)) {
          # For the third format
          year_num <- as.integer(match_result[1,2])
          champion_name <- match_result[1,3]
        } else {
          # For first and second formats
          year_num <- as.integer(match_result[1,2])
          champion_name <- match_result[1,4]
        }
        
        # Clean champion name
        champion_name <- str_replace(champion_name, "def\\..*", "")
        champion_name <- str_trim(champion_name)
        
        # Add to results
        results <- rbind(results,
                        data.frame(
                          SeasonYear = year_num,
                          ChampionTeam = champion_name,
                          stringsAsFactors = FALSE
                        ))
        break
      }
    }
  }
  
  # Remove duplicates
  results <- distinct(results, SeasonYear, ChampionTeam)
  
  # Save to file
  write.csv(results, champions_file, row.names = FALSE)
  message("Champions list saved to ", champions_file)
  
  return(results)
}

###############################################################################
# 3. DATA COLLECTION
###############################################################################

# Step 1: Download champions list
champions <- download_champions_list()
message("Found ", nrow(champions), " champions")

# Step 2: Download regular season data for specified range
start_year <- 1974
end_year <- 2024

for(year in start_year:end_year) {
  message("\nProcessing regular season ", year)
  
  # Random delay to prevent overload
  wait_time <- runif(1, 1, 3)
  message("Waiting ", round(wait_time, 1), " seconds...")
  Sys.sleep(wait_time)
  
  # Download and save raw HTML for regular season
  html_file <- download_season_html(year, is_playoffs = FALSE)
  
  if(!is.null(html_file)) {
    # Extract and save tables
    extract_all_tables(html_file, year, is_playoffs = FALSE)
  }
  
  # Process playoffs data for the same year
  message("\nProcessing playoffs ", year)
  
  # Additional delay before downloading playoffs
  wait_time <- runif(1, 1, 3)
  message("Waiting ", round(wait_time, 1), " seconds...")
  Sys.sleep(wait_time)
  
  # Download and save raw HTML for playoffs
  playoffs_html <- download_season_html(year, is_playoffs = TRUE)
  
  if(!is.null(playoffs_html)) {
    # Extract and save tables
    extract_all_tables(playoffs_html, year, is_playoffs = TRUE)
  }
}

# Step 3: Download current season data separately
current_season <- 2025  # Adjust as needed
message("\nProcessing current season ", current_season)
current_html <- download_season_html(current_season, is_playoffs = FALSE)
if(!is.null(current_html)) {
  extract_all_tables(current_html, current_season, is_playoffs = FALSE)
}

###############################################################################
# 4. SUMMARY
###############################################################################
message("\nData collection completed!")
message("Raw HTML files stored in: ", raw_data_dir)
message("Raw tables stored in: ", raw_tables_dir)
message("Champions list stored in: ", champions_file)

# Display counts of collected data
reg_season_dirs <- list.files(raw_tables_dir, pattern = "^season_", full.names = TRUE)
playoffs_season_dirs <- list.files(playoffs_tables_dir, pattern = "^season_", full.names = TRUE)

reg_season_count <- length(reg_season_dirs)
playoffs_season_count <- length(playoffs_season_dirs)

reg_table_count <- sum(sapply(reg_season_dirs, function(dir) {
  length(list.files(dir, pattern = "\\.(rds|csv)$"))
}))

playoffs_table_count <- sum(sapply(playoffs_season_dirs, function(dir) {
  length(list.files(dir, pattern = "\\.(rds|csv)$"))
}))

message("Total regular seasons collected: ", reg_season_count)
message("Total regular season tables extracted: ", reg_table_count)
message("Total playoff seasons collected: ", playoffs_season_count)
message("Total playoff tables extracted: ", playoffs_table_count)
message("Overall total tables: ", reg_table_count + playoffs_table_count)
