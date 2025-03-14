###############################################################################
# NBA Championship Prediction Project
# Script 3: Exploratory Data Analysis
# 
# Purpose: Explore and visualize NBA data to identify patterns and relationships
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
required_packages <- c("dplyr", "ggplot2", "tidyr", "scales", "brms", "bayesplot", "ggrepel")
for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Define paths
base_path <- "/Users/itamar/Documents/GitHub/nba-championship-prediction/data"
playoffs_path <- "/Users/itamar/Documents/GitHub/nba-championship-prediction/playoffs"
input_file <- file.path(base_path, "full_advanced_stats_filtered_complete.csv")
playoffs_file <- file.path(playoffs_path, "playoffs_advanced_stats_filtered_complete.csv")
output_dir <- file.path(base_path, "results")

# Ensure output directory exists
if(!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  message("Created directory: ", output_dir)
}

# Create a custom theme with white background
theme_white <- function(base_size = 12) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "#EEEEEE"),
      panel.grid.minor = element_line(color = "#F5F5F5"),
      legend.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(15, 15, 15, 15)
    )
}

###############################################################################
# 2. LOAD AND PREPARE DATA
###############################################################################
message("Loading regular season data from ", basename(input_file))
nba_data <- read.csv(input_file)

# Add Season column if it doesn't exist
if(!"Season" %in% names(nba_data) && "SeasonYear" %in% names(nba_data)) {
  nba_data$Season <- as.character(nba_data$SeasonYear)
}

# Make sure Champion is a factor if it exists in regular season data
if("Champion" %in% names(nba_data)) {
  nba_data$Champion <- factor(nba_data$Champion, levels = c(0, 1))
}

###############################################################################
# 3. CHAMPIONSHIP FACTORS
###############################################################################
message("\n=== VISUALIZING FACTORS RELATED TO CHAMPIONSHIPS ===")

###############################################################################
# 4. VISUALIZATIONS - Win Percentage & Net Rating
###############################################################################

# plot Win Percentage & Net Rating, with Champions highlighted
win_nrtg_plot <- ggplot(nba_data, aes(x = WinPct, y = NRtg, color = Champion)) +
  geom_point(alpha = 0.7, size = 2) +
  scale_color_manual(values = c("0" = "gray", "1" = "green"), 
                     labels = c("0" = "Non-Champion", "1" = "Champion")) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", aes(group = 1), size = 1.2) +
  labs(
    title = "Win Percentage vs. Net Rating",
    subtitle = "Champions highlighted in green",
    x = "Win Percentage",
    y = "Net Rating",
    color = "Champion Status"
  ) +
  theme_white() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))

# Save plot
ggsave(file.path(output_dir, "winpct_nrtg_champions.png"), win_nrtg_plot, width = 10, height = 7)
print(win_nrtg_plot)

###############################################################################
# 5. VISUALIZATIONS - TOP N TEAMS: PREDICTED CHAMPIONS 
###############################################################################

# function: calculate champions in top N
calculate_top_n_champions <- function(data, variable, n_values = c(1, 3, 5, 10)) {
  results <- data.frame()
  
  for(n in n_values) {
    result <- data %>%
      group_by(Season) %>%
      arrange(desc(!!sym(variable)), .by_group = TRUE) %>%
      mutate(Rank = row_number()) %>%
      filter(Rank <= n) %>%
      summarize(
        TopN = n,
        Variable = variable,
        Champions = sum(Champion == 1),
        Teams = n(),
        ChampionPct = Champions / Teams * 100
      )
    
    results <- bind_rows(results, result)
  }
  
  return(results %>% 
           group_by(TopN, Variable) %>% 
           summarize(
             Seasons = n(),
             Champions = sum(Champions),
             Teams = sum(Teams),
             ChampionPct = Champions / Teams * 100,
             .groups = 'drop'
           ))
}

# use function to calculate champions in top N in NRtg & WinPct
top_n_nrtg <- calculate_top_n_champions(nba_data, "NRtg")
top_n_winpct <- calculate_top_n_champions(nba_data, "WinPct")
top_n_combined <- bind_rows(top_n_nrtg, top_n_winpct)

# plot Champion Predicted by top N teams in Net Rating and Win Percentage
top_n_plot <- ggplot(top_n_combined, aes(x = factor(TopN), y = Champions, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(ChampionPct, 1), "%\n(", Champions, ")")), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +
  scale_fill_manual(values = c("NRtg" = "blue", "WinPct" = "red"),
                    labels = c("NRtg" = "Net Rating", "WinPct" = "Win Percentage")) +
  # Set y-axis scale manually
  scale_y_continuous(limits = c(0, 55)) +  
  labs(
    title = "Championships Predicted for Top N Teams",
    subtitle = "By Net Rating and Win Percentage (Success Rate highlighted on each bar) ",
    x = "Top N Teams",
    y = "Championships predicted",
    fill = "Ranking Variable"
  ) +
  theme_white()

# Save plot
ggsave(file.path(output_dir, "top_n_champions.png"), top_n_plot, width = 12, height = 7)
print(top_n_plot)

###############################################################################
# 6. VISUALIZATIONS - Metrics changes over time
###############################################################################

# calculate mean for each season
metrics_over_time <- nba_data %>%
  group_by(Season) %>%
  summarize(
    ORtg_Avg = mean(ORtg, na.rm = TRUE),
    DRtg_Avg = mean(DRtg, na.rm = TRUE),
    ORB_Pct_Avg = mean(ORB_Pct, na.rm = TRUE),
    DRB_Pct_Avg = if("DRB_Pct" %in% names(nba_data)) mean(DRB_Pct, na.rm = TRUE) else NA,
    Teams = n(),
    .groups = 'drop'
  )

# Convert to long format for plotting
metrics_long <- metrics_over_time %>%
  select(-Teams) %>%
  pivot_longer(
    cols = ends_with("_Avg"),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  mutate(
    Metric = gsub("_Avg", "", Metric),
    Season_Num = as.numeric(Season)
  )

# Calculate overall means to center the values
metric_means <- metrics_long %>%
  group_by(Metric) %>%
  summarize(
    Overall_Mean = mean(Value, na.rm = TRUE),
    .groups = 'drop'
  )

# Join season means with overall mean and calculate deviation
metrics_centered <- metrics_long %>%
  left_join(metric_means, by = "Metric") %>%
  mutate(Deviation = Value - Overall_Mean)

# Plot selected metrics over time
metrics_plot <- ggplot(metrics_centered, aes(x = Season_Num, y = Deviation, color = Metric, group = Metric)) +
  geom_line(size = 1.2) +  
  geom_point(size = 1.4) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "NBA Metrics Changes Over Time",
    subtitle = "Deviation from overall mean",
    x = "Season",
    y = "Deviation from Mean",
    color = "Metric"
  ) +
  theme_white() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 9),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.margin = margin(t = 10),
    legend.text = element_text(size = 10),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  guides(color = guide_legend(nrow = 1)) +
  scale_x_continuous(breaks = seq(min(metrics_centered$Season_Num), 
                                 max(metrics_centered$Season_Num), by = 5))

# Save plot
ggsave(file.path(output_dir, "metrics_over_time.png"), metrics_plot, width = 12, height = 8)
print(metrics_plot)

###############################################################################
# 7. CURRENT SEASON EXPLORATORY ANALYSIS and VISUALIZATIONS
###############################################################################

message("\n=== ANALYZING CURRENT SEASON DATA ===")

# Load current season data
current_season_file <- file.path(base_path, "current_season.csv")

  current_data <- read.csv(current_season_file)
  message("Current season data dimensions: ", nrow(current_data), " x ", ncol(current_data))
  
  # Top teams by Net Rating
  top_nrtg <- current_data %>%
    arrange(desc(NRtg)) %>%
    head(10) %>%
    mutate(
      WinPct = round(WinPct * 100, 1),
      Record = paste0(W, "-", L)
    )
  
  # Create a bar plot of top teams by Net Rating 
  top_nrtg_plot <- ggplot(top_nrtg, aes(x = reorder(Team, NRtg), y = NRtg)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = paste0(round(NRtg, 1), " (", Record, ")")), hjust = -0.1, size = 3.5) +
    coord_flip(ylim = c(0, max(top_nrtg$NRtg) * 1.15)) +  # Add extra space for labels
    labs(
      title = "Top 10 Teams by Net Rating - Current Season",
      x = NULL,
      y = "Net Rating"
    ) +
    theme_white() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.margin = margin(10, 30, 10, 10)  # Add extra margin on the right for labels
    )
  
  # Save plot
  ggsave(file.path(output_dir, "current_top_nrtg.png"), top_nrtg_plot, width = 10, height = 7)
  print(top_nrtg_plot)

###############################################################################
# 8. SUMMARY
###############################################################################
message("\n=== EDA SUMMARY ===")
message("Exploratory data analysis complete!")
message("Total plots generated: ", length(list.files(output_dir, pattern = "\\.png$")))
message("Plots have been saved to: ", output_dir)

# Display basic correlation matrix
message("\nCorrelation matrix for un-standerized variables:")
selected_vars <- c("NRtg", "ORtg", "DRtg", "Age",
                   "eFG", "TOV_Pct", "ORB_Pct", "FTr",
                   "Opp_eFG", "Opp_TOV", "DRB_Pct", "Opp_FTr", 
                   "WinPct" )
available_vars <- selected_vars[selected_vars %in% names(nba_data)]
cor_matrix <- cor(nba_data[available_vars], use = "complete.obs")
print(round(cor_matrix, 2))

# Key findings summary
message("\nKey findings:")
message("3. Championship percentage for top team by Win%: ", 
        round(top_n_winpct$ChampionPct[top_n_winpct$TopN == 1], 1), "%")
message("4. Championship percentage for top team by Net Rating: ", 
        round(top_n_nrtg$ChampionPct[top_n_nrtg$TopN == 1], 1), "%")

# Print current season Oklahoma City Thunder stats if available
okc_data <- current_data %>% filter(grepl("Oklahoma City|Thunder", Team))
message("\nOklahoma City Thunder current season stats:")
message("Record: ", okc_data$W, "-", okc_data$L, " (", round(okc_data$WinPct * 100, 1), "%)")
message("Net Rating: ", round(okc_data$NRtg, 1))
message("Offensive Rating: ", round(okc_data$ORtg, 1))
message("Defensive Rating: ", round(okc_data$DRtg, 1))
message("Average Age: ", round(okc_data$Age, 1))

