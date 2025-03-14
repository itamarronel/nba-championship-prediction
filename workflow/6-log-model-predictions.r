###############################################################################
# NBA Championship Prediction Project
# Script 6: Model Evaluation and Predictions (Simplified)
#
# Purpose: Evaluate model performance and generate predictions for current season
# Author: Itamar Ronel (Optimized)
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
required_packages <- c("dplyr", "ggplot2", "tidyr", "pROC")
for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Define paths
base_path <- "/Users/itamar/Documents/GitHub/nba-championship-prediction/data"
model_file <- file.path(base_path, "nba_models.rds")
data_file <- file.path(base_path, "nba_relative_data.rds")
current_season_file <- file.path(base_path, "current_season.csv")
output_dir <- file.path(base_path, "results")

# Evaluation parameters
TOP_N <- 3  # Number of top teams to consider
PLOT_WIDTH <- 10
PLOT_HEIGHT <- 7
PLOT_RES <- 300

# clean white theme
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

# Set seed for reproducibility
set.seed(123)

###############################################################################
# 2. LOAD MODEL AND DATA
###############################################################################
# Load the models
message("Loading models from ", basename(model_file))
model_obj <- readRDS(model_file)

# Extract models and parameters
simple_model <- model_obj$simple_model
focused_model <- model_obj$focused_model
full_model <- model_obj$full_model
winpct_model <- model_obj$winpct_model
min_win_pct <- model_obj$min_win_pct

# Get thresholds
threshold_simple <- model_obj$threshold_simple
threshold_focused <- model_obj$threshold_focused
threshold_full <- model_obj$threshold_full

# המרת הספים מ-data.frame למספרים פשוטים
threshold_simple <- as.numeric(threshold_simple[1,1])
threshold_focused <- as.numeric(threshold_focused[1,1])
threshold_full <- as.numeric(threshold_full[1,1])

message("Converted thresholds to numeric:")
message("Simple: ", threshold_simple)
message("Focused: ", threshold_focused)
message("Full: ", threshold_full)

# Load the model data
message("Loading model data from ", basename(data_file))
model_data <- readRDS(data_file)

###############################################################################
# 3. PREPARE HISTORICAL DATA
###############################################################################
message("\n=== PREPARING HISTORICAL DATA ===")

# Calculate rank for each model (since they don't exist)
hist_predictions <- model_data %>%
  group_by(Season) %>%
  mutate(
    Simple_Rank = rank(-Simple_Prob),
    Focused_Rank = rank(-Focused_Prob),
    Full_Rank = rank(-Full_Prob)
  ) %>%
  ungroup()

# Add contender flags based on top teams instead of thresholds
hist_predictions$Simple_Contender <- hist_predictions$Simple_Rank <= 5  # Top 5 teams per season
hist_predictions$Focused_Contender <- hist_predictions$Focused_Rank <= 5
hist_predictions$Full_Contender <- hist_predictions$Full_Rank <= 5

# Display how many contenders we have
message("Number of Simple model contenders: ", sum(hist_predictions$Simple_Contender))
message("Number of Focused model contenders: ", sum(hist_predictions$Focused_Contender))
message("Number of Full model contenders: ", sum(hist_predictions$Full_Contender))

###############################################################################
# 4. EVALUATE HISTORICAL PERFORMANCE
###############################################################################
message("\n=== EVALUATING MODELS ON HISTORICAL DATA ===")

# Create season-by-season summary focusing on champions
season_summary <- hist_predictions %>%
  filter(Champion == 1) %>%
  select(Season, Team, Simple_Rank, Focused_Rank, Full_Rank) %>%
  mutate(
    SimpleInTopN = Simple_Rank <= TOP_N,
    FocusedInTopN = Focused_Rank <= TOP_N,
    FullInTopN = Full_Rank <= TOP_N,
    ChampionTeam = Team
  )

threshold_summary <- hist_predictions %>%
  filter(Champion == 1) %>%
  select(Season, Team, Simple_Contender, Focused_Contender, Full_Contender)

# Add decade information
threshold_summary$Decade <- paste0(substr(threshold_summary$Season, 1, 3), "0s")

# Convert the booleans to 1/0 success flags
threshold_summary <- threshold_summary %>%
  mutate(
    Simple_Threshold_Success  = ifelse(Simple_Contender, 1, 0),
    Focused_Threshold_Success = ifelse(Focused_Contender, 1, 0),
    Full_Threshold_Success    = ifelse(Full_Contender, 1, 0)
  )

# Calculate accuracy for each model (champion in top N)
simple_accuracy <- mean(season_summary$SimpleInTopN)
focused_accuracy <- mean(season_summary$FocusedInTopN)
full_accuracy <- mean(season_summary$FullInTopN)

message("\nHistorical accuracy (champion in top ", TOP_N, "):")
message("Simple model: ", round(simple_accuracy * 100, 1), "%")
message("Focused model: ", round(focused_accuracy * 100, 1), "%")
message("Full model: ", round(full_accuracy * 100, 1), "%")

# Save results
write.csv(season_summary, file.path(output_dir, "champion_rank_by_season.csv"), row.names = FALSE)

# Calculate accuracy by decade
season_summary$Decade <- paste0(substr(season_summary$Season, 1, 3), "0s")

decade_accuracy <- season_summary %>%
  group_by(Decade) %>%
  summarize(
    Seasons = n(),
    Simple_Success = sum(SimpleInTopN),
    Focused_Success = sum(FocusedInTopN),
    Full_Success = sum(FullInTopN),
    Simple_Accuracy = Simple_Success / Seasons * 100,
    Focused_Accuracy = Focused_Success / Seasons * 100,
    Full_Accuracy = Full_Success / Seasons * 100,
    .groups = 'drop'
  )

message("\nAccuracy by decade (champion in top ", TOP_N, "):")
for(i in 1:nrow(decade_accuracy)) {
  message(sprintf("%s: Simple = %.1f%%, Focused = %.1f%%, Full = %.1f%%", 
                  decade_accuracy$Decade[i],
                  decade_accuracy$Simple_Accuracy[i],
                  decade_accuracy$Focused_Accuracy[i],
                  decade_accuracy$Full_Accuracy[i]))
}

decade_threshold_accuracy <- threshold_summary %>%
  group_by(Decade) %>%
  summarize(
    Seasons          = n(),
    Simple_Success   = sum(Simple_Threshold_Success),
    Focused_Success  = sum(Focused_Threshold_Success),
    Full_Success     = sum(Full_Threshold_Success),
    Simple_Accuracy  = 100 * Simple_Success  / Seasons,
    Focused_Accuracy = 100 * Focused_Success / Seasons,
    Full_Accuracy    = 100 * Full_Success    / Seasons,
    .groups          = 'drop'
  )

# print and save decade accuracy
print(decade_threshold_accuracy)
write.csv(decade_threshold_accuracy, file.path(output_dir, "decade_threshold_accuracy.csv"), row.names = FALSE)
message("\nDecade threshold accuracy saved to: ", file.path(output_dir, "decade_threshold_accuracy.csv"))

total_threshold_accuracy <- threshold_summary %>%
  summarize(
    Seasons          = n(),
    Simple_Success   = sum(Simple_Threshold_Success),
    Focused_Success  = sum(Focused_Threshold_Success),
    Full_Success     = sum(Full_Threshold_Success),
    Simple_Accuracy  = 100 * Simple_Success  / Seasons,
    Focused_Accuracy = 100 * Focused_Success / Seasons,
    Full_Accuracy    = 100 * Full_Success    / Seasons,
    .groups          = 'drop'
  )

# print and save total accuracy
print(total_threshold_accuracy)
write.csv(total_threshold_accuracy, file.path(output_dir, "total_threshold_accuracy.csv"), row.names = FALSE)
message("\nTotal threshold accuracy saved to: ", file.path(output_dir, "total_threshold_accuracy.csv"))

###############################################################################
# 5. VISUALIZATIONS
###############################################################################
message("\n=== CREATING VISUALIZATIONS ===")

# 1. Champion rank by season - Focused model
rank_plot_data <- season_summary %>%
  pivot_longer(
    cols = Focused_Rank,
    names_to = "Model",
    values_to = "Rank"
  ) %>%
  mutate(
    Model = gsub("_Rank", "", Model),
    InTopN = Rank <= TOP_N,
    Season_num = as.numeric(Season)
  )

champion_rank_plot <- ggplot(rank_plot_data, aes(x = Season_num, y = Rank, color = InTopN)) +
  geom_point(size = 3) +
  geom_line(aes(group = 1), alpha = 0.3) +
  geom_hline(yintercept = TOP_N + 0.5, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
  scale_x_continuous(
    breaks = seq(floor(min(rank_plot_data$Season_num) / 10) * 10,
                 ceiling(max(rank_plot_data$Season_num) / 10) * 10,
                 by = 10),
    labels = function(x) paste0(substr(as.character(x), 1, 3), "0s")
  ) +
  labs(
    title = "Rank of Actual Champion in Focused Model Predictions by Season",
    subtitle = sprintf("Success rate (Champion in top %d): Focused = %.1f%%", TOP_N, focused_accuracy * 100),
    x = "Season",
    y = "Rank of Actual Champion",
    color = paste0("Champion in Top ", TOP_N)
  ) +
  theme_white()

print(champion_rank_plot)
ggsave(file.path(output_dir, "champion_rank_by_season.png"), champion_rank_plot, 
       width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_RES)
message("Champion rank chart saved to: ", file.path(output_dir, "champion_rank_by_season.png"))

# Function to calculate predicted championships for a given model based on top N teams
calculate_top_n_champions_model <- function(data, model, n_values = c(1, 3, 5, 10)) {
  results <- data.frame()
  model_prob_col <- paste0(model, "_Prob")
  model_rank_col <- paste0(model, "_Rank")
  
  for(n in n_values) {
    result <- data %>%
      filter(!!sym(model_rank_col) <= n) %>%
      group_by(Season) %>%
      summarize(
        TopN = n,
        Model = model,
        Champions = sum(Champion == 1),
        Teams = n(),
        ChampionPct = Champions / Teams * 100,
        .groups = 'drop'
      )
    
    results <- bind_rows(results, result)
  }
  
  # Summarize results for each TopN across seasons
  results %>% 
    group_by(TopN, Model) %>% 
    summarize(
      Seasons = n(),
      Champions = sum(Champions),
      Teams = sum(Teams),
      ChampionPct = Champions / Teams * 100,
      .groups = 'drop'
    )
}

# Calculate predicted championships for each model
top_n_simple   <- calculate_top_n_champions_model(hist_predictions, "Simple")
top_n_focused  <- calculate_top_n_champions_model(hist_predictions, "Focused")
top_n_full     <- calculate_top_n_champions_model(hist_predictions, "Full")

# Combine results and update model names to include the word "Model"
top_n_combined <- bind_rows(top_n_simple, top_n_focused, top_n_full) %>%
  mutate(Model = paste(Model, "Model"))

# Create the plot
top_n_plot <- ggplot(top_n_combined, aes(x = factor(TopN), y = Champions, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(ChampionPct, 1), "%\n(", Champions, ")")),
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) +
  scale_fill_manual(values = c("Simple Model" = "blue", "Focused Model" = "green", "Full Model" = "red"),
                    labels = c("Simple Model" = "Simple Model", 
                               "Focused Model" = "Focused Model", 
                               "Full Model" = "Full Model")) +
  scale_y_continuous(limits = c(0, 55)) +  
  labs(
    title = "Predicted Championships for Top N Teams",
    subtitle = "Model Predictions (Success Rate indicated on each bar)",
    x = "Top N Teams",
    y = "Predicted Championships",
    fill = "Model Prediction"
  ) +
  theme_white()

# Save the plot
ggsave(file.path(output_dir, "top_n_champions_models.png"), top_n_plot, width = 12, height = 7)
print(top_n_plot)

###############################################################################
# 6. CURRENT SEASON PREDICTIONS
###############################################################################
message("\n=== GENERATING PREDICTIONS FOR CURRENT SEASON ===")

# Load current season data
current_data <- read.csv(current_season_file)
message("Current season data loaded: ", nrow(current_data), " teams")

# Add Season column if missing
if(!"Season" %in% names(current_data)) {
  current_data$Season <- "2024-25"
}

# Function to normalize statistics by season (only for current season)
normalize_by_season <- function(data, var_name) {
  data %>%
    group_by(Season) %>%
    mutate(!!paste0(var_name, "_Z") := 
             (!!sym(var_name) - mean(!!sym(var_name), na.rm = TRUE)) /
             sd(!!sym(var_name), na.rm = TRUE)) %>%
    ungroup()
}

# Filter competitive teams
current_filtered <- current_data %>% filter(WinPct >= min_win_pct)

# Normalize statistics (list from model formula)
model_variables <- c("WinPct", "ORtg", "DRtg", "Age", "eFG", "TOV_Pct", 
                     "ORB_Pct", "FTr", "Opp_eFG", "Opp_TOV", "DRB_Pct", "Opp_FTr")

for(col in model_variables) {
  if(col %in% names(current_filtered)) {
    current_filtered <- normalize_by_season(current_filtered, col)
  }
}

# Calculate WinPct residuals
if(all(c("ORtg_Z", "DRtg_Z", "WinPct_Z") %in% names(current_filtered))) {
  current_filtered$PredictedWinPctZ <- predict(winpct_model, newdata = current_filtered)
  current_filtered$WinPct_Resid <- current_filtered$WinPct_Z - current_filtered$PredictedWinPctZ
}

message("Competitive teams in current season: ", nrow(current_filtered))

# Generate predictions
current_filtered$Simple_Prob <- predict(simple_model, newdata = current_filtered, type = "response")
current_filtered$Focused_Prob <- predict(focused_model, newdata = current_filtered, type = "response") 
current_filtered$Full_Prob <- predict(full_model, newdata = current_filtered, type = "response")

# Calculate ranks
current_predictions <- current_filtered %>%
  group_by(Season) %>%
  mutate(
    Simple_Rank = rank(-Simple_Prob),
    Focused_Rank = rank(-Focused_Prob),
    Full_Rank = rank(-Full_Prob)
  ) %>%
  ungroup()

# Add contender flags using numeric thresholds
current_predictions$Simple_Contender <- current_predictions$Simple_Prob >= threshold_simple
current_predictions$Focused_Contender <- current_predictions$Focused_Prob >= threshold_focused
current_predictions$Full_Contender <- current_predictions$Full_Prob >= threshold_full

# Display contenders for current season
message("Number of current season contenders (per model): ",
        sum(current_predictions$Simple_Contender), " / ",
        sum(current_predictions$Focused_Contender), " / ",
        sum(current_predictions$Full_Contender))

# Create formatted report with all models
report <- current_predictions %>%
  arrange(desc(Focused_Prob)) %>%
  select(
    Team, Season, W, L, WinPct, 
    Simple_Prob, Focused_Prob, Full_Prob,
    Simple_Rank, Focused_Rank, Full_Rank,
    Simple_Contender, Focused_Contender, Full_Contender
  ) %>%
  mutate(
    Record = paste0(W, "-", L),
    WinPct = paste0(round(WinPct * 100, 1), "%"),
    Simple_Prob = paste0(round(Simple_Prob * 100, 1), "%"),
    Focused_Prob = paste0(round(Focused_Prob * 100, 1), "%"),
    Full_Prob = paste0(round(Full_Prob * 100, 1), "%")
  )

# Save predictions
write.csv(report, file.path(output_dir, "current_season_predictions.csv"), row.names = FALSE)
message("Current season predictions saved to: ", file.path(output_dir, "current_season_predictions.csv"))

# Print top contenders for each model
top_simple <- current_predictions %>% filter(Simple_Rank == 1)
top_focused <- current_predictions %>% filter(Focused_Rank == 1)
top_full <- current_predictions %>% filter(Full_Rank == 1)

message("\nTop Championship Contenders by Model:")
message("Simple model: ", top_simple$Team, " (", top_simple$W, "-", top_simple$L, ", ", 
        round(top_simple$WinPct * 100, 1), "%) - ", round(top_simple$Simple_Prob * 100, 1), "%")
message("Focused model: ", top_focused$Team, " (", top_focused$W, "-", top_focused$L, ", ", 
        round(top_focused$WinPct * 100, 1), "%) - ", round(top_focused$Focused_Prob * 100, 1), "%")
message("Full model: ", top_full$Team, " (", top_full$W, "-", top_full$L, ", ", 
        round(top_full$WinPct * 100, 1), "%) - ", round(top_full$Full_Prob * 100, 1), "%")

# Create visualization comparing all models for current season
current_plot_data <- current_predictions %>%
  pivot_longer(
    cols = c(Simple_Prob, Focused_Prob, Full_Prob),
    names_to = "Model",
    values_to = "Probability"
  ) %>%
  mutate(Model = gsub("_Prob", "", Model))

current_plot <- ggplot(
  current_plot_data %>% arrange(desc(Probability)) %>% head(30),
  aes(x = reorder(paste(Team, Model), Probability), y = Probability * 100, fill = Model)
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("Simple" = "blue", "Focused" = "green", "Full" = "red")) +
  labs(
    title = "Current NBA Season Championship Probability",
    subtitle = "Comparison of all three models",
    x = NULL,
    y = "Championship Probability (%)"
  ) +
  theme_white()

ggsave(file.path(output_dir, "current_season_comparison.png"), current_plot, 
       width = PLOT_WIDTH, height = PLOT_HEIGHT * 1.5, dpi = PLOT_RES)
message("Current season comparison plot saved to: ", file.path(output_dir, "current_season_comparison.png"))

###############################################################################
# 7. SUMMARY
###############################################################################
message("\n=== PREDICTION SUMMARY ===")
message("Model evaluation and predictions complete!")
message("\nKey metrics:")
message("- Simple model accuracy: ", round(simple_accuracy * 100, 1), "%")
message("- Focused model accuracy: ", round(focused_accuracy * 100, 1), "%")
message("- Full model accuracy: ", round(full_accuracy * 100, 1), "%")
message("\nAll results saved to: ", output_dir)
