###############################################################################
# NBA Championship Prediction Project
# Script 4: Regression Analysis: Regular Season VS Playoffs with Win Residuals
# 
# Purpose: Explore relationships between regular season stats and playoff performance
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
required_packages <- c("dplyr", "ggplot2", "tidyr", "scales", "brms", "bayesplot")
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

message("Loading playoff data from ", basename(playoffs_file))
playoff_data <- read.csv(playoffs_file)

# Add Season column if it doesn't exist
if(!"Season" %in% names(nba_data) && "SeasonYear" %in% names(nba_data)) {
  nba_data$Season <- as.character(nba_data$SeasonYear)
}

if(!"Season" %in% names(playoff_data) && "SeasonYear" %in% names(playoff_data)) {
  playoff_data$Season <- as.character(playoff_data$SeasonYear)
}

# Make sure Champion is a factor if it exists in regular season data
if("Champion" %in% names(nba_data)) {
  nba_data$Champion <- factor(nba_data$Champion, levels = c(0, 1))
}

###############################################################################
# 3. CALCULATE WIN PERCENTAGE RESIDUALS
###############################################################################
# Calculate residuals for WinPct ~ NRtg
message("\n=== CALCULATING WIN PERCENTAGE RESIDUALS ===")
resid_formula <- "WinPct ~ NRtg"
fit_winpct_all <- lm(as.formula(resid_formula), data = nba_data)
nba_data$WinPct_Resid <- residuals(fit_winpct_all)

message("Linear model summary for Win Percentage predicted by Net Rating:")
print(summary(fit_winpct_all))

###############################################################################
# 4. CORRELATION BETWEEN PREDICTORS AND PLAYOFF NET RATING
###############################################################################
message("\n=== ANALYZING RELATIONSHIP BETWEEN PREDICTORS AND PLAYOFF PERFORMANCE ===")

# Merge regular season and playoff data
merged_data <- inner_join(
  nba_data %>% select(Team, SeasonYear, Team_Season, NRtg, WinPct_Resid) %>% 
    rename(RS_NRtg = NRtg),
  playoff_data %>% select(Team, SeasonYear, Team_Season, NRtg) %>% 
    rename(PO_NRtg = NRtg),
  by = "Team_Season"
)

message("Merged data (all playoff teams): ", nrow(merged_data), " rows")

# Calculate correlations for each predictor
correlation_nrtg <- cor(merged_data$RS_NRtg, merged_data$PO_NRtg)
correlation_resid <- cor(merged_data$WinPct_Resid, merged_data$PO_NRtg)

message("\nCorrelations:")
message("  - Regular Season NRtg vs Playoff NRtg: ", round(correlation_nrtg, 3))
message("  - WinPct Residuals vs Playoff NRtg: ", round(correlation_resid, 3))

# Save correlation results to CSV
corr_results <- data.frame(
  correlation_nrtg = correlation_nrtg,
  correlation_resid = correlation_resid
)
write.csv(corr_results, file.path(output_dir, "correlation_results_with_residuals.csv"), row.names = FALSE)

###############################################################################
# 5. VISUALIZATIONS - CORRELATION
###############################################################################

# Create scatterplots for NRtg and Win Percentage residuals
plot1 <- ggplot(merged_data, aes(x = RS_NRtg, y = PO_NRtg)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue", size = 1.2) +
  labs(
    title = "Relationship between Regular Season Net Rating and Playoff Net Rating",
    x = "Regular Season Net Rating",
    y = "Playoff Net Rating"
  ) +
  theme_white() +
  annotate("text", x = min(merged_data$RS_NRtg) + 1, y = max(merged_data$PO_NRtg) - 1, 
           label = paste("Correlation:", round(correlation_nrtg, 3)), 
           hjust = 0, vjust = 1)

plot2 <- ggplot(merged_data, aes(x = WinPct_Resid, y = PO_NRtg)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", size = 1.2) +
  labs(
    title = "Relationship between Win Percentage Residuals and Playoff Net Rating",
    x = "Win Percentage Residuals",
    y = "Playoff Net Rating"
  ) +
  theme_white() +
  annotate("text", x = min(merged_data$WinPct_Resid) + 0.02, y = max(merged_data$PO_NRtg) - 1, 
           label = paste("Correlation:", round(correlation_resid, 3)), 
           hjust = 0, vjust = 1)

# Save plots
ggsave(file.path(output_dir, "rs_nrtg_vs_po_nrtg.png"), plot1, width = 10, height = 7)
ggsave(file.path(output_dir, "winpct_resid_vs_po_nrtg.png"), plot2, width = 10, height = 7)
print(plot1)
print(plot2)

###############################################################################
# 6. BAYESIAN REGRESSION ANALYSIS WITH RESIDUALS
###############################################################################

message("\n=== BAYESIAN REGRESSION ANALYSIS WITH RESIDUALS ===")

# Setting informative priors for model with residuals and interaction
priors_resid <- c(
  prior(normal(-2, 2), class = "Intercept"),   # Prior for intercept
  prior(normal(1, 1), class = "b", coef = "RS_NRtg"),  # Prior for NRtg coefficient
  prior(normal(0, 1), class = "b", coef = "WinPct_Resid"),  # Prior for residuals coefficient
  prior(normal(0, 1), class = "b", coef = "RS_NRtg:WinPct_Resid")  # Prior for interaction term
)

# Fit the model with residuals and interaction
bayes_model_resid <- brm(
  formula = PO_NRtg ~ RS_NRtg * WinPct_Resid,  # * adds main effects and interaction
  data = merged_data,
  family = gaussian(),
  prior = priors_resid,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  seed = 123,
  file = file.path(base_path, "nba_nrtg_resid_interaction_model")  # Save compiled model
)

# Print model summary
print(summary(bayes_model_resid))

###############################################################################
# 7. VISUALIZATIONS - BAYESIAN REGRESSION
###############################################################################

# Plot posterior distributions for the model with residuals and interaction
posterior_plot_resid <- mcmc_areas(
  as.matrix(bayes_model_resid), 
  pars = c("b_Intercept", "b_RS_NRtg", "b_WinPct_Resid", "b_RS_NRtg:WinPct_Resid"), 
  prob = 0.965
) +
  labs(
    title = "Posterior Distributions - Model with WinPct Residuals and Interaction",
    subtitle = paste0(
      "Intercept: ", round(fixef(bayes_model_resid)[1, 1], 2), 
      " [", round(fixef(bayes_model_resid)[1, 3], 2), ", ", 
      round(fixef(bayes_model_resid)[1, 4], 2), "]\n",
      "RS_NRtg coefficient: ", round(fixef(bayes_model_resid)[2, 1], 2),
      " [", round(fixef(bayes_model_resid)[2, 3], 2), ", ", 
      round(fixef(bayes_model_resid)[2, 4], 2), "]\n",
      "WinPct_Resid coefficient: ", round(fixef(bayes_model_resid)[3, 1], 2),
      " [", round(fixef(bayes_model_resid)[3, 3], 2), ", ", 
      round(fixef(bayes_model_resid)[3, 4], 2), "]\n",
      "Interaction coefficient: ", round(fixef(bayes_model_resid)[4, 1], 2),
      " [", round(fixef(bayes_model_resid)[4, 3], 2), ", ", 
      round(fixef(bayes_model_resid)[4, 4], 2), "]"
    )
  ) +
  theme_white() +
  theme(plot.subtitle = element_text(size = 10))

# Save posterior plot
ggsave(file.path(output_dir, "posterior_model_with_residuals.png"), posterior_plot_resid, width = 10, height = 7)
print(posterior_plot_resid)

###############################################################################
# 8. PREDICTION FOR CURRENT SEASON (TOP 10 TEAMS)
###############################################################################

# Define path to current season file
current_season_file <- file.path(base_path, "current_season.csv")

# Load current season data
cat("\nLoading current season data from:", current_season_file, "\n")
current_season <- read.csv(current_season_file)

# Calculate residuals for current season
cat("\nCalculating Win Percentage residuals for current season:\n")
current_season$WinPct_Resid <- NA  # Initialize column

# Check if WinPct exists in current_season
if("WinPct" %in% names(current_season) && "NRtg" %in% names(current_season)) {
  # Use the same model from regular season to calculate residuals
  current_season$WinPct_Pred <- predict(fit_winpct_all, newdata = current_season)
  current_season$WinPct_Resid <- current_season$WinPct - current_season$WinPct_Pred
  cat("Residuals calculated using the linear model.\n")
} else {
  cat("Warning: WinPct or NRtg not found in current_season data. Residuals will not be available.\n")
}

# Filter top 10 teams by Net Rating
top10_current <- current_season %>%
  arrange(desc(NRtg)) %>%
  slice(1:10)

# Rename NRtg to RS_NRtg to match the model formula
top10_current <- top10_current %>%
  rename(RS_NRtg = NRtg)

cat("\nTop 10 teams in current season (by NRtg):\n")
print(top10_current[, c("Team", "RS_NRtg", "WinPct_Resid")])

# Calculate predictions using the model with residuals
if(all(!is.na(top10_current$WinPct_Resid))) {
  predict_top10_resid <- predict(
    bayes_model_resid, 
    newdata = top10_current, 
    probs = c(0.0175, 0.9825)
  )
  
  # Add results to the dataframe
  top10_current$Predict_Mean  <- predict_top10_resid[, "Estimate"]
  top10_current$Predict_lower <- predict_top10_resid[, "Q1.75"]
  top10_current$Predict_upper <- predict_top10_resid[, "Q98.25"]
  
  cat("\n=== TOP 10 Current Season Predictions (Model with Residuals) ===\n")
  print(top10_current[, c("Team", "RS_NRtg", "WinPct_Resid",
                          "Predict_Mean", "Predict_lower", "Predict_upper")])
  
  # Save results to CSV
  output_pred_csv <- file.path(output_dir, "current_season_top10_predictions.csv")
  write.csv(top10_current, output_pred_csv, row.names = FALSE)
  cat("\nSaved top 10 predictions to:", output_pred_csv, "\n")
  
  ###############################################################################
  # 9. VISUALIZATION - CURRENT SEASON PREDICTIONS
  ###############################################################################
  
  # Create prediction plot with confidence intervals
  plot_pred <- ggplot(top10_current, 
                     aes(y = reorder(Team, Predict_Mean))) +
    # Vertical line at x=0
    geom_vline(xintercept = 0, linetype = "dashed") +
    
    # Predictions with CI
    geom_errorbarh(
      aes(xmin = Predict_lower, xmax = Predict_upper),
      height = 0.2, 
      color = "blue",
      alpha = 0.7
    ) +
    geom_point(aes(x = Predict_Mean), shape = 19, color = "blue", size = 2) +
    
    labs(
      title = "Predicted Playoff Net Rating for Top 10 Teams",
      subtitle = "Using Model with Win Percentage Residuals\nHorizontal bars = 96.5% Credible Intervals",
      x = "Predicted Playoff Net Rating",
      y = "Team"
    ) +
    theme_white()
  
  # Print and save the plot
  print(plot_pred)
  ggsave(file.path(output_dir, "top10_predictions.png"),
         plot_pred, width = 10, height = 7)
} else {
  cat("Warning: Cannot make predictions without Win Percentage residuals.\n")
}

# Create interaction visualization
message("\n=== CREATING INTERACTION VISUALIZATION ===")

# Create a grid of values for the predictors
nrtg_range <- seq(min(merged_data$RS_NRtg), max(merged_data$RS_NRtg), length.out = 20)
resid_values <- c(-0.1, 0, 0.1)  # Low, average, and high residual values
grid_data <- expand.grid(
  RS_NRtg = nrtg_range,
  WinPct_Resid = resid_values
)

# Predict values for the grid
grid_predictions <- fitted(
  bayes_model_resid,
  newdata = grid_data,
  probs = c(0.025, 0.975)
)

# Add predictions to the grid data
grid_data$prediction <- grid_predictions[, "Estimate"]
grid_data$lower <- grid_predictions[, "Q2.5"]
grid_data$upper <- grid_predictions[, "Q97.5"]

# Create a factor for better labeling
grid_data$resid_group <- factor(
  grid_data$WinPct_Resid,
  levels = c(-0.1, 0, 0.1),
  labels = c("Underperforming (-0.1)", "Average (0)", "Overperforming (+0.1)")
)

# Create interaction plot
interaction_plot <- ggplot(grid_data, aes(x = RS_NRtg, y = prediction, color = resid_group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = resid_group), alpha = 0.2, color = NA) +
  labs(
    title = "Interaction Effect Between Net Rating and Win Percentage Residuals",
    subtitle = "Predicted Playoff Net Rating with 95% credible intervals",
    x = "Regular Season Net Rating",
    y = "Predicted Playoff Net Rating",
    color = "Win % Residual Group",
    fill = "Win % Residual Group"
  ) +
  theme_white() +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1")

# Print and save the interaction plot
print(interaction_plot)
ggsave(file.path(output_dir, "interaction_effect.png"), interaction_plot, width = 10, height = 7)

cat("\n=== DONE: NBA Championship Prediction Analysis with Win Percentage Residuals and Interaction ===\n")

# Save Bayesian regression summary to text
sink(file.path(output_dir, "bayesian_regression_summary.txt"))
cat("\n==== Bayesian Model Summary with Win Percentage Residuals ====\n")
print(summary(bayes_model_resid, probs = c(0.0175, 0.9825)))
cat("\nBayesian R^2:\n")
print(bayes_R2(bayes_model_resid, probs = c(0.0175, 0.9825)))
sink() # close the sink
