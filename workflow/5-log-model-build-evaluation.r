###############################################################################
# NBA Championship Prediction Project
# Script 5: Statistical Modeling
#
# Purpose: Build logistic regression models to analyze championship factors
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
required_packages <- c("dplyr", "tidyr", "pROC", "caret", "ggplot2")
for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# Define paths
base_path <- "/Users/itamar/Documents/GitHub/nba-championship-prediction/data"
input_file <- file.path(base_path, "full_advanced_stats_filtered_complete.csv")
model_output_file <- file.path(base_path, "nba_models.rds")
data_output_file <- file.path(base_path, "nba_relative_data.rds")
output_dir <- file.path(base_path, "results")

# Model configuration
MIN_WIN_PCT <- 0.5
MODEL_VARIABLES <- c("WinPct", "NRtg", "ORtg", "DRtg", "Age",
                     "eFG", "TOV_Pct", "ORB_Pct", "FTr",
                     "Opp_eFG", "Opp_TOV", "DRB_Pct", "Opp_FTr")

# Plot configuration
PLOT_WIDTH <- 10
PLOT_HEIGHT <- 7
PLOT_RES <- 300

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

# Set seed for reproducibility
set.seed(123)

###############################################################################
# 2. LOAD AND PREPARE DATA
###############################################################################
message("Loading pre-processed NBA data from ", basename(input_file))
nba_data <- read.csv(input_file)

# Make sure Champion is a factor
nba_data$Champion <- factor(nba_data$Champion, levels = c(0, 1))

# Create required columns directly
nba_data$Season <- as.character(nba_data$SeasonYear)
nba_data$Team_Season <- paste(nba_data$Team, nba_data$SeasonYear)
nba_data$WinPct <- nba_data$W / (nba_data$W + nba_data$L)

# Display data summary
message("\n=== DATA SUMMARY ===")
message("Total rows: ", nrow(nba_data))
message("Unique seasons: ", length(unique(nba_data$Season)))
message("Champions: ", sum(nba_data$Champion == 1, na.rm = TRUE))

# Use variables that are available in the dataset
model_vars <- intersect(MODEL_VARIABLES, names(nba_data))
message("\nUsing the following variables in the model: ", paste(model_vars, collapse = ", "))

###############################################################################
# 3. NORMALIZE STATISTICS BY SEASON
###############################################################################
message("\n=== NORMALIZING STATISTICS RELATIVE TO EACH SEASON (ALL TEAMS) ===")

# Normalize all stats for all teams
for(col in model_vars) {
  nba_data <- nba_data %>%
    group_by(Season) %>%
    mutate(!!paste0(col, "_Z") := 
             (!!sym(col) - mean(!!sym(col), na.rm = TRUE)) /
             sd(!!sym(col), na.rm = TRUE)) %>%
    ungroup()
  message("Normalized ", col, " by season (all teams)")
}

# Filter to competitive teams
message("\n=== FILTERING COMPETITIVE TEAMS ===")
nba_competitive <- nba_data %>%
  filter(WinPct >= MIN_WIN_PCT)

message("Filtered to ", nrow(nba_competitive), " competitive teams (>= ",
        MIN_WIN_PCT*100, "% wins)")
message("Champions in competitive dataset: ", sum(nba_competitive$Champion == 1, na.rm = TRUE))

###############################################################################
# 4. CREATE WIN PERCENTAGE RESIDUALS 
###############################################################################
message("\n=== Creating WinPct residual after partialing out ORtg and DRtg ===")

# Filter rows with complete data for modeling
z_vars <- paste0(model_vars, "_Z")
complete_rows <- complete.cases(nba_competitive[, c("Champion", z_vars)])
model_data <- nba_competitive[complete_rows, ]
message("Using ", sum(complete_rows), " complete rows for modeling")

# Calculate residuals using all teams
resid_formula <- "WinPct_Z ~ ORtg_Z + DRtg_Z"
fit_winpct_all <- lm(as.formula(resid_formula), data = nba_data)

# Extract residuals for competitive teams
nba_data$WinPct_Resid_All <- residuals(fit_winpct_all)
model_data$WinPct_Resid <- nba_data$WinPct_Resid_All[match(model_data$Team_Season, nba_data$Team_Season)]

message("Using residuals calculated from all teams")

###############################################################################
# 5. BUILD MULTIPLE LOGISTIC REGRESSION MODELS
###############################################################################
message("\n=== BUILDING CHAMPIONSHIP PREDICTION MODELS ===")

# Model 1: Simple model (4 basic predictors)
simple_formula <- "Champion ~ ORtg_Z + DRtg_Z + WinPct_Resid + Age_Z"
message("Simple Model Formula: ", simple_formula)
simple_model <- glm(as.formula(simple_formula), data = model_data, family = binomial(link = "logit"))

# Model 2: Focused model (4 basic predictors + selected interactions)
focused_formula <- "Champion ~ ORtg_Z + DRtg_Z + WinPct_Resid + Age_Z + 
                    ORtg_Z:TOV_Pct_Z + ORtg_Z:ORB_Pct_Z + ORtg_Z:FTr_Z"
message("Focused Model Formula: ", focused_formula)
focused_model <- glm(as.formula(focused_formula), data = model_data, family = binomial(link = "logit"))

# Model 3: Full model (4 basic predictors + all interactions)
full_formula <- "Champion ~ ORtg_Z + DRtg_Z + WinPct_Resid + Age_Z +
                 (ORtg_Z:eFG_Z) + (ORtg_Z:TOV_Pct_Z) + (ORtg_Z:ORB_Pct_Z) + (ORtg_Z:FTr_Z) +
                 (DRtg_Z:Opp_eFG_Z) + (DRtg_Z:Opp_TOV_Z) + (DRtg_Z:DRB_Pct_Z) + (DRtg_Z:Opp_FTr_Z)"
message("Full Model Formula: ", full_formula)
full_model <- glm(as.formula(full_formula), data = model_data, family = binomial(link = "logit"))

###############################################################################
# 6. MODEL SUMMARIES AND COMPARISON
###############################################################################
message("\n=== DETAILED MODEL SUMMARIES ===")

# Function to extract coefficient details
extract_coef_details <- function(model_summary) {
  coef_df <- as.data.frame(coef(model_summary))
  colnames(coef_df) <- c("Estimate", "Std.Error", "z.value", "p.value")
  coef_df$Variable <- rownames(coef_df)
  coef_df$OddsRatio <- exp(coef_df$Estimate)
  coef_df$CI_Lower <- exp(coef_df$Estimate - 1.96 * coef_df$Std.Error)
  coef_df$CI_Upper <- exp(coef_df$Estimate + 1.96 * coef_df$Std.Error)
  return(coef_df)
}

# Simple model summary
message("\n--- SIMPLE MODEL SUMMARY ---")
simple_summary <- summary(simple_model)
print(simple_summary)
simple_coef <- extract_coef_details(simple_summary)
message("\nSimple Model Coefficients and Odds Ratios:")
print(simple_coef[, c("Variable", "Estimate", "Std.Error", "p.value", "OddsRatio", "CI_Lower", "CI_Upper")])

# Focused model summary
message("\n--- FOCUSED MODEL SUMMARY ---")
focused_summary <- summary(focused_model)
print(focused_summary)
focused_coef <- extract_coef_details(focused_summary)
message("\nFocused Model Coefficients and Odds Ratios:")
print(focused_coef[, c("Variable", "Estimate", "Std.Error", "p.value", "OddsRatio", "CI_Lower", "CI_Upper")])

# Full model summary
message("\n--- FULL MODEL SUMMARY ---")
full_summary <- summary(full_model)
print(full_summary)
full_coef <- extract_coef_details(full_summary)
message("\nFull Model Coefficients and Odds Ratios:")
print(full_coef[, c("Variable", "Estimate", "Std.Error", "p.value", "OddsRatio", "CI_Lower", "CI_Upper")])

# Save coefficient tables
write.csv(simple_coef, file.path(output_dir, "simple_model_coefficients.csv"), row.names = FALSE)
write.csv(focused_coef, file.path(output_dir, "focused_model_coefficients.csv"), row.names = FALSE)
write.csv(full_coef, file.path(output_dir, "full_model_coefficients.csv"), row.names = FALSE)

message("\n=== MODEL COMPARISON ===")

# Create function to summarize model metrics
get_model_metrics <- function(model, name) {
  model_summary <- summary(model)
  auc_val <- auc(roc(model_data$Champion, predict(model, type = "response")))
  
  data.frame(
    Model = name,
    AIC = model_summary$aic,
    Deviance = model_summary$deviance,
    DF = model_summary$df.residual,
    McFadden_R2 = 1 - model_summary$deviance/model_summary$null.deviance,
    AUC = auc_val
  )
}

# Compare models
model_comparison <- rbind(
  get_model_metrics(simple_model, "Simple"),
  get_model_metrics(focused_model, "Focused"),
  get_model_metrics(full_model, "Full")
)

message("\nModel Comparison Summary:")
print(model_comparison)

# Save model comparison
write.csv(model_comparison, file.path(output_dir, "model_comparison.csv"), row.names = FALSE)

# Add predictions from each model to the dataset
model_data$Simple_Prob <- predict(simple_model, type = "response")
model_data$Focused_Prob <- predict(focused_model, type = "response")
model_data$Full_Prob <- predict(full_model, type = "response")

# Display correlation matrix
message("\nCorrelation matrix for selected variables:")
selected_vars <- c("WinPct_Z", "ORtg_Z", "DRtg_Z", "Age_Z",
                   "eFG_Z", "TOV_Pct_Z", "ORB_Pct_Z", "FTr_Z",
                   "Opp_eFG_Z", "Opp_TOV_Z", "DRB_Pct_Z", "Opp_FTr_Z", 
                   "WinPct_Resid", "Full_Prob", "Focused_Prob", "Simple_Prob" )
available_vars <- selected_vars[selected_vars %in% names(model_data)]
cor_matrix <- cor(model_data[available_vars], use = "complete.obs")
print(round(cor_matrix, 2))

###############################################################################
# 7. ROC CURVE COMPARISON
###############################################################################
message("\n=== ROC CURVE COMPARISON ===")

# Calculate ROC objects
roc_simple <- roc(model_data$Champion, model_data$Simple_Prob)
roc_focused <- roc(model_data$Champion, model_data$Focused_Prob)
roc_full <- roc(model_data$Champion, model_data$Full_Prob)

# Find optimal thresholds
threshold_simple <- coords(roc_simple, "best", ret = "threshold")[1]
threshold_focused <- coords(roc_focused, "best", ret = "threshold")[1]
threshold_full <- coords(roc_full, "best", ret = "threshold")[1]

message("Optimal thresholds:")
message("Simple model: ", round(threshold_simple, 4))
message("Focused model: ", round(threshold_focused, 4))
message("Full model: ", round(threshold_full, 4))

# Create ROC comparison plot
roc_plot <- ggroc(list(Simple = roc_simple, Focused = roc_focused, Full = roc_full)) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "gray") +
  scale_color_manual(values = c("Simple" = "blue", "Focused" = "green", "Full" = "red")) +
  labs(
    title = "ROC Curve Comparison for Championship Prediction Models",
    subtitle = paste(
      "AUC: Simple =", round(auc(roc_simple), 3),
      "Focused =", round(auc(roc_focused), 3),
      "Full =", round(auc(roc_full), 3)
    ),
    x = "Specificity",
    y = "Sensitivity"
  ) +
  theme_white()

# Save ROC plot
ggsave(file.path(output_dir, "roc_curve_comparison.png"), roc_plot, 
       width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_RES)
message("ROC curve comparison saved to: ", file.path(output_dir, "roc_curve_comparison.png"))

###############################################################################
# 8. CHAMPION PROBABILITY DISTRIBUTION
###############################################################################
message("\n=== PROBABILITY DISTRIBUTION VISUALIZATION ===")

# Prepare data for plotting
prob_data <- model_data %>%
  select(Team, Season, Champion, Simple_Prob, Focused_Prob, Full_Prob) %>%
  tidyr::pivot_longer(
    cols = c(Simple_Prob, Focused_Prob, Full_Prob),
    names_to = "Model",
    values_to = "ChampProb"
  ) %>%
  mutate(
    Champion_Label = ifelse(Champion == 1, "Champion", "Non-Champion"),
    Model = gsub("_Prob", "", Model)
  )

# Create density plot
prob_dist_plot <- ggplot(prob_data, aes(x = ChampProb, fill = Champion_Label)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Model, ncol = 1) +
  scale_fill_manual(values = c("Non-Champion" = "gray", "Champion" = "green")) +
  labs(
    title = "Distribution of Championship Probabilities by Model",
    subtitle = paste0("Competitive Teams Only (>", MIN_WIN_PCT*100, "% wins)"),
    x = "Predicted Championship Probability",
    y = "Density",
    fill = "Champion Status"
  ) +
  theme_white()

print(prob_dist_plot)

# Save density plot
ggsave(file.path(output_dir, "champion_prob_distribution.png"), prob_dist_plot, 
       width = PLOT_WIDTH, height = PLOT_HEIGHT*1.5, dpi = PLOT_RES)
message("Probability distribution chart saved to: ", file.path(output_dir, "champion_prob_distribution.png"))

###############################################################################
# 9. TOP PREDICTED TEAMS
###############################################################################
message("\n=== TOP PREDICTED TEAMS BY MODEL ===")

# Find top 10 teams by each model
top_teams <- model_data %>%
  mutate(
    Simple_Rank = rank(-Simple_Prob),
    Focused_Rank = rank(-Focused_Prob),
    Full_Rank = rank(-Full_Prob)
  ) %>%
  filter(Simple_Rank <= 10 | Focused_Rank <= 10 | Full_Rank <= 10) %>%
  select(Team, Season, Champion, Simple_Prob, Focused_Prob, Full_Prob, Simple_Rank, Focused_Rank, Full_Rank) 

# Display top teams from simple model
message("\nTop 10 teams by Simple model:")
top_simple <- top_teams %>% 
  arrange(Simple_Rank) %>% 
  filter(Simple_Rank <= 10)
for(i in 1:nrow(top_simple)) {
  is_champ <- ifelse(top_simple$Champion[i] == 1, "CHAMPION", "")
  message(sprintf("%2d. %s (%s): %.1f%% %s", 
                  top_simple$Simple_Rank[i],
                  top_simple$Team[i], 
                  top_simple$Season[i],
                  top_simple$Simple_Prob[i] * 100,
                  is_champ))
}

# Display top teams from Focused model
message("\nTop 10 teams by Focused model:")
top_focused <- top_teams %>% 
  arrange(Focused_Rank) %>% 
  filter(Focused_Rank <= 10)
for(i in 1:nrow(top_focused)) {
  is_champ <- ifelse(top_focused$Champion[i] == 1, "CHAMPION", "")
  message(sprintf("%2d. %s (%s): %.1f%% %s", 
                  top_focused$Focused_Rank[i],
                  top_focused$Team[i], 
                  top_focused$Season[i],
                  top_focused$Focused_Prob[i] * 100,
                  is_champ))
}

# Display top teams from Full model
message("\nTop 10 teams by Full model:")
top_full <- top_teams %>% 
  arrange(Full_Rank) %>% 
  filter(Full_Rank <= 10)
for(i in 1:nrow(top_simple)) {
  is_champ <- ifelse(top_full$Champion[i] == 1, "CHAMPION", "")
  message(sprintf("%2d. %s (%s): %.1f%% %s", 
                  top_full$Full_Rank[i],
                  top_full$Team[i], 
                  top_full$Season[i],
                  top_full$Full_Prob[i] * 100,
                  is_champ))
}

# Save model objects for evaluation script
model_obj <- list(
  simple_model = simple_model,
  focused_model = focused_model,
  full_model = full_model,
  simple_formula = simple_formula,
  focused_formula = focused_formula,
  full_formula = full_formula,
  threshold_simple = threshold_simple,
  threshold_focused = threshold_focused,
  threshold_full = threshold_full,
  winpct_model = fit_winpct_all,
  min_win_pct = MIN_WIN_PCT,
  metrics = model_comparison,
  data_summary = list(
    n_rows = nrow(model_data),
    n_champions = sum(model_data$Champion == 1),
    n_seasons = length(unique(model_data$Season))
  )
)

saveRDS(model_obj, model_output_file)
message("Models saved to: ", basename(model_output_file))

saveRDS(model_data, data_output_file)
message("Processed data saved to: ", basename(data_output_file))

message("\nModel building complete!")
