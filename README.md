# NBA Championship Prediction Project - Introduction to R

## Dataset Selection

I've been an Oklahoma City Thunder fan for over a decade, experiencing both highs and lows as we've been among the league's leading teams but never won a championship. In the current season (about three-quarters complete), an exceptional rebuilding process has culminated in one of the youngest teams in the league that, according to many metrics, is considered historically strong - a remarkable regular season for such an inexperienced team.

Commentators typically resolve this contradiction by separating statistics from intangibles. In layman's terms: "Statistically they should win the championship, but age and experience will prevent them from reaching the top."

This sparked several empirical questions in my research-oriented mind:

Is it actually true that they "should statistically win the championship"? More broadly, what's the relationship between regular season and playoff performance? What impact does regular season performance have on championship odds - that binary outcome where only one team wins each year?

As for those "unmeasured intangibles," I'll approach them scientifically: explain as much variance as possible while respecting statistical noise, whether from model limitations or basketball's inherent randomness.

And ultimately: What are Oklahoma City's chances of winning this year's championship?

## Preliminary Data Presentation

For this project, I used readily available data with strong, common metrics from basketball-reference: advanced statistics tables for teams in each season and playoffs over the last 51 years. Without API access, I carefully collected data to avoid suspicion and blocking, saving complete pages through simple downloads to avoid repeated requests.

My data collection pipeline consisted of three scripts:

**Script 1: Data Collection**
- Created directories and paths for data storage
- Built a safe downloading function with delays and retry logic
- Developed functions for sequential downloading of HTML pages
- Extracted and saved tables from HTML files
- Downloaded and processed the champions list from nba.com

**Script 2: Preprocessing**
- Created a main function to collect metrics and combine them into one file
- Developed helper functions to process stats tables into clean dataframes
- Added team-year identifiers and champion coding
- Built separate functions for current season data collection
- Cleaned unnecessary columns and handled missing values

**Script 3: Exploratory Analysis**
- Created a scatter plot of win percentage and net rating
- Built functions to analyze the predictive power of top N teams
- Tracked metrics changes over time
- Identified current season leaders
- Generated correlation matrices for all variables

The scatter plot reveals high correlation between net rating and win percentage, with champions clearly concentrated in the upper right quadrant:

![Win Percentage vs Net Rating](https://raw.githubusercontent.com/itamarronel/nba-championship-prediction/main/images/winpct_nrtg_champions.png)

The correlation between these metrics is .97. This presents a collinearity issue for modeling, as these two powerful metrics effectively duplicate each other.

Win percentage and net rating already provide effective predictive heuristics for championship contenders:

![Championships Predicted for Top N Teams](https://raw.githubusercontent.com/itamarronel/nba-championship-prediction/main/images/top_n_champions.png)

The net rating leader won 19 of 51 championships, while the top 3 teams by win percentage captured 41 of 51 championships. The top 5 by wins identified 47 of 51 champions, with only one champion falling outside the top ten in both metrics (Houston's second championship in 1995).

Another challenge is the lack of stability over time. Creating a model spanning five decades means we can't directly compare metrics across eras - Jordan's 1992 Bulls would rank outside today's top 15 in offensive rating. The change in several selected metrics over the years can be seen in the following graph:

![NBA Metrics Changes Over Time](https://raw.githubusercontent.com/itamarronel/nba-championship-prediction/main/images/metrics_over_time.png)

For the current season, OKC finds itself in a tight race with Cleveland at the top of the net rating chart, with Boston close behind:

![Current Season Leaders](https://raw.githubusercontent.com/itamarronel/nba-championship-prediction/main/images/current_top_nrtg.png)

## Research Question and Model Selection

I'll first examine the relationship between regular season and playoff performance using linear regression. Then, since championships are rare binary outcomes, I'll use logistic regression to predict championship probability.

While hierarchical logistic regression would be more appropriate with one champion per season, I've opted for simpler models given my current knowledge (I'll study hierarchical models next semester).

This study is essentially a proof of concept using readily available data. I've focused on team-level metrics rather than incorporating player data, complex season-to-season relationships, or sophisticated validation techniques. If successful, the model could later be enhanced with additional features.

My approach involves two stages:

1. Linear regression between playoff and regular season net rating, adding the effect of winning % to estimate the general relationship .
2. Logistic regression modeling championship odds using available predictors: offensive and defensive ratings, win percentage, age, and the "Four Factors" metrics for both offense and defense (opponent avrages): effective FG%, offensive/defensive rebound rate, turnovers and free throw rate,

The metrics exhibit a hierarchical relationship that complicates modeling:

![DAG](https://raw.githubusercontent.com/itamarronel/nba-championship-prediction/main/images/dagitty-model.png)

To address this, I made two key design decisions:

1. Include the Four Factors metrics only as interactions with their respective ratings, not as main effects.
2. Use win percentage residuals (after accounting for offensive and defensive ratings) instead of raw win percentage.

These decisions help isolate each variable's contribution while reducing collinearity.

## Data Processing

For the linear regression, I used playoff net rating as the dependent variable with multiple predictors: regular season net rating, win percentage residuals, and their interaction. Win percentage residuals represent how much a team over- or under-performs in win percentage compared to what would be expected based on their net rating. This captures a team's ability to win games beyond what their efficiency metrics would predict - potentially reflecting intangibles like clutch performance, coaching, or experience.

For the logistic regression, the dependent variable was championship status (0/1). Main predictors included standardized offensive rating, defensive rating, win percentage residuals, and age. The Four Factors metrics were included only as interactions with their respective ratings. I restricted the analysis to competitive teams (win percentage ≥ 0.5).

I used several packages beyond the course curriculum, including:
- httr for controlled HTML downloading
- rvest for HTML parsing
- stringr for text extraction
- scales, bayesplot, pROC, and caret for visualization and model evaluation

## Data Analysis

### Linear Regression: Regular Season vs. Playoff Performance (Script 4)

**Script 4** analyzes the relationship between regular season performance metrics and playoff results through Bayesian regression. The script calculates win percentage residuals, merges regular and playoff data, conducts correlation analysis, and visualizes the relationships through scatter plots. The Bayesian model includes regular season net rating, win percentage residuals, and their interaction as predictors of playoff net rating.

I began by examining the correlation between regular season and playoff performance. I found a correlation of .584 between regular season and playoff net rating:

![Relationship between Regular Season and Playoff Net Rating](https://raw.githubusercontent.com/itamarronel/nba-championship-prediction/main/images/rs_vs_po_all_teams.png)

When analyzing win percentage residuals, I discovered a much weaker correlation (0.048) with playoff net rating:

![Relationship between Win Percentage Residuals and Playoff Net Rating](https://raw.githubusercontent.com/itamarronel/nba-championship-prediction/main/images/winpct_resid_vs_po_nrtg.png)

To gain deeper insights, I built a multiple Bayesian regression model that included regular season net rating, win percentage residuals, and their interaction as predictors. This model addresses the key question: does a team's ability to win beyond what their net rating would predict influence their playoff performance? The results showed:

- Intercept: -7.08 [-7.62, -6.53]
- Regular Season Net Rating coefficient: 1.35 [1.22, 1.47]
- Win Percentage Residuals coefficient: 1.00 [-0.96, 2.96]
- Interaction coefficient: 2.64 [1.13, 4.13]
- Bayesian R²: 0.351 [0.302, 0.396]

![Posterior Distributions - Model with WinPct Residuals](https://raw.githubusercontent.com/itamarronel/nba-championship-prediction/main/images/posterior_model_with_residuals.png)

The interaction term shows strong evidence of a positive effect, suggesting that teams with both high net rating and the ability to win more games than expected (positive residuals) tend to perform better in the playoffs. This is visualized in the interaction plot:

![Interaction Effect Between Net Rating and Win Percentage Residuals](https://raw.githubusercontent.com/itamarronel/nba-championship-prediction/main/images/interaction_effect.png)

The plot shows that teams that overperform in win percentage (green line) see a steeper increase in playoff performance as their regular season net rating increases, compared to teams that underperform (red line).

For the current season's top teams, the model predicts the following playoff net ratings:

![Predicted Playoff Net Rating for Top 10 Teams](https://raw.githubusercontent.com/itamarronel/nba-championship-prediction/main/images/top10_predictions.png)

Cleveland and Oklahoma City are projected to have the highest playoff net ratings, with Boston close behind. However, the wide credible intervals highlight the considerable uncertainty in playoff performance prediction.

### Logistic Regression: Championship Prediction (Script 5)

**Script 5** builds and evaluates three logistic regression models for championship prediction, each with increasing complexity. The script normalizes statistics by season, calculates win percentage residuals, and evaluates model performance through metrics like AIC, ROC curves, and AUC. It also visualizes the distribution of championship probabilities to compare how the models separate champions from non-champions.

I developed three models of increasing complexity:

1. **Simple Model**: Four main predictors only (ORtg_Z, DRtg_Z, WinPct_Resid, Age_Z)
2. **Focused Model**: Main predictors plus selected interactions with strong evidence (ORtg_Z:TOV_Pct_Z, ORtg_Z:ORB_Pct_Z, ORtg_Z:FTr_Z)
3. **Full Model**: Main predictors plus all eight theoretical interactions

The models performed similarly, with all showing good discriminative ability:

![ROC Curve Comparison](https://raw.githubusercontent.com/itamarronel/nba-championship-prediction/main/images/roc_curve_comparison.png)

The distribution of predicted probabilities shows how champions tend to receive higher probabilities, though there's no perfect separation:

![Distribution of Championship Probabilities by Model](https://raw.githubusercontent.com/itamarronel/nba-championship-prediction/main/images/champion_prob_distribution.png)

### Model Evaluation and Current Season Predictions (Script 6)

**Script 6** evaluates model performance by comparing their ability to identify champions through two approaches: (1) using probability thresholds and (2) ranking teams by probability and selecting the top N. The script calculates historical accuracy overall and by decade, creates visualizations of model performance, and generates predictions for the current season.

When evaluating the models on their ability to include the eventual champion among their top 3 predicted teams:

- Simple Model: 70.6% accuracy
- Focused Model: 74.5% accuracy
- Full Model: 76.5% accuracy

The Focused Model correctly identified the eventual champion in its top 3 predicted teams approximately 75% of the time, as shown in this visualization of actual champion ranks over the years:

![Rank of Actual Champion in Focused Model Predictions by Season](https://raw.githubusercontent.com/itamarronel/nba-championship-prediction/main/images/champion_rank_by_season.png)

Compared to the simple heuristic of just looking at the top N teams by Net Rating or Win Percentage shown earlier, our models provide a modest improvement in predictive accuracy:

![Predicted Championships for Top N Teams](https://raw.githubusercontent.com/itamarronel/nba-championship-prediction/main/images/top_n_champions_models.png)

Using probability thresholds, the models identified over 90% of champions (92.2% for the simple and focused models, 94.1% for the full model), showing strong ability to identify true championship contenders, though they were less effective at differentiating between them.

For the current season (2024-2025), the Focused Model predicts:

1. **Cleveland Cavaliers**: 34.1% championship probability
2. **Oklahoma City Thunder**: 18.4% championship probability
3. **Boston Celtics**: 8.5% championship probability
4. **Los Angeles Lakers**: 2.1% championship probability
5. **Denver Nuggets**: 1.6% championship probability

![Current NBA Season Championship Probability](https://raw.githubusercontent.com/itamarronel/nba-championship-prediction/main/images/current_season_comparison.png)

My beloved Oklahoma City Thunder is certainly a strong championship contender but doesn't have the highest probability. Cleveland's higher ranking stems largely from their win percentage residual (they're winning more than expected given their ratings). Oklahoma City's youth slightly reduces their odds, but their impressive net rating keeps them firmly in contention. Their current win-to-Pythagorean win difference is -1, while Cleveland's is +3, which may partially explain the probability difference.

## Conclusions

There is a strong relationship between regular season performance and playoff outcomes, particularly championship probability, but as expected, there's considerable noise that makes precise predictions difficult.

Unsurprisingly, younger teams tend to be less successful in the postseason. Interestingly, good teams that win more games than their net rating would predict tend to have greater playoff success.

Both of these factors slightly work against Oklahoma City's odds, but not enough to significantly change their status as a leading championship contender. Are they "statistically supposed to win the championship"? No, they don't even have the highest predicted odds. But I'm still optimistic. This is our year.
