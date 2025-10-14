#################################################################################
### Football Player Market Valuation Prediction
### Using Machine Learning Methods on European Top 5 Leagues Data
###
### Author: Minxing Huang
### Affiliation: London School of Economics and Political Science
###              MSc Econometrics and Mathematical Economics
### Contact: m.huang45@lse.ac.uk
### Date: October 2025
### 
### Purpose: This script demonstrates data cleaning, feature engineering, and 
###          machine learning modeling skills for predoc position applications
###
### Data Source: Transfermarkt dataset from Kaggle
###              (davidcariboo/player-scores)
###
### This file outputs the following:
###
### 1. cleaned_player_data.csv - Cleaned dataset with engineered features
###    Used for: All subsequent analyses
###
### 2. model_predictions.csv - Test set predictions from Random Forest model
###    Used for: Model evaluation and diagnostics
###
### 3. model_evaluation_summary.csv - Performance metrics comparison
###    Used for: Presenting results in application materials
###
### 4. Figures:
###    - fig1_value_distribution.png - Market value distribution
###    - fig2_value_by_position.png - Value comparison across positions
###    - fig3_age_value_relationship.png - Age-value profile
###    - fig4_predicted_vs_actual.png - Model fit visualization
###    - fig5_feature_importance.png - Variable importance ranking
###
### Instructions:
### 1. Set working directory in Section 0
### 2. Ensure all required packages are installed (Section 1)
### 3. Place raw data files in data/ subdirectory
### 4. Run entire script to reproduce results
###
#################################################################################


#################################################################################
#################################################################################
#################### 0. SETUP: PATHS AND DIRECTORIES ############################
#################################################################################
#################################################################################

# Clear workspace ----
rm(list = ls())

# Set project root directory ----
# MODIFY THIS PATH to match your local directory
project_root <- "/Users/huangminxing/Documents/Rscript/Football_Valuation_Project"

# Define subdirectories ----
data_dir <- file.path(project_root, "data")
output_dir <- file.path(project_root, "output")
figures_dir <- file.path(output_dir, "figures")
tables_dir <- file.path(output_dir, "tables")

# Create directories if they don't exist ----
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)

# Set working directory ----
setwd(project_root)

cat("Working directory:", getwd(), "\n")
cat("Data directory:", data_dir, "\n")
cat("Output directory:", output_dir, "\n\n")


#################################################################################
#################################################################################
#################### 1. LOAD REQUIRED PACKAGES ##################################
#################################################################################
#################################################################################

# Function to install and load packages ----
load_packages <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      cat("Installing package:", pkg, "\n")
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
  cat("All packages loaded successfully!\n\n")
}

# List of required packages ----
required_packages <- c(
  "tidyverse",    # Data manipulation and visualization
  "tidymodels",   # Unified modeling framework
  "lubridate",    # Date handling
  "scales",       # Formatting outputs
  "skimr",        # Quick data overview
  "ranger",       # Random forest engine
  "vip",          # Variable importance plots
  "corrplot",     # Correlation visualization
  "knitr",        # Table formatting
  "kableExtra"    # Enhanced tables
)

# Load all packages ----
load_packages(required_packages)


#################################################################################
#################################################################################
#################### 2. READ RAW DATA ###########################################
#################################################################################
#################################################################################


# Read all data files ----
players <- read_csv(file.path(data_dir, "players.csv"), 
                    show_col_types = FALSE)
player_valuations <- read_csv(file.path(data_dir, "player_valuations.csv"), 
                              show_col_types = FALSE)
appearances <- read_csv(file.path(data_dir, "appearances.csv"), 
                        show_col_types = FALSE)
games <- read_csv(file.path(data_dir, "games.csv"), 
                  show_col_types = FALSE)
competitions <- read_csv(file.path(data_dir, "competitions.csv"), 
                         show_col_types = FALSE)
clubs <- read_csv(file.path(data_dir, "clubs.csv"), 
                  show_col_types = FALSE)

# Report data dimensions ----
cat("Data files loaded:\n")
cat("- Players:", nrow(players), "rows\n")
cat("- Player valuations:", nrow(player_valuations), "rows\n")
cat("- Appearances:", nrow(appearances), "rows\n")
cat("- Games:", nrow(games), "rows\n")
cat("- Competitions:", nrow(competitions), "rows\n")
cat("- Clubs:", nrow(clubs), "rows\n\n")


#################################################################################
#################################################################################
#################### 3. DATA PREPROCESSING ######################################
#################################################################################
#################################################################################


# 3.1 Convert date format in games table ----
games <- games %>%
  mutate(date = ymd(date))



#################################################################################
#################################################################################
#################### 4. SAMPLE SELECTION ########################################
#################################################################################
#################################################################################


# 4.1 Identify top 5 European leagues ----
# These are the most liquid and competitive labor markets in European football
top5_leagues <- competitions %>%
  filter(
    name %in% c(
      "premier-league",    # English Premier League
      "laliga",            # Spanish La Liga  
      "bundesliga",        # German Bundesliga
      "serie-a",           # Italian Serie A
      "ligue-1"            # French Ligue 1
    )
  ) %>%
  pull(competition_id)

# This step could be also done by hand. 

cat("Top 5 leagues identified:", length(top5_leagues), "leagues\n")
# Supposed to be: Top 5 leagues identified: 5 leagues

# 4.2 Filter games in top 5 leagues ----
top5_games <- games %>%
  filter(competition_id %in% top5_leagues) %>%
  select(game_id, date, competition_id, season, round, 
         home_club_id, away_club_id, home_club_goals, away_club_goals,
         home_club_name, away_club_name, aggregate, competition_type)

cat("Games in top 5 leagues:", nrow(top5_games), "games\n")

# 4.3 Create sample indicator for tracking ----
# Following best practice: track which observations are in final sample
in_top5_games <- games %>%
  mutate(in_top5_sample = if_else(game_id %in% top5_games$game_id, 1, 0))

cat("Sample indicator created\n")


#################################################################################
#################################################################################
#################### 5. CONSTRUCT PLAYER-LEVEL VARIABLES ########################
#################################################################################
#################################################################################


# 5.1 Join appearances with top 5 league games
# Remove date from appearances table to avoid duplicate columns
top5_appearances <- appearances %>%
  select(-date) %>%  # Drop date column from appearances
  inner_join(
    top5_games %>% select(game_id, date, competition_id),
    by = "game_id"
  )

# Verify date format ----
cat("Date column class in top5_appearances:", class(top5_appearances$date), "\n")
cat("Appearances in top 5 leagues:", nrow(top5_appearances), "rows\n")

# 5.2 Calculate player statistics ----
# Aggregate performance metrics at player level
player_stats <- top5_appearances %>%
  group_by(player_id) %>%
  summarise(
    total_appearances = n(),
    total_goals = sum(goals, na.rm = TRUE),
    total_assists = sum(assists, na.rm = TRUE),
    total_minutes = sum(minutes_played, na.rm = TRUE),
    yellow_cards = sum(yellow_cards, na.rm = TRUE),
    red_cards = sum(red_cards, na.rm = TRUE),
    seasons_played = n_distinct(year(date)),
    .groups = "drop"
  )

# 5.3 Add player names and current club information ----
player_stats <- player_stats %>%
  left_join(
    players %>% select(player_id, name, current_club_id, date_of_birth, 
                       position, country_of_citizenship),
    by = "player_id"
  ) %>%
  left_join(
    clubs %>% select(club_id, name),
    by = c("current_club_id" = "club_id")
  ) %>%
  rename(
    player_name = name.x,
    club_name = name.y
  )

cat("Player statistics calculated:", nrow(player_stats), "players\n")


#################################################################################
#################################################################################
#################### 6. FILTER CORE PLAYERS (EXCLUDE FRINGE) ####################
#################################################################################
#################################################################################


# Define fringe player criteria 
# Rationale: Players with minimal appearances have unstable/unreliable valuations
# Criteria informed by labor economics literature on soccer markets
MIN_APPEARANCES <- 20
MIN_MINUTES <- 450

# Create filter indicator (for tracking)
player_stats <- player_stats %>%
  mutate(
    is_fringe = if_else(
      total_appearances < MIN_APPEARANCES | total_minutes < MIN_MINUTES,
      1, 0
    )
  )

# Apply filter 
core_players <- player_stats %>%
  filter(is_fringe == 0)

cat("Players before filtering:", nrow(player_stats), "\n")
cat("Core players after filtering:", nrow(core_players), "\n")
cat("Filtering rate:", 
    round((1 - nrow(core_players)/nrow(player_stats)) * 100, 1), "%\n\n")


#################################################################################
#################################################################################
#################### 7. MERGE WITH VALUATION DATA ###############################
#################################################################################
#################################################################################


# 7.1 Get latest valuation for each player
latest_valuations <- player_valuations %>%
  group_by(player_id) %>%
  arrange(desc(date)) %>%
  slice(1) %>%
  ungroup() %>%
  select(player_id, date, market_value_in_eur) %>%
  filter(
    !is.na(market_value_in_eur),
    market_value_in_eur > 0
  )

cat("Latest valuations extracted:", nrow(latest_valuations), "players\n")

# 7.2 Merge with player statistics
analysis_data <- core_players %>%
  inner_join(latest_valuations, by = "player_id", suffix = c("", "_valuation")) %>%
  filter(
    !is.na(position),
    !is.na(market_value_in_eur)
  )

cat("Final analysis sample:", nrow(analysis_data), "players\n")
cat("Market value range: €", 
    format(min(analysis_data$market_value_in_eur), big.mark = ","), 
    " to €",
    format(max(analysis_data$market_value_in_eur), big.mark = ","), "\n\n")


#################################################################################
#################################################################################
#################### 8. FEATURE ENGINEERING #####################################
#################################################################################
#################################################################################



modeling_data <- analysis_data %>%
  mutate(
    # Age calculation 
    age = as.numeric(difftime(Sys.Date(), date_of_birth, units = "days")) / 365.25,
    
    # Per-90-minute performance metrics 
    # Standard in football analytics literature
    goals_per_90 = (total_goals / total_minutes) * 90,
    assists_per_90 = (total_assists / total_minutes) * 90,
    
    # Position grouping
    # Consolidate detailed positions into broader categories
    position_group = case_when(
      str_detect(position, "Attack") ~ "Forward",
      str_detect(position, "Midfield") ~ "Midfielder",
      str_detect(position, "Defence|Defender|Back") ~ "Defender",
      str_detect(position, "Goalkeeper") ~ "Goalkeeper",
      TRUE ~ "Other"
    ),
    
    # Log transformation of market value 
    # Addresses right-skewed distribution, standard in hedonic pricing models
    log_value = log(market_value_in_eur + 1)
  ) %>%
  # Select modeling variables
  select(
    player_id, player_name, club_name, age, position_group,
    total_appearances, total_goals, total_assists, total_minutes,
    goals_per_90, assists_per_90, yellow_cards, red_cards, 
    seasons_played, market_value_in_eur, log_value,
    country_of_citizenship, date_of_birth, position
  )

cat("Features engineered successfully\n")
cat("Final variables:", ncol(modeling_data), "\n")
cat("Final observations:", nrow(modeling_data), "\n\n")

# Report position distribution 
cat("Position distribution:\n")
print(modeling_data %>% count(position_group) %>% arrange(desc(n)))


#################################################################################
#################################################################################
#################### 9. DESCRIPTIVE STATISTICS ##################################
#################################################################################
#################################################################################



# 9.1 Summary statistics
desc_stats <- modeling_data %>%
  select(age, total_appearances, total_goals, total_assists, 
         total_minutes, market_value_in_eur) %>%
  summary()

print(desc_stats)

# 9.2 Save summary statistics table
summary_table <- modeling_data %>%
  select(age, total_appearances, total_goals, total_assists, 
         goals_per_90, assists_per_90, market_value_in_eur) %>%
  skim() %>%
  as_tibble()

write_csv(summary_table, file.path(tables_dir, "summary_statistics.csv"))
cat("\nSummary statistics saved to:", 
    file.path(tables_dir, "summary_statistics.csv"), "\n")


#################################################################################
#################################################################################
#################### 10. VISUALIZATIONS #########################################
#################################################################################
#################################################################################



# Set global theme for consistency 
theme_set(theme_minimal(base_size = 11))

# 10.1 Market value distribution
# NOTE: Figure 1 in application materials
fig1 <- ggplot(modeling_data, aes(x = market_value_in_eur)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  scale_x_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Distribution of Player Market Values in Top 5 European Leagues",
    subtitle = paste0("Sample: ", nrow(modeling_data), 
                      " core players with ≥20 appearances"),
    x = "Market Value (Million EUR)",
    y = "Number of Players",
    caption = "Data source: Transfermarkt via Kaggle"
  ) +
  theme_minimal()

ggsave(file.path(figures_dir, "fig1_value_distribution.png"), 
       fig1, width = 8, height = 5, dpi = 300)
cat("Figure 1 saved\n")

# 10.2 Market value by position
# NOTE: Figure 2 in application materials
fig2 <- modeling_data %>%
  ggplot(aes(x = position_group, y = market_value_in_eur, 
             fill = position_group)) +
  geom_boxplot() +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = "M"),
    trans = "log10"
  ) +
  labs(
    title = "Market Value Comparison by Position",
    subtitle = "Log scale to account for right-skewed distribution",
    x = "Position",
    y = "Market Value (Million EUR, Log Scale)",
    caption = "Data source: Transfermarkt via Kaggle"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(file.path(figures_dir, "fig2_value_by_position.png"), 
       fig2, width = 8, height = 5, dpi = 300)
cat("Figure 2 saved\n")

# 10.3 Age-value relationship
# NOTE: Figure 3 in application materials
# Shows inverted U-shape consistent with human capital theory
fig3 <- modeling_data %>%
  ggplot(aes(x = age, y = market_value_in_eur)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = "M"),
    trans = "log10"
  ) +
  labs(
    title = "Relationship between Age and Market Value",
    subtitle = "Inverted U-shape consistent with human capital depreciation",
    x = "Age (years)",
    y = "Market Value (Million EUR, Log Scale)",
    caption = "Red line: LOESS smoothed fit with 95% confidence interval"
  ) +
  theme_minimal()

ggsave(file.path(figures_dir, "fig3_age_value_relationship.png"), 
       fig3, width = 8, height = 5, dpi = 300)
cat("Figure 3 saved\n")

# 10.4 Correlation matrix ----
# NOTE: Appendix Figure A1
correlation_data <- modeling_data %>%
  select(age, total_appearances, total_goals, total_assists,
         goals_per_90, assists_per_90, yellow_cards,
         seasons_played, log_value) %>%
  cor(use = "complete.obs")

png(file.path(figures_dir, "figA1_correlation_matrix.png"), 
    width = 8, height = 6, units = "in", res = 300)
corrplot(correlation_data, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix of Key Variables",
         mar = c(0, 0, 2, 0))
dev.off()
cat("Correlation matrix saved\n\n")


#################################################################################
#################################################################################
#################### 11. PREPARE MODELING DATA ##################################
#################################################################################
#################################################################################


# 11.1 Select modeling variables
model_df <- modeling_data %>%
  select(
    log_value,              # Dependent variable
    age,                    # Continuous predictor
    position_group,         # Categorical predictor
    total_appearances,      # Experience proxy
    goals_per_90,          # Offensive productivity
    assists_per_90,        # Offensive productivity
    yellow_cards,          # Disciplinary record
    seasons_played         # Career length proxy
  ) %>%
  drop_na()

cat("Modeling dataset prepared\n")
cat("- Observations:", nrow(model_df), "\n")
cat("- Variables:", ncol(model_df), "\n\n")

# 11.2 Data split with stratification ----
# Stratified sampling ensures similar distribution of target variable
set.seed(123)  # For reproducibility
data_split <- initial_split(model_df, prop = 0.8, strata = log_value)
train_data <- training(data_split)
test_data <- testing(data_split)

cat("Data split (stratified on log_value):\n")
cat("- Training set:", nrow(train_data), "observations\n")
cat("- Test set:", nrow(test_data), "observations\n\n")


#################################################################################
#################################################################################
#################### 12. MODEL SPECIFICATION ####################################
#################################################################################
#################################################################################


# 12.1 Define preprocessing recipe 
value_recipe <- recipe(log_value ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors()) %>%    # One-hot encoding
  step_normalize(all_numeric_predictors())    # Standardization



# 12.2 Linear regression specification
lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

lm_workflow <- workflow() %>%
  add_recipe(value_recipe) %>%
  add_model(lm_spec)


# 12.3 Random forest specification
# Using ranger for computational efficiency
rf_spec <- rand_forest(
  trees = 500,           # Number of trees
  mtry = NULL,          # Will use default (sqrt(p))
  min_n = 5             # Minimum node size
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

rf_workflow <- workflow() %>%
  add_recipe(value_recipe) %>%
  add_model(rf_spec)



#################################################################################
#################################################################################
#################### 13. MODEL TRAINING #########################################
#################################################################################
#################################################################################



# 13.1 Train linear regression ----
cat("Training linear regression model...\n")
lm_fit <- lm_workflow %>%
  fit(train_data)
cat("Linear regression trained\n")

# 13.2 Train random forest ----
cat("Training random forest model...\n")
rf_fit <- rf_workflow %>%
  fit(train_data)
cat("Random forest trained\n\n")


#################################################################################
#################################################################################
#################### 14. MODEL EVALUATION #######################################
#################################################################################

# 14.1 Generate predictions on test set 
lm_pred <- predict(lm_fit, test_data) %>%
  bind_cols(test_data)

rf_pred <- predict(rf_fit, test_data) %>%
  bind_cols(test_data)

# 14.2 Calculate evaluation metrics ----
metrics_set <- metric_set(rmse, rsq, mae, mape)

lm_metrics <- lm_pred %>%
  metrics_set(truth = log_value, estimate = .pred)

rf_metrics <- rf_pred %>%
  metrics_set(truth = log_value, estimate = .pred)

# 14.3 Model comparison table ----
model_comparison <- bind_rows(
  lm_metrics %>% mutate(model = "Linear Regression"),
  rf_metrics %>% mutate(model = "Random Forest")
) %>%
  select(model, .metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  mutate(across(where(is.numeric), ~round(., 4)))

print(model_comparison)

# Save model comparison ----
write_csv(model_comparison, 
          file.path(tables_dir, "model_evaluation_summary.csv"))
cat("\nModel evaluation saved to:", 
    file.path(tables_dir, "model_evaluation_summary.csv"), "\n")

# 14.4 Interpretation 

rf_rsq <- rf_metrics %>% filter(.metric == "rsq") %>% pull(.estimate)
rf_rmse <- rf_metrics %>% filter(.metric == "rmse") %>% pull(.estimate)

cat("Random Forest Performance:\n")
cat("- R-squared:", round(rf_rsq, 3), 
    paste0("(", round(rf_rsq * 100, 1), "% variance explained)\n"))
cat("- RMSE (log scale):", round(rf_rmse, 3), "\n")
cat("- Interpretation: The model explains", round(rf_rsq * 100, 1), 
    "% of variation in player valuations,\n")
cat("  which is considered excellent for sports labor market predictions.\n\n")


#################################################################################
#################################################################################
#################### 15. ADDITIONAL DIAGNOSTICS #################################
#################################################################################
#################################################################################


# 15.1 Predicted vs Actual (Random Forest) 
# NOTE: Figure 4 in application materials
fig4 <- rf_pred %>%
  ggplot(aes(x = log_value, y = .pred)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", 
              linetype = "dashed", linewidth = 1) +
  labs(
    title = "Random Forest Model: Predicted vs Actual Values",
    subtitle = paste0("R² = ", round(rf_rsq, 3), 
                      " | RMSE = ", round(rf_rmse, 3)),
    x = "Actual Market Value (Log Scale)",
    y = "Predicted Market Value (Log Scale)",
    caption = "Red dashed line: Perfect prediction (45° line)"
  ) +
  theme_minimal()

ggsave(file.path(figures_dir, "fig4_predicted_vs_actual.png"), 
       fig4, width = 8, height = 6, dpi = 300)
cat("Figure 4 saved\n")

# 15.2 Feature importance ----
# NOTE: Figure 5 in application materials
fig5 <- rf_fit %>%
  extract_fit_parsnip() %>%
  vip(num_features = 10, aesthetics = list(fill = "steelblue")) +
  labs(
    title = "Feature Importance Ranking",
    subtitle = "Based on impurity reduction in Random Forest model",
    x = "Importance",
    y = "Feature"
  ) +
  theme_minimal()

ggsave(file.path(figures_dir, "fig5_feature_importance.png"), 
       fig5, width = 8, height = 6, dpi = 300)
cat("Figure 5 saved\n")

# 15.3 Residual analysis

rf_residuals <- rf_pred %>%
  mutate(
    residual = log_value - .pred,
    abs_residual = abs(residual)
  )

figA2 <- rf_residuals %>%
  ggplot(aes(x = .pred, y = residual)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_smooth(method = "loess", color = "darkred", se = TRUE) +
  labs(
    title = "Residual Plot: Random Forest Model",
    subtitle = "Good model should show random scatter around zero",
    x = "Predicted Market Value (Log Scale)",
    y = "Residuals (Actual - Predicted)",
    caption = "Red line: Zero residual | Dark red: LOESS smoothed pattern"
  ) +
  theme_minimal()

ggsave(file.path(figures_dir, "figA2_residual_plot.png"), 
       figA2, width = 8, height = 5, dpi = 300)
cat("Residual plot saved\n\n")


#################################################################################
#################################################################################
#################### 16. SAVE RESULTS ###########################################
#################################################################################
#################################################################################

# 16.1 Save cleaned data 
# NOTE: Output 1 - For subsequent analyses
write_csv(modeling_data, file.path(output_dir, "cleaned_player_data.csv"))
cat("Cleaned data saved:", 
    file.path(output_dir, "cleaned_player_data.csv"), "\n")

# 16.2 Save predictions
# NOTE: Output 2 - For model evaluation
rf_pred_export <- rf_pred %>%
  mutate(player_id = test_data$player_id) %>%
  left_join(
    modeling_data %>% select(player_id, player_name, club_name, 
                             age, position_group),
    by = "player_id"
  ) %>%
  mutate(
    actual_value_eur = exp(log_value),
    predicted_value_eur = exp(.pred),
    error_eur = actual_value_eur - predicted_value_eur,
    error_pct = (error_eur / actual_value_eur) * 100
  ) %>%
  select(player_id, player_name, club_name, age, position_group,
         actual_value_eur, predicted_value_eur, error_eur, error_pct)

write_csv(rf_pred_export, file.path(output_dir, "model_predictions.csv"))
cat("Model predictions saved:", 
    file.path(output_dir, "model_predictions.csv"), "\n")

# 16.3 Create README file ----
readme_content <- paste0(
  "# Football Player Valuation Prediction\n\n",
  "## Project Overview\n",
  "This analysis predicts football player market values using machine learning methods.\n\n",
  "## Key Results\n",
  "- Sample size: ", nrow(modeling_data), " players from top 5 European leagues\n",
  "- Random Forest R²: ", round(rf_rsq, 3), " (", round(rf_rsq * 100, 1), "% variance explained)\n",
  "- RMSE: ", round(rf_rmse, 3), " (log scale)\n\n",
  "## Files Generated\n",
  "### Data\n",
  "1. cleaned_player_data.csv - Cleaned dataset with engineered features\n",
  "2. model_predictions.csv - Test set predictions from Random Forest\n\n",
  "### Tables\n",
  "1. summary_statistics.csv - Descriptive statistics\n",
  "2. model_evaluation_summary.csv - Model performance metrics\n\n",
  "### Figures\n",
  "1. fig1_value_distribution.png - Market value distribution\n",
  "2. fig2_value_by_position.png - Value by position boxplots\n",
  "3. fig3_age_value_relationship.png - Age-value profile\n",
  "4. fig4_predicted_vs_actual.png - Model fit visualization\n",
  "5. fig5_feature_importance.png - Variable importance\n",
  "6. figA1_correlation_matrix.png - Correlation heatmap\n",
  "7. figA2_residual_plot.png - Residual diagnostics\n\n",
  "## Contact\n",
  "Minxing Huang | m.huang45@lse.ac.uk\n",
  "LSE MSc Econometrics and Mathematical Economics\n",
  "Date: ", format(Sys.Date(), "%B %d, %Y"), "\n"
)

writeLines(readme_content, file.path(output_dir, "README.txt"))
cat("README file created\n\n")


#################################################################################
#################################################################################
#################### 17. SESSION INFO AND COMPLETION ############################
#################################################################################
#################################################################################



cat("All outputs saved to:", output_dir, "\n")
cat("- Figures:", length(list.files(figures_dir)), "files\n")
cat("- Tables:", length(list.files(tables_dir)), "files\n")
cat("- Data:", length(list.files(output_dir, pattern = "*.csv")), "files\n\n")

cat("Session Information:\n")
cat("- R version:", R.version.string, "\n")
cat("- Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("- Platform:", R.version$platform, "\n\n")

# Save session info for reproducibility ----
sink(file.path(output_dir, "session_info.txt"))
cat("Session Information for Reproducibility\n")
cat("========================================\n\n")
sessionInfo()
sink()

cat("Script execution completed successfully!\n")
cat("===========================================\n")

#################################################################################
############################# END OF SCRIPT #####################################
#################################################################################

