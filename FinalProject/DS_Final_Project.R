# Install and load required packages
devtools::install_github("JaseZiv/worldfootballR")
library(worldfootballR)
library(dplyr)
library(lubridate)
library(ggplot2)
library(car)

# Retrieve 2023-24 Liverpool match data from fbref
liverpool_matches_2023_24 <- fb_team_match_results(team_url = "https://fbref.com/en/squads/822bd0ba/2023-2024/Liverpool-Stats")

# Filter for Premier League matches only and sort by date
liverpool_matches_2023_24 <- liverpool_matches_2023_24 %>%
  filter(Comp == "Premier League") %>%
  arrange(as.Date(Date))

# Limit to first 34 matches for comparison with next season
liverpool_matches_2023_24 <- liverpool_matches_2023_24 %>%
  head(34)

# Get shooting statistics for 2023-24 season
liverpool_shooting_2023_24 <- fb_team_match_log_stats(
  team_urls = "https://fbref.com/en/squads/822bd0ba/2023-2024/Liverpool-Stats", 
  stat_type = "shooting"
)

# Filter shooting data for Premier League matches
liverpool_shooting_2023_24 <- liverpool_shooting_2023_24 %>%
  filter(Comp == "Premier League")

# Initialize empty dataframe for combined match and shooting data
liverpool_complete_2023_24 <- data.frame()

# Extract match dates for processing
match_dates <- as.Date(liverpool_matches_2023_24$Date)

# Loop through each match date to combine shooting data for and against
for(match_date in match_dates) {
  for_row <- liverpool_shooting_2023_24 %>%
    filter(ForAgainst == "For" & as.Date(Date) == match_date)
  
  against_row <- liverpool_shooting_2023_24 %>%
    filter(ForAgainst == "Against" & as.Date(Date) == match_date)
  
  if(nrow(for_row) == 0 || nrow(against_row) == 0) next
  
  # Select relevant columns from shooting data for Liverpool
  for_row <- for_row %>% 
    select(Date, Opponent, Sh_Standard, Gls_Standard, xG_Expected, Venue, Result)
  
  # Select relevant columns from shooting data against Liverpool
  against_row <- against_row %>% 
    select(Sh_Standard, Gls_Standard, xG_Expected)
  
  # Combine data and calculate derived metrics, marking as 2023-24 season (Season = 0)
  combined_row <- for_row %>%
    mutate(
      Shots_Against = against_row$Sh_Standard,
      Goals_Against = against_row$Gls_Standard,
      xG_Against = against_row$xG_Expected,
      xG_difference = xG_Expected - xG_Against,
      ShotConversionRate = Gls_Standard / Sh_Standard,
      HomeGame = ifelse(Venue == "Home", 1, 0),
      Season = 0,
      Points = case_when(
        Result == "W" ~ 3,
        Result == "D" ~ 1,
        Result == "L" ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  # Add to compiled dataframe
  liverpool_complete_2023_24 <- bind_rows(liverpool_complete_2023_24, combined_row)
}

# Repeat process for 2024-25 season data
liverpool_matches_2024_25 <- fb_team_match_results(team_url = "https://fbref.com/en/squads/822bd0ba/Liverpool-Stats")

# Filter for Premier League matches
liverpool_matches_2024_25 <- liverpool_matches_2024_25 %>%
  filter(Comp == "Premier League")

# Get shooting statistics for 2024-25 season
liverpool_shooting_2024_25 <- fb_team_match_log_stats(
  team_urls = "https://fbref.com/en/squads/822bd0ba/Liverpool-Stats", 
  stat_type = "shooting"
)

# Filter shooting data for Premier League matches
liverpool_shooting_2024_25 <- liverpool_shooting_2024_25 %>%
  filter(Comp == "Premier League")

# Initialize empty dataframe for combined 2024-25 data
liverpool_complete_2024_25 <- data.frame()

# Extract match dates for 2024-25 season
match_dates_2425 <- as.Date(liverpool_matches_2024_25$Date)

# Loop through each match date, limiting to matches before May 2025
for(match_date in match_dates_2425) {
  if(match_date > as.Date("2025-05-01")) next
  
  for_row <- liverpool_shooting_2024_25 %>%
    filter(ForAgainst == "For" & as.Date(Date) == match_date)
  
  against_row <- liverpool_shooting_2024_25 %>%
    filter(ForAgainst == "Against" & as.Date(Date) == match_date)
  
  if(nrow(for_row) == 0 || nrow(against_row) == 0) next
  
  # Select relevant columns from shooting data for Liverpool
  for_row <- for_row %>% 
    select(Date, Opponent, Sh_Standard, Gls_Standard, xG_Expected, Venue, Result)
  
  # Select relevant columns from shooting data against Liverpool
  against_row <- against_row %>% 
    select(Sh_Standard, Gls_Standard, xG_Expected)
  
  # Combine data and calculate derived metrics, marking as 2024-25 season (Season = 1)
  combined_row <- for_row %>%
    mutate(
      Shots_Against = against_row$Sh_Standard,
      Goals_Against = against_row$Gls_Standard,
      xG_Against = against_row$xG_Expected,
      xG_difference = xG_Expected - xG_Against,
      ShotConversionRate = Gls_Standard / Sh_Standard,
      HomeGame = ifelse(Venue == "Home", 1, 0),
      Season = 1,
      Points = case_when(
        Result == "W" ~ 3,
        Result == "D" ~ 1,
        Result == "L" ~ 0,
        TRUE ~ NA_real_
      )
    )
  
  # Add to compiled dataframe
  liverpool_complete_2024_25 <- bind_rows(liverpool_complete_2024_25, combined_row)
}

# Combine data from both seasons
liverpool_regression_data_complete <- bind_rows(
  liverpool_complete_2023_24,
  liverpool_complete_2024_25
)

# Check distribution of matches across seasons
table(liverpool_regression_data_complete$Season)

# Center continuous predictors to reduce multicollinearity
liverpool_regression_data_complete <- liverpool_regression_data_complete %>%
  mutate(
    xG_difference_c = xG_difference - mean(xG_difference, na.rm = TRUE),
    ShotConversionRate_c = ShotConversionRate - mean(ShotConversionRate, na.rm = TRUE)
  )

# Save processed data to CSV file
write.csv(liverpool_regression_data_complete, "liverpool_regression_data_complete.csv", row.names = FALSE)

# Fit regression model with interaction terms
model_complete <- lm(Points ~ xG_difference_c + ShotConversionRate_c + 
                       HomeGame + Season + xG_difference_c:Season + ShotConversionRate_c:Season, 
                     data = liverpool_regression_data_complete)

# Display model summary statistics
summary(model_complete)
# Check for multicollinearity using Variance Inflation Factors
vif(model_complete)


# Load visualization libraries
library(ggplot2)
library(dplyr)


# Visualization 1: Initial bar chart of key predictors (scaled for presentation)
coef_data <- data.frame(
  Variable = c("Shot Conversion Rate\n(0-1 scale)", "Home Game", "xG Difference"),
  Estimate = c(
    coef(model_complete)["ShotConversionRate_c"], 
    coef(model_complete)["HomeGame"],
    coef(model_complete)["xG_difference_c"]
  ),
  p_value = c(
    summary(model_complete)$coefficients["ShotConversionRate_c", "Pr(>|t|)"],
    summary(model_complete)$coefficients["HomeGame", "Pr(>|t|)"],
    summary(model_complete)$coefficients["xG_difference_c", "Pr(>|t|)"]
  )
)

# Scale shot conversion coefficient to show effect of 0.1 change (10%)
coef_data_scaled <- coef_data
coef_data_scaled$Estimate[1] <- coef_data_scaled$Estimate[1] * 0.1

# Add significance stars to coefficient values
coef_data$Significance <- ifelse(coef_data$p_value < 0.001, "***", 
                                 ifelse(coef_data$p_value < 0.01, "**",
                                        ifelse(coef_data$p_value < 0.05, "*", "")))

# Order variables by effect size
coef_data$Variable <- factor(coef_data$Variable, levels = coef_data$Variable[order(abs(coef_data$Estimate))])

# Create bar chart of key predictors
viz1 <- ggplot(coef_data_scaled, aes(x = Variable, y = Estimate, fill = Variable)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(Estimate, 2), " ", coef_data$Significance)), 
            vjust = ifelse(coef_data_scaled$Estimate > 0, -0.5, 1.5), size = 5) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Practical Impact of Key Factors on Points",
       subtitle = "Shot Conversion Rate shows effect of a 10% increase",
       x = "", y = "Effect on Points") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

# Visualization 2: Points by Season with Home/Away Split
# Add predicted values to dataset
liverpool_regression_data_complete$Predicted <- predict(model_complete)

# Calculate average points by season and location
points_summary <- liverpool_regression_data_complete %>%
  mutate(Location = ifelse(HomeGame == 1, "Home", "Away")) %>%
  group_by(Season, Location) %>%
  summarize(
    Avg_Points = mean(Points, na.rm = TRUE),
    Avg_xG_diff = mean(xG_difference_c, na.rm = TRUE),
    Avg_Shot_Conv = mean(ShotConversionRate_c, na.rm = TRUE),
    Count = n()
  )

# Create grouped bar chart comparing home/away performance across seasons
viz2 <- ggplot(points_summary, aes(x = Season, y = Avg_Points, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_text(aes(label = round(Avg_Points, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Home" = "#1b9e77", "Away" = "#d95f02")) +
  labs(title = "Average Points by Season and Location",
       subtitle = "Home vs. Away Performance",
       x = "Season",
       y = "Average Points") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

# Display first two visualizations
viz1  
viz2


# Visualization 3: Improved Key Predictors Bar Chart with better labeling
coef_data <- data.frame(
  Variable = c("Shot Conversion Rate\n(10% increase)", "Home Game", "xG Difference"),
  Estimate = c(
    coef(model_complete)["ShotConversionRate_c"] * 0.1, # Scale to show 10% increase 
    coef(model_complete)["HomeGame"],
    coef(model_complete)["xG_difference_c"]
  ),
  p_value = c(
    summary(model_complete)$coefficients["ShotConversionRate_c", "Pr(>|t|)"],
    summary(model_complete)$coefficients["HomeGame", "Pr(>|t|)"],
    summary(model_complete)$coefficients["xG_difference_c", "Pr(>|t|)"]
  )
)

# Add significance stars
coef_data$Significance <- ifelse(coef_data$p_value < 0.001, "***", 
                                 ifelse(coef_data$p_value < 0.01, "**",
                                        ifelse(coef_data$p_value < 0.05, "*", "")))

# Create improved bar chart with better formatting
viz3 <- ggplot(coef_data, aes(x = Variable, y = Estimate, fill = Variable)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = paste0(round(Estimate, 2), " ", Significance)), 
            vjust = ifelse(coef_data$Estimate > 0, -0.5, 1.5), size = 5) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Practical Impact of Key Factors on Points",
       subtitle = "Shot Conversion Rate shows effect of a 10% increase",
       x = "", y = "Effect on Points") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")) +
  expand_limits(y = c(0, max(coef_data$Estimate) * 1.2))

# Visualization 4: Season Comparison with better labeling
points_summary <- liverpool_regression_data_complete %>%
  mutate(Location = ifelse(HomeGame == 1, "Home", "Away"),
         Season_Label = ifelse(Season == 0, "2023-2024", "2024-2025")) %>%
  group_by(Season, Season_Label, Location) %>%
  summarize(
    Avg_Points = mean(Points, na.rm = TRUE),
    Count = n()
  )

# Create improved season comparison chart
viz4 <- ggplot(points_summary, aes(x = Season_Label, y = Avg_Points, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_text(aes(label = round(Avg_Points, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("Home" = "#1b9e77", "Away" = "#d95f02")) +
  labs(title = "Average Points by Season and Location",
       subtitle = "Home vs. Away Performance",
       x = "Season",
       y = "Average Points") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        # Add more margin space
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")) +
  scale_y_continuous(limits = c(0, max(points_summary$Avg_Points) * 1.2))

# Display improved visualizations
viz3
viz4 


# Advanced visualizations section
library(gridExtra)
library(effects)

# Coefficient Plot - Create forest plot for regression coefficients
coef_data <- data.frame(
  term = c("Intercept", "xG_difference_c", "ShotConversionRate_c", "HomeGame", 
           "Season", "xG_difference_c:Season", "ShotConversionRate_c:Season"),
  estimate = coef(model_complete),
  std.error = summary(model_complete)$coefficients[, "Std. Error"]
)

# Calculate confidence intervals for coefficients
coef_data$lower <- coef_data$estimate - 1.96 * coef_data$std.error
coef_data$upper <- coef_data$estimate + 1.96 * coef_data$std.error
coef_data$significance <- ifelse(abs(coef_data$estimate/coef_data$std.error) > 1.96, "Significant", "Not Significant")

# Remove intercept for better visualization
coef_data_plot <- coef_data[-1,]
coef_data_plot$term <- factor(coef_data_plot$term, levels = coef_data_plot$term[order(coef_data_plot$estimate)])

# Create coefficient plot with confidence intervals
p1 <- ggplot(coef_data_plot, aes(x = term, y = estimate, color = significance)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Coefficient Estimates with 95% Confidence Intervals",
       x = "",
       y = "Estimate",
       color = "Statistical Significance") +
  scale_color_manual(values = c("Significant" = "#1b9e77", "Not Significant" = "#d95f02")) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"))

print(p1)

# Create interaction effects plot to show shot conversion rate by season
shot_data <- expand.grid(
  ShotConversionRate_c = seq(min(liverpool_regression_data_complete$ShotConversionRate_c),
                             max(liverpool_regression_data_complete$ShotConversionRate_c),
                             length.out = 100),
  xG_difference_c = 0,  # Set to mean (0 for centered variable)
  HomeGame = 0,         # Set to reference level (away game)
  Season = unique(liverpool_regression_data_complete$Season)
)

# Calculate predicted points based on model
shot_data$Points <- predict(model_complete, newdata = shot_data)

# Plot interaction effect for shot conversion rate by season
p2 <- ggplot(shot_data, aes(x = ShotConversionRate_c, y = Points, color = factor(Season))) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  labs(title = "Interaction Effect: Shot Conversion Rate by Season",
       x = "Shot Conversion Rate (centered)",
       y = "Predicted Points",
       color = "Season") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"))

print(p2)

# Create visualization for home vs away effect with xG difference
home_away_data <- expand.grid(
  xG_difference_c = seq(min(liverpool_regression_data_complete$xG_difference_c),
                        max(liverpool_regression_data_complete$xG_difference_c),
                        length.out = 100),
  ShotConversionRate_c = 0,  # Set to mean
  HomeGame = c(0, 1),       # Home and away
  Season = unique(liverpool_regression_data_complete$Season)[1]  # Just use first season
)

# Calculate predicted points for home/away comparison
home_away_data$Points <- predict(model_complete, newdata = home_away_data)
home_away_data$Location <- ifelse(home_away_data$HomeGame == 1, "Home", "Away")

# Plot home vs away effect
p3 <- ggplot(home_away_data, aes(x = xG_difference_c, y = Points, color = Location)) +
  geom_line(linewidth = 1.2) +
  theme_minimal() +
  labs(title = "Home vs Away Game Effect",
       x = "Expected Goals Difference (centered)",
       y = "Predicted Points",
       color = "Location") +
  scale_color_manual(values = c("Home" = "#1b9e77", "Away" = "#d95f02")) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"))

print(p3)

# Create observed vs predicted points plot for model validation
liverpool_regression_data_complete$predicted <- predict(model_complete)
liverpool_regression_data_complete$residuals <- residuals(model_complete)

# Plot observed vs predicted points
p4 <- ggplot(liverpool_regression_data_complete, aes(x = predicted, y = Points)) +
  geom_point(aes(color = factor(Season)), alpha = 0.7, size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "darkgray") +
  theme_minimal() +
  labs(title = "Observed vs Predicted Points",
       x = "Predicted Points",
       y = "Observed Points",
       color = "Season") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"))

print(p4)

# Create residual analysis plot for model diagnostics
p5 <- ggplot(liverpool_regression_data_complete, aes(x = predicted, y = residuals)) +
  geom_point(aes(color = factor(Season)), alpha = 0.7, size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgray") +
  theme_minimal() +
  labs(title = "Residual Analysis",
       x = "Predicted Points",
       y = "Residuals",
       color = "Season") +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"))

print(p5)