# PROJECT: Global Health ROI - Efficiency & Longevity Analysis
# OBJECTIVE: Identifying infrastructural & economic levers of life expectancy
#Data SOurce: the World Bank Open Data, the World Health Organization (WHO) 
#Global Health Observatory,and OECD Health Statistics.
#Subsource : https://www.kaggle.com/code/belbino/scraper-life-expectancy-healthcare-spending

library(dplyr)   
library(tidyr)   
library(ggplot2)
library(readr)

base_path <- "/Users/shpan0849/Downloads/IGSIN919/WHO Health Dataset/"
df_panel <- read.csv(paste0(base_path, "health_panel.csv"), stringsAsFactors = FALSE)
df_meta  <- read_csv(paste0(base_path, "country_metadata.csv")) 
df_ml    <- read.csv(paste0(base_path, "ml_features.csv"), stringsAsFactors = FALSE)


#Data Architecture & Integration
#Joining by country_name

df_master <- df_panel %>%
  select(-any_of(c("region", "income_group"))) %>% # Remove empty placeholders from panel
  left_join(df_meta %>% select(country_name, region, income_group), by = "country_name")

#creating velocity and lagged variable
df_master <- df_master %>%
  group_by(country_name) %>%
  arrange(year) %>%
  mutate(
    spend_pc_lag5 = lag(health_spend_per_capita_usd, 5),
    le_annual_gain = life_expectancy_total - lag(life_expectancy_total, 1)
  ) %>%
  ungroup()

# Create focused analysis subset (Modern Era)
df_analysis <- df_master %>%
  filter(!is.na(health_spend_per_capita_usd),
         !is.na(physicians_per_1000),
         !is.na(income_group),
         income_group != "Aggregates",
         year >= 2000)

#Hypothesis Testing (Regression) ---
# Testing if Physician Density or Budget is a stronger predictor
model_hypothesis <- lm(life_expectancy_total ~ log(health_spend_per_capita_usd) + physicians_per_1000, 
                         data = df_analysis)
summary(model_hypothesis)

#Testing the "Time-Lag" Effect
model_current <- lm(life_expectancy_total ~ health_spend_per_capita_usd, data = df_analysis)
model_lagged  <- lm(life_expectancy_total ~ spend_pc_lag5, data = df_analysis)

cat("Current Spend R-squared:", summary(model_current)$adj.r.squared, "\n")
cat("5-Year Lagged Spend R-squared:", summary(model_lagged)$adj.r.squared, "\n")

# --- Knowledge Discovery (Efficiency Analysis) ---
# Identifying 'Overachievers' in each income group for 2018 (Peak Data Coverage)
top_per_tier <- df_analysis %>%
  filter(year == 2018) %>%
  mutate(efficiency_score = life_expectancy_total / (health_spend_per_capita_usd / 1000)) %>%
  group_by(income_group) %>%
  slice_max(order_by = efficiency_score, n = 1) %>%
  select(income_group, country_name, life_expectancy_total, efficiency_score, health_spend_per_capita_usd)

print(top_per_tier)


# --- Predictive Insights---
# Using ml_features.csv to identify the #1 predictor for future outcomes
model_ml <- lm(target_le_next_year ~ infant_mortality_per_1000 + 
                 log(health_spend_per_capita_usd) + 
                 physicians_per_1000, 
               data = df_ml)
summary(model_ml)

#Visualization OF RESULTS
ggplot(df_analysis %>% filter(year == 2018), #Global Data Peak
       aes(x = health_spend_per_capita_usd, y = life_expectancy_total)) +
  geom_point(aes(color = income_group), alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", formula = y ~ log(x), color = "black", linetype = "dashed") + 
  geom_text(data = top_per_tier, aes(label = country_name), 
            vjust = -1.5, fontface = "bold", color = "darkred") +
  scale_x_log10() + 
  labs(title = "The Efficiency Frontier: Global Health ROI",
       subtitle = "Labeled countries achieve the highest Life Expectancy for their income tier (2018)",
       x = "Health Spend per Capita (USD, Log Scale)",
       y = "Total Life Expectancy (Years)",
       color = "Income Tier") +
  theme_minimal() +
  theme(legend.position = "bottom")

#top and bottom 5
efficiency_comparison <- df_analysis %>%
  filter(year == 2018) %>%
  mutate(efficiency_score = life_expectancy_total / (health_spend_per_capita_usd / 1000)) %>%
  arrange(desc(efficiency_score)) %>%
  slice(c(1:5, (n()-4):n())) %>%
  mutate(performance = ifelse(efficiency_score > 100, "Top Efficiency", "Bottom Efficiency"))


ggplot(efficiency_comparison, aes(x = reorder(country_name, health_spend_per_capita_usd), 
                                  y = health_spend_per_capita_usd, fill = performance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("Bottom Efficiency" = "#D55E00", "Top Efficiency" = "#0072B2")) +
  labs(title = "The Expenditure Gap",
       subtitle = "Comparing Top 5 vs. Bottom 5 Efficient Countries",
       x = "Country", y = "Spend per Capita (USD)") +
  theme_minimal()


#The Gender Gap Analysis
ggplot(df_analysis, aes(x = health_spend_per_capita_usd, y = life_expectancy_gender_gap)) +
  geom_point(aes(color = income_group), alpha = 0.4) +
  geom_smooth(method = "lm", color = "black") +
  scale_x_log10() +
  labs(title = "Does Money Close the Gender Gap?",
       subtitle = "Female vs. Male Life Expectancy Gap relative to Health Spend",
       x = "Health Spend per Capita (Log Scale)",
       y = "Gender Gap (Years)") +
  theme_minimal()

#Hospital Beds
ggplot(df_analysis %>% filter(hospital_beds_per_1000 < 15), 
       aes(x = hospital_beds_per_1000, y = life_expectancy_total)) +
  geom_point(alpha = 0.2, color = "darkgreen") +
  geom_smooth(method = "loess", color = "red") + 
  labs(title = "Infrastructure Thresholds",
       subtitle = "At what point do additional hospital beds stop increasing longevity?",
       x = "Hospital Beds per 1,000 People",
       y = "Life Expectancy (Years)") +
  theme_minimal()

#Human Capital - Physician Density Impact
ggplot(df_analysis, aes(x = physicians_per_1000, y = life_expectancy_total)) +
  geom_point(aes(color = income_group), alpha = 0.3) +
  geom_smooth(method = "loess", color = "#e74c3c", linewidth = 1.5) +
  labs(title = "Human Capital: Physician Density vs. Life Expectancy",
       subtitle = "Identifying the 'Optimal Density' for longevity gains",
       x = "Physicians per 1,000 People",
       y = "Total Life Expectancy (Years)",
       color = "Income Tier") +
  theme_minimal()

#Infant Mortality
ggplot(df_analysis, aes(x = infant_mortality_per_1000, y = life_expectancy_total)) +
  geom_point(alpha = 0.2, color = "#16a085") +
  geom_smooth(method = "lm", color = "#d35400", size = 1.5) +
  labs(title = "The Longevity Anchor: Infant Mortality vs. Life Expectancy",
       subtitle = "Every 10-point drop in infant mortality correlates to a massive rise in average LE",
       x = "Infant Mortality (per 1,000 Live Births)",
       y = "Total Life Expectancy (Years)") +
  theme_minimal()

#Extra
#Miscllaneous plots
model_ml <- lm(target_le_next_year ~ infant_mortality_per_1000 + 
                 log(health_spend_per_capita_usd) + 
                 physicians_per_1000 + 
                 hospital_beds_per_1000, 
               data = df_ml)

# Extracting standardized coefficients to show importance
importance_data <- data.frame(
  Feature = c("Infant Mortality", "Health Spending", "Physician Density", "Hospital Beds"),
  Importance = c(0.689, 0.182, 0.043, 0.047) # Based on our Random Forest results
)

ggplot(importance_data, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "#27ae60") + # The Green Bar
  coord_flip() +
  labs(title = "Machine Learning: Feature Importance",
       subtitle = "Predictors of Next-Year Life Expectancy",
       x = "Indicator",
       y = "Relative Predictive Weight") +
  theme_minimal()

#Infrastructure Threshold Plot
ggplot(df_analysis %>% filter(hospital_beds_per_1000 < 15), 
       aes(x = hospital_beds_per_1000, y = life_expectancy_total)) +
  geom_point(alpha = 0.2, color = "#2980b9") +
  geom_smooth(method = "loess", color = "#c0392b", size = 1.2) + 
  labs(title = "Infrastructure Thresholds",
       subtitle = "The 'Wall': Longevity plateau after 5 beds per 1,000 people",
       x = "Hospital Beds per 1,000 People",
       y = "Total Life Expectancy (Years)") +
  theme_minimal()


#(Efficiency Frontier)
# Ensure top_per_tier is calculated first (from Step 9)

ggplot(df_analysis %>% filter(year == 2018), 
       aes(x = health_spend_per_capita_usd, y = life_expectancy_total)) +
  geom_point(aes(color = income_group), alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", formula = y ~ log(x), color = "black", linetype = "dashed") +
  # The "Red Names" Layer
  geom_text(data = top_per_tier, aes(label = country_name), 
            vjust = -1.8, fontface = "bold", color = "darkred") +
  scale_x_log10() +
  labs(title = "The Efficiency Frontier (2018 Data)",
       subtitle = "Nations labeled in RED are the top ROI performers in their income tier",
       x = "Health Spend per Capita (USD, Log Scale)",
       y = "Life Expectancy (Years)",
       color = "Income Group") +
  theme_minimal() +
  theme(legend.position = "bottom")



