# 4/20 Traffic Fatality Analysis - Visualization and EDA
# Exploratory data analysis and visualizations

#### Load packages -------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(zoo)  # for rolling averages

#### Load cleaned data ---------------------------------------------------------

load("cleaned_420_data.RData")

#### Summary Statistics --------------------------------------------------------

# Overall daily fatality statistics
overall_stats <- daily_fatalities_all %>%
  summarise(
    mean = mean(total_fatalities, na.rm = TRUE),
    median = median(total_fatalities, na.rm = TRUE),
    sd = sd(total_fatalities, na.rm = TRUE),
    min = min(total_fatalities, na.rm = TRUE),
    max = max(total_fatalities, na.rm = TRUE),
    n_days = n()
  )

print("Overall daily fatality statistics (1992-2016):")
print(overall_stats)

# Compare 4/20 to control weeks (same day of week)
april_comparison <- april_analysis %>%
  group_by(is_420) %>%
  summarise(
    mean_total = mean(total_fatalities, na.rm = TRUE),
    sd_total = sd(total_fatalities, na.rm = TRUE),
    mean_evening = mean(evening_fatalities, na.rm = TRUE),
    sd_evening = sd(evening_fatalities, na.rm = TRUE),
    n_days = n(),
    .groups = "drop"
  ) %>%
  mutate(
    treatment = if_else(is_420, "4/20", "Control (4/13 & 4/27)")
  )

print("4/20 vs Control weeks comparison:")
print(april_comparison)

# Calculate difference
diff_evening <- april_comparison %>%
  summarise(
    diff_mean = diff(mean_evening),
    pct_increase = 100 * diff(mean_evening) / mean_evening[1]
  )

print("Evening fatality difference (4/20 - Control):")
print(diff_evening)

# Day of week effects
dow_stats <- daily_fatalities_treatment %>%
  group_by(dow) %>%
  summarise(
    mean_total = mean(total_fatalities, na.rm = TRUE),
    mean_evening = mean(evening_fatalities, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

print("Day of week effects:")
print(dow_stats)

#### Statistical Tests ---------------------------------------------------------

# T-test: 4/20 evening vs control evening fatalities
evening_ttest <- t.test(
  evening_fatalities ~ is_420,
  data = april_analysis
)

print("\nT-test: 4/20 evening vs control evening fatalities:")
print(evening_ttest)

# Test for equal variances
variance_test <- bartlett.test(
  evening_fatalities ~ is_420,
  data = april_analysis
)

print("\nBartlett's test for equal variances:")
print(variance_test)

# Variance by group
variance_by_group <- april_analysis %>%
  group_by(is_420) %>%
  summarise(variance = var(evening_fatalities, na.rm = TRUE))

print("\nVariance by group:")
print(variance_by_group)

#### Visualizations ------------------------------------------------------------

# 1. Boxplot: 4/20 vs Control
p1 <- ggplot(april_analysis, aes(x = is_420, y = evening_fatalities)) +
  geom_boxplot(
    aes(fill = is_420),
    outlier.shape = NA,
    alpha = 0.7
  ) +
  geom_jitter(
    width = 0.2,
    alpha = 0.5,
    size = 2
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#e74c3c", "FALSE" = "#3498db"),
    labels = c("Control Days", "4/20")
  ) +
  scale_x_discrete(
    labels = c("FALSE" = "Control\n(4/13 & 4/27)", "TRUE" = "4/20")
  ) +
  labs(
    title = "Evening Traffic Fatalities: 4/20 vs Control Days",
    subtitle = "Comparing 4/20 to same day-of-week in adjacent weeks (4:20 PM - midnight)",
    x = "",
    y = "Evening Fatalities",
    fill = "Date Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

print(p1)

# 2. Distribution comparison
p2 <- ggplot(april_analysis, aes(x = evening_fatalities, fill = is_420)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(
    values = c("TRUE" = "#e74c3c", "FALSE" = "#3498db"),
    labels = c("Control Days", "4/20")
  ) +
  labs(
    title = "Distribution of Evening Fatalities",
    subtitle = "Density plot comparing 4/20 to control dates",
    x = "Evening Fatalities",
    y = "Density",
    fill = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p2)

# 3. Time series with rolling average
ts_data <- daily_fatalities_all %>%
  arrange(date) %>%
  mutate(
    rolling_avg_7 = rollmean(total_fatalities, k = 7, fill = NA, align = "right"),
    rolling_avg_365 = rollmean(total_fatalities, k = 365, fill = NA, align = "right"),
    is_420 = (month(date) == 4 & day(date) == 20)
  )

p3 <- ggplot(ts_data, aes(x = date)) +
  geom_point(
    aes(y = total_fatalities),
    color = "grey70",
    alpha = 0.3,
    size = 0.5
  ) +
  geom_line(
    aes(y = rolling_avg_365),
    color = "steelblue",
    size = 1
  ) +
  geom_point(
    data = filter(ts_data, is_420),
    aes(y = total_fatalities),
    color = "#e74c3c",
    size = 2,
    alpha = 0.7
  ) +
  labs(
    title = "Daily Traffic Fatalities Over Time (1992-2016)",
    subtitle = "Red points indicate 4/20; blue line shows 365-day rolling average",
    x = "Date",
    y = "Daily Fatalities"
  ) +
  theme_minimal(base_size = 12)

print(p3)

# 4. Year-over-year comparison for 4/20
yearly_420 <- april_analysis %>%
  filter(is_420) %>%
  arrange(year)

p4 <- ggplot(yearly_420, aes(x = year)) +
  geom_line(aes(y = evening_fatalities), color = "#e74c3c", size = 1) +
  geom_point(aes(y = evening_fatalities), color = "#e74c3c", size = 3) +
  geom_smooth(
    aes(y = evening_fatalities),
    method = "lm",
    se = TRUE,
    color = "darkred",
    linetype = "dashed"
  ) +
  labs(
    title = "Evening Fatalities on 4/20 by Year",
    subtitle = "Trend line shows linear regression fit",
    x = "Year",
    y = "Evening Fatalities on 4/20"
  ) +
  theme_minimal(base_size = 12)

print(p4)

# 5. Day of week comparison
p5 <- dow_stats %>%
  ggplot(aes(x = dow, y = mean_evening)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_text(
    aes(label = round(mean_evening, 1)),
    vjust = -0.5,
    size = 3.5
  ) +
  labs(
    title = "Average Evening Fatalities by Day of Week",
    subtitle = "Understanding day-of-week patterns is critical for causal inference",
    x = "Day of Week",
    y = "Mean Evening Fatalities"
  ) +
  theme_minimal(base_size = 12)

print(p5)

# 6. Calendar heatmap for April
april_heatmap_data <- daily_fatalities_treatment %>%
  filter(month == 4, day <= 30) %>%
  mutate(
    week_of_month = ceiling(day / 7),
    highlight = if_else(day %in% c(13, 20, 27), "Treatment/Control", "Other")
  )

p6 <- ggplot(april_heatmap_data, aes(x = dow, y = week_of_month)) +
  geom_tile(aes(fill = evening_fatalities), color = "white", size = 0.5) +
  geom_text(aes(label = day), color = "white", fontface = "bold") +
  scale_fill_gradient(low = "#3498db", high = "#e74c3c", name = "Evening\nFatalities") +
  scale_y_reverse() +
  labs(
    title = "April Daily Evening Fatalities",
    subtitle = "Pattern of fatalities across April dates (all years combined)",
    x = "",
    y = "Week of Month"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank()
  )

print(p6)

#### Export summary table ------------------------------------------------------

summary_table <- april_comparison %>%
  select(treatment, n_days, mean_evening, sd_evening) %>%
  mutate(
    ci_lower = mean_evening - 1.96 * sd_evening / sqrt(n_days),
    ci_upper = mean_evening + 1.96 * sd_evening / sqrt(n_days)
  )

print("\nSummary table for reporting:")
print(summary_table)

# Save plots
ggsave("plot_boxplot_420_vs_control.png", p1, width = 8, height = 6, dpi = 300)
ggsave("plot_distribution_comparison.png", p2, width = 8, height = 6, dpi = 300)
ggsave("plot_timeseries_with_rolling_avg.png", p3, width = 10, height = 6, dpi = 300)
ggsave("plot_yearly_420_trend.png", p4, width = 8, height = 6, dpi = 300)
ggsave("plot_dow_effects.png", p5, width = 8, height = 6, dpi = 300)
ggsave("plot_april_heatmap.png", p6, width = 8, height = 6, dpi = 300)

print("\nAll plots saved!")