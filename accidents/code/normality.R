# Testing Normality of Fatality Data
# Multiple approaches to assess whether data follows a normal distribution

#### Load packages -------------------------------------------------------------

library(tidyverse)
library(moments)     # for skewness and kurtosis
library(nortest)     # for additional normality tests
library(ggpubr)      # for ggqqplot

#### Load cleaned data ---------------------------------------------------------

load("cleaned_420_data.RData")

#### Prepare data for testing --------------------------------------------------

# Extract evening fatalities for 4/20 and control groups
data_420 <- april_analysis %>% 
  filter(is_420 == TRUE) %>% 
  pull(evening_fatalities)

data_control <- april_analysis %>% 
  filter(is_420 == FALSE) %>% 
  pull(evening_fatalities)

data_all <- april_analysis %>% 
  pull(evening_fatalities)

#### 1. Visual Inspection - Histograms -----------------------------------------

p_hist <- april_analysis %>%
  ggplot(aes(x = evening_fatalities, fill = is_420)) +
  geom_histogram(
    aes(y = ..density..),
    bins = 15,
    alpha = 0.6,
    position = "identity",
    color = "white"
  ) +
  geom_density(aes(color = is_420), size = 1.2, fill = NA) +
  # Add normal distribution overlay
  stat_function(
    fun = dnorm,
    args = list(mean = mean(data_all), sd = sd(data_all)),
    linetype = "dashed",
    size = 1,
    color = "black"
  ) +
  scale_fill_manual(
    values = c("TRUE" = "#e74c3c", "FALSE" = "#3498db"),
    labels = c("Control", "4/20")
  ) +
  scale_color_manual(
    values = c("TRUE" = "#e74c3c", "FALSE" = "#3498db"),
    labels = c("Control", "4/20")
  ) +
  labs(
    title = "Distribution of Evening Fatalities",
    subtitle = "Dashed line shows normal distribution with same mean/SD",
    x = "Evening Fatalities",
    y = "Density",
    fill = "",
    color = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p_hist)
ggsave("normality_histogram.png", p_hist, width = 10, height = 6, dpi = 300)

#### 2. Q-Q Plots (Quantile-Quantile) -----------------------------------------

# Q-Q plot for all data
p_qq_all <- ggplot(april_analysis, aes(sample = evening_fatalities)) +
  stat_qq() +
  stat_qq_line(color = "#e74c3c", size = 1) +
  labs(
    title = "Q-Q Plot: All April Data",
    subtitle = "Points should fall on line if data is normal",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal(base_size = 12)

print(p_qq_all)

# Q-Q plots by group
p_qq_group <- ggplot(april_analysis, aes(sample = evening_fatalities, color = is_420)) +
  stat_qq() +
  stat_qq_line(size = 1) +
  scale_color_manual(
    values = c("TRUE" = "#e74c3c", "FALSE" = "#3498db"),
    labels = c("Control", "4/20")
  ) +
  facet_wrap(~is_420, labeller = labeller(is_420 = c("FALSE" = "Control", "TRUE" = "4/20"))) +
  labs(
    title = "Q-Q Plots by Group",
    subtitle = "Assessing normality separately for 4/20 and control dates",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

print(p_qq_group)
ggsave("normality_qq_plots.png", p_qq_group, width = 10, height = 5, dpi = 300)

#### 3. Shapiro-Wilk Test ------------------------------------------------------

# Most commonly used normality test
# H0: Data is normally distributed
# If p < 0.05, reject H0 (data is NOT normal)

shapiro_all <- shapiro.test(data_all)
shapiro_420 <- shapiro.test(data_420)
shapiro_control <- shapiro.test(data_control)

cat("\n=== SHAPIRO-WILK TEST ===\n")
cat("All data: W =", round(shapiro_all$statistic, 4), ", p-value =", format.pval(shapiro_all$p.value, digits = 4), "\n")
cat("4/20 only: W =", round(shapiro_420$statistic, 4), ", p-value =", format.pval(shapiro_420$p.value, digits = 4), "\n")
cat("Control only: W =", round(shapiro_control$statistic, 4), ", p-value =", format.pval(shapiro_control$p.value, digits = 4), "\n")
cat("\nInterpretation: p < 0.05 suggests data is NOT normally distributed\n")

#### 4. Anderson-Darling Test --------------------------------------------------

# More powerful than Shapiro-Wilk, especially for detecting departures in tails

ad_all <- ad.test(data_all)
ad_420 <- ad.test(data_420)
ad_control <- ad.test(data_control)

cat("\n=== ANDERSON-DARLING TEST ===\n")
cat("All data: A =", round(ad_all$statistic, 4), ", p-value =", format.pval(ad_all$p.value, digits = 4), "\n")
cat("4/20 only: A =", round(ad_420$statistic, 4), ", p-value =", format.pval(ad_420$p.value, digits = 4), "\n")
cat("Control only: A =", round(ad_control$statistic, 4), ", p-value =", format.pval(ad_control$p.value, digits = 4), "\n")

#### 5. Kolmogorov-Smirnov Test ------------------------------------------------

# Tests if data comes from a specific distribution
ks_all <- ks.test(data_all, "pnorm", mean = mean(data_all), sd = sd(data_all))
ks_420 <- ks.test(data_420, "pnorm", mean = mean(data_420), sd = sd(data_420))
ks_control <- ks.test(data_control, "pnorm", mean = mean(data_control), sd = sd(data_control))

cat("\n=== KOLMOGOROV-SMIRNOV TEST ===\n")
cat("All data: D =", round(ks_all$statistic, 4), ", p-value =", format.pval(ks_all$p.value, digits = 4), "\n")
cat("4/20 only: D =", round(ks_420$statistic, 4), ", p-value =", format.pval(ks_420$p.value, digits = 4), "\n")
cat("Control only: D =", round(ks_control$statistic, 4), ", p-value =", format.pval(ks_control$p.value, digits = 4), "\n")

#### 6. Descriptive Statistics -------------------------------------------------

# Skewness: 0 for normal, positive = right-skewed, negative = left-skewed
# Kurtosis: 3 for normal, >3 = heavy tails, <3 = light tails

skew_all <- skewness(data_all)
skew_420 <- skewness(data_420)
skew_control <- skewness(data_control)

kurt_all <- kurtosis(data_all)
kurt_420 <- kurtosis(data_420)
kurt_control <- kurtosis(data_control)

cat("\n=== SKEWNESS (0 = symmetric, like normal) ===\n")
cat("All data:", round(skew_all, 4), "\n")
cat("4/20 only:", round(skew_420, 4), "\n")
cat("Control only:", round(skew_control, 4), "\n")

cat("\n=== KURTOSIS (3 = normal tails) ===\n")
cat("All data:", round(kurt_all, 4), "\n")
cat("4/20 only:", round(kurt_420, 4), "\n")
cat("Control only:", round(kurt_control, 4), "\n")

#### 7. Summary Table ----------------------------------------------------------

normality_summary <- tibble(
  Group = c("All April Data", "4/20 Only", "Control Only"),
  N = c(length(data_all), length(data_420), length(data_control)),
  Mean = c(mean(data_all), mean(data_420), mean(data_control)),
  SD = c(sd(data_all), sd(data_420), sd(data_control)),
  Skewness = c(skew_all, skew_420, skew_control),
  Kurtosis = c(kurt_all, kurt_420, kurt_control),
  `Shapiro p-value` = c(shapiro_all$p.value, shapiro_420$p.value, shapiro_control$p.value),
  `AD p-value` = c(ad_all$p.value, ad_420$p.value, ad_control$p.value),
  Normal = if_else(`Shapiro p-value` > 0.05, "Yes", "No")
) %>%
  mutate(across(where(is.numeric), ~round(.x, 4)))

cat("\n=== NORMALITY SUMMARY TABLE ===\n")
print(normality_summary)

write_csv(normality_summary, "normality_test_summary.csv")

#### 8. Visual Summary Panel ---------------------------------------------------

# Create a comprehensive visual panel
p_density <- april_analysis %>%
  ggplot(aes(x = evening_fatalities, fill = is_420)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(
    values = c("TRUE" = "#e74c3c", "FALSE" = "#3498db"),
    labels = c("Control", "4/20")
  ) +
  labs(title = "A. Density Plot", x = "Evening Fatalities", y = "Density", fill = "") +
  theme_minimal() +
  theme(legend.position = "top")

p_qq <- ggplot(april_analysis, aes(sample = evening_fatalities)) +
  stat_qq(color = "#3498db", alpha = 0.6) +
  stat_qq_line(color = "#e74c3c", size = 1) +
  labs(title = "B. Q-Q Plot", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

p_box <- april_analysis %>%
  ggplot(aes(x = is_420, y = evening_fatalities, fill = is_420)) +
  geom_boxplot(alpha = 0.6, outlier.color = "#e74c3c") +
  scale_fill_manual(
    values = c("TRUE" = "#e74c3c", "FALSE" = "#3498db")
  ) +
  scale_x_discrete(labels = c("FALSE" = "Control", "TRUE" = "4/20")) +
  labs(title = "C. Boxplot", x = "", y = "Evening Fatalities") +
  theme_minimal() +
  theme(legend.position = "none")

# Histogram with normal overlay
p_hist_norm <- april_analysis %>%
  ggplot(aes(x = evening_fatalities)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "#3498db", alpha = 0.6) +
  stat_function(
    fun = dnorm,
    args = list(mean = mean(data_all), sd = sd(data_all)),
    color = "#e74c3c",
    size = 1.2
  ) +
  labs(
    title = "D. Histogram with Normal Curve",
    x = "Evening Fatalities",
    y = "Density"
  ) +
  theme_minimal()

# Combine into panel
library(patchwork)
p_panel <- (p_density | p_qq) / (p_box | p_hist_norm) +
  plot_annotation(
    title = "Normality Assessment: Evening Traffic Fatalities",
    subtitle = "Multiple visual approaches to assess normality assumption",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

print(p_panel)
ggsave("normality_visual_panel.png", p_panel, width = 12, height = 10, dpi = 300)

#### 9. Conclusion -------------------------------------------------------------

cat("\n=== NORMALITY ASSESSMENT CONCLUSION ===\n\n")

if (shapiro_all$p.value < 0.05) {
  cat("⚠️  The data does NOT appear to be normally distributed.\n")
  cat("   - Shapiro-Wilk test: p =", format.pval(shapiro_all$p.value, digits = 3), "(< 0.05)\n\n")
  cat("RECOMMENDATIONS:\n")
  cat("1. DO NOT use t-tests (they assume normality)\n")
  cat("2. Use non-parametric tests:\n")
  cat("   - Wilcoxon rank-sum test (Mann-Whitney U)\n")
  cat("   - Permutation tests\n")
  cat("3. Use count models for regression:\n")
  cat("   - Poisson regression\n")
  cat("   - Negative binomial regression (preferred)\n")
  cat("4. Bootstrap confidence intervals\n\n")
  cat("This is EXPECTED for count data (fatalities are counts, not continuous)!\n")
} else {
  cat("✓ The data appears approximately normally distributed.\n")
  cat("  - Shapiro-Wilk test: p =", format.pval(shapiro_all$p.value, digits = 3), "(> 0.05)\n\n")
  cat("However, since fatalities are COUNT data, consider using:\n")
  cat("  - Poisson or negative binomial regression (still preferred)\n")
}

cat("\n=== FILES SAVED ===\n")
cat("- normality_histogram.png\n")
cat("- normality_qq_plots.png\n")
cat("- normality_visual_panel.png\n")
cat("- normality_test_summary.csv\n")