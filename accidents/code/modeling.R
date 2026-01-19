# 4/20 Traffic Fatality Analysis - Statistical Modeling
# Regression models to estimate causal effect of 4/20 on traffic fatalities

#### Load packages -------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(MASS)        # for negative binomial regression
library(lmtest)      # for robust standard errors
library(sandwich)    # for heteroskedasticity-consistent SEs
library(broom)       # for tidy model output
library(stargazer)   # for publication-ready tables

#### Load cleaned data ---------------------------------------------------------

load("cleaned_420_data.RData")

#### Prepare modeling dataset --------------------------------------------------

# Create comprehensive modeling dataset
model_data <- april_analysis %>%
  mutate(
    # Treatment indicator
    treatment_420 = as.numeric(is_420),
    
    # Time trends
    year_centered = year - mean(year),
    year_factor = factor(year),
    
    # Day of week
    dow_factor = factor(dow),
    
    # Post-2004 indicator (marijuana legalization era)``
    post_2004 = as.numeric(year >= 2004),
    
    # Interaction term
    treatment_post2004 = treatment_420 * post_2004
  )

print("Modeling dataset prepared:")
print(glimpse(model_data))

# Check balance across treatment and control
balance_check <- model_data %>%
  group_by(treatment_420) %>%
  summarise(
    n_obs = n(),
    mean_year = mean(year),
    n_years = n_distinct(year),
    .groups = "drop"
  )

print("\nBalance check - treatment vs control:")
print(balance_check)

#### Model 1: Simple comparison (naive) ----------------------------------------

# This ignores day-of-week and time trends - expect bias!
model1_lm <- lm(evening_fatalities ~ treatment_420, data = model_data)

print("\n=== MODEL 1: Simple OLS (Naive) ===")
summary(model1_lm)

# Robust standard errors
model1_robust <- coeftest(model1_lm, vcov = vcovHC(model1_lm, type = "HC1"))
print("\nRobust standard errors:")
print(model1_robust)

#### Model 2: Control for day of week ------------------------------------------

model2_lm <- lm(evening_fatalities ~ treatment_420 + dow_factor, data = model_data)

print("\n=== MODEL 2: OLS with Day-of-Week Controls ===")
summary(model2_lm)

model2_robust <- coeftest(model2_lm, vcov = vcovHC(model2_lm, type = "HC1"))
print("\nRobust standard errors:")
print(model2_robust)

#### Model 3: Add year trend ---------------------------------------------------

model3_lm <- lm(
  evening_fatalities ~ treatment_420 + dow_factor + year_centered,
  data = model_data
)

print("\n=== MODEL 3: OLS with Day-of-Week and Year Trend ===")
summary(model3_lm)

model3_robust <- coeftest(model3_lm, vcov = vcovHC(model3_lm, type = "HC1"))
print("\nRobust standard errors:")
print(model3_robust)

#### Model 4: Year fixed effects -----------------------------------------------

model4_lm <- lm(
  evening_fatalities ~ treatment_420 + dow_factor + year_factor,
  data = model_data
)

print("\n=== MODEL 4: OLS with Day-of-Week and Year Fixed Effects ===")
summary(model4_lm)

model4_robust <- coeftest(model4_lm, vcov = vcovHC(model4_lm, type = "HC1"))
print("\nRobust standard errors:")
print(model4_robust)

#### Model 5: Poisson regression (count data) ----------------------------------

# Poisson is appropriate for count data
model5_poisson <- glm(
  evening_fatalities ~ treatment_420 + dow_factor + year_centered,
  family = poisson(link = "log"),
  data = model_data
)

print("\n=== MODEL 5: Poisson Regression ===")
summary(model5_poisson)

# Check for overdispersion
dispersion_test <- sum(residuals(model5_poisson, type = "pearson")^2) / model5_poisson$df.residual
print(paste("\nDispersion parameter:", round(dispersion_test, 2)))
print("(Value > 1 indicates overdispersion - use negative binomial instead)")

# Calculate incidence rate ratio (IRR)
irr_poisson <- exp(coef(model5_poisson)["treatment_420"])
print(paste("\nIncidence Rate Ratio (IRR) for 4/20:", round(irr_poisson, 4)))
print(paste("Interpretation:", round((irr_poisson - 1) * 100, 2), "% increase in fatalities"))

#### Model 6: Negative binomial (addresses overdispersion) ---------------------

model6_nb <- glm.nb(
  evening_fatalities ~ treatment_420 + dow_factor + year_centered,
  data = model_data
)

print("\n=== MODEL 6: Negative Binomial Regression ===")
summary(model6_nb)

# Calculate IRR
irr_nb <- exp(coef(model6_nb)["treatment_420"])
ci_nb <- exp(confint(model6_nb)["treatment_420", ])

print(paste("\nIncidence Rate Ratio (IRR) for 4/20:", round(irr_nb, 4)))
print(paste("95% CI: [", round(ci_nb[1], 4), ",", round(ci_nb[2], 4), "]"))
print(paste("Interpretation:", round((irr_nb - 1) * 100, 2), "% increase in fatalities"))

#### Model 7: Heterogeneous effects by time period -----------------------------

model7_nb <- glm.nb(
  evening_fatalities ~ treatment_420 + post_2004 + treatment_post2004 + 
    dow_factor + year_centered,
  data = model_data
)

print("\n=== MODEL 7: Negative Binomial with Time Heterogeneity ===")
summary(model7_nb)

# Calculate IRRs for each period
irr_pre2004 <- exp(coef(model7_nb)["treatment_420"])
irr_post2004 <- exp(coef(model7_nb)["treatment_420"] + coef(model7_nb)["treatment_post2004"])

print(paste("\nIRR (Pre-2004):", round(irr_pre2004, 4)))
print(paste("IRR (Post-2004):", round(irr_post2004, 4)))

#### Model 8: Difference-in-differences specification --------------------------

# Compare 4/20 to adjacent weeks, accounting for secular trends
model8_did <- glm.nb(
  evening_fatalities ~ treatment_420 + year_factor + dow_factor,
  data = model_data
)

print("\n=== MODEL 8: Difference-in-Differences (Year FE) ===")
summary(model8_did)

irr_did <- exp(coef(model8_did)["treatment_420"])
ci_did <- exp(confint(model8_did)["treatment_420", ])

print(paste("\nDiD Incidence Rate Ratio:", round(irr_did, 4)))
print(paste("95% CI: [", round(ci_did[1], 4), ",", round(ci_did[2], 4), "]"))

#### Robustness check: Total fatalities (not just evening) ---------------------

model9_total <- glm.nb(
  total_fatalities ~ treatment_420 + dow_factor + year_centered,
  data = model_data
)

print("\n=== MODEL 9: Negative Binomial - Total Daily Fatalities ===")
print("(Not just evening - less precise but checks if effect is time-specific)")
summary(model9_total)

irr_total <- exp(coef(model9_total)["treatment_420"])
print(paste("\nIRR for total daily fatalities:", round(irr_total, 4)))

#### Model comparison table ----------------------------------------------------

# Extract key results
model_comparison <- tibble(
  Model = c(
    "1. Simple OLS",
    "2. OLS + DOW",
    "3. OLS + DOW + Trend",
    "4. OLS + DOW + Year FE",
    "5. Poisson",
    "6. Neg. Binomial",
    "7. NB + Heterogeneity",
    "8. NB + Year FE (DiD)"
  ),
  Coefficient = c(
    coef(model1_lm)["treatment_420"],
    coef(model2_lm)["treatment_420"],
    coef(model3_lm)["treatment_420"],
    coef(model4_lm)["treatment_420"],
    coef(model5_poisson)["treatment_420"],
    coef(model6_nb)["treatment_420"],
    coef(model7_nb)["treatment_420"],
    coef(model8_did)["treatment_420"]
  ),
  SE = c(
    summary(model1_lm)$coefficients["treatment_420", "Std. Error"],
    summary(model2_lm)$coefficients["treatment_420", "Std. Error"],
    summary(model3_lm)$coefficients["treatment_420", "Std. Error"],
    summary(model4_lm)$coefficients["treatment_420", "Std. Error"],
    summary(model5_poisson)$coefficients["treatment_420", "Std. Error"],
    summary(model6_nb)$coefficients["treatment_420", "Std. Error"],
    summary(model7_nb)$coefficients["treatment_420", "Std. Error"],
    summary(model8_did)$coefficients["treatment_420", "Std. Error"]
  ),
  P_value = c(
    summary(model1_lm)$coefficients["treatment_420", "Pr(>|t|)"],
    summary(model2_lm)$coefficients["treatment_420", "Pr(>|t|)"],
    summary(model3_lm)$coefficients["treatment_420", "Pr(>|t|)"],
    summary(model4_lm)$coefficients["treatment_420", "Pr(>|t|)"],
    summary(model5_poisson)$coefficients["treatment_420", "Pr(>|z|)"],
    summary(model6_nb)$coefficients["treatment_420", "Pr(>|z|)"],
    summary(model7_nb)$coefficients["treatment_420", "Pr(>|z|)"],
    summary(model8_did)$coefficients["treatment_420", "Pr(>|z|)"]
  )
) %>%
  mutate(
    Significant = if_else(P_value < 0.05, "***", 
                          if_else(P_value < 0.10, "*", "")),
    IRR = if_else(Model %in% c("5. Poisson", "6. Neg. Binomial", 
                               "7. NB + Heterogeneity", "8. NB + Year FE (DiD)"),
                  exp(Coefficient), NA_real_),
    `Pct Increase` = if_else(!is.na(IRR), (IRR - 1) * 100, NA_real_)
  )

print("\n=== MODEL COMPARISON TABLE ===")
print(model_comparison)

#### Visualize model results ---------------------------------------------------

# Plot coefficients from count models
count_models_plot <- model_comparison %>%
  filter(Model %in% c("6. Neg. Binomial", "7. NB + Heterogeneity", "8. NB + Year FE (DiD)")) %>%
  mutate(
    ci_lower = IRR - 1.96 * SE * IRR,
    ci_upper = IRR + 1.96 * SE * IRR
  )

p_models <- ggplot(count_models_plot, aes(x = Model, y = IRR)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  geom_point(size = 4, color = "#e74c3c") +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.2,
    size = 1,
    color = "#e74c3c"
  ) +
  coord_flip() +
  labs(
    title = "Estimated Effect of 4/20 on Evening Traffic Fatalities",
    subtitle = "Incidence Rate Ratios (IRR) with 95% confidence intervals",
    x = "",
    y = "Incidence Rate Ratio (IRR)\n(1.0 = no effect)",
    caption = "Models control for day-of-week and time trends"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, color = "grey50")
  )

print(p_models)
ggsave("plot_model_comparison.png", p_models, width = 10, height = 6, dpi = 300)

#### Predicted values visualization --------------------------------------------

# Generate predictions for 4/20 vs control
pred_data <- expand_grid(
  treatment_420 = c(0, 1),
  dow_factor = factor("Sat", levels = levels(model_data$dow_factor)),
  year_centered = 0
)

pred_data$predicted <- predict(model6_nb, newdata = pred_data, type = "response")
pred_data$se <- predict(model6_nb, newdata = pred_data, type = "response", se.fit = TRUE)$se.fit

pred_data <- pred_data %>%
  mutate(
    ci_lower = predicted - 1.96 * se,
    ci_upper = predicted + 1.96 * se,
    date_type = if_else(treatment_420 == 1, "4/20", "Control")
  )

p_predictions <- ggplot(pred_data, aes(x = date_type, y = predicted, fill = date_type)) +
  geom_col(alpha = 0.7, width = 0.6) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.2,
    size = 1
  ) +
  geom_text(
    aes(label = round(predicted, 1)),
    vjust = -0.5,
    size = 5,
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("4/20" = "#e74c3c", "Control" = "#3498db")) +
  labs(
    title = "Predicted Evening Fatalities: 4/20 vs Control Days",
    subtitle = "Based on negative binomial model with day-of-week and year controls",
    x = "",
    y = "Predicted Evening Fatalities",
    caption = "Error bars show 95% confidence intervals"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 0, color = "grey50")
  )

print(p_predictions)
ggsave("plot_predicted_fatalities.png", p_predictions, width = 8, height = 6, dpi = 300)

#### Save model results --------------------------------------------------------

# Save all model objects
save(
  model1_lm, model2_lm, model3_lm, model4_lm,
  model5_poisson, model6_nb, model7_nb, model8_did, model9_total,
  model_comparison,
  file = "model_results.RData"
)

# Export comparison table to CSV
write_csv(model_comparison, "model_comparison_table.csv")

print("\n=== Analysis Complete ===")
print("Models saved to 'model_results.RData'")
print("Comparison table saved to 'model_comparison_table.csv'")
print("Plots saved as PNG files")

#### Key Takeaways -------------------------------------------------------------

cat("\n=== KEY FINDINGS ===\n")
cat("Preferred model: Model 6 (Negative Binomial with DOW + Year Trend)\n")
cat(paste("  - IRR:", round(irr_nb, 4), "\n"))
cat(paste("  - 95% CI: [", round(ci_nb[1], 4), ",", round(ci_nb[2], 4), "]\n"))
cat(paste("  - Percentage increase:", round((irr_nb - 1) * 100, 2), "%\n"))
cat(paste("  - P-value:", format.pval(summary(model6_nb)$coefficients["treatment_420", "Pr(>|z|)"], digits = 3), "\n"))
cat("\nInterpretation: On 4/20 from 4:20 PM to midnight, there is an estimated\n")
cat(paste0(round((irr_nb - 1) * 100, 2), "% increase in traffic fatalities compared to control dates.\n"))
cat("\nNote: Models control for day-of-week effects and secular time trends.\n")
cat("Control dates are the same day-of-week in adjacent weeks (April 13 & 27).\n")