# TidyTuesday - 4/20 Traffic Fatality Analysis

## Project Overview
R-based statistical analysis examining whether traffic fatalities increase on April 20th (4/20) during evening hours. This project replicates and extends the methodology from Harper and Palayew (2019).

## Research Question
Do evening traffic fatalities (4:20 PM - midnight) increase on 4/20 compared to control dates (same day-of-week in adjacent weeks: April 13 & 27)?

## Data Source
- **FARS** (Fatality Analysis Reporting System) crash data from 1992-2016
- Downloaded from Harper and Palayew's Open Science Framework repository
- Geographic lookup data from US Census Bureau

## Project Structure

### `/accidents/code/`
Main analysis scripts:

1. **data_cleaning.R**
   - Downloads raw FARS data (.dta files) and geographic lookups
   - Filters to drivers only (per_typ == 1)
   - Creates treatment indicators (is_420, is_evening, is_control_week)
   - Generates 4 analysis datasets:
     - `daily_fatalities_all` - All dates, overall trends
     - `daily_fatalities_treatment` - Treatment vs control comparison
     - `evening_fatalities` - Evening-only (4:20 PM - midnight)
     - `april_analysis` - April dates only (treatment + controls)
   - Saves: `cleaned_420_data.RData`

2. **data_visualization.R**
   - Summary statistics and exploratory data analysis
   - Statistical tests (t-tests, variance tests)
   - 6 publication-ready visualizations:
     - Boxplot: 4/20 vs control
     - Distribution comparison (density plots)
     - Time series with rolling averages
     - Year-over-year 4/20 trends
     - Day-of-week effects
     - April calendar heatmap
   - Saves: PNG files for all plots

3. **modeling.R**
   - 9 regression models with varying specifications:
     - Model 1-4: OLS with different controls
     - Model 5: Poisson regression
     - Model 6: Negative binomial (preferred model)
     - Model 7: NB with time heterogeneity
     - Model 8: Difference-in-differences with year FE
     - Model 9: Robustness check (total daily fatalities)
   - Controls for day-of-week effects and secular trends
   - Calculates Incidence Rate Ratios (IRR) and confidence intervals
   - Generates model comparison tables and visualizations
   - Saves: `model_results.RData`, CSV tables, plots

4. **normality.R**
   - Additional statistical checks (not examined in detail)

### `/accidents/` (Root)
- `.RDataTmp` - R workspace
- `data_cleaning.txt` - TidyTuesday data loading instructions
- `data_viz.txt` - (not examined)

## Key Methods

### Treatment Design
- **Treatment**: April 20
- **Controls**: Same day-of-week in adjacent weeks (April 13 & 27)
- **Time Window**: 4:20 PM - midnight (1620-2359 hours)
- **Years**: 1992-2016 (n=25 years)

### Statistical Approach
- Day-of-week controls (critical for avoiding bias)
- Year fixed effects and linear trends
- Negative binomial regression (addresses overdispersion in count data)
- Robust standard errors (HC1)
- Multiple model specifications for robustness

### Key Variables
- `is_420` - Treatment indicator (TRUE on April 20)
- `is_evening` - Evening time period (4:20 PM - midnight)
- `is_control_week` - Control dates (April 13 & 27)
- `dow` - Day of week
- `period` - Remote (1992-2003) vs Recent (2004-2016)

## Dependencies

### R Packages
```r
# Data manipulation
library(tidyverse)
library(haven)
library(lubridate)
library(fs)

# Statistical modeling
library(MASS)        # Negative binomial
library(lmtest)      # Robust SEs
library(sandwich)    # Heteroskedasticity-consistent SEs
library(broom)       # Tidy output

# Visualization
library(zoo)         # Rolling averages

# Tables
library(stargazer)   # Publication-ready tables
```

## Expected Outputs

### Data Files
- `cleaned_420_data.RData` - 4 cleaned datasets

### Model Results
- `model_results.RData` - All 9 model objects
- `model_comparison_table.csv` - Summary of all models

### Visualizations (PNG, 300 DPI)
- `plot_boxplot_420_vs_control.png`
- `plot_distribution_comparison.png`
- `plot_timeseries_with_rolling_avg.png`
- `plot_yearly_420_trend.png`
- `plot_dow_effects.png`
- `plot_april_heatmap.png`
- `plot_model_comparison.png`
- `plot_predicted_fatalities.png`

## Workflow

1. Run `data_cleaning.R` first (downloads data, creates datasets)
2. Run `data_visualization.R` (EDA and descriptive statistics)
3. Run `modeling.R` (causal inference and regression analysis)

## Notes

- Data cleaning script handles missing time data (coded as 99)
- Preferred model: Model 6 (Negative Binomial with DOW + year trend)
- Analysis focuses on drivers only to match original study
- Control dates ensure day-of-week effects don't confound results
- Post-2004 period may show different effects (marijuana legalization era)

## References
Harper, S., & Palayew, A. (2019). The annual cannabis holiday and fatal traffic crashes. Injury Prevention.
