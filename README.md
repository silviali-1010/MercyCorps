# MercyCorps

# Overview
This project investigates whether the Fragile States Index (FSI) is a reliable predictor of humanitarian need, measured as the proportion of a country’s population in need of humanitarian assistance. The analysis was conducted as part of a Harris Policy Labs engagement with Mercy Corps.
We use panel econometric models with varying lag structures (0–5 years) to evaluate the predictive power of FSI scores, both overall and by sub-indicator, across 23 long-term humanitarian crisis countries (2008–2024).

# Data Availability and Sources

Dependent Variable (Humanitarian Need):
People in Need: United Nations Office for the Coordination of Humanitarian Affairs (OCHA), Global Humanitarian Overview datasets.
Population Estimates: UN Department of Economic and Social Affairs, World Population Prospects.

Independent Variables (FSI Indicators):
Fragile States Index: The Fund for Peace, FSI Indicators.
Country Selection:
Analysis restricted to “long-term” humanitarian crisis countries (≥5 years on OCHA’s list; N=23).
See Appendix 1 of the memo for full country categorization.

All datasets are publicly available.

# Code and Methods
Language: R
Packages Used:
tidyverse (data cleaning and visualization)
plm (panel regressions with fixed effects)
lm (OLS regression)
stargazer (regression tables)

# Methodology
Initial Crisis Regression: OLS regression on year 0 (first crisis year per country).
Panel Regression: OLS regression on country-year panel.
Panel with Fixed Effects: Country and year FE regressions.
Lag Structures: Independent variables lagged by 0, 1, 2, 3, and 5 years to capture predictive effects.

# Key Findings
Overall FSI Score: Statistically significant predictor of humanitarian need in panel regressions across all lags. Weak performance in initial crisis and FE models.
Sub-Indicators: Limited predictive power. Human Flight and Brain Drain, Refugees and IDPs, and Economy were the most consistent predictors, though results varied across lag structures and models.
Conclusion: FSI is valuable for broad cross-country predictions but limited for targeted, indicator-level forecasting. Should be combined with other indices or qualitative assessments.

# Acknowledgements
This analysis was conducted as part of the Harris Policy Labs course at the University of Chicago, in collaboration with Mercy Corps. The work may not necessarily represent the views of the Harris School, Harris School faculty, or Mercy Corps.
