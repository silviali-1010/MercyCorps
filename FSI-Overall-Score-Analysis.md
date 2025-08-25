FSI Crisis Year (Year 0) Regressions Results
================
Silvia Li
2025-02-17

``` r
setwd("/Users/lixiaoya/Desktop/Mercy Corps Policy Lab/Data")
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(readxl)
library(dplyr)
library(fixest)
library(plm)
```

    ## 
    ## Attaching package: 'plm'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, lag, lead

``` r
ocha <- read.csv("GHO 2025 Development Needs - Weighted averages.csv")
```

``` r
## making pin numeric
ocha <- ocha %>%
  mutate(new_pin = str_replace_all(people_in_need, ",", "")) %>%
  mutate(numeric_pin = as.numeric(new_pin))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `numeric_pin = as.numeric(new_pin)`.
    ## Caused by warning:
    ## ! NAs introduced by coercion

``` r
## filtering to columns we need
ocha <- ocha[c("country", "calendar_year", "year_of_crisis",
                        "numeric_pin", "years_since_crisis")]

ocha <- ocha %>%
  mutate(country_year = paste0(country, calendar_year))
```

``` r
## adding in UN population data
library(readxl)
un_pop <- read_excel("/Users/lixiaoya/Desktop/Mercy Corps Policy Lab/Data/Total Population.xlsx")
```

``` r
## concat year and country in ocha
ocha <- ocha %>%
  mutate(country_year = paste0(country, calendar_year))
```

``` r
## concat year and country in UN pop
un_pop <- un_pop %>%
  mutate(country_year = paste0(Country, Time))

## making population as numeric
un_pop <- un_pop %>%
  mutate(total_country_population = as.numeric(Value))
```

``` r
## merging columns to get population for each country in each year
merged_ocha_un <- merge(un_pop, ocha, by="country_year")
```

``` r
## filtering so I only have columns I need
merged_ocha_un <- merged_ocha_un[c("country_year", "Country", 
                                   "total_country_population", "calendar_year", 
                                   "numeric_pin", "years_since_crisis", 
                                   "year_of_crisis")]

# Convert 'numeric_pin' to numeric if it's not
merged_ocha_un$numeric_pin <- as.numeric(as.character(merged_ocha_un$numeric_pin))

# Check and convert 'total_country_population' to numeric if necessary
merged_ocha_un$total_country_population <- as.numeric(as.character(merged_ocha_un$total_country_population))

# After ensuring both are numeric, perform the operation
merged_ocha_un <- merged_ocha_un %>%
  mutate(pin_proportion_of_country = numeric_pin / total_country_population)
```

``` r
## adding in FSI
fsi <- read_excel("XL_FSI_data_nonlag.xlsx")
```

``` r
# Transforming the data from wide to long format
fsi <- fsi %>%
  pivot_longer(
    cols = matches("_Total$"), 
    names_to = "year", 
    values_to = "total"
  ) %>%
  mutate(year = gsub("_Total", "", year))  # Removing the '_Total' suffix from the year column
```

``` r
fsi <- fsi %>%
  mutate(country_year = paste0(Country, year))
```

``` r
## filtering again so I don't have two country columns when I combine
fsi <- fsi[c("country_year", "year", "total")]

#merge fsi and ocha data
complete_data <- merge(merged_ocha_un, fsi, by="country_year")

## filtering to clean it up a bit 
complete_data <- complete_data[c("country_year", "Country", 
                                 "total_country_population", "calendar_year", 
                                 "numeric_pin", "pin_proportion_of_country", 
                                 "total", "years_since_crisis", 
                                 "year_of_crisis")]
```

``` r
# add fsi data from 2006
fsi_more <- read_excel("XL_FSI_data_nonlag.xlsx")

#make it from wide to long
fsi_more <- fsi_more %>% mutate(across(is.logical, ~as.character(.x))) %>% 
  tidyr::pivot_longer(!c(Country,), names_to = "year", 
                      values_to = c("total"))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `across(is.logical, ~as.character(.x))`.
    ## Caused by warning:
    ## ! Use of bare predicate functions was deprecated in tidyselect 1.1.0.
    ## ℹ Please use wrap predicates in `where()` instead.
    ##   # Was:
    ##   data %>% select(is.logical)
    ## 
    ##   # Now:
    ##   data %>% select(where(is.logical))

``` r
## renaming year to calendar year 

fsi_more$year <- recode(fsi_more$year,
                        "2006_Total" = 2006, "2007_Total" = 2007, "2008_Total" = 2008, "2009_Total" = 2009,
                        "2010_Total" = 2010, "2011_Total" = 2011, "2012_Total" = 2012, "2013_Total" = 2013,
                        "2014_Total" = 2014, "2015_Total" = 2015, "2016_Total" = 2016, "2017_Total" = 2017,
                        "2018_Total" = 2018, "2019_Total" = 2019, "2020_Total" = 2020, "2021_Total" = 2021,
                        "2022_Total" = 2022, "2023_Total" = 2023)
```

``` r
fsi_more$calendar_year <- fsi_more$year

## filtering for columns I need
fsi_more <- fsi_more[c("Country", "calendar_year", "total")]
```

``` r
## adding years before 2011, getting blanks for all values except score, year, country
complete_data <- dplyr::bind_rows(complete_data, fsi_more)
```

``` r
## making a new column for lag index scores - 0 year lag
complete_data <- complete_data %>%
  group_by(Country) %>%
  mutate(fsi_calendar_year_before_0_years = total[match(calendar_year - 0, 
                                             calendar_year)]) %>% 
  ungroup
```

``` r
## making a new column for lag index scores - 1 year lag
complete_data <- complete_data %>%
  group_by(Country) %>%
  mutate(fsi_calendar_year_before_1_year = total[match(calendar_year - 1, 
                                             calendar_year)]) %>% 
  ungroup
```

``` r
## making a new column for lag index scores - 2 year lag
complete_data <- complete_data %>%
  group_by(Country) %>%
  mutate(fsi_calendar_year_before_2_year = total[match(calendar_year - 2, 
                                             calendar_year)]) %>% 
  ungroup
```

``` r
## making a new column for lag index scores - 3 year lag
complete_data <- complete_data %>%
  group_by(Country) %>%
  mutate(fsi_calendar_year_before_3_year = total[match(calendar_year - 3, 
                                             calendar_year)]) %>% 
  ungroup
```

``` r
## making a new column for lag index scores - 5 year lag
complete_data <- complete_data %>%
  group_by(Country) %>%
  mutate(fsi_calendar_year_before_5_year = total[match(calendar_year - 5, 
                                             calendar_year)]) %>% 
  ungroup
```

``` r
## taking only the fully filled out rows I need
clean_data <- complete_data[c(1:253), ]
```

``` r
## Regression for 0 year lag for year of crisis
lag_0 <- lm(clean_data$pin_proportion_of_country[clean_data$years_since_crisis == 0] 
            ~ clean_data$fsi_calendar_year_before_0_years[clean_data$years_since_crisis == 0])
summary(lag_0)
```

    ## 
    ## Call:
    ## lm(formula = clean_data$pin_proportion_of_country[clean_data$years_since_crisis == 
    ##     0] ~ clean_data$fsi_calendar_year_before_0_years[clean_data$years_since_crisis == 
    ##     0])
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.17334 -0.10212 -0.01630  0.05884  0.34508 
    ## 
    ## Coefficients:
    ##                                                                                  Estimate
    ## (Intercept)                                                                     -0.169811
    ## clean_data$fsi_calendar_year_before_0_years[clean_data$years_since_crisis == 0]  0.003829
    ##                                                                                 Std. Error
    ## (Intercept)                                                                       0.287181
    ## clean_data$fsi_calendar_year_before_0_years[clean_data$years_since_crisis == 0]   0.002912
    ##                                                                                 t value
    ## (Intercept)                                                                      -0.591
    ## clean_data$fsi_calendar_year_before_0_years[clean_data$years_since_crisis == 0]   1.315
    ##                                                                                 Pr(>|t|)
    ## (Intercept)                                                                        0.561
    ## clean_data$fsi_calendar_year_before_0_years[clean_data$years_since_crisis == 0]    0.204
    ## 
    ## Residual standard error: 0.1415 on 19 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.08344,    Adjusted R-squared:  0.0352 
    ## F-statistic:  1.73 on 1 and 19 DF,  p-value: 0.2041

``` r
nobs(lag_0)
```

    ## [1] 21

``` r
## Regression for 1 year lag for year of crisis
lag_1 <- lm(clean_data$pin_proportion_of_country[clean_data$years_since_crisis == 0] 
            ~ clean_data$fsi_calendar_year_before_1_year[clean_data$years_since_crisis == 0])
summary(lag_1)
```

    ## 
    ## Call:
    ## lm(formula = clean_data$pin_proportion_of_country[clean_data$years_since_crisis == 
    ##     0] ~ clean_data$fsi_calendar_year_before_1_year[clean_data$years_since_crisis == 
    ##     0])
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.17657 -0.11290 -0.01324  0.05694  0.33227 
    ## 
    ## Coefficients:
    ##                                                                                 Estimate
    ## (Intercept)                                                                    -0.179412
    ## clean_data$fsi_calendar_year_before_1_year[clean_data$years_since_crisis == 0]  0.003953
    ##                                                                                Std. Error
    ## (Intercept)                                                                      0.238128
    ## clean_data$fsi_calendar_year_before_1_year[clean_data$years_since_crisis == 0]   0.002424
    ##                                                                                t value
    ## (Intercept)                                                                     -0.753
    ## clean_data$fsi_calendar_year_before_1_year[clean_data$years_since_crisis == 0]   1.630
    ##                                                                                Pr(>|t|)
    ## (Intercept)                                                                       0.460
    ## clean_data$fsi_calendar_year_before_1_year[clean_data$years_since_crisis == 0]    0.119
    ## 
    ## Residual standard error: 0.1384 on 19 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.1227, Adjusted R-squared:  0.07656 
    ## F-statistic: 2.658 on 1 and 19 DF,  p-value: 0.1195

``` r
nobs(lag_1)
```

    ## [1] 21

``` r
## Regression for 2 year lag for year of crisis
lag_2 <- lm(clean_data$pin_proportion_of_country[clean_data$years_since_crisis == 0] 
            ~ clean_data$fsi_calendar_year_before_2_year[clean_data$years_since_crisis == 0])
summary(lag_2)
```

    ## 
    ## Call:
    ## lm(formula = clean_data$pin_proportion_of_country[clean_data$years_since_crisis == 
    ##     0] ~ clean_data$fsi_calendar_year_before_2_year[clean_data$years_since_crisis == 
    ##     0])
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.182664 -0.098882 -0.006096  0.051401  0.310250 
    ## 
    ## Coefficients:
    ##                                                                                 Estimate
    ## (Intercept)                                                                    -0.218705
    ## clean_data$fsi_calendar_year_before_2_year[clean_data$years_since_crisis == 0]  0.004359
    ##                                                                                Std. Error
    ## (Intercept)                                                                      0.228601
    ## clean_data$fsi_calendar_year_before_2_year[clean_data$years_since_crisis == 0]   0.002328
    ##                                                                                t value
    ## (Intercept)                                                                     -0.957
    ## clean_data$fsi_calendar_year_before_2_year[clean_data$years_since_crisis == 0]   1.872
    ##                                                                                Pr(>|t|)
    ## (Intercept)                                                                      0.3507
    ## clean_data$fsi_calendar_year_before_2_year[clean_data$years_since_crisis == 0]   0.0766
    ##                                                                                 
    ## (Intercept)                                                                     
    ## clean_data$fsi_calendar_year_before_2_year[clean_data$years_since_crisis == 0] .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1358 on 19 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.1558, Adjusted R-squared:  0.1113 
    ## F-statistic: 3.505 on 1 and 19 DF,  p-value: 0.07665

``` r
nobs(lag_2)
```

    ## [1] 21

``` r
## Regression for 3 year lag for year of crisis
lag_3 <- lm(clean_data$pin_proportion_of_country[clean_data$years_since_crisis == 0] 
            ~ clean_data$fsi_calendar_year_before_3_year[clean_data$years_since_crisis == 0])
summary(lag_3)
```

    ## 
    ## Call:
    ## lm(formula = clean_data$pin_proportion_of_country[clean_data$years_since_crisis == 
    ##     0] ~ clean_data$fsi_calendar_year_before_3_year[clean_data$years_since_crisis == 
    ##     0])
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.194197 -0.101920 -0.004379  0.052871  0.313258 
    ## 
    ## Coefficients:
    ##                                                                                 Estimate
    ## (Intercept)                                                                    -0.224282
    ## clean_data$fsi_calendar_year_before_3_year[clean_data$years_since_crisis == 0]  0.004440
    ##                                                                                Std. Error
    ## (Intercept)                                                                      0.239371
    ## clean_data$fsi_calendar_year_before_3_year[clean_data$years_since_crisis == 0]   0.002452
    ##                                                                                t value
    ## (Intercept)                                                                     -0.937
    ## clean_data$fsi_calendar_year_before_3_year[clean_data$years_since_crisis == 0]   1.810
    ##                                                                                Pr(>|t|)
    ## (Intercept)                                                                      0.3605
    ## clean_data$fsi_calendar_year_before_3_year[clean_data$years_since_crisis == 0]   0.0861
    ##                                                                                 
    ## (Intercept)                                                                     
    ## clean_data$fsi_calendar_year_before_3_year[clean_data$years_since_crisis == 0] .
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1365 on 19 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.1471, Adjusted R-squared:  0.1022 
    ## F-statistic: 3.277 on 1 and 19 DF,  p-value: 0.08609

``` r
nobs(lag_3)
```

    ## [1] 21

``` r
## Regression for 5 year lag for year of crisis
lag_5 <- lm(clean_data$pin_proportion_of_country[clean_data$years_since_crisis == 0] 
            ~ clean_data$fsi_calendar_year_before_5_year[clean_data$years_since_crisis == 0])
summary(lag_5)
```

    ## 
    ## Call:
    ## lm(formula = clean_data$pin_proportion_of_country[clean_data$years_since_crisis == 
    ##     0] ~ clean_data$fsi_calendar_year_before_5_year[clean_data$years_since_crisis == 
    ##     0])
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.20744 -0.09801 -0.00721  0.06582  0.33873 
    ## 
    ## Coefficients:
    ##                                                                                 Estimate
    ## (Intercept)                                                                    -0.108342
    ## clean_data$fsi_calendar_year_before_5_year[clean_data$years_since_crisis == 0]  0.003288
    ##                                                                                Std. Error
    ## (Intercept)                                                                      0.255852
    ## clean_data$fsi_calendar_year_before_5_year[clean_data$years_since_crisis == 0]   0.002639
    ##                                                                                t value
    ## (Intercept)                                                                     -0.423
    ## clean_data$fsi_calendar_year_before_5_year[clean_data$years_since_crisis == 0]   1.246
    ##                                                                                Pr(>|t|)
    ## (Intercept)                                                                       0.677
    ## clean_data$fsi_calendar_year_before_5_year[clean_data$years_since_crisis == 0]    0.230
    ## 
    ## Residual standard error: 0.1481 on 17 degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## Multiple R-squared:  0.08368,    Adjusted R-squared:  0.02978 
    ## F-statistic: 1.552 on 1 and 17 DF,  p-value: 0.2297

``` r
nobs(lag_5)
```

    ## [1] 19

``` r
## run panel regressions with fixed effect
## Regression for 0 year lag for year of crisis with the fixed effect
fe_model_0_lag <- feols(pin_proportion_of_country ~ fsi_calendar_year_before_0_years | Country  + calendar_year, data = clean_data)
```

    ## NOTE: 12 observations removed because of NA values (LHS: 1, RHS: 11).

``` r
summary(fe_model_0_lag)
```

    ## OLS estimation, Dep. Var.: pin_proportion_of_country
    ## Observations: 241
    ## Fixed-effects: Country: 23,  calendar_year: 13
    ## Standard-errors: Clustered (Country) 
    ##                                  Estimate Std. Error t value Pr(>|t|)    
    ## fsi_calendar_year_before_0_years 0.009419   0.004531 2.07894  0.04949 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## RMSE: 0.093683     Adj. R2: 0.744579
    ##                  Within R2: 0.081228

``` r
## Regression for 1 year lag for year of crisis
fe_model_1_lag <- feols(pin_proportion_of_country ~ fsi_calendar_year_before_1_year | Country  + calendar_year, data = clean_data)
```

    ## NOTE: 14 observations removed because of NA values (LHS: 1, RHS: 13).

``` r
summary(fe_model_1_lag)
```

    ## OLS estimation, Dep. Var.: pin_proportion_of_country
    ## Observations: 239
    ## Fixed-effects: Country: 23,  calendar_year: 13
    ## Standard-errors: Clustered (Country) 
    ##                                 Estimate Std. Error t value Pr(>|t|) 
    ## fsi_calendar_year_before_1_year 0.006272    0.00441 1.42244  0.16892 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## RMSE: 0.09507     Adj. R2: 0.737724
    ##                 Within R2: 0.03785

``` r
## Regression for 2 year lag for year of crisis
fe_model_2_lag <- feols(pin_proportion_of_country ~ fsi_calendar_year_before_2_year | Country  + calendar_year, data = clean_data)
```

    ## NOTE: 16 observations removed because of NA values (LHS: 1, RHS: 15).

``` r
summary(fe_model_2_lag)
```

    ## OLS estimation, Dep. Var.: pin_proportion_of_country
    ## Observations: 237
    ## Fixed-effects: Country: 23,  calendar_year: 13
    ## Standard-errors: Clustered (Country) 
    ##                                 Estimate Std. Error t value Pr(>|t|) 
    ## fsi_calendar_year_before_2_year  0.00433   0.003439 1.25891  0.22126 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## RMSE: 0.095459     Adj. R2: 0.736575
    ##                  Within R2: 0.022113

``` r
## Regression for 3 year lag for year of crisis
fe_model_3_lag <- feols(pin_proportion_of_country ~ fsi_calendar_year_before_3_year | Country  + calendar_year, data = clean_data)
```

    ## NOTE: 18 observations removed because of NA values (LHS: 1, RHS: 17).

``` r
summary(fe_model_3_lag)
```

    ## OLS estimation, Dep. Var.: pin_proportion_of_country
    ## Observations: 235
    ## Fixed-effects: Country: 22,  calendar_year: 13
    ## Standard-errors: Clustered (Country) 
    ##                                 Estimate Std. Error t value Pr(>|t|) 
    ## fsi_calendar_year_before_3_year 0.003098   0.002762 1.12168  0.27466 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## RMSE: 0.095215     Adj. R2: 0.740323
    ##                  Within R2: 0.012602

``` r
## Regression for 5 year lag for year of crisis
fe_model_5_lag <- feols(pin_proportion_of_country ~ fsi_calendar_year_before_5_year | Country  + calendar_year, data = clean_data)
```

    ## NOTE: 22 observations removed because of NA values (LHS: 1, RHS: 21).

``` r
summary(fe_model_5_lag)
```

    ## OLS estimation, Dep. Var.: pin_proportion_of_country
    ## Observations: 231
    ## Fixed-effects: Country: 22,  calendar_year: 13
    ## Standard-errors: Clustered (Country) 
    ##                                  Estimate Std. Error   t value Pr(>|t|) 
    ## fsi_calendar_year_before_5_year -0.000316   0.001988 -0.159184  0.87504 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## RMSE: 0.095495     Adj. R2: 0.737165
    ##                  Within R2: 1.58e-4

``` r
ggplot(clean_data, aes(x = years_since_crisis, y = total, group = Country)) +
  geom_line(aes(color = Country)) + 
  labs(x="Years since Crisis", y = "FSI Score")
```

    ## Warning: Removed 11 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](FSI-Overall-Score-Analysis_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
## run panel regressions without fixed effect
## Regression for 0 year lag 
no_fe_model_0_lag <- plm(pin_proportion_of_country ~ fsi_calendar_year_before_0_years, data = clean_data, model = "pooling")
summary(no_fe_model_0_lag)
```

    ## Pooling Model
    ## 
    ## Call:
    ## plm(formula = pin_proportion_of_country ~ fsi_calendar_year_before_0_years, 
    ##     data = clean_data, model = "pooling")
    ## 
    ## Unbalanced Panel: n = 241, T = 1-1, N = 241
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -0.342716 -0.123353 -0.044528  0.124476  0.675900 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t-value  Pr(>|t|)    
    ## (Intercept)                      -0.7456095  0.1233336 -6.0455 5.687e-09 ***
    ## fsi_calendar_year_before_0_years  0.0100719  0.0012111  8.3163 6.922e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    9.6949
    ## Residual Sum of Squares: 7.5191
    ## R-Squared:      0.22443
    ## Adj. R-Squared: 0.22119
    ## F-statistic: 69.1616 on 1 and 239 DF, p-value: 6.9223e-15

``` r
nobs(no_fe_model_0_lag)
```

    ## [1] 241

``` r
## Regression for 1 year lag
no_fe_model_1_lag <- plm(pin_proportion_of_country ~ fsi_calendar_year_before_1_year, data = clean_data, model = "pooling")
summary(no_fe_model_1_lag)
```

    ## Pooling Model
    ## 
    ## Call:
    ## plm(formula = pin_proportion_of_country ~ fsi_calendar_year_before_1_year, 
    ##     data = clean_data, model = "pooling")
    ## 
    ## Unbalanced Panel: n = 239, T = 1-1, N = 239
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -0.327652 -0.131305 -0.046848  0.115390  0.692865 
    ## 
    ## Coefficients:
    ##                                   Estimate Std. Error t-value  Pr(>|t|)    
    ## (Intercept)                     -0.6170510  0.1188494 -5.1919 4.478e-07 ***
    ## fsi_calendar_year_before_1_year  0.0088155  0.0011693  7.5394 9.990e-13 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    9.6561
    ## Residual Sum of Squares: 7.7882
    ## R-Squared:      0.19345
    ## Adj. R-Squared: 0.19004
    ## F-statistic: 56.8431 on 1 and 237 DF, p-value: 9.9896e-13

``` r
nobs(no_fe_model_1_lag)
```

    ## [1] 239

``` r
## Regression for 2 year lag 
no_fe_model_2_lag <- plm(pin_proportion_of_country ~ fsi_calendar_year_before_2_year, data = clean_data, model = "pooling")
summary(no_fe_model_2_lag)
```

    ## Pooling Model
    ## 
    ## Call:
    ## plm(formula = pin_proportion_of_country ~ fsi_calendar_year_before_2_year, 
    ##     data = clean_data, model = "pooling")
    ## 
    ## Unbalanced Panel: n = 237, T = 1-1, N = 237
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -0.347814 -0.138186 -0.046796  0.123008  0.684684 
    ## 
    ## Coefficients:
    ##                                   Estimate Std. Error t-value  Pr(>|t|)    
    ## (Intercept)                     -0.5458168  0.1169049 -4.6689 5.093e-06 ***
    ## fsi_calendar_year_before_2_year  0.0081142  0.0011516  7.0461 2.023e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    9.6258
    ## Residual Sum of Squares: 7.9469
    ## R-Squared:      0.17442
    ## Adj. R-Squared: 0.17091
    ## F-statistic: 49.6482 on 1 and 235 DF, p-value: 2.0235e-11

``` r
nobs(no_fe_model_2_lag)
```

    ## [1] 237

``` r
## Regression for 3 year lag 
no_fe_model_3_lag <- plm(pin_proportion_of_country ~ fsi_calendar_year_before_3_year, data = clean_data, model = "pooling")
summary(no_fe_model_3_lag)
```

    ## Pooling Model
    ## 
    ## Call:
    ## plm(formula = pin_proportion_of_country ~ fsi_calendar_year_before_3_year, 
    ##     data = clean_data, model = "pooling")
    ## 
    ## Unbalanced Panel: n = 235, T = 1-1, N = 235
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -0.346205 -0.140320 -0.044847  0.113899  0.676291 
    ## 
    ## Coefficients:
    ##                                   Estimate Std. Error t-value Pr(>|t|)    
    ## (Intercept)                     -0.4865306  0.1154758 -4.2133 3.60e-05 ***
    ## fsi_calendar_year_before_3_year  0.0075292  0.0011389  6.6108 2.58e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    9.599
    ## Residual Sum of Squares: 8.083
    ## R-Squared:      0.15794
    ## Adj. R-Squared: 0.15433
    ## F-statistic: 43.7028 on 1 and 233 DF, p-value: 2.5798e-10

``` r
nobs(no_fe_model_3_lag)
```

    ## [1] 235

``` r
## Regression for 5 year lag 
no_fe_model_5_lag <- plm(pin_proportion_of_country ~ fsi_calendar_year_before_5_year, data = clean_data, model = "pooling")
summary(no_fe_model_5_lag)
```

    ## Pooling Model
    ## 
    ## Call:
    ## plm(formula = pin_proportion_of_country ~ fsi_calendar_year_before_5_year, 
    ##     data = clean_data, model = "pooling")
    ## 
    ## Unbalanced Panel: n = 231, T = 1-1, N = 231
    ## 
    ## Residuals:
    ##      Min.   1st Qu.    Median   3rd Qu.      Max. 
    ## -0.337024 -0.135949 -0.045775  0.102700  0.698832 
    ## 
    ## Coefficients:
    ##                                   Estimate Std. Error t-value  Pr(>|t|)    
    ## (Intercept)                     -0.3149810  0.1120175 -2.8119  0.005352 ** 
    ## fsi_calendar_year_before_5_year  0.0058535  0.0011122  5.2630 3.257e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    9.405
    ## Residual Sum of Squares: 8.3902
    ## R-Squared:      0.1079
    ## Adj. R-Squared: 0.10401
    ## F-statistic: 27.6989 on 1 and 229 DF, p-value: 3.2567e-07

``` r
nobs(no_fe_model_5_lag)
```

    ## [1] 231

``` r
ggplot(clean_data, aes(x = calendar_year, y = total, group = Country)) +
  geom_line(aes(color = Country)) + 
  labs(x="Year", y = "FSI Score") + 
  scale_x_continuous(breaks=seq(2011, 2024, by=1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

    ## Warning: Removed 11 rows containing missing values or values outside the scale range
    ## (`geom_line()`).

![](FSI-Overall-Score-Analysis_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->
