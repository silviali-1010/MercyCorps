FSI_Subindicator Analysis
================
Silvia Li
2025-02-27

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
library(ggplot2)
library(stringr)
library(fixest)

ocha <- read.csv("GHO 2025 Development Needs - Weighted averages.csv")
fsi_full<- read_excel("XL_FSI_fulldata.xlsx")
fsi_more <- read_excel("XL_FSI_data.xlsx")
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

## Convert 'numeric_pin' to numeric if it's not
merged_ocha_un$numeric_pin <- as.numeric(as.character(merged_ocha_un$numeric_pin))

## Check and convert 'total_country_population' to numeric if necessary
merged_ocha_un$total_country_population <- as.numeric(as.character(merged_ocha_un$total_country_population))

## After ensuring both are numeric, perform the operation
merged_ocha_un <- merged_ocha_un %>%
  mutate(pin_proportion_of_country = numeric_pin / total_country_population)
```

``` r
## Make dataframe from wide to long
fsi_panel <- fsi_full %>%
  pivot_longer(
    cols = -Country,  # Exclude the country column if it exists; otherwise, adjust accordingly
    names_to = "year_indicator",
    values_to = "value"
  ) %>%
  mutate(
    year = str_extract(year_indicator, "^[0-9]+"),  # Extract the year from the combined column
    indicator = str_remove(year_indicator, "^[0-9]+_")  # Remove the year part to leave just the indicator
  ) %>%
  select(-year_indicator) %>%  # Drop the combined column as it's no longer needed
  pivot_wider(
    names_from = indicator,
    values_from = value,
    names_prefix = ""  # Removes any prefix from the column names
  )

# View the panel dataframe
print(fsi_panel)
```

    ## # A tibble: 299 × 15
    ##    Country year  Total `C1: Security Apparatus` `C2: Factionalized Elites`
    ##    <chr>   <chr> <dbl>                    <dbl>                      <dbl>
    ##  1 Sudan   2011   109.                     9.6                         9.9
    ##  2 Sudan   2012   109.                     9.7                         9.9
    ##  3 Sudan   2013   111                      9.8                        10  
    ##  4 Sudan   2014   110.                     9.6                        10  
    ##  5 Sudan   2015   111.                     9.5                         9.8
    ##  6 Sudan   2016   112.                     9.2                        10  
    ##  7 Sudan   2017   111.                     9                           9.7
    ##  8 Sudan   2018   109.                     8.7                         9.7
    ##  9 Sudan   2019   108                      8.4                         9.7
    ## 10 Sudan   2020   105.                     8.38                        9.4
    ## # ℹ 289 more rows
    ## # ℹ 10 more variables: `C3: Group Grievance` <dbl>, `E1: Economy` <dbl>,
    ## #   `E2: Economic Inequality` <dbl>, `E3: Human Flight and Brain Drain` <dbl>,
    ## #   `P1: State Legitimacy` <dbl>, `P2: Public Services` <dbl>,
    ## #   `P3: Human Rights` <dbl>, `S1: Demographic Pressures` <dbl>,
    ## #   `S2: Refugees and IDPs` <dbl>, `X1: External Intervention` <dbl>

``` r
fsi_panel <- fsi_panel %>%
  mutate(country_year = paste0(Country, year))
```

``` r
#merge fsi and ocha data
complete_full_data <- merge(merged_ocha_un, fsi_panel, by="country_year")

#delete repeat column and rename the country column
complete_full_data <- complete_full_data %>%
  select(-c(Country.y))  

complete_full_data <- complete_full_data %>%
  rename(Country = Country.x)
```

``` r
# Extract year and indicator, then adjust column names
fsi_more <- fsi_more %>%
  pivot_longer(
    cols = -Country,  # Exclude the Country column
    names_to = "year_indicator",
    values_to = "value"
  ) %>%
  mutate(
    year = str_extract(year_indicator, "^[0-9]+"),  # Extract the year from the combined column
    indicator = str_remove(year_indicator, "^[0-9]+_")  # Remove the year part to leave just the indicator
  ) %>%
  select(-year_indicator) %>%
  pivot_wider(
    names_from = indicator,
    values_from = value,
    names_prefix = ""  # Removes any prefix from the column names
  ) %>%
  rename(calendar_year = year)  # Rename 'year' to 'calendar_year'

# Concatenate country and calendar year to create a unique identifier
fsi_more <- fsi_more %>%
  mutate(country_year = paste0(Country, calendar_year))

# Ensure calendar_year is an integer
fsi_more <- fsi_more %>%
  mutate(calendar_year = as.integer(calendar_year))  # Convert calendar_year to integer

# Now try to bind the rows again
complete_full_data <- dplyr::bind_rows(complete_full_data, fsi_more)
```

``` r
## making a new column for lag index scores - 0 year lag
complete_full_data <- complete_full_data %>%
  group_by(Country) %>%
  mutate(
    fsi_calendar_year_before_0_years = Total,
    security_apparatus_calendar_year_before_0_years = `C1: Security Apparatus`,
    factionalized_elites_calendar_year_before_0_years = `C2: Factionalized Elites`,
    group_grievance_calendar_year_before_0_years = `C3: Group Grievance`,
    economy_calendar_year_before_0_years = `E1: Economy`,
    economic_inequality_calendar_year_before_0_years = `E2: Economic Inequality`,
    humanflight__calendar_year_before_0_years = `E3: Human Flight and Brain Drain`,
    state_legitimacy_calendar_year_before_0_years = `P1: State Legitimacy`,
    public_services_calendar_year_before_0_years = `P2: Public Services`,
    humanrights_calendar_year_before_0_years = `P3: Human Rights`,
    demographic_pressures_calendar_year_before_0_years = `S1: Demographic Pressures`,
    refugees_and_IDPs_calendar_year_before_0_years = `S2: Refugees and IDPs`,
    external_intervention_calendar_year_before_0_years = `X1: External Intervention`,
  ) %>%
  ungroup()
```

``` r
## making a new column for lag index scores - 1 year lag
complete_full_data <- complete_full_data %>%
  group_by(Country) %>%
  mutate(
    fsi_calendar_year_before_1_year = Total[match(calendar_year - 1, calendar_year)],
    security_apparatus_calendar_year_before_1_year = `C1: Security Apparatus`[match(calendar_year - 1, calendar_year)],
    factionalized_elites_calendar_year_before_1_year = `C2: Factionalized Elites`[match(calendar_year - 1, calendar_year)],
    group_grievance_calendar_year_before_1_year = `C3: Group Grievance`[match(calendar_year - 1, calendar_year)],
    economy_calendar_year_before_1_year = `E1: Economy`[match(calendar_year - 1, calendar_year)],
    economic_inequality_calendar_year_before_1_year = `E2: Economic Inequality`[match(calendar_year - 1, calendar_year)],
    humanflight_calendar_year_before_1_year = `E3: Human Flight and Brain Drain`[match(calendar_year - 1, calendar_year)],
    state_legitimacy_calendar_year_before_1_year = `P1: State Legitimacy`[match(calendar_year - 1, calendar_year)],
    public_services_calendar_year_before_1_year = `P2: Public Services`[match(calendar_year - 1, calendar_year)],
    humanrights_calendar_year_before_1_year = `P3: Human Rights`[match(calendar_year - 1, calendar_year)],
    demographic_pressures_calendar_year_before_1_year = `S1: Demographic Pressures`[match(calendar_year - 1, calendar_year)],
    refugees_and_IDPs_calendar_year_before_1_year = `S2: Refugees and IDPs`[match(calendar_year - 1, calendar_year)],
    external_intervention_calendar_year_before_1_year = `X1: External Intervention`[match(calendar_year - 1, calendar_year)]
  ) %>%
  ungroup()  # Remove the grouping for further operations
```

``` r
## Do the same for 2 year lag
complete_full_data <- complete_full_data %>%
  group_by(Country) %>%
  mutate(
    fsi_calendar_year_before_2_years = Total[match(calendar_year - 2, calendar_year)],
    security_apparatus_calendar_year_before_2_years = `C1: Security Apparatus`[match(calendar_year - 2, calendar_year)],
    factionalized_elites_calendar_year_before_2_years = `C2: Factionalized Elites`[match(calendar_year - 2, calendar_year)],
    group_grievance_calendar_year_before_2_years = `C3: Group Grievance`[match(calendar_year - 2, calendar_year)],
    economy_calendar_year_before_2_years = `E1: Economy`[match(calendar_year - 2, calendar_year)],
    economic_inequality_calendar_year_before_2_years = `E2: Economic Inequality`[match(calendar_year - 2, calendar_year)],
    humanflight_calendar_year_before_2_years = `E3: Human Flight and Brain Drain`[match(calendar_year - 2, calendar_year)],
    state_legitimacy_calendar_year_before_2_years = `P1: State Legitimacy`[match(calendar_year - 2, calendar_year)],
    public_services_calendar_year_before_2_years = `P2: Public Services`[match(calendar_year - 2, calendar_year)],
    humanrights_calendar_year_before_2_years = `P3: Human Rights`[match(calendar_year - 2, calendar_year)],
    demographic_pressures_calendar_year_before_2_years = `S1: Demographic Pressures`[match(calendar_year - 2, calendar_year)],
    refugees_and_IDPs_calendar_year_before_2_years = `S2: Refugees and IDPs`[match(calendar_year - 2, calendar_year)],
    external_intervention_calendar_year_before_2_years = `X1: External Intervention`[match(calendar_year - 2, calendar_year)],
  ) %>%
  ungroup()  # Remove the grouping for further unrestricted operations on the data frame
```

``` r
complete_full_data <- complete_full_data %>%
  group_by(Country) %>%
  mutate(
    fsi_calendar_year_before_3_years = Total[match(calendar_year - 3, calendar_year)],
    security_apparatus_calendar_year_before_3_years = `C1: Security Apparatus`[match(calendar_year - 3, calendar_year)],
    factionalized_elites_calendar_year_before_3_years = `C2: Factionalized Elites`[match(calendar_year - 3, calendar_year)],
    group_grievance_calendar_year_before_3_years = `C3: Group Grievance`[match(calendar_year - 3, calendar_year)],
    economy_calendar_year_before_3_years = `E1: Economy`[match(calendar_year - 3, calendar_year)],
    economic_inequality_calendar_year_before_3_years = `E2: Economic Inequality`[match(calendar_year - 3, calendar_year)],
    humanflight_calendar_year_before_3_years = `E3: Human Flight and Brain Drain`[match(calendar_year - 3, calendar_year)],
    state_legitimacy_calendar_year_before_3_years = `P1: State Legitimacy`[match(calendar_year - 3, calendar_year)],
    public_services_calendar_year_before_3_years = `P2: Public Services`[match(calendar_year - 3, calendar_year)],
    humanrights_calendar_year_before_3_years = `P3: Human Rights`[match(calendar_year - 3, calendar_year)],
    demographic_pressures_calendar_year_before_3_years = `S1: Demographic Pressures`[match(calendar_year - 3, calendar_year)],
    refugees_and_IDPs_calendar_year_before_3_years = `S2: Refugees and IDPs`[match(calendar_year - 3, calendar_year)],
    external_intervention_calendar_year_before_3_years = `X1: External Intervention`[match(calendar_year - 3, calendar_year)],
  ) %>%
  ungroup()  # Remove the grouping for further unrestricted operations on the data frame
```

``` r
complete_full_data <- complete_full_data %>%
  group_by(Country) %>%
  mutate(
    fsi_calendar_year_before_5_years = Total[match(calendar_year - 5, calendar_year)],
    security_apparatus_calendar_year_before_5_years = `C1: Security Apparatus`[match(calendar_year - 5, calendar_year)],
    factionalized_elites_calendar_year_before_5_years = `C2: Factionalized Elites`[match(calendar_year - 5, calendar_year)],
    group_grievance_calendar_year_before_5_years = `C3: Group Grievance`[match(calendar_year - 5, calendar_year)],
    economy_calendar_year_before_5_years = `E1: Economy`[match(calendar_year - 5, calendar_year)],
    economic_inequality_calendar_year_before_5_years = `E2: Economic Inequality`[match(calendar_year - 5, calendar_year)],
    humanflight_calendar_year_before_5_years = `E3: Human Flight and Brain Drain`[match(calendar_year - 5, calendar_year)],
    state_legitimacy_calendar_year_before_5_years = `P1: State Legitimacy`[match(calendar_year - 5, calendar_year)],
    public_services_calendar_year_before_5_years = `P2: Public Services`[match(calendar_year - 5, calendar_year)],
    humanrights_calendar_year_before_5_years = `P3: Human Rights`[match(calendar_year - 5, calendar_year)],
    demographic_pressures_calendar_year_before_5_years = `S1: Demographic Pressures`[match(calendar_year - 5, calendar_year)],
    refugees_and_IDPs_calendar_year_before_5_years = `S2: Refugees and IDPs`[match(calendar_year - 5, calendar_year)],
    external_intervention_calendar_year_before_5_years = `X1: External Intervention`[match(calendar_year - 5, calendar_year)]
  ) %>%
  ungroup()  # Remove the grouping for further unrestricted operations on the data frame
```

``` r
## making a initial year 0 dataframe
initial_crisis_year_data <- complete_full_data %>% filter(years_since_crisis == 0)

## Regression for 0 year lag for year of crisis
crisis_reg_lag_0 <- lm(pin_proportion_of_country ~ 
                   security_apparatus_calendar_year_before_0_years +
                   factionalized_elites_calendar_year_before_0_years +
                   group_grievance_calendar_year_before_0_years +
                   economy_calendar_year_before_0_years +
                   economic_inequality_calendar_year_before_0_years +
                   humanflight__calendar_year_before_0_years +
                   state_legitimacy_calendar_year_before_0_years +
                   public_services_calendar_year_before_0_years +
                   humanrights_calendar_year_before_0_years +
                   demographic_pressures_calendar_year_before_0_years +
                   refugees_and_IDPs_calendar_year_before_0_years +
                   external_intervention_calendar_year_before_0_years,
                 data = initial_crisis_year_data)
summary(crisis_reg_lag_0)
```

    ## 
    ## Call:
    ## lm(formula = pin_proportion_of_country ~ security_apparatus_calendar_year_before_0_years + 
    ##     factionalized_elites_calendar_year_before_0_years + group_grievance_calendar_year_before_0_years + 
    ##     economy_calendar_year_before_0_years + economic_inequality_calendar_year_before_0_years + 
    ##     humanflight__calendar_year_before_0_years + state_legitimacy_calendar_year_before_0_years + 
    ##     public_services_calendar_year_before_0_years + humanrights_calendar_year_before_0_years + 
    ##     demographic_pressures_calendar_year_before_0_years + refugees_and_IDPs_calendar_year_before_0_years + 
    ##     external_intervention_calendar_year_before_0_years, data = initial_crisis_year_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.16397 -0.06020  0.00774  0.03721  0.23166 
    ## 
    ## Coefficients:
    ##                                                    Estimate Std. Error t value
    ## (Intercept)                                        -0.02714    0.40923  -0.066
    ## security_apparatus_calendar_year_before_0_years    -0.13920    0.08836  -1.575
    ## factionalized_elites_calendar_year_before_0_years   0.04643    0.09784   0.475
    ## group_grievance_calendar_year_before_0_years        0.07683    0.07053   1.089
    ## economy_calendar_year_before_0_years                0.09842    0.08519   1.155
    ## economic_inequality_calendar_year_before_0_years    0.07864    0.10019   0.785
    ## humanflight__calendar_year_before_0_years           0.06484    0.03678   1.763
    ## state_legitimacy_calendar_year_before_0_years      -0.20944    0.15417  -1.359
    ## public_services_calendar_year_before_0_years        0.03223    0.07444   0.433
    ## humanrights_calendar_year_before_0_years            0.04953    0.08423   0.588
    ## demographic_pressures_calendar_year_before_0_years -0.09274    0.10583  -0.876
    ## refugees_and_IDPs_calendar_year_before_0_years     -0.01835    0.05754  -0.319
    ## external_intervention_calendar_year_before_0_years  0.07644    0.07199   1.062
    ##                                                    Pr(>|t|)
    ## (Intercept)                                           0.949
    ## security_apparatus_calendar_year_before_0_years       0.154
    ## factionalized_elites_calendar_year_before_0_years     0.648
    ## group_grievance_calendar_year_before_0_years          0.308
    ## economy_calendar_year_before_0_years                  0.281
    ## economic_inequality_calendar_year_before_0_years      0.455
    ## humanflight__calendar_year_before_0_years             0.116
    ## state_legitimacy_calendar_year_before_0_years         0.211
    ## public_services_calendar_year_before_0_years          0.677
    ## humanrights_calendar_year_before_0_years              0.573
    ## demographic_pressures_calendar_year_before_0_years    0.406
    ## refugees_and_IDPs_calendar_year_before_0_years        0.758
    ## external_intervention_calendar_year_before_0_years    0.319
    ## 
    ## Residual standard error: 0.1434 on 8 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.6033, Adjusted R-squared:  0.008264 
    ## F-statistic: 1.014 on 12 and 8 DF,  p-value: 0.5091

``` r
nobs(crisis_reg_lag_0)
```

    ## [1] 21

``` r
## Regression for 1 year lag for year of crisis
crisis_reg_lag_1 <- lm(pin_proportion_of_country ~ 
                   security_apparatus_calendar_year_before_1_year +
                   factionalized_elites_calendar_year_before_1_year +
                   group_grievance_calendar_year_before_1_year +
                   economy_calendar_year_before_1_year +
                   economic_inequality_calendar_year_before_1_year +
                   humanflight_calendar_year_before_1_year +
                   state_legitimacy_calendar_year_before_1_year +
                   public_services_calendar_year_before_1_year +
                   humanrights_calendar_year_before_1_year +
                   demographic_pressures_calendar_year_before_1_year +
                   refugees_and_IDPs_calendar_year_before_1_year +
                   external_intervention_calendar_year_before_1_year,
                 data = initial_crisis_year_data)
summary(crisis_reg_lag_1)
```

    ## 
    ## Call:
    ## lm(formula = pin_proportion_of_country ~ security_apparatus_calendar_year_before_1_year + 
    ##     factionalized_elites_calendar_year_before_1_year + group_grievance_calendar_year_before_1_year + 
    ##     economy_calendar_year_before_1_year + economic_inequality_calendar_year_before_1_year + 
    ##     humanflight_calendar_year_before_1_year + state_legitimacy_calendar_year_before_1_year + 
    ##     public_services_calendar_year_before_1_year + humanrights_calendar_year_before_1_year + 
    ##     demographic_pressures_calendar_year_before_1_year + refugees_and_IDPs_calendar_year_before_1_year + 
    ##     external_intervention_calendar_year_before_1_year, data = initial_crisis_year_data)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.132894 -0.068086 -0.004444  0.030305  0.246253 
    ## 
    ## Coefficients:
    ##                                                    Estimate Std. Error t value
    ## (Intercept)                                        0.008098   0.327083   0.025
    ## security_apparatus_calendar_year_before_1_year    -0.074573   0.095397  -0.782
    ## factionalized_elites_calendar_year_before_1_year   0.001994   0.085463   0.023
    ## group_grievance_calendar_year_before_1_year        0.012786   0.075764   0.169
    ## economy_calendar_year_before_1_year                0.039013   0.063833   0.611
    ## economic_inequality_calendar_year_before_1_year   -0.035681   0.066540  -0.536
    ## humanflight_calendar_year_before_1_year            0.101294   0.043455   2.331
    ## state_legitimacy_calendar_year_before_1_year      -0.075506   0.153799  -0.491
    ## public_services_calendar_year_before_1_year        0.119798   0.107644   1.113
    ## humanrights_calendar_year_before_1_year            0.002148   0.104491   0.021
    ## demographic_pressures_calendar_year_before_1_year -0.105026   0.128983  -0.814
    ## refugees_and_IDPs_calendar_year_before_1_year      0.047856   0.046851   1.021
    ## external_intervention_calendar_year_before_1_year  0.014793   0.056406   0.262
    ##                                                   Pr(>|t|)  
    ## (Intercept)                                         0.9809  
    ## security_apparatus_calendar_year_before_1_year      0.4569  
    ## factionalized_elites_calendar_year_before_1_year    0.9820  
    ## group_grievance_calendar_year_before_1_year         0.8702  
    ## economy_calendar_year_before_1_year                 0.5580  
    ## economic_inequality_calendar_year_before_1_year     0.6064  
    ## humanflight_calendar_year_before_1_year             0.0481 *
    ## state_legitimacy_calendar_year_before_1_year        0.6367  
    ## public_services_calendar_year_before_1_year         0.2981  
    ## humanrights_calendar_year_before_1_year             0.9841  
    ## demographic_pressures_calendar_year_before_1_year   0.4391  
    ## refugees_and_IDPs_calendar_year_before_1_year       0.3369  
    ## external_intervention_calendar_year_before_1_year   0.7997  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1454 on 8 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.5923, Adjusted R-squared:  -0.01937 
    ## F-statistic: 0.9683 on 12 and 8 DF,  p-value: 0.5368

``` r
nobs(crisis_reg_lag_1)
```

    ## [1] 21

``` r
## Regression for 2 year lag for year of crisis
crisis_reg_lag_2 <- lm(pin_proportion_of_country ~ 
                   security_apparatus_calendar_year_before_2_years +
                   factionalized_elites_calendar_year_before_2_years +
                   group_grievance_calendar_year_before_2_years +
                   economy_calendar_year_before_2_years +
                   economic_inequality_calendar_year_before_2_years +
                   humanflight_calendar_year_before_2_years +
                   state_legitimacy_calendar_year_before_2_years +
                   public_services_calendar_year_before_2_years +
                   humanrights_calendar_year_before_2_years +
                   demographic_pressures_calendar_year_before_2_years +
                   refugees_and_IDPs_calendar_year_before_2_years +
                   external_intervention_calendar_year_before_2_years,
                 data = initial_crisis_year_data)
summary(crisis_reg_lag_2)
```

    ## 
    ## Call:
    ## lm(formula = pin_proportion_of_country ~ security_apparatus_calendar_year_before_2_years + 
    ##     factionalized_elites_calendar_year_before_2_years + group_grievance_calendar_year_before_2_years + 
    ##     economy_calendar_year_before_2_years + economic_inequality_calendar_year_before_2_years + 
    ##     humanflight_calendar_year_before_2_years + state_legitimacy_calendar_year_before_2_years + 
    ##     public_services_calendar_year_before_2_years + humanrights_calendar_year_before_2_years + 
    ##     demographic_pressures_calendar_year_before_2_years + refugees_and_IDPs_calendar_year_before_2_years + 
    ##     external_intervention_calendar_year_before_2_years, data = initial_crisis_year_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.14238 -0.05566 -0.01919  0.08047  0.19972 
    ## 
    ## Coefficients:
    ##                                                     Estimate Std. Error t value
    ## (Intercept)                                        -0.007359   0.417352  -0.018
    ## security_apparatus_calendar_year_before_2_years     0.039489   0.097504   0.405
    ## factionalized_elites_calendar_year_before_2_years  -0.050573   0.068894  -0.734
    ## group_grievance_calendar_year_before_2_years       -0.052936   0.058675  -0.902
    ## economy_calendar_year_before_2_years                0.042348   0.069645   0.608
    ## economic_inequality_calendar_year_before_2_years   -0.045551   0.065041  -0.700
    ## humanflight_calendar_year_before_2_years            0.056677   0.038582   1.469
    ## state_legitimacy_calendar_year_before_2_years       0.049925   0.133686   0.373
    ## public_services_calendar_year_before_2_years        0.062589   0.128712   0.486
    ## humanrights_calendar_year_before_2_years           -0.006379   0.124927  -0.051
    ## demographic_pressures_calendar_year_before_2_years -0.060286   0.161611  -0.373
    ## refugees_and_IDPs_calendar_year_before_2_years      0.026522   0.053245   0.498
    ## external_intervention_calendar_year_before_2_years -0.026231   0.045945  -0.571
    ##                                                    Pr(>|t|)
    ## (Intercept)                                           0.986
    ## security_apparatus_calendar_year_before_2_years       0.696
    ## factionalized_elites_calendar_year_before_2_years     0.484
    ## group_grievance_calendar_year_before_2_years          0.393
    ## economy_calendar_year_before_2_years                  0.560
    ## economic_inequality_calendar_year_before_2_years      0.504
    ## humanflight_calendar_year_before_2_years              0.180
    ## state_legitimacy_calendar_year_before_2_years         0.719
    ## public_services_calendar_year_before_2_years          0.640
    ## humanrights_calendar_year_before_2_years              0.961
    ## demographic_pressures_calendar_year_before_2_years    0.719
    ## refugees_and_IDPs_calendar_year_before_2_years        0.632
    ## external_intervention_calendar_year_before_2_years    0.584
    ## 
    ## Residual standard error: 0.1522 on 8 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.5532, Adjusted R-squared:  -0.1169 
    ## F-statistic: 0.8256 on 12 and 8 DF,  p-value: 0.6311

``` r
nobs(crisis_reg_lag_2)
```

    ## [1] 21

``` r
## Regression for 3 year lag for year of crisis
crisis_reg_lag_3 <- lm(pin_proportion_of_country ~ 
                   security_apparatus_calendar_year_before_3_years +
                   factionalized_elites_calendar_year_before_3_years +
                   group_grievance_calendar_year_before_3_years +
                   economy_calendar_year_before_3_years +
                   economic_inequality_calendar_year_before_3_years +
                   humanflight_calendar_year_before_3_years +
                   state_legitimacy_calendar_year_before_3_years +
                   public_services_calendar_year_before_3_years +
                   humanrights_calendar_year_before_3_years +
                   demographic_pressures_calendar_year_before_3_years +
                   refugees_and_IDPs_calendar_year_before_3_years +
                   external_intervention_calendar_year_before_3_years,
                 data = initial_crisis_year_data)
summary(crisis_reg_lag_3)
```

    ## 
    ## Call:
    ## lm(formula = pin_proportion_of_country ~ security_apparatus_calendar_year_before_3_years + 
    ##     factionalized_elites_calendar_year_before_3_years + group_grievance_calendar_year_before_3_years + 
    ##     economy_calendar_year_before_3_years + economic_inequality_calendar_year_before_3_years + 
    ##     humanflight_calendar_year_before_3_years + state_legitimacy_calendar_year_before_3_years + 
    ##     public_services_calendar_year_before_3_years + humanrights_calendar_year_before_3_years + 
    ##     demographic_pressures_calendar_year_before_3_years + refugees_and_IDPs_calendar_year_before_3_years + 
    ##     external_intervention_calendar_year_before_3_years, data = initial_crisis_year_data)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.142948 -0.064448 -0.005456  0.060397  0.218533 
    ## 
    ## Coefficients:
    ##                                                    Estimate Std. Error t value
    ## (Intercept)                                        -0.32797    0.40581  -0.808
    ## security_apparatus_calendar_year_before_3_years    -0.15258    0.14391  -1.060
    ## factionalized_elites_calendar_year_before_3_years   0.01640    0.05963   0.275
    ## group_grievance_calendar_year_before_3_years        0.04605    0.06621   0.696
    ## economy_calendar_year_before_3_years                0.15615    0.11042   1.414
    ## economic_inequality_calendar_year_before_3_years    0.03508    0.07627   0.460
    ## humanflight_calendar_year_before_3_years            0.02961    0.03333   0.888
    ## state_legitimacy_calendar_year_before_3_years      -0.11960    0.11301  -1.058
    ## public_services_calendar_year_before_3_years        0.13946    0.13636   1.023
    ## humanrights_calendar_year_before_3_years            0.06113    0.12847   0.476
    ## demographic_pressures_calendar_year_before_3_years -0.21230    0.21293  -0.997
    ## refugees_and_IDPs_calendar_year_before_3_years      0.03717    0.04113   0.904
    ## external_intervention_calendar_year_before_3_years  0.05580    0.06101   0.915
    ##                                                    Pr(>|t|)
    ## (Intercept)                                           0.442
    ## security_apparatus_calendar_year_before_3_years       0.320
    ## factionalized_elites_calendar_year_before_3_years     0.790
    ## group_grievance_calendar_year_before_3_years          0.506
    ## economy_calendar_year_before_3_years                  0.195
    ## economic_inequality_calendar_year_before_3_years      0.658
    ## humanflight_calendar_year_before_3_years              0.400
    ## state_legitimacy_calendar_year_before_3_years         0.321
    ## public_services_calendar_year_before_3_years          0.336
    ## humanrights_calendar_year_before_3_years              0.647
    ## demographic_pressures_calendar_year_before_3_years    0.348
    ## refugees_and_IDPs_calendar_year_before_3_years        0.393
    ## external_intervention_calendar_year_before_3_years    0.387
    ## 
    ## Residual standard error: 0.1459 on 8 degrees of freedom
    ##   (2 observations deleted due to missingness)
    ## Multiple R-squared:  0.5897, Adjusted R-squared:  -0.02585 
    ## F-statistic: 0.958 on 12 and 8 DF,  p-value: 0.5432

``` r
nobs(crisis_reg_lag_3)
```

    ## [1] 21

``` r
## Regression for 5 year lag for year of crisis
crisis_reg_lag_5 <- lm(pin_proportion_of_country ~
                   security_apparatus_calendar_year_before_5_years +
                   factionalized_elites_calendar_year_before_5_years +
                   group_grievance_calendar_year_before_5_years +
                   economy_calendar_year_before_5_years +
                   economic_inequality_calendar_year_before_5_years +
                   humanflight_calendar_year_before_5_years +
                   state_legitimacy_calendar_year_before_5_years +
                   public_services_calendar_year_before_5_years +
                   humanrights_calendar_year_before_5_years +
                   demographic_pressures_calendar_year_before_5_years +
                   refugees_and_IDPs_calendar_year_before_5_years +
                   external_intervention_calendar_year_before_5_years,
                 data = initial_crisis_year_data)
summary(crisis_reg_lag_5)
```

    ## 
    ## Call:
    ## lm(formula = pin_proportion_of_country ~ security_apparatus_calendar_year_before_5_years + 
    ##     factionalized_elites_calendar_year_before_5_years + group_grievance_calendar_year_before_5_years + 
    ##     economy_calendar_year_before_5_years + economic_inequality_calendar_year_before_5_years + 
    ##     humanflight_calendar_year_before_5_years + state_legitimacy_calendar_year_before_5_years + 
    ##     public_services_calendar_year_before_5_years + humanrights_calendar_year_before_5_years + 
    ##     demographic_pressures_calendar_year_before_5_years + refugees_and_IDPs_calendar_year_before_5_years + 
    ##     external_intervention_calendar_year_before_5_years, data = initial_crisis_year_data)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.127872 -0.046920 -0.001219  0.052098  0.099862 
    ## 
    ## Coefficients:
    ##                                                    Estimate Std. Error t value
    ## (Intercept)                                        -0.37689    0.50714  -0.743
    ## security_apparatus_calendar_year_before_5_years     0.05551    0.06507   0.853
    ## factionalized_elites_calendar_year_before_5_years  -0.11553    0.05600  -2.063
    ## group_grievance_calendar_year_before_5_years       -0.04163    0.04410  -0.944
    ## economy_calendar_year_before_5_years                0.04351    0.05784   0.752
    ## economic_inequality_calendar_year_before_5_years   -0.10016    0.06117  -1.637
    ## humanflight_calendar_year_before_5_years            0.06017    0.03042   1.978
    ## state_legitimacy_calendar_year_before_5_years       0.11519    0.07313   1.575
    ## public_services_calendar_year_before_5_years       -0.11900    0.06751  -1.763
    ## humanrights_calendar_year_before_5_years            0.01283    0.04253   0.302
    ## demographic_pressures_calendar_year_before_5_years  0.19909    0.10607   1.877
    ## refugees_and_IDPs_calendar_year_before_5_years     -0.01927    0.02510  -0.768
    ## external_intervention_calendar_year_before_5_years -0.01736    0.03232  -0.537
    ##                                                    Pr(>|t|)  
    ## (Intercept)                                          0.4855  
    ## security_apparatus_calendar_year_before_5_years      0.4264  
    ## factionalized_elites_calendar_year_before_5_years    0.0847 .
    ## group_grievance_calendar_year_before_5_years         0.3816  
    ## economy_calendar_year_before_5_years                 0.4804  
    ## economic_inequality_calendar_year_before_5_years     0.1527  
    ## humanflight_calendar_year_before_5_years             0.0953 .
    ## state_legitimacy_calendar_year_before_5_years        0.1663  
    ## public_services_calendar_year_before_5_years         0.1284  
    ## humanrights_calendar_year_before_5_years             0.7730  
    ## demographic_pressures_calendar_year_before_5_years   0.1096  
    ## refugees_and_IDPs_calendar_year_before_5_years       0.4717  
    ## external_intervention_calendar_year_before_5_years   0.6104  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1189 on 6 degrees of freedom
    ##   (4 observations deleted due to missingness)
    ## Multiple R-squared:  0.7915, Adjusted R-squared:  0.3745 
    ## F-statistic: 1.898 on 12 and 6 DF,  p-value: 0.2221

``` r
nobs(crisis_reg_lag_5)
```

    ## [1] 19

``` r
## Panel Regression for 0 year lag 
# Linear regression model using the newly created columns as predictors
panel_reg_lag_0 <- lm(pin_proportion_of_country ~ 
                   security_apparatus_calendar_year_before_0_years +
                   factionalized_elites_calendar_year_before_0_years +
                   group_grievance_calendar_year_before_0_years +
                   economy_calendar_year_before_0_years +
                   economic_inequality_calendar_year_before_0_years +
                   humanflight__calendar_year_before_0_years +
                   state_legitimacy_calendar_year_before_0_years +
                   public_services_calendar_year_before_0_years +
                   humanrights_calendar_year_before_0_years +
                   demographic_pressures_calendar_year_before_0_years +
                   refugees_and_IDPs_calendar_year_before_0_years +
                   external_intervention_calendar_year_before_0_years,
                 data = complete_full_data)
summary(panel_reg_lag_0)
```

    ## 
    ## Call:
    ## lm(formula = pin_proportion_of_country ~ security_apparatus_calendar_year_before_0_years + 
    ##     factionalized_elites_calendar_year_before_0_years + group_grievance_calendar_year_before_0_years + 
    ##     economy_calendar_year_before_0_years + economic_inequality_calendar_year_before_0_years + 
    ##     humanflight__calendar_year_before_0_years + state_legitimacy_calendar_year_before_0_years + 
    ##     public_services_calendar_year_before_0_years + humanrights_calendar_year_before_0_years + 
    ##     demographic_pressures_calendar_year_before_0_years + refugees_and_IDPs_calendar_year_before_0_years + 
    ##     external_intervention_calendar_year_before_0_years, data = complete_full_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.33774 -0.10065 -0.00700  0.08838  0.60581 
    ## 
    ## Coefficients:
    ##                                                     Estimate Std. Error t value
    ## (Intercept)                                        -0.705120   0.151769  -4.646
    ## security_apparatus_calendar_year_before_0_years     0.017194   0.016302   1.055
    ## factionalized_elites_calendar_year_before_0_years  -0.019220   0.016623  -1.156
    ## group_grievance_calendar_year_before_0_years       -0.038290   0.015211  -2.517
    ## economy_calendar_year_before_0_years                0.064313   0.015115   4.255
    ## economic_inequality_calendar_year_before_0_years    0.002977   0.016522   0.180
    ## humanflight__calendar_year_before_0_years          -0.028963   0.011621  -2.492
    ## state_legitimacy_calendar_year_before_0_years       0.048415   0.022840   2.120
    ## public_services_calendar_year_before_0_years       -0.017430   0.019557  -0.891
    ## humanrights_calendar_year_before_0_years           -0.002563   0.023976  -0.107
    ## demographic_pressures_calendar_year_before_0_years  0.005569   0.014312   0.389
    ## refugees_and_IDPs_calendar_year_before_0_years      0.062674   0.020001   3.134
    ## external_intervention_calendar_year_before_0_years  0.017833   0.013666   1.305
    ##                                                    Pr(>|t|)    
    ## (Intercept)                                        5.72e-06 ***
    ## security_apparatus_calendar_year_before_0_years     0.29268    
    ## factionalized_elites_calendar_year_before_0_years   0.24880    
    ## group_grievance_calendar_year_before_0_years        0.01251 *  
    ## economy_calendar_year_before_0_years               3.06e-05 ***
    ## economic_inequality_calendar_year_before_0_years    0.85719    
    ## humanflight__calendar_year_before_0_years           0.01340 *  
    ## state_legitimacy_calendar_year_before_0_years       0.03511 *  
    ## public_services_calendar_year_before_0_years        0.37376    
    ## humanrights_calendar_year_before_0_years            0.91497    
    ## demographic_pressures_calendar_year_before_0_years  0.69757    
    ## refugees_and_IDPs_calendar_year_before_0_years      0.00195 ** 
    ## external_intervention_calendar_year_before_0_years  0.19326    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1588 on 228 degrees of freedom
    ##   (426 observations deleted due to missingness)
    ## Multiple R-squared:  0.4067, Adjusted R-squared:  0.3754 
    ## F-statistic: 13.02 on 12 and 228 DF,  p-value: < 2.2e-16

``` r
nobs(panel_reg_lag_0)
```

    ## [1] 241

``` r
## Panel Regression for 1 year lag
panel_reg_lag_1 <- lm(pin_proportion_of_country ~ 
                   security_apparatus_calendar_year_before_1_year +
                   factionalized_elites_calendar_year_before_1_year +
                   group_grievance_calendar_year_before_1_year +
                   economy_calendar_year_before_1_year +
                   economic_inequality_calendar_year_before_1_year +
                   humanflight_calendar_year_before_1_year +
                   state_legitimacy_calendar_year_before_1_year +
                   public_services_calendar_year_before_1_year +
                   humanrights_calendar_year_before_1_year +
                   demographic_pressures_calendar_year_before_1_year +
                   refugees_and_IDPs_calendar_year_before_1_year +
                   external_intervention_calendar_year_before_1_year,
                 data = complete_full_data)
summary(panel_reg_lag_1)
```

    ## 
    ## Call:
    ## lm(formula = pin_proportion_of_country ~ security_apparatus_calendar_year_before_1_year + 
    ##     factionalized_elites_calendar_year_before_1_year + group_grievance_calendar_year_before_1_year + 
    ##     economy_calendar_year_before_1_year + economic_inequality_calendar_year_before_1_year + 
    ##     humanflight_calendar_year_before_1_year + state_legitimacy_calendar_year_before_1_year + 
    ##     public_services_calendar_year_before_1_year + humanrights_calendar_year_before_1_year + 
    ##     demographic_pressures_calendar_year_before_1_year + refugees_and_IDPs_calendar_year_before_1_year + 
    ##     external_intervention_calendar_year_before_1_year, data = complete_full_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.40947 -0.10159 -0.01377  0.09671  0.56724 
    ## 
    ## Coefficients:
    ##                                                   Estimate Std. Error t value
    ## (Intercept)                                       -0.53990    0.14193  -3.804
    ## security_apparatus_calendar_year_before_1_year     0.01406    0.01692   0.831
    ## factionalized_elites_calendar_year_before_1_year  -0.01028    0.01762  -0.584
    ## group_grievance_calendar_year_before_1_year       -0.02652    0.01601  -1.656
    ## economy_calendar_year_before_1_year                0.07788    0.01613   4.827
    ## economic_inequality_calendar_year_before_1_year    0.01113    0.01709   0.651
    ## humanflight_calendar_year_before_1_year           -0.03168    0.01212  -2.613
    ## state_legitimacy_calendar_year_before_1_year       0.03149    0.02542   1.239
    ## public_services_calendar_year_before_1_year       -0.02070    0.01938  -1.068
    ## humanrights_calendar_year_before_1_year           -0.01596    0.02462  -0.648
    ## demographic_pressures_calendar_year_before_1_year -0.01400    0.01588  -0.882
    ## refugees_and_IDPs_calendar_year_before_1_year      0.06485    0.01824   3.556
    ## external_intervention_calendar_year_before_1_year  0.01537    0.01370   1.122
    ##                                                   Pr(>|t|)    
    ## (Intercept)                                       0.000183 ***
    ## security_apparatus_calendar_year_before_1_year    0.406804    
    ## factionalized_elites_calendar_year_before_1_year  0.560129    
    ## group_grievance_calendar_year_before_1_year       0.099151 .  
    ## economy_calendar_year_before_1_year               2.55e-06 ***
    ## economic_inequality_calendar_year_before_1_year   0.515555    
    ## humanflight_calendar_year_before_1_year           0.009568 ** 
    ## state_legitimacy_calendar_year_before_1_year      0.216780    
    ## public_services_calendar_year_before_1_year       0.286738    
    ## humanrights_calendar_year_before_1_year           0.517547    
    ## demographic_pressures_calendar_year_before_1_year 0.378835    
    ## refugees_and_IDPs_calendar_year_before_1_year     0.000458 ***
    ## external_intervention_calendar_year_before_1_year 0.263105    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1636 on 226 degrees of freedom
    ##   (428 observations deleted due to missingness)
    ## Multiple R-squared:  0.3735, Adjusted R-squared:  0.3403 
    ## F-statistic: 11.23 on 12 and 226 DF,  p-value: < 2.2e-16

``` r
nobs(panel_reg_lag_1)
```

    ## [1] 239

``` r
## Panel Regression for 2 year lag
panel_reg_lag_2 <- lm(pin_proportion_of_country ~ 
                   security_apparatus_calendar_year_before_2_years +
                   factionalized_elites_calendar_year_before_2_years +
                   group_grievance_calendar_year_before_2_years +
                   economy_calendar_year_before_2_years +
                   economic_inequality_calendar_year_before_2_years +
                   humanflight_calendar_year_before_2_years +
                   state_legitimacy_calendar_year_before_2_years +
                   public_services_calendar_year_before_2_years +
                   humanrights_calendar_year_before_2_years +
                   demographic_pressures_calendar_year_before_2_years +
                   refugees_and_IDPs_calendar_year_before_2_years +
                   external_intervention_calendar_year_before_2_years,
                 data = complete_full_data)
summary(panel_reg_lag_2)
```

    ## 
    ## Call:
    ## lm(formula = pin_proportion_of_country ~ security_apparatus_calendar_year_before_2_years + 
    ##     factionalized_elites_calendar_year_before_2_years + group_grievance_calendar_year_before_2_years + 
    ##     economy_calendar_year_before_2_years + economic_inequality_calendar_year_before_2_years + 
    ##     humanflight_calendar_year_before_2_years + state_legitimacy_calendar_year_before_2_years + 
    ##     public_services_calendar_year_before_2_years + humanrights_calendar_year_before_2_years + 
    ##     demographic_pressures_calendar_year_before_2_years + refugees_and_IDPs_calendar_year_before_2_years + 
    ##     external_intervention_calendar_year_before_2_years, data = complete_full_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.3799 -0.1127 -0.0135  0.1161  0.5475 
    ## 
    ## Coefficients:
    ##                                                     Estimate Std. Error t value
    ## (Intercept)                                        -0.409847   0.134233  -3.053
    ## security_apparatus_calendar_year_before_2_years     0.016504   0.017338   0.952
    ## factionalized_elites_calendar_year_before_2_years  -0.010604   0.017791  -0.596
    ## group_grievance_calendar_year_before_2_years       -0.026672   0.015931  -1.674
    ## economy_calendar_year_before_2_years                0.087294   0.015884   5.496
    ## economic_inequality_calendar_year_before_2_years    0.021191   0.017212   1.231
    ## humanflight_calendar_year_before_2_years           -0.035695   0.011923  -2.994
    ## state_legitimacy_calendar_year_before_2_years       0.008606   0.026843   0.321
    ## public_services_calendar_year_before_2_years       -0.010226   0.019575  -0.522
    ## humanrights_calendar_year_before_2_years           -0.008317   0.024713  -0.337
    ## demographic_pressures_calendar_year_before_2_years -0.040479   0.017252  -2.346
    ## refugees_and_IDPs_calendar_year_before_2_years      0.067456   0.015882   4.247
    ## external_intervention_calendar_year_before_2_years  0.013362   0.013448   0.994
    ##                                                    Pr(>|t|)    
    ## (Intercept)                                         0.00254 ** 
    ## security_apparatus_calendar_year_before_2_years     0.34218    
    ## factionalized_elites_calendar_year_before_2_years   0.55176    
    ## group_grievance_calendar_year_before_2_years        0.09548 .  
    ## economy_calendar_year_before_2_years               1.05e-07 ***
    ## economic_inequality_calendar_year_before_2_years    0.21955    
    ## humanflight_calendar_year_before_2_years            0.00307 ** 
    ## state_legitimacy_calendar_year_before_2_years       0.74882    
    ## public_services_calendar_year_before_2_years        0.60190    
    ## humanrights_calendar_year_before_2_years            0.73679    
    ## demographic_pressures_calendar_year_before_2_years  0.01983 *  
    ## refugees_and_IDPs_calendar_year_before_2_years     3.17e-05 ***
    ## external_intervention_calendar_year_before_2_years  0.32147    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1644 on 224 degrees of freedom
    ##   (430 observations deleted due to missingness)
    ## Multiple R-squared:  0.3707, Adjusted R-squared:  0.337 
    ## F-statistic:    11 on 12 and 224 DF,  p-value: < 2.2e-16

``` r
nobs(panel_reg_lag_2)
```

    ## [1] 237

``` r
## Panel Regression for 3 year lag
panel_reg_lag_3 <- lm(pin_proportion_of_country ~ 
                   security_apparatus_calendar_year_before_3_years +
                   factionalized_elites_calendar_year_before_3_years +
                   group_grievance_calendar_year_before_3_years +
                   economy_calendar_year_before_3_years +
                   economic_inequality_calendar_year_before_3_years +
                   humanflight_calendar_year_before_3_years +
                   state_legitimacy_calendar_year_before_3_years +
                   public_services_calendar_year_before_3_years +
                   humanrights_calendar_year_before_3_years +
                   demographic_pressures_calendar_year_before_3_years +
                   refugees_and_IDPs_calendar_year_before_3_years +
                   external_intervention_calendar_year_before_3_years,
                 data = complete_full_data)
summary(panel_reg_lag_3)
```

    ## 
    ## Call:
    ## lm(formula = pin_proportion_of_country ~ security_apparatus_calendar_year_before_3_years + 
    ##     factionalized_elites_calendar_year_before_3_years + group_grievance_calendar_year_before_3_years + 
    ##     economy_calendar_year_before_3_years + economic_inequality_calendar_year_before_3_years + 
    ##     humanflight_calendar_year_before_3_years + state_legitimacy_calendar_year_before_3_years + 
    ##     public_services_calendar_year_before_3_years + humanrights_calendar_year_before_3_years + 
    ##     demographic_pressures_calendar_year_before_3_years + refugees_and_IDPs_calendar_year_before_3_years + 
    ##     external_intervention_calendar_year_before_3_years, data = complete_full_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.38306 -0.11300 -0.01535  0.11923  0.55784 
    ## 
    ## Coefficients:
    ##                                                      Estimate Std. Error
    ## (Intercept)                                        -0.3114517  0.1327623
    ## security_apparatus_calendar_year_before_3_years     0.0096909  0.0185153
    ## factionalized_elites_calendar_year_before_3_years  -0.0014753  0.0183037
    ## group_grievance_calendar_year_before_3_years       -0.0172168  0.0162208
    ## economy_calendar_year_before_3_years                0.0876503  0.0166110
    ## economic_inequality_calendar_year_before_3_years    0.0279889  0.0178342
    ## humanflight_calendar_year_before_3_years           -0.0371024  0.0118023
    ## state_legitimacy_calendar_year_before_3_years      -0.0168853  0.0287552
    ## public_services_calendar_year_before_3_years       -0.0003232  0.0213990
    ## humanrights_calendar_year_before_3_years           -0.0036304  0.0260250
    ## demographic_pressures_calendar_year_before_3_years -0.0585312  0.0199366
    ## refugees_and_IDPs_calendar_year_before_3_years      0.0648859  0.0137398
    ## external_intervention_calendar_year_before_3_years  0.0172680  0.0132448
    ##                                                    t value Pr(>|t|)    
    ## (Intercept)                                         -2.346  0.01986 *  
    ## security_apparatus_calendar_year_before_3_years      0.523  0.60122    
    ## factionalized_elites_calendar_year_before_3_years   -0.081  0.93583    
    ## group_grievance_calendar_year_before_3_years        -1.061  0.28966    
    ## economy_calendar_year_before_3_years                 5.277 3.12e-07 ***
    ## economic_inequality_calendar_year_before_3_years     1.569  0.11798    
    ## humanflight_calendar_year_before_3_years            -3.144  0.00190 ** 
    ## state_legitimacy_calendar_year_before_3_years       -0.587  0.55766    
    ## public_services_calendar_year_before_3_years        -0.015  0.98796    
    ## humanrights_calendar_year_before_3_years            -0.139  0.88918    
    ## demographic_pressures_calendar_year_before_3_years  -2.936  0.00368 ** 
    ## refugees_and_IDPs_calendar_year_before_3_years       4.722 4.13e-06 ***
    ## external_intervention_calendar_year_before_3_years   1.304  0.19367    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.168 on 222 degrees of freedom
    ##   (432 observations deleted due to missingness)
    ## Multiple R-squared:  0.347,  Adjusted R-squared:  0.3117 
    ## F-statistic: 9.831 on 12 and 222 DF,  p-value: 2.505e-15

``` r
nobs(panel_reg_lag_3)
```

    ## [1] 235

``` r
## Panel Regression for 5 year lag
panel_reg_lag_5 <- lm(pin_proportion_of_country ~
                   security_apparatus_calendar_year_before_5_years +
                   factionalized_elites_calendar_year_before_5_years +
                   group_grievance_calendar_year_before_5_years +
                   economy_calendar_year_before_5_years +
                   economic_inequality_calendar_year_before_5_years +
                   humanflight_calendar_year_before_5_years +
                   state_legitimacy_calendar_year_before_5_years +
                   public_services_calendar_year_before_5_years +
                   humanrights_calendar_year_before_5_years +
                   demographic_pressures_calendar_year_before_5_years +
                   refugees_and_IDPs_calendar_year_before_5_years +
                   external_intervention_calendar_year_before_5_years,
                 data = complete_full_data)
summary(panel_reg_lag_5)
```

    ## 
    ## Call:
    ## lm(formula = pin_proportion_of_country ~ security_apparatus_calendar_year_before_5_years + 
    ##     factionalized_elites_calendar_year_before_5_years + group_grievance_calendar_year_before_5_years + 
    ##     economy_calendar_year_before_5_years + economic_inequality_calendar_year_before_5_years + 
    ##     humanflight_calendar_year_before_5_years + state_legitimacy_calendar_year_before_5_years + 
    ##     public_services_calendar_year_before_5_years + humanrights_calendar_year_before_5_years + 
    ##     demographic_pressures_calendar_year_before_5_years + refugees_and_IDPs_calendar_year_before_5_years + 
    ##     external_intervention_calendar_year_before_5_years, data = complete_full_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.48294 -0.11496 -0.00853  0.09387  0.61157 
    ## 
    ## Coefficients:
    ##                                                     Estimate Std. Error t value
    ## (Intercept)                                         0.020643   0.130302   0.158
    ## security_apparatus_calendar_year_before_5_years    -0.002757   0.018524  -0.149
    ## factionalized_elites_calendar_year_before_5_years   0.012761   0.018676   0.683
    ## group_grievance_calendar_year_before_5_years       -0.028578   0.015513  -1.842
    ## economy_calendar_year_before_5_years                0.085406   0.017444   4.896
    ## economic_inequality_calendar_year_before_5_years    0.007201   0.018350   0.392
    ## humanflight_calendar_year_before_5_years           -0.030827   0.011497  -2.681
    ## state_legitimacy_calendar_year_before_5_years      -0.039115   0.028398  -1.377
    ## public_services_calendar_year_before_5_years       -0.002172   0.020269  -0.107
    ## humanrights_calendar_year_before_5_years            0.011890   0.023456   0.507
    ## demographic_pressures_calendar_year_before_5_years -0.064556   0.022242  -2.902
    ## refugees_and_IDPs_calendar_year_before_5_years      0.077353   0.011990   6.452
    ## external_intervention_calendar_year_before_5_years  0.009236   0.012899   0.716
    ##                                                    Pr(>|t|)    
    ## (Intercept)                                         0.87427    
    ## security_apparatus_calendar_year_before_5_years     0.88181    
    ## factionalized_elites_calendar_year_before_5_years   0.49514    
    ## group_grievance_calendar_year_before_5_years        0.06680 .  
    ## economy_calendar_year_before_5_years               1.90e-06 ***
    ## economic_inequality_calendar_year_before_5_years    0.69514    
    ## humanflight_calendar_year_before_5_years            0.00789 ** 
    ## state_legitimacy_calendar_year_before_5_years       0.16981    
    ## public_services_calendar_year_before_5_years        0.91478    
    ## humanrights_calendar_year_before_5_years            0.61274    
    ## demographic_pressures_calendar_year_before_5_years  0.00408 ** 
    ## refugees_and_IDPs_calendar_year_before_5_years     7.05e-10 ***
    ## external_intervention_calendar_year_before_5_years  0.47476    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1697 on 218 degrees of freedom
    ##   (436 observations deleted due to missingness)
    ## Multiple R-squared:  0.3322, Adjusted R-squared:  0.2955 
    ## F-statistic: 9.038 on 12 and 218 DF,  p-value: 5.006e-14

``` r
nobs(panel_reg_lag_5)
```

    ## [1] 231

``` r
# fixed effect panel regression for lag year = 0
fe_model_0_lag <- feols(pin_proportion_of_country ~
                   security_apparatus_calendar_year_before_0_years +
                   factionalized_elites_calendar_year_before_0_years +
                   group_grievance_calendar_year_before_0_years +
                   economy_calendar_year_before_0_years +
                   economic_inequality_calendar_year_before_0_years +
                   humanflight__calendar_year_before_0_years +
                   state_legitimacy_calendar_year_before_0_years +
                   public_services_calendar_year_before_0_years +
                   humanrights_calendar_year_before_0_years +
                   demographic_pressures_calendar_year_before_0_years +
                   refugees_and_IDPs_calendar_year_before_0_years +
                   external_intervention_calendar_year_before_0_years
                   | Country  + calendar_year, data = complete_full_data)
```

    ## NOTE: 426 observations removed because of NA values (LHS: 415, RHS: 35).

``` r
summary (fe_model_0_lag)
```

    ## OLS estimation, Dep. Var.: pin_proportion_of_country
    ## Observations: 241
    ## Fixed-effects: Country: 23,  calendar_year: 13
    ## Standard-errors: Clustered (Country) 
    ##                                                     Estimate Std. Error
    ## security_apparatus_calendar_year_before_0_years     0.050943   0.021647
    ## factionalized_elites_calendar_year_before_0_years  -0.032216   0.043635
    ## group_grievance_calendar_year_before_0_years       -0.027249   0.026899
    ## economy_calendar_year_before_0_years                0.034134   0.019087
    ## economic_inequality_calendar_year_before_0_years    0.019401   0.026296
    ## humanflight__calendar_year_before_0_years           0.005624   0.027167
    ## state_legitimacy_calendar_year_before_0_years       0.038142   0.028135
    ## public_services_calendar_year_before_0_years        0.019869   0.028547
    ## humanrights_calendar_year_before_0_years            0.044181   0.035281
    ## demographic_pressures_calendar_year_before_0_years  0.020560   0.039982
    ## refugees_and_IDPs_calendar_year_before_0_years     -0.032092   0.025664
    ## external_intervention_calendar_year_before_0_years -0.014116   0.029482
    ##                                                      t value Pr(>|t|)    
    ## security_apparatus_calendar_year_before_0_years     2.353351 0.027954 *  
    ## factionalized_elites_calendar_year_before_0_years  -0.738292 0.468140    
    ## group_grievance_calendar_year_before_0_years       -1.013016 0.322065    
    ## economy_calendar_year_before_0_years                1.788302 0.087510 .  
    ## economic_inequality_calendar_year_before_0_years    0.737776 0.468448    
    ## humanflight__calendar_year_before_0_years           0.207028 0.837892    
    ## state_legitimacy_calendar_year_before_0_years       1.355712 0.188946    
    ## public_services_calendar_year_before_0_years        0.696021 0.493704    
    ## humanrights_calendar_year_before_0_years            1.252285 0.223616    
    ## demographic_pressures_calendar_year_before_0_years  0.514243 0.612210    
    ## refugees_and_IDPs_calendar_year_before_0_years     -1.250461 0.224268    
    ## external_intervention_calendar_year_before_0_years -0.478815 0.636795    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## RMSE: 0.086977     Adj. R2: 0.767359
    ##                  Within R2: 0.208072

``` r
nobs(fe_model_0_lag)
```

    ## [1] 241

``` r
# fixed effect panel regression for lag year = 1
fe_model_1_lag <- feols(pin_proportion_of_country ~
                   security_apparatus_calendar_year_before_1_year +
                   factionalized_elites_calendar_year_before_1_year +
                   group_grievance_calendar_year_before_1_year +
                   economy_calendar_year_before_1_year +
                   economic_inequality_calendar_year_before_1_year +
                   humanflight_calendar_year_before_1_year +
                   state_legitimacy_calendar_year_before_1_year +
                   public_services_calendar_year_before_1_year +
                   humanrights_calendar_year_before_1_year +
                   demographic_pressures_calendar_year_before_1_year +
                   refugees_and_IDPs_calendar_year_before_1_year +
                   external_intervention_calendar_year_before_1_year
                   | Country  + calendar_year, data = complete_full_data)
```

    ## NOTE: 428 observations removed because of NA values (LHS: 415, RHS: 60).

``` r
summary (fe_model_1_lag)
```

    ## OLS estimation, Dep. Var.: pin_proportion_of_country
    ## Observations: 239
    ## Fixed-effects: Country: 23,  calendar_year: 13
    ## Standard-errors: Clustered (Country) 
    ##                                                    Estimate Std. Error
    ## security_apparatus_calendar_year_before_1_year     0.055806   0.023026
    ## factionalized_elites_calendar_year_before_1_year  -0.014364   0.035087
    ## group_grievance_calendar_year_before_1_year       -0.020572   0.020792
    ## economy_calendar_year_before_1_year                0.032280   0.017492
    ## economic_inequality_calendar_year_before_1_year    0.007313   0.029236
    ## humanflight_calendar_year_before_1_year           -0.003716   0.023585
    ## state_legitimacy_calendar_year_before_1_year       0.029169   0.022789
    ## public_services_calendar_year_before_1_year       -0.007280   0.030194
    ## humanrights_calendar_year_before_1_year            0.028905   0.028118
    ## demographic_pressures_calendar_year_before_1_year  0.018432   0.036599
    ## refugees_and_IDPs_calendar_year_before_1_year     -0.057050   0.023917
    ## external_intervention_calendar_year_before_1_year  0.001204   0.028172
    ##                                                     t value Pr(>|t|)    
    ## security_apparatus_calendar_year_before_1_year     2.423580 0.024043 *  
    ## factionalized_elites_calendar_year_before_1_year  -0.409374 0.686224    
    ## group_grievance_calendar_year_before_1_year       -0.989440 0.333206    
    ## economy_calendar_year_before_1_year                1.845425 0.078479 .  
    ## economic_inequality_calendar_year_before_1_year    0.250149 0.804793    
    ## humanflight_calendar_year_before_1_year           -0.157538 0.876258    
    ## state_legitimacy_calendar_year_before_1_year       1.279915 0.213908    
    ## public_services_calendar_year_before_1_year       -0.241113 0.811700    
    ## humanrights_calendar_year_before_1_year            1.027959 0.315139    
    ## demographic_pressures_calendar_year_before_1_year  0.503628 0.619529    
    ## refugees_and_IDPs_calendar_year_before_1_year     -2.385330 0.026106 *  
    ## external_intervention_calendar_year_before_1_year  0.042728 0.966304    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## RMSE: 0.087657     Adj. R2: 0.764253
    ##                  Within R2: 0.182035

``` r
# fixed effect panel regression for lag year = 2
fe_model_2_lag <- feols(pin_proportion_of_country ~
                   security_apparatus_calendar_year_before_2_years +
                   factionalized_elites_calendar_year_before_2_years +
                   group_grievance_calendar_year_before_2_years +
                   economy_calendar_year_before_2_years +
                   economic_inequality_calendar_year_before_2_years +
                   humanflight_calendar_year_before_2_years +
                   state_legitimacy_calendar_year_before_2_years +
                   public_services_calendar_year_before_2_years +
                   humanrights_calendar_year_before_2_years +
                   demographic_pressures_calendar_year_before_2_years +
                   refugees_and_IDPs_calendar_year_before_2_years +
                   external_intervention_calendar_year_before_2_years
                   | Country  + calendar_year, data = complete_full_data)
```

    ## NOTE: 430 observations removed because of NA values (LHS: 415, RHS: 85).

``` r
summary (fe_model_2_lag)
```

    ## OLS estimation, Dep. Var.: pin_proportion_of_country
    ## Observations: 237
    ## Fixed-effects: Country: 23,  calendar_year: 13
    ## Standard-errors: Clustered (Country) 
    ##                                                     Estimate Std. Error
    ## security_apparatus_calendar_year_before_2_years     0.047454   0.026374
    ## factionalized_elites_calendar_year_before_2_years  -0.059959   0.037845
    ## group_grievance_calendar_year_before_2_years       -0.011457   0.019481
    ## economy_calendar_year_before_2_years                0.036372   0.018497
    ## economic_inequality_calendar_year_before_2_years    0.030938   0.024809
    ## humanflight_calendar_year_before_2_years           -0.020839   0.021910
    ## state_legitimacy_calendar_year_before_2_years       0.013432   0.021336
    ## public_services_calendar_year_before_2_years        0.015205   0.024643
    ## humanrights_calendar_year_before_2_years            0.024487   0.027984
    ## demographic_pressures_calendar_year_before_2_years -0.018885   0.022999
    ## refugees_and_IDPs_calendar_year_before_2_years     -0.054388   0.016632
    ## external_intervention_calendar_year_before_2_years  0.019305   0.028092
    ##                                                      t value Pr(>|t|)    
    ## security_apparatus_calendar_year_before_2_years     1.799307 0.085703 .  
    ## factionalized_elites_calendar_year_before_2_years  -1.584336 0.127388    
    ## group_grievance_calendar_year_before_2_years       -0.588125 0.562438    
    ## economy_calendar_year_before_2_years                1.966319 0.062004 .  
    ## economic_inequality_calendar_year_before_2_years    1.247025 0.225502    
    ## humanflight_calendar_year_before_2_years           -0.951124 0.351875    
    ## state_legitimacy_calendar_year_before_2_years       0.629547 0.535475    
    ## public_services_calendar_year_before_2_years        0.617004 0.543565    
    ## humanrights_calendar_year_before_2_years            0.875030 0.391011    
    ## demographic_pressures_calendar_year_before_2_years -0.821113 0.420391    
    ## refugees_and_IDPs_calendar_year_before_2_years     -3.270048 0.003502 ** 
    ## external_intervention_calendar_year_before_2_years  0.687208 0.499133    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## RMSE: 0.086705     Adj. R2: 0.770089
    ##                  Within R2: 0.193231

``` r
# fixed effect panel regression for lag year = 3
fe_model_3_lag <- feols(pin_proportion_of_country ~
                   security_apparatus_calendar_year_before_3_years +
                   factionalized_elites_calendar_year_before_3_years +
                   group_grievance_calendar_year_before_3_years +
                   economy_calendar_year_before_3_years +
                   economic_inequality_calendar_year_before_3_years +
                   humanflight_calendar_year_before_3_years +
                   state_legitimacy_calendar_year_before_3_years +
                   public_services_calendar_year_before_3_years +
                   humanrights_calendar_year_before_3_years +
                   demographic_pressures_calendar_year_before_3_years +
                   refugees_and_IDPs_calendar_year_before_3_years +
                   external_intervention_calendar_year_before_3_years
                   | Country  + calendar_year, data = complete_full_data)
```

    ## NOTE: 432 observations removed because of NA values (LHS: 415, RHS: 110).

``` r
summary (fe_model_3_lag)
```

    ## OLS estimation, Dep. Var.: pin_proportion_of_country
    ## Observations: 235
    ## Fixed-effects: Country: 22,  calendar_year: 13
    ## Standard-errors: Clustered (Country) 
    ##                                                     Estimate Std. Error
    ## security_apparatus_calendar_year_before_3_years     0.028026   0.027124
    ## factionalized_elites_calendar_year_before_3_years  -0.056575   0.032268
    ## group_grievance_calendar_year_before_3_years        0.021478   0.022264
    ## economy_calendar_year_before_3_years                0.030453   0.017665
    ## economic_inequality_calendar_year_before_3_years    0.032995   0.025449
    ## humanflight_calendar_year_before_3_years           -0.028386   0.020251
    ## state_legitimacy_calendar_year_before_3_years      -0.007780   0.031207
    ## public_services_calendar_year_before_3_years        0.036926   0.030864
    ## humanrights_calendar_year_before_3_years            0.014756   0.024283
    ## demographic_pressures_calendar_year_before_3_years -0.040659   0.023197
    ## refugees_and_IDPs_calendar_year_before_3_years     -0.040840   0.016339
    ## external_intervention_calendar_year_before_3_years  0.010027   0.025457
    ##                                                      t value Pr(>|t|)    
    ## security_apparatus_calendar_year_before_3_years     1.033260 0.313236    
    ## factionalized_elites_calendar_year_before_3_years  -1.753304 0.094136 .  
    ## group_grievance_calendar_year_before_3_years        0.964728 0.345660    
    ## economy_calendar_year_before_3_years                1.723948 0.099409 .  
    ## economic_inequality_calendar_year_before_3_years    1.296507 0.208868    
    ## humanflight_calendar_year_before_3_years           -1.401676 0.175620    
    ## state_legitimacy_calendar_year_before_3_years      -0.249316 0.805539    
    ## public_services_calendar_year_before_3_years        1.196436 0.244860    
    ## humanrights_calendar_year_before_3_years            0.607681 0.549915    
    ## demographic_pressures_calendar_year_before_3_years -1.752728 0.094238 .  
    ## refugees_and_IDPs_calendar_year_before_3_years     -2.499524 0.020800 *  
    ## external_intervention_calendar_year_before_3_years  0.393865 0.697651    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## RMSE: 0.087948     Adj. R2: 0.765551
    ##                  Within R2: 0.157559

``` r
# fixed effect panel regression for lag year = 5
fe_model_5_lag <- feols(pin_proportion_of_country ~
                   security_apparatus_calendar_year_before_5_years +
                   factionalized_elites_calendar_year_before_5_years +
                   group_grievance_calendar_year_before_5_years +
                   economy_calendar_year_before_5_years +
                   economic_inequality_calendar_year_before_5_years +
                   humanflight_calendar_year_before_5_years +
                   state_legitimacy_calendar_year_before_5_years +
                   public_services_calendar_year_before_5_years +
                   humanrights_calendar_year_before_5_years +
                   demographic_pressures_calendar_year_before_5_years +
                   refugees_and_IDPs_calendar_year_before_5_years +
                   external_intervention_calendar_year_before_5_years
                   | Country  + calendar_year, data = complete_full_data)
```

    ## NOTE: 436 observations removed because of NA values (LHS: 415, RHS: 158).

``` r
summary (fe_model_5_lag)
```

    ## OLS estimation, Dep. Var.: pin_proportion_of_country
    ## Observations: 231
    ## Fixed-effects: Country: 22,  calendar_year: 13
    ## Standard-errors: Clustered (Country) 
    ##                                                     Estimate Std. Error
    ## security_apparatus_calendar_year_before_5_years    -0.003651   0.020328
    ## factionalized_elites_calendar_year_before_5_years  -0.058814   0.032718
    ## group_grievance_calendar_year_before_5_years        0.010137   0.030637
    ## economy_calendar_year_before_5_years               -0.000128   0.019822
    ## economic_inequality_calendar_year_before_5_years   -0.013457   0.030887
    ## humanflight_calendar_year_before_5_years           -0.010927   0.024372
    ## state_legitimacy_calendar_year_before_5_years      -0.016067   0.042103
    ## public_services_calendar_year_before_5_years        0.046376   0.029764
    ## humanrights_calendar_year_before_5_years            0.029404   0.022212
    ## demographic_pressures_calendar_year_before_5_years -0.008587   0.034351
    ## refugees_and_IDPs_calendar_year_before_5_years      0.014655   0.016585
    ## external_intervention_calendar_year_before_5_years -0.020889   0.025924
    ##                                                      t value Pr(>|t|)    
    ## security_apparatus_calendar_year_before_5_years    -0.179596 0.859192    
    ## factionalized_elites_calendar_year_before_5_years  -1.797640 0.086629 .  
    ## group_grievance_calendar_year_before_5_years        0.330858 0.744030    
    ## economy_calendar_year_before_5_years               -0.006443 0.994920    
    ## economic_inequality_calendar_year_before_5_years   -0.435689 0.667508    
    ## humanflight_calendar_year_before_5_years           -0.448351 0.658491    
    ## state_legitimacy_calendar_year_before_5_years      -0.381610 0.706585    
    ## public_services_calendar_year_before_5_years        1.558128 0.134146    
    ## humanrights_calendar_year_before_5_years            1.323821 0.199793    
    ## demographic_pressures_calendar_year_before_5_years -0.249968 0.805041    
    ## refugees_and_IDPs_calendar_year_before_5_years      0.883604 0.386914    
    ## external_intervention_calendar_year_before_5_years -0.805779 0.429401    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## RMSE: 0.090873     Adj. R2: 0.747837
    ##                  Within R2: 0.094589
