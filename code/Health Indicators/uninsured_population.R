# Installs & Loads Required Packages --------------------------------------
install.packages("tidycensus", "tidyverse", "dplyr", "tigris", "ggplot2", "sf")
library(tidycensus)
library(tidyverse)
library(dplyr)
library(tigris)
library(ggplot2)
library(sf)

# Retrieves ACS Tables ----------------------------------------------------
#identifies county fips
county_fips <- c(550, 620, 650, 700, 710, 735, 740, 800, 810,
                 830, 073, 093, 095, 115, 175, 199)

#pulls total uninsured Virginia population and calculates percentage
va_cov <- get_acs(geography = "state",
                  state = "VA",
                  year = 2019,
                  variables = "S2701_C04_001",
                  summary_var = "S2701_C01_001") %>% 
  mutate(pct_tot_uninsured = 100 * (estimate / summary_est)) %>% 
  select(NAME, variable, pct_tot_uninsured)

#pulls total uninsured Hampton population and calculates percentage
hampton_tot_cov <- get_acs(geography = "county",
                           state = "VA",
                           county = county_fips,
                           year = 2019,
                           variables = "S2701_C04_001",
                           summary_var = "S2701_C01_001",
                           geometry = TRUE) %>% 
  mutate(hampt_pct_tot = 100 * (estimate / summary_est)) %>% 
  select(NAME, variable, hampt_pct_tot)

#pulls black uninsured Hampton population and calculates percentage
hampton_black_cov <- get_acs(geography = "county",
                             state = "VA",
                             county = county_fips,
                             year = 2019,
                             variables = "S2701_C04_017",
                             summary_var = "S2701_C01_017",
                             geometry = TRUE) %>% 
  mutate(pct_b_uninsured = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, pct_b_uninsured)

# Plots Data -----------------------------------------------
#plots Hampton uninsured data for black population
hmp_black_bar <- hampton_black_cov %>% 
  mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
  mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
  ggplot(aes(x = NAME, y = pct_b_uninsured)) + geom_col() +
  theme_minimal() +
  labs(title = "Hampton Roads: Black Uninsured Population",
       y = "Percent (%)",
       x = "Counties of Hampton Roads",
       caption = "Source: ACS 5 Year Estimate")

hmp_black_bar

#plots Hampton uninsured data for total population
hmp_tot_bar <- hampton_tot_cov %>% 
  mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
  mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
  ggplot(aes(x = NAME, y = hampt_pct_tot)) + geom_col() +
  theme_minimal() +
  labs(title = "Hampton Roads: Total Uninsured Population",
       y = "Percent (%)",
       x = "Counties of Hampton Roads",
       caption = "Source: ACS 5 Year Estimate")

hmp_tot_bar

#plots Virginia uninsured data for total population
va_tot_bar <- va_cov %>% 
  ggplot(aes(x = NAME, y = pct_tot_uninsured)) + geom_col() +
  theme_minimal() +
  labs(title = "Virginia: Total Uninsured Population",
       y = "Percent (%)",
       x = "Virginia",
       caption = "Source: ACS 5 Year Estimate")

va_tot_bar

# Plots Comparison Stacked Barchart ---------------------------------------
#resets column labels to allow for rbind
hampton_tot_cov <- get_acs(geography = "county",
                           state = "VA",
                           county = county_fips,
                           year = 2019,
                           variables = "S2701_C04_001",
                           summary_var = "S2701_C01_001",
                           geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>% 
  select(NAME, variable, estimate)

hampton_black_cov <- get_acs(geography = "county",
                             state = "VA",
                             county = county_fips,
                             year = 2019,
                             variables = "S2701_C04_017",
                             summary_var = "S2701_C01_017",
                             geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>%
  select(NAME, variable, estimate)

#rbinds data frames
b_tothamp_totva <- rbind(hampton_black_cov, hampton_tot_cov)

#creates stacked barchart to compare total uninsured population to black uninsured population
stack_bar <- b_tothamp_totva %>% 
  mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
  mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
  ggplot(aes(fill = variable, y = estimate, x = NAME)) +
    geom_bar(position = "stack", stat = "identity") +
    theme_minimal() +
     labs(title = "Hampton Roads Uninsured Population Comparison",
       y = "Percent (%)",
       x = "Counties of Hampton Roads",
       caption = "Source: ACS 5 Year Estimate Table S2701") +
       theme(axis.text.x = element_text(angle = 40))

stack_bar + scale_fill_manual(values = c("cornsilk4", "navy"))

