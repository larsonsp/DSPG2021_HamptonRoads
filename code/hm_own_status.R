# Installs and Loads  Required Packages-----------------------------------------------------
install.packages("tidycensus", "tidyverse", "dplyr", "tigris", "ggplot2", "sf")
library(tidycensus)
library(tidyverse)
library(dplyr)
library(tigris)
library(ggplot2)
library(sf)

#saves shapefiles for future use
tigris_use_cache = TRUE

#identifies counties by fip code
county_fips <- c(550, 620, 650, 700, 710, 735, 740, 800, 810,
                 830, 073, 093, 095, 115, 175, 199)


# Pulls ACS Housing Data for Each County of Hampton Roads----------------------------------

#creates table with all variables
housing_tbl <- get_acs(geography = "county",
                         state = "VA",
                         county = county_fips,
                         variables = c("S2502_C02_003", "S2502_C04_003", "S2502_C06_003"),
                         geometry = TRUE)

#pulls and maps overall population of homeowners in proportion to total home occupancy
tot_hm_own <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      variables = c(overall_owned_housing = "S2502_C03_001"),
                      summary_var = "S2502_C01_001",
                      geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>% 
  mutate(NAME = c("Gloucester", "IW", "JC", "Southampton", "York", "Portsmouth", "Hampton", "Norfolk", "NN", "Ches", "Mathews", "F", "W", "Suffolk", "VB", "P")) %>% 
  ggplot() + geom_sf(aes(fill = estimate)) +
  geom_sf_label(aes(label = NAME), label.padding = unit(.5,"mm"), size = 2) +
  labs(title = "Homeowners Proportionate to Total Population Occupancy",
       x = "Longitude",
       y = "Latitude",
       caption = "ACS 2019 5 Year Estimates Table S2502")

tot_hm_own

#pulls and maps overall population of renters in proportion to total home occupancy
tot_hm_rent <- get_acs(geography = "county",
                       state = "VA",
                       county = county_fips,
                       variables = c(overall_rented_housing = "S2502_C05_001"),
                       summary_var = "S2502_C01_001",
                       geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>% 
  mutate(NAME = c("Gloucester", "IW", "JC", "Southampton", "York", "Portsmouth", "Hampton", "Norfolk", "NN", "Ches", "Mathews", "F", "W", "Suffolk", "VB", "P")) %>% 
  ggplot() + geom_sf(aes(fill = estimate)) +
  geom_sf_label(aes(label = NAME), label.padding = unit(.5,"mm"), size = 2) +
  labs(title = "Home Renters Proportionate to Total Population Occupancy",
       x = "Longitude",
       y = "Latitude",
       caption = "ACS 2019 5 Year Estimates Table S2502")

tot_hm_rent

#pulls and maps black homeowners in proportion to black home occupancy
b_hm_own <- get_acs(geography = "county",
                  state = "VA",
                  county = county_fips,
                  variables = c(black_owned_housing = "S2502_C03_003"),
                  summary_var = "S2502_C01_003",
                  geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(NAME = c("Gloucester", "IW", "JC", "Southampton", "York", "Portsmouth", "Hampton", "Norfolk", "NN", "Ches", "Mathews", "F", "W", "Suffolk", "VB", "P")) %>% 
  ggplot() + geom_sf(aes(fill = Percent)) +
  geom_sf_label(aes(label = NAME), label.padding = unit(.5,"mm"), size = 2) +
  labs(title = "",
       x = "",
       y = "",
       caption = "ACS 2019 5 Year Estimates Table S2502")

b_hm_own

#pulls and maps black renters in proportion to black home occupancy
b_hm_rent <- get_acs(geography = "county",
                  state = "VA",
                  county = county_fips,
                  variables = c(black_rented_housing = "S2502_C05_003"),
                  summary_var = "S2502_C01_003",
                  geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est)) %>% 
  mutate(NAME = c("Gloucester", "IW", "JC", "Southampton", "York", "Portsmouth", "Hampton", "Norfolk", "NN", "Ches", "Mathews", "F", "W", "Suffolk", "VB", "P")) %>% 
  ggplot() + geom_sf(aes(fill = estimate)) +
  geom_sf_label(aes(label = NAME), label.padding = unit(.5,"mm"),size = 2) +
  labs(title = "Black Renters Proportionate to Total Black Occupancy",
       x = "",
       y = "",
       caption = "ACS 2019 5 Year Estimates Table S2502")
  
b_hm_rent







