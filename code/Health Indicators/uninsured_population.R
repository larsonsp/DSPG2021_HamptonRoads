# Installs & Loads Required Packages --------------------------------------
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

#pulls total uninsured Hampton poulation and calculates percentage
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
                             summary_var = "S2701_C04_001",
                             geometry = TRUE) %>% 
  mutate(pct_b_uninsured = 100 * (estimate / summary_est)) %>% 
  select(NAME, variable, pct_b_uninsured)

ggplot(data = hampton_black_cov) + geom_bar(mapping = aes(y = pct_b_uninsured))




uninsured <- function(data_frame) {
  get_acs(geography = "county",
          state = "VA",
          county = county_fips,
          variables = varcode,
          year = 2019,
          geometry = TRUE) %>%
    ggplot() + geom_sf(aes(fill = estimate))
  
}