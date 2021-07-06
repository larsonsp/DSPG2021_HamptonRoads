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


# Pulls Virginia Table ----------------------------------------------------
va_gen_vet_status <- get_acs(geography = "state",
                             state = "VA",
                             variables = c(va_vet_status = "S2101_C03_001"),
                             summary_var = "S2101_C01_001",
                             geometry = TRUE) %>% 
  mutate(estimate = 100 * (estimate / summary_est))

va_gen_vet_status


gen_vet_status_plot <- get_acs(geography = "county",
                          state = "VA",
                          county = county_fips,
                          variables = c(gen_vet_proportion = "S2101_C03_001"),
                          summary_var = "S2101_C01_001",
                          geometry = TRUE) %>% 
  mutate(estimate = 100 * (sum(estimate) / sum(summary_est)))
  

gen_vet_status_plot

black_vet_status_plot <- get_acs(geography = "county",
                               state = "VA",
                               county = county_fips,
                               variables = c(black_vet_proportion = "S2101_C03_015"),
                               summary_var = "S2101_C01_015",
                               geometry = TRUE) %>% 
  mutate(estimate = 100 * (sum(estimate) / sum(summary_est)))

black_vet_status_plot

# Pulls and maps Veteran Status Tables ------------------------------------

gen_vet_status <- get_acs(geography = "county",
                          state = "VA",
                          county = county_fips,
                          variables = c(gen_vet_proportion = "S2101_C03_001"),
                          summary_var = "S2101_C01_001",
                          geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(NAME = c("Gloucester", "IW", "JC", "Southampton", "York", "Portsmouth", "Hampton", "Norfolk", "NN", "Ches", "Mathews", "F", "W", "Suffolk", "VB", "P")) %>% 
  ggplot() + geom_sf(aes(fill = Percent)) +
  geom_sf_label(aes(label = NAME), label.padding = unit(.5,"mm"), size = 2) +
  labs(title = "",
       x = "",
       y = "",
       caption = "ACS 2019 5 Year Estimates Table S2101")

gen_vet_status

black_vet_status <- get_acs(geography = "county",
                            state = "VA",
                            county = county_fips,
                            variables = c(black_vet_proportion = "S2101_C03_015"),
                            summary_var = "S2101_C01_015",
                            geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(NAME = c("Gloucester", "IW", "JC", "Southampton", "York", "Portsmouth", "Hampton", "Norfolk", "NN", "Ches", "Mathews", "F", "W", "Suffolk", "VB", "P")) %>% 
  ggplot() + geom_sf(aes(fill = Percent)) +
  geom_sf_label(aes(label = NAME), label.padding = unit(.5,"mm"), size = 2) +
  labs(title = "",
       x = "",
       y = "",
       caption = "ACS 2019 5 Year Estimates Table S2101")

black_vet_status

int_acc <- get_acs(geography = "county",
                   state = "VA",
                   county = county_fips,
                   Year = 2019,
                   table = "B28003")
                  



