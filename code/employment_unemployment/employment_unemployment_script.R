# Installs & Loads Required Packages --------------------------------------
install.packages("tidycensus", "tidyverse", "dplyr", "tigris", "ggplot2", "sf", "plotly")
library(tidycensus)
library(tidyverse)
library(dplyr)
library(tigris)
library(ggplot2)
library(sf)
library(plotly)

# Retrieves ACS Tables ----------------------------------------------------
#identifies county fips
county_fips <- c(550, 620, 650, 700, 710, 735, 740, 800, 810,
                 830, 073, 093, 095, 115, 175, 199)

#pulls Virginia total population unemployment rate
va_unemp_rate <- get_acs(geography = "state",
                         state = "VA",
                         year = 2019,
                         variables = c(Virginia_General_Unemployment_Rate = "S2301_C04_001"))

va_black_unemp <- get_acs(geography = "state",
                          state = "VA",
                          year = 2019,
                          variables = c(Virginia_Black_Unemployment_Rate = "S2301_C04_013"))


#pulls Hampton Roads total population unemployment rate
hampton_unemp_rate <- get_acs(geography = "county",
                              state = "VA",
                              county = county_fips,
                              year = 2019,
                              variables = c("Total Population"= "S2301_C04_001"),
                              geometry = TRUE)
hamp_sum_unemp <- get_acs(geography = "county",
                  state = "VA",
                  county = county_fips,
                  year = 2019,
                  variables = c(Hampton_Roads_General_Unemployment_Rate = "S2301_C04_001"),
                  geometry = TRUE) %>% 
  mutate(estimate = (sum(estimate)) / (16)) %>%
  filter(NAME == "Gloucester County, Virginia") %>% 
  mutate(NAME = "Hampton Roads")


hamp_sum_black_unemp <- get_acs(geography = "county",
                                state = "VA",
                                county = county_fips,
                                year = 2019,
                                variables = c(Hampton_Roads_Black_Unemployment_Rate = "S2301_C04_013"),
                                geometry = TRUE) %>% 
  mutate(estimate = (sum(estimate)) / (16)) %>%
  filter(NAME == "Gloucester County, Virginia") %>% 
  mutate(NAME = "Hampton Roads")

sums <- data.frame(va_unemp_rate, va_black_unemp, hamp_sum_unemp, hamp_sum_black_unemp)
  
#pulls Hampton Roads black population unemployment rate
hampton_b_unemp_rate <- get_acs(geography = "county",
                                state = "VA",
                                county = county_fips,
                                year = 2019,
                                variables = c("Black Population" = "S2301_C04_013"),
                                geometry = TRUE)

# Plots Proportional Black Unemployment & Population Unemployment ---------

#binds rows for graph
compare_unemp <- rbind(hampton_unemp_rate, hampton_b_unemp_rate)

#graphs side by side bar chart
#"#D55E00","#0072B2" 
sums_stack <- sums %>% ggplot(aes(fill = variable, y = estimate, x = NAME)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  labs(title = "Unemployment Rate",
       y = "Unemployment Rate (%)",
       x = "Geography",
       caption = "Source: ACS 5 Year Estimate Table S2301")

sums_stack
                                
  
stack_unemp <- compare_unemp %>% 
  mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
  mutate(NAME = str_remove(NAME, "city, Virginia")) %>%
  arrange(desc(NAME)) %>% 
  ggplot(aes(fill = variable, y = estimate, x = NAME)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_hline(yintercept = va_unemp_rate$estimate, linetype="dashed", color = "red") +
  geom_hline(yintercept = hamp_sum_unemp$estimate, linetype = "dashed", color = "black") +
  geom_text(aes(label = paste(round(estimate, digits = 2), "%")), vjust = -0.2, size = 3, colour = "black",
            position = position_dodge(0.9))+
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(title = "",
       y = "Unemployment Rate (%)",
       x = "",
       caption = "Source: 2019 ACS 5 Year Estimate Table S2301") +
  theme(axis.text.x = element_text(angle = 40)) +
  scale_fill_manual(values = c("#D55E00", "#0072B2"))

stack_unemp








