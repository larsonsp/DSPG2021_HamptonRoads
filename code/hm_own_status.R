
# Sets Working Directory --------------------------------------------------
setwd("/Users/mattb24/Documents/DSPG_Hampton_Roads/DSPG2021_HamptonRoads/code/hampton_roads_home_ownership")

# Installs and Loads  Required Packages-----------------------------------------------------
install.packages("tidycensus", "tidyverse", "dplyr", "tigris", "ggplot2", "sf", "leaflet", "RColorBrewer", "htmlwidgets")
library(tidycensus)
library(tidyverse)
library(dplyr)
library(tigris)
library(ggplot2)
library(sf)
library(leaflet)
library(RColorBrewer)
library(htmlwidgets)

#saves shapefiles for future use
tigris_use_cache = TRUE

#identifies counties by fip code
county_fips <- c(550, 620, 650, 700, 710, 735, 740, 800, 810,
                 830, 073, 093, 095, 115, 175, 199)


# Pulls ACS Housing Data for Each County of Hampton Roads and ggplots them----------------------------------

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
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2))
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
  mutate(Percent = 100 * (estimate / summary_est)) %>%
  mutate(Percent = round(Percent, 2))
  mutate(NAME = c("Gloucester", "IW", "JC", "Southampton", "York", "Portsmouth", "Hampton", "Norfolk", "NN", "Ches", "Mathews", "F", "W", "Suffolk", "VB", "P")) %>% 
  ggplot() + geom_sf(aes(fill = estimate)) +
  geom_sf_label(aes(label = NAME), label.padding = unit(.5,"mm"), size = 2) +
  labs(title = "Home Renters Proportionate to Total Population Occupancy",
       x = "Longitude",
       y = "Latitude",
       caption = "ACS 2019 5 Year Estimates Table S2502")

tot_hm_rent

#pulls 2019 ACS and maps black homeowners in proportion to total black home occupancy
b_hm_own <- get_acs(geography = "county",
                  state = "VA",
                  county = county_fips,
                  variables = c(black_owned_housing = "S2502_C03_003"),
                  summary_var = "S2502_C01_003",
                  geometry = TRUE) %>% 
                  mutate(Percent = 100 * (estimate / summary_est)) %>% 
                  mutate(Percent = round(Percent, 2)) %>% 
  mutate(NAME = c("Gloucester", "IW", "JC", "Southampton", "York", "Portsmouth", "Hampton", "Norfolk", "NN", "Ches", "Mathews", "F", "W", "Suffolk", "VB", "P")) %>% 
  ggplot() + geom_sf(aes(fill = Percent)) +
  geom_sf_label(aes(label = NAME), label.padding = unit(.5,"mm"), size = 2) +
  labs(title = "",
       x = "",
       y = "",
       caption = "ACS 2019 5 Year Estimates Table S2502")

b_hm_own

#pulls 2019 ACS and maps black renters in proportion to total black home occupancy
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


# Adds interactivity and time elements to black population variable------------------------------------

#sets palette for leaflet maps
pal <- colorNumeric(palette = "viridis", domain = b_hm_own$Percent)

#pulls 2019 black homeownership data w/o mapping ggplot
b_hm_own_19 <- get_acs(geography = "county",
                    state = "VA",
                    county = county_fips,
                    variables = c(black_owned_housing = "S2502_C03_003"),
                    summary_var = "S2502_C01_003",
                    geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2))

##2019 black homeownership leaflet map
b_hm_own_leaf_19 <- b_hm_own_19 %>% 
  st_transform() %>% 
  leaflet(options = leafletOptions(minZoom = 8.5)) %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>% 
  addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
              highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), label = ~paste0(NAME,  " Black Homeowners: ",
                                                                                                                   Percent, "%")) %>% 
  addLegend("topleft",
            pal = pal,
            values = ~ Percent,
            title = "Black Homeowners",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)
###saves map as an html
saveWidget(b_hm_own_leaf_19, file = "b_hm_own_leaf_19.html")

#pulls 2018 data w/o mapping ggplot
b_hm_own_18 <- get_acs(geography = "county",
                       state = "VA",
                       county = county_fips,
                       variables = c(black_owned_housing = "S2502_C03_003"),
                       summary_var = "S2502_C01_003",
                       year = 2018,
                       geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2))

b_hm_own_18

##2018 black homeownership leaflet map
b_hm_own_leaf_18 <- b_hm_own_18 %>% 
  st_transform() %>% 
  leaflet(options = leafletOptions(minZoom = 8.5)) %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>% 
  addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
              highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), label = ~paste0(NAME,  " Black Homeowners: ",
                                                                                                                   Percent, "%")) %>% 
  addLegend("topleft",
            pal = pal,
            values = ~ Percent,
            title = "Black Homeowners",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)

#pulls 2017 data w/o mapping ggplot
b_hm_own_17 <- get_acs(geography = "county",
                       state = "VA",
                       county = county_fips,
                       variables = c(black_owned_housing = "S2502_C03_003"),
                       summary_var = "S2502_C01_003",
                       year = 2017,
                       geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2))

b_hm_own_17

##2017 black homeownership leaflet map
b_hm_own_leaf_17 <- b_hm_own_17 %>% 
  st_transform() %>% 
  leaflet(options = leafletOptions(minZoom = 8.5)) %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>% 
  addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
              highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), label = ~paste0(NAME,  " Black Homeowners: ",
                                                                                                                   Percent, "%")) %>% 
  addLegend("topleft",
            pal = pal,
            values = ~ Percent,
            title = "Black Homeowners",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)

###saves map as html
saveWidget(b_hm_own_leaf_17, file = "b_hm_own_leaf_17.html")

# Adds interactivity and time elements to total population variable -------

#pulls 2019 total population data w/0 ggplot
tot_hm_own_19 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      variables = c(overall_owned_housing = "S2502_C03_001"),
                      summary_var = "S2502_C01_001",
                      geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2))

##2019 total population homeownership leaflet map
tot_hm_own_leaf_19 <- tot_hm_own_19 %>% 
  st_transform() %>% 
  leaflet(options = leafletOptions(minZoom = 8.5)) %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>% 
  addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
              label = ~paste0(NAME , " General Population Homeowners: ", Percent, "%"),
              highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3)) %>% 
  addLegend("topleft",
            pal = pal,
            values = ~ Percent,
            title = "General Population Homeowners",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)

###saves map as an html
saveWidget(tot_hm_own_leaf_19, file = "tot_hm_own_leaf_19.html")

#pulls 2018 data w/o mapping ggplot
tot_hm_own_18 <- get_acs(geography = "county",
                      state = "VA",
                      county = county_fips,
                      variables = c(overall_owned_housing = "S2502_C03_001"),
                      summary_var = "S2502_C01_001",
                      year = 2018,
                      geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2))

##2018 total population homeownership leaflet map
tot_hm_own_leaf_18 <- tot_hm_own_18 %>% 
  st_transform() %>% 
  leaflet(options = leafletOptions(minZoom = 8.5)) %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>% 
  addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
              label = ~paste0(NAME , " General Population Homeowners: ", Percent, "%"),
              highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3)) %>% 
  addLegend("topleft",
            pal = pal,
            values = ~ Percent,
            title = "General Population Homeowners",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)

###saves map as html
saveWidget(tot_hm_own_leaf_18, file = "tot_hm_own_leaf_18.html")

#pulls 2017 data w/o mapping ggplot
tot_hm_own_17 <- get_acs(geography = "county",
                         state = "VA",
                         county = county_fips,
                         variables = c(overall_owned_housing = "S2502_C03_001"),
                         summary_var = "S2502_C01_001",
                         year = 2017,
                         geometry = TRUE) %>% 
  mutate(Percent = 100 * (estimate / summary_est)) %>% 
  mutate(Percent = round(Percent, 2))

##2017 total population homeownership leaflet map
tot_hm_own_leaf_17 <- tot_hm_own_17 %>% 
  st_transform() %>% 
  leaflet(options = leafletOptions(minZoom = 8.5)) %>% 
  addProviderTiles("CartoDB.PositronNoLabels") %>% 
  addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
              label = ~paste0(NAME , " General Population Homeowners: ", Percent, "%"),
              highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3)) %>% 
  addLegend("topleft",
            pal = pal,
            values = ~ Percent,
            title = "General Population Homeowners",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)

###saves map as html
saveWidget(tot_hm_own_leaf_17, file = "tot_hm_own_leaf_17.html")
  





  










  

  




