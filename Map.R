library(tidycensus)
library(tidyverse)
library(dplyr)
library(stringr)

setwd("~/GitPractice/DSPG2021_HamptonRoads")

census_api_key("5dff03cc06392730a33b6cc8b5f354730915dd20")

va_table <- function(varcode, year){
  data.frame(get_acs(geography = "state", state = 51,
                     table = varcode, year = year)) %>% drop_na()
}

va_table2 <- function(varcode, year){
  data.frame(get_acs(geography = "state", state = 51,
                     variable = varcode, year = year)) %>% drop_na()
}

#DP03_0004

hampton_map <- function(varcode){
  get_acs(geography = "county",
          state = 51,
          county = c(550,620,650,700,710,735,740,800,810,830,73,93,95,115,175,199),
          variable = varcode,
          year = 2019,
          geometry = TRUE) %>% 
    ggplot() + geom_sf(aes(fill = estimate))
}

hamp_maps <-hampton_map("S1401_C01_001")

years <- c(2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 2011, 2010)
 
general_economic_characteristics <- va_table("DP03", 2019)
general_school_enrollment <- va_table("S1401", 2019)


black_economic_characteristics <- va_table("B23002B", 2019)
black_school_enrollment <- va_table("B14007B", 2019)

general_employment_characteristics <- va_table2("DP03_0004", 2019)



for(i in 2:length(general_economic_characteristics)) {
    tmp <- hampton_map(general_economic_characteristics$variable)
    hamp_maps <- rbind(hamp_maps, tmp)
}



#general_economic_characteristics$variable   %>% 
#  ggplot() + geom_sf(aes(fill = estimate))

#hamp_maps1 <-hampton_map("S1401_C01_001")

#hamp_maps1
#hampton_map("S1401")
#map2 = hampton_map2("DP03_0001")
#map2


