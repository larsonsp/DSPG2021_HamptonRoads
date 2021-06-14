
library(tidycensus)
library(tidyverse)
library(dplyr)

#Race for VA
va_table <- function(varcode, year){
  data.frame(get_acs(geography = "state", state = 51,
                     table = varcode,
                     year = year))}

races <- va_table("B02001", 2019)

#va_races <- c(races[2:8, 3:5])
va_races <- races[c(2:8), c(3:5)]

row_names <- c("White", "Black", "America/Alaska Native", "Asian", "Hawaiian/Pacific Islander", "Other", "Two or more")
row.names(va_races) <- row_names

##Graph
slices <- va_races[,2]
pie(slices, labels = row_names, main = "Races in VA")

#######################################################################
##Race for Hampton Roads
county_fips <- c(550, 620, 650, 700, 710, 735, 740, 800, 810,
                830, 073, 093, 095, 115, 175, 199)

hamp_data <- get_acs(geography = "county", state = 51,
                     county = county_fips[1],
                     table = "B02001",
                     year = 2019)

for(i in 2:length(county_fips)) {
  tmp <- get_acs(geography = "county", state = 51,
                 county = county_fips[i],
                 table = "B02001",
                 year = 2019)
  hamp_data <- rbind(hamp_data, tmp)
}
#combining data from counties
