library(tidycensus)
library(tidyverse)
library(dplyr)
library(stringr)
library(tidycensus)
library(tidyverse)
library (stringr)
library(ggplot2)
library(olsrr)
library(stats)
library(psych)
library(viridis)
library(ggthemes)
library(ggmap)
library(ggspatial)
library(sf)
library(leaflet)
library(tigris)
library(readr)
library(hash)
library(readxl)
library(sos)
library(plotly)

setwd("~/GitPractice/DSPG2021_HamptonRoads")
census_api_key("5dff03cc06392730a33b6cc8b5f354730915dd20")

va_table <- function(varcode, year) {
  data.frame(get_acs(geography = "state", state = 51,
                     table = varcode, year = year)) %>% drop_na()
}
va_variable <- function(varcode, year) {
  data.frame(get_acs(geography = "state", state = 51,
                     variable = varcode, year = year)) %>% drop_na()
}

fips_codes <- c(550,620,650,700,710,735,740,800,810,830,73,93,95,115,175,199)
hampton_education_map <- function(varcode, summary, year){
  get_acs(geography = "county",
          state = 51,
          county = fips_codes,
          variable = varcode, 
          summary_var = summary,
          year = year, 
          geometry = TRUE) %>% 
    mutate(pct = 100 * (estimate / summary_est)) %>% 
    select(NAME, variable, pct)  %>% 
    ggplot() + geom_sf(aes(fill = pct)) + geom_sf_label(aes(label = NAME), label.padding = unit(.5,"mm"), size = 2) +
    labs(title = "Population 25 years and older with Bachelor's degree or higher",
    caption = "Source: ACS 5 Year Estimate Table S1501") + theme(axis.text.x = element_text(angle = 40))
}

hampton_black_map <- function(varcode, summary, year){
  get_acs(geography = "county",
          state = 51,
          county = fips_codes,
          variable = varcode, 
          summary_var = summary,
          year = year, 
          geometry = TRUE) %>% 
    mutate(pct = 100 * (estimate / summary_est)) %>% 
    select(NAME, variable, pct)  %>% 
    ggplot() + geom_sf(aes(fill = pct)) + geom_sf_label(aes(label = NAME), label.padding = unit(.5,"mm"), size = 2) +
    labs(title = "Black Population 25 years and older with Bachelor's degree or higher",
         caption = "Source: ACS 5 Year Estimate Table C15002") + theme(axis.text.x = element_text(angle = 40))
}

#View(hampton_black_map(c("C15002B_011", "C15002B_006"), "C15002B_001"), 2019)

#General Bachelor's or Higher
#hampton_education_map("S1501_C01_015", "S1501_C01_006", 2019)

#Black Bachelor's or Higher
#hampton_black_map(c("C15002B_011", "C15002B_006"), "C15002B_001")
#females
#hampton_black_map(c("C15002B_011"), "C15002B_001", 2019)
#males 
#hampton_black_map(c("C15002B_006"), "C15002B_001", 2019)

#General HS Education

#General Master's or Professional Degree

#Employment Sector

#Political Enrollment


#later on, will have this include all years

#hamp_general_education_table_2019 <- va_table("S1501", 2019)
#View(hamp_general_education_table_2019)

#hamp_general_economic_table_2019 <- va_table("DP03", 2019)
#View(hamp_general_economic_table_2019)
#five year estimates
years <- c(2019, 2018, 2017, 2016, 2015, 2014)

for (i in length(years)) {
  hampton_education_map("S1501_C01_015", "S1501_C01_006", year)
}

county_stats <- function(varcode, summary, year) { 
  get_acs(geography = "county",
                  county = fips_codes,
                  state = 51,
                  year = year,
                  variables = varcode, summary_var = summary) %>% 
  mutate(pct_tot = 100 * (estimate / summary_est)) %>% 
    select(NAME, variable, pct_tot) 
}



county_stats2 <- function(varcode, year) { 
  get_acs(geography = "county",
          county = fips_codes,
          state = 51,
          year = year,
          variables = varcode) %>% 
    mutate(Male = estimate) %>% 
    mutate(Year = year)  %>% 
    select(Year, NAME, variable, Male) 
}

county_stats3 <- function(varcode, year) { 
  get_acs(geography = "county",
          county = fips_codes,
          state = 51,
          year = year,
          variables = varcode) %>% 
    mutate(Female = estimate) %>% 
    mutate(variable2 = variable) %>% 
    select(variable2, Female) 
}

county_stats4 <- function(varcode, year) { 
  get_acs(geography = "county",
          county = fips_codes,
          state = 51,
          year = year,
          variables = varcode) %>% 
    mutate(Total = estimate) %>% 
    mutate(variable3 = variable) %>% 
    select(variable3, Total)  
}


black_totalMale <- county_stats2("C15002B_006", 2019)
black_totalFemale <- county_stats3("C15002B_011", 2019)

black_totalFemale


black_totalBoth <- county_stats4("C15002B_001", 2019)
black_total <- cbind(black_totalMale, black_totalFemale, black_totalBoth)
black_total$BlackGeneral <- (black_total$Male + black_total$Female)/black_total$Total
black_total


#plots general data for black education
years <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019) 
for (i in 1:length(years)) { 
  #plots general data for education
  
  black_totalMale <- county_stats2("C15002B_006", years[i])
  black_totalFemale <- county_stats3("C15002B_011", years[i])
  black_totalBoth <- county_stats4("C15002B_001", years[i])
  black_total <- cbind(black_totalMale, black_totalFemale, black_totalBoth)
  black_total$BlackGeneral <- ((black_total$Male + black_total$Female)/black_total$Total) * 100
  write_csv(black_total, file = paste("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment", toString((years[i])),  ".csv", sep = ""))
  #black_total <- read.csv(paste("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/TableC15002BFiveYearEstimates/blackEducationalAttainment", toString((years[i])),  ".csv", sep = ""))
}

generalEducationalAttainment2010 <- read.csv("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/TableS1501FiveYearEstimates/generalEducationalAttainment2010.csv")
generalBlackEducationalAttainment2010 <- read.csv("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/TableC15002BFiveYearEstimates/generalBlackEducationalAttainment2010.csv")
colnames(generalEducationalAttainment2010) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
colnames(generalBlackEducationalAttainment2010) <- c( "Year", "Name", "Variable", "Male", "variable2", "Female", "variable3", "Total", "Bachelor or Higher as Highest Attainment %")
generalEducationalAttainment2010$Variable  <- rep(c("General Population"), 16)
generalBlackEducationalAttainment2010$Variable  <- rep(c("Black Population"), 16)
modifiedGeneralBlackEducationalAttainment2010 <- cbind(generalBlackEducationalAttainment2010$Name, generalBlackEducationalAttainment2010$Variable, generalBlackEducationalAttainment2010$`Bachelor or Higher as Highest Attainment %`)
modifiedGeneralEducationalAttainment2010 <- cbind(generalEducationalAttainment2010$Name, generalEducationalAttainment2010$Variable, generalEducationalAttainment2010$`Bachelor or Higher as Highest Attainment %`)

modifiedGeneralBlackEducationalAttainment2010
generalTotal <- rbind(modifiedGeneralEducationalAttainment2010, modifiedGeneralBlackEducationalAttainment2010)
generalTotal
colnames(generalTotal)  <- c("Name", "Variable","Bachelor or Higher as Highest Attainment %")
generalTotal <- as.data.frame.matrix(generalTotal) 
generalTotal$`Bachelor or Higher as Highest Attainment %`
generalTotal$`Bachelor or Higher as Highest Attainment %` <-  as.numeric(as.character(generalTotal$`Bachelor or Higher as Highest Attainment %`))

va_tot_education_bar2019 <- generalTotal %>% 
  mutate(Name = str_remove(Name, "County, Virginia")) %>% 
  mutate(Name = str_remove(Name, "city, Virginia")) %>%
  arrange(desc(Name)) %>% 
  ggplot(aes(fill = Variable, y = `Bachelor or Higher as Highest Attainment %`, x = Name)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(title = "",
       y = "Percent (%)",
       x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +
  scale_color_viridis_d() +  scale_fill_viridis_d()
ggplotly(va_tot_education_bar2019)



va_tot_education_bar2019 <- generalTotal  %>% 
  mutate(Name = str_remove(Name, "County, Virginia")) %>% 
  mutate(Name = str_remove(Name, "city, Virginia")) %>% 
  ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Variable)) + geom_col() +
  geom_bar (stat="identity", position = "dodge") + 
  theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2019)",
                         y = "Percent (%)",
                         x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
#adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
hide_legend(ggplotly(va_tot_education_bar2019, tooltip=c("x", "y"))) %>% 
  layout(annotations = list(x = 1, y = -0.43, text = "Source: ACS 5 Year Estimate Table S1501 and ACS 5 Year Estimate Table C15002B", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=10, color="black"))
  )
#generalTotal <- dplyr::full_join(generalEducationalAttainment2019, generalBlackEducationalAttainment2019)


county_stats1 <- function(varcode, year) { 
  get_acs(geography = "county",
          county = fips_codes,
          state = 51,
          year = year,
          variables = varcode) %>% 
    mutate(individualEstimates = estimate) %>% 
    select(NAME, variable, individualEstimates) 
}

va_stats <- function(varcode, summary, year) { 
  get_acs(geography = "state",
          county = fips_codes,
          state = 51,
          year = year,
          variables = varcode, summary_var = summary) %>% 
    mutate(pct_tot = 100 * (estimate / summary_est)) %>% 
    select(NAME, variable, pct_tot) 
}




#total or general virginia employment
va_total <- county_stats("DP03_0004", "DP03_0001", 2019)
View(va_total)


#this extracts one value for general virginia employment; putting it as a function in case I want to do it over years
general_va_cutoff_employment <- va_stats("DP03_0004", "DP03_0001", 2019)


#plots general employment data for total population
va_tot_employment_bar <- va_total  %>% 
  mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
  mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
  ggplot(aes(x = NAME, y = pct_tot, fill = NAME, ordered = TRUE)) + geom_hline(yintercept=general_va_cutoff_employment$pct_tot, linetype="dashed", color = "red") +  geom_col() +
  theme_minimal() + labs(title = "Virginia: Employment Rate",
       y = "Percent (%)",
       x = "Counties of Hampton Roads",
       caption = "Source: ACS 5 Year Estimate") + theme(axis.text.x = element_text(angle = 40))  

#this is likely not the most efficient way of coloring the scale but it works so using it for now, will hopefully change later...  

va_tot_employment_bar




#plots general data for education
years <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019) 
for (i in 1:length(years)) { 
  #plots general data for education
  va_total2 = county_stats("S1501_C01_015", "S1501_C01_006", years[i])
  gender <- rep(c("female", "male"), 16)
  va_total2 <- cbind(va_total2, gender)
  write_csv(va_total2, file = paste("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/TableS1501FiveYearEstimates/generalEducationalAttainment", toString((years[i])),  ".csv", sep = ""))
  va_total2CSV <- read.csv(paste("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/TableS1501FiveYearEstimates/generalEducationalAttainment", toString((years[i])),  ".csv", sep = ""))
  
}
  

#this extracts one value for general virginia education; putting it as a function in case I want to do it over years
#general_va_cutoff_education <- va_stats("S1501_C01_015", "S1501_C01_006", 2019)

write_csv(va_total2, file = "C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/TableS1501FiveYearEstimates/generalEducationalAttainment" + toString(2019) + ".csv")
va_total2CSV <- read.csv("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/TableS1501FiveYearEstimates/generalEducationalAttainment" + toString(2019) + ".csv")

#plots general Black data for total population
va_tot_education_bar <- va_total2CSV  %>% 
  mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
  mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
  ggplot(aes(x = NAME, y = pct_tot, fill = NAME)) + geom_col() +
  theme_minimal() + labs(title = "Virginia: Population 25 years and older with Bachelor's degree or higher",
                         y = "Percent (%)",
                         x = "Counties of Hampton Roads",
                         caption = "Source: ACS 5 Year Estimate Table S1501") + theme(axis.text.x = element_text(angle = 40))
  

#this is likely not the most efficient way of coloring the scale but it works so using it for now, will hopefully change later...  


va_tot_education_bar





#plots general data for black education
years <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019) 
for (i in 1:length(years)) { 
  #plots general data for education
 
  va_total2 = county_stats(c("C15002B_006", "C15002B_011"), "C15002B_001", years[i])
  gender <- rep(c("Male", "Female"), 16)
  va_total2 <- cbind(va_total2, gender)
  write_csv(va_total2, file = paste("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/TableC15002BFiveYearEstimates/blackEducationalAttainment", toString((years[i])),  ".csv", sep = ""))
  va_total2CSV <- read.csv(paste("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/TableC15002BFiveYearEstimates/blackEducationalAttainment", toString((years[i])),  ".csv", sep = ""))
}

#if year is 2017, 2016, 2015
#S1501_C02_015
va_total2 = va_table("S1501", 2010)
View(va_total2)


years <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019) 
for (i in 1:length(years)) { 
  #plots general data
  
  #these years just directly had the percentages so no need to sum up the numbers over the estimate
  if (years[i] == "2017"|| years[i] == "2016" || years[i] == "2015") {
    va_total2 <-  county_stats1("S1501_C02_015", years[i])
  }
  
  #these years just directly had the percentages so no need to sum up the numbers over the estimate
  if (years[i] == "2010"|| years[i] == "2011" || years[i] == "2012" || years[i] == "2013" || years[i] == "2014")  {
    va_total2 <-  county_stats1("S1501_C01_015", years[i])
  }
  
  else {
    va_total2 <- county_stats("S1501_C01_015", "S1501_C01_006", years[i])
  }
  
  write_csv(va_total2, file = paste("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/TableS1501FiveYearEstimates/generalEducationalAttainment", toString((years[i])),  ".csv", sep = ""))
  #va_total2CSV <- read.csv(paste("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/TableS1501FiveYearEstimates/generalEducationalAttainment", toString((years[i])),  ".csv", sep = ""))
  
}





hamp_black_education_variables_2019 <- va_table("C15002B", 2019)
View(hamp_black_education_variables_2019)


#plots Black data for education
black_total = county_stats(c("C15002B_006", "C15002B_011"), "C15002B_001", 2019)
gender <- rep(c("male", "female"), 16)
black_total <- cbind(black_total, gender)
black_total <-  black_total %>% mutate(NAME = str_remove(NAME, "County, Virginia")) %>% mutate(NAME = str_remove(NAME, "city, Virginia"))
black_total


#black_total %>% mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
#  mutate(NAME = str_remove(NAME, "city, Virginia"))  %>% 
#  ggplot() + geom_col(aes(x=NAME, y=pct_tot, group=gender, fill=gender)) + geom_hline(yintercept=general_va_cutoff_education$pct_tot, linetype="dashed", color = "black")  + geom_hline(yintercept=mean(black_total$pct_tot), linetype="dashed", color = "white")  +
#  labs(title = "Hampton Roads: Black Population 25 years and older with Bachelor's degree or higher as highest level of educational attainment", y = "Percent (%)",
#  x = "Hampton Roads",caption = "Source: ACS 5 Year Estimate Table C15002B") + theme(axis.text.x = element_text(angle = 40)) 

black_total  <- black_total  %>%
  ggplot(aes(x = gender2, y = pct_tot, fill = gender2)) + geom_col() +
  theme_minimal() + labs(title = "Virginia: Population 25 years and older with Bachelor's degree or higher",
                         y = "Percent (%)",
                         x = "Counties of Hampton Roads",
                         caption = "Source: ACS 5 Year Estimate Table S1501") + theme(axis.text.x = element_text(angle = 40))

black_total<- black_total  %>%
  ggplot(aes(x = NAME, y = pct_tot, fill = NAME, group=gender)) + geom_col() +
  theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2019)",
                         y = "Percent (%)",
                         x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
black_total


black_total2 = va_stats(c("C15002B_006", "C15002B_011"), "C15002B_001", 2019)
black_total2
gender <- rep(c("male", "female"))
gender2 <- rep(c("male", "female"))
black_total2 <- cbind(black_total2, gender, gender2)

black_total2
#Virginia: Population 25 years and older with Bachelor's degree or higher
black_total2Visual  <- black_total2  %>%  
ggplot(aes(fill = gender, y = pct_tot, x = gender)) + geom_col() + theme_minimal() + labs(title = "2019 Data",
y = "Percent (%)", x = "Counties of Hampton Roads", caption = "Source: ACS 5 Year Estimate Table C15002B") + theme(axis.text.x = element_text(angle = 40))  +  scale_color_viridis_d() +  scale_fill_viridis_d()
hide_legend(ggplotly(black_total2Visual))


hamp_black_employment_variables_2019 <- va_table("C23002B", 2019)

#plots black data for employment
black_total = county_stats(c("C23002B_007", "C23002B_020"), "C23002B_001", 2019)
gender <- rep(c("male", "female"), 16)
black_total <- cbind(black_total, gender)
#black_total %>% ggplot() + geom_col(aes(x=NAME, y=pct_tot, group=gender, color=gender))
black_total %>% mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
  mutate(NAME = str_remove(NAME, "city, Virginia"))  %>% 
  ggplot() + geom_hline(yintercept=general_va_cutoff$pct_tot, linetype="dashed", color = "red") + geom_col(aes(x=NAME, y=pct_tot, group=gender, fill=gender)) 
  + labs(title = "Virginia: Black Employment Rate", y = "Percent (%)",
       x = "Counties of Hampton Roads", caption = "Source: ACS 5 Year Estimate") + theme(axis.text.x = element_text(angle = 40)) 


#this is likely not the most efficient way of coloring the scale but it works so using it for now, will hopefully change later...  


#hamp_black_economic_variables_2019 <- va_variable("DP03_0001", 2019)
#this extracts one value for general virginia HS education; putting it as a function in case I want to do it over years
general_va_cutoff_HS_education <- va_stats("S1501_C01_009", "S1501_C01_006", 2019)

general_va_cutoff_HS_education
#general HS graduation rate
general_HS <- county_stats("S1501_C01_009", "S1501_C01_006", 2019) 
general_HS <- general_HS  %>% 
  mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
  mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
  ggplot(aes(x = NAME, y = pct_tot, fill = NAME)) + geom_col() +
  theme_minimal() + labs(title = "Hampton Roads: Percentage of HS Graduates",
                         y = "Percent (%)",
                         x = "Hampton Roads",
                         caption = "Source: ACS 5 Year Estimate Table S1501") + theme(axis.text.x = element_text(angle = 40))  

#this is likely not the most efficient way of coloring the scale but it works so using it for now, will hopefully change later...  

general_HS


black_HS  = county_stats(c("C15002B_004", "C15002B_009"), "C15002B_001", 2019)
gender2 <- rep(c("male", "female"), 16)
black_HS <- cbind(black_HS, gender2)

black_HS
#black_total %>% ggplot() + geom_col(aes(x=NAME, y=pct_tot, group=gender, color=gender))
black_rates <- black_HS %>% 
  mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
  mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
  ggplot(aes(x = NAME, y = pct_tot, group=gender, fill=gender)) + geom_col() + geom_hline(yintercept=general_va_cutoff_education$pct_tot, linetype="dashed", color = "black") +
  geom_hline(yintercept=mean(black_total$pct_tot), linetype="dashed", color = "white") + theme_minimal() + 
  labs(title = "Hampton Roads: Percentage of Black HS Graduates", y = "Percent (%)", x = "Hampton Roads",
  caption = "Source: ACS 5 Year Estimate Table C15002B") + theme(axis.text.x = element_text(angle = 40))



#this is likely not the most efficient way of coloring the scale but it works so using it for now, will hopefully change later...  
black_rates


general_professional_degrees <- county_stats("S1501_C01_012", "S1501_C01_006", 2019)



general_va_cutoff_professional_education <- va_stats("S1501_C01_012", "S1501_C01_006", 2019)
general_professional_degrees <- general_professional_degrees  %>% 
  mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
  mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
  ggplot(aes(x = NAME, y = pct_tot, fill = NAME)) + geom_col() + geom_hline(yintercept=general_va_cutoff_professional_education$pct_tot, linetype="dashed", color = "red") +
  geom_hline(yintercept=mean(general_professional_degrees$pct_tot), linetype="dashed", color = "gray") + theme_minimal() + labs(title = "Virginia: Percentage of Graduate or Professional Degrees",
  y = "Percent (%)", x = "Hampton Roads", caption = "Source: ACS 5 Year Estimate") + theme(axis.text.x = element_text(angle = 40))


general_professional_degrees$labels




general_va_cutoff_professional_education <- va_stats("S1501_C01_012", "S1501_C01_006", 2019)
general_professional_degrees <- general_professional_degrees  %>% 
  mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
  mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
  ggplot(aes(x = NAME, y = pct_tot, fill = NAME)) + geom_col() + geom_hline(yintercept=general_va_cutoff_professional_education$pct_tot, linetype="dashed", color = "red") +
  geom_hline(yintercept=mean(general_professional_degrees$pct_tot), linetype="dashed", color = "gray") + theme_minimal() + labs(title = "Virginia: Percentage of Graduate or Professional Degrees",
                                                                                                                                y = "Percent (%)", x = "Hampton Roads", caption = "Source: ACS 5 Year Estimate") + theme(axis.text.x = element_text(angle = 40))


general_professional_degrees$labels


employment_types  = county_stats1(c("DP03_0033", "DP03_0034", "DP03_0035", "DP03_0036", "DP03_0037", "DP03_0038", "DP03_0039", "DP03_0040", "DP03_0041", "DP03_0042", "DP03_0043", "DP03_0044", "DP03_0045"), 2019)
sectors <- rep(c("Agriculture, forestry, fishing and hunting, and mining", "Construction", "Manufacturing", "Wholesale trade", "Retail trade", "Transportation and warehousing, and utilities", "Information", "Finance and insurance, and real estate and rental and leasing", "Professional, scientific, and management, and administrative and waste management services", "Educational services, and health care and social assistance", "Arts, entertainment, and recreation, and accommodation and food services", "Other services, except public administration", "Public administration"), 16)
employment_types  <- cbind(employment_types, sectors)
employment_types  %>% 
  arrange(desc(individualEstimates))  %>% 
  group_by(NAME)  %>% slice(1:2)



#for (sector in sectors) {
#  chosenSelector <- employment_types[which(employment_types$sectors == sector), ]$individualEstimates
#  hist(chosenSelector)
#}


#plots general data for professional degrees
years <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019) 
for (i in 1:length(years)) { 
  #plots general data for education
  
  employment_types  = county_stats1(c("DP03_0033", "DP03_0034", "DP03_0035", "DP03_0036", "DP03_0037", "DP03_0038", "DP03_0039", "DP03_0040", "DP03_0041", "DP03_0042", "DP03_0043", "DP03_0044", "DP03_0045"), years[i])
  sectors <- rep(c("Agriculture, forestry, fishing and hunting, and mining", "Construction", "Manufacturing", "Wholesale trade", "Retail trade", "Transportation and warehousing, and utilities", "Information", "Finance and insurance, and real estate and rental and leasing", "Professional, scientific, and management, and administrative and waste management services", "Educational services, and health care and social assistance", "Arts, entertainment, and recreation, and accommodation and food services", "Other services, except public administration", "Public administration"), 16)
  employment_types  <- cbind(employment_types, sectors)
  employment_types <- employment_types %>% 
    arrange(desc(individualEstimates))  %>% 
    group_by(NAME)  %>% slice(1:2)
  write_csv(employment_types, file = paste("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/TableDP03FiveYearEstimates/top2employmentSectors", toString((years[i])),  ".csv", sep = ""))
  va_total2CSV <- read.csv(paste("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/TableDP03FiveYearEstimates/top2employmentSectors", toString((years[i])),  ".csv", sep = ""))
  
}






employment_types
ggplot(employment_types, aes (x="", y = sectors, fill = factor(sectors))) + 
  geom_col(position = 'stack', width = 1) +
  theme_classic() +
  theme(plot.title = element_text(hjust=1),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  labs(fill = "Sectors",
       x = NULL,
       y = NULL,
       title = "Pie Chart of Employment Sectors for General Hampton") + 
  coord_polar("y")



teacherSalaryData <- read_excel("20162017TeacherSalariesDataForHampton.xlsx") %>% drop_na()
generalTeacherSalaryData <- read_excel("20162017TeacherSalariesData.xlsx") %>% drop_na()
teacherMeans <- data.frame(NAME=teacherSalaryData$`Division Name`, bachelorSalaries=teacherSalaryData$`Bachelor's Starting Salary`, masterSalaries=teacherSalaryData$`Master's Starting Salary`, phDSalaries=teacherSalaryData$`Doctorate Starting Salary`, population=teacherSalaryData$`Population`)
generalTeacherMeans <- data.frame(NAME=generalTeacherSalaryData$`Division Name`, bachelorSalaries=generalTeacherSalaryData$`Bachelor's Starting Salary`, masterSalaries=generalTeacherSalaryData$`Master's Starting Salary`, phDSalaries=generalTeacherSalaryData $`Doctorate Starting Salary`, population=generalTeacherSalaryData$`Population`)
generalTeacherMeans

teacherBachelorSalary <- teacherMeans  %>% 
ggplot(aes(x = NAME, y = bachelorSalaries, fill = NAME)) + geom_col()  + geom_hline(yintercept=mean(teacherMeans$bachelorSalaries), linetype="dashed", color = "gray") + theme_minimal() + 
labs(title = "Virginia: Bachelor's Starting Salary", y = "Salary", x = "Hampton Roads", caption = "Source: Virginia 2016-2017 Teacher Salary Report") + 
theme(axis.text.x = element_text(angle = 40))
teacherBachelorSalary  


teacherMasterSalary <- teacherMeans  %>% 
  ggplot(aes(x = NAME, y = masterSalaries, fill = NAME)) + geom_col()  + geom_hline(yintercept=mean(teacherMeans$masterSalaries), linetype="dashed", color = "gray") + theme_minimal() + 
  labs(title = "Virginia: Master's Starting Salary", y = "Salary", x = "Hampton Roads", caption = "Source: Virginia 2016-2017 Teacher Salary Report") + 
  theme(axis.text.x = element_text(angle = 40))
teacherMasterSalary 



teacherPhDSalary <- teacherMeans  %>% 
  ggplot(aes(x = NAME, y = phDSalaries, fill = NAME)) + geom_col()  + geom_hline(yintercept=mean(teacherMeans$phDSalaries), linetype="dashed", color = "gray") + theme_minimal() + 
  labs(title = "Virginia: PhD's Starting Salary", y = "Salary", x = "Hampton Roads", caption = "Source: Virginia 2016-2017 Teacher Salary Report") + 
  theme(axis.text.x = element_text(angle = 40))
teacherPhDSalary


#teacherMeans <- teacherMeans[order(teacherMeans$NAME),]

#teacherMeans <- cbind(teacherSalaryData$`Division Name`, teacherMeans)


#plots general data for education
va_total2 = county_stats("S1501_C01_015", "S1501_C01_006", 2019)

#this extracts one value for general virginia education; putting it as a function in case I want to do it over years
general_va_cutoff_education <- va_stats("S1501_C01_015", "S1501_C01_006", 2019)

#plots general Black data for total population
va_total2  <- va_total2 %>% 
  mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
  mutate(NAME = str_remove(NAME, "city, Virginia"))
va_total2$NAME 


#va_total2 <- va_total2[order(va_total2$NAME),]
#va_total2$teacherSalaries  <- teacherMeans$teacherSalaries
#salariesAndEducation <-  rbind(va_total2, teacherMeans$teacherSalaries) 
#salariesAndEducation
#va_total2$teacherSalaries


#require(gridExtra)
#va_tot_education_bar <- va_total2 %>%  ggplot(aes(x = NAME, y = pct_tot, fill = NAME)) +  geom_col() + geom_hline(yintercept=general_va_cutoff_education$pct_tot, linetype="dashed", color = "red") + 
#  geom_hline(yintercept=mean(va_total2$pct_tot), linetype="dashed", color = "gray")  + theme_minimal() + labs(title = "Virginia: Population 25 years and older with Bachelor's degree or higher",
#  y = "Percent (%)", x = "Hampton Roads", caption = "Source: ACS 5 Year Estimate") + theme(axis.text.x = element_text(angle = 40))


#va_tot_education_bar2 <- va_total2 %>%  ggplot(aes(x = NAME, y = teacherSalaries, fill = NAME)) +  geom_col() + theme_minimal() + labs(title = "Virginia: Bachelor's degree or Higher Teachers", y = "Salary Estimates", x = "Hampton Roads", caption = "Source: ACS 5 Year Estimate") + theme(axis.text.x = element_text(angle = 40))
#va_tot_education_bar2


#grid.arrange(va_tot_education_bar,va_tot_education_bar2, ncol=2)



for (division in teacher) {
  chosenSelector <- employment_types[which(employment_types$sectors == sector), ]$pct_tot
  hist(chosenSelector)
}



for (i in 1:length(years)) {
  general_va_cutoff_HS_education <- va_stats("S1501_C01_009", "S1501_C01_006", years[i])
  general_HS <- county_stats("S1501_C01_009", "S1501_C01_006", years[i]) 
  general_HS <- general_HS  %>% 
  mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
  mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
  ggplot(aes(x = NAME, y = pct_tot, fill = NAME)) + geom_col() + geom_hline(yintercept=general_va_cutoff_HS_education$pct_tot, linetype="dashed", color = "black")  + 
  geom_hline(yintercept=mean(general_HS$pct_tot), linetype="dashed", color = "white") +
  theme_minimal() + labs(title = "Hampton Roads: Percentage of HS Graduates", y = "Percent (%)", x = "Counties of Hampton Roads",
  caption = "Source: ACS 5 Year Estimate Table S1501") + theme(axis.text.x = element_text(angle = 40))  
  #this is likely not the most efficient way of coloring the scale but it works so using it for now, will hopefully change later...  
  general_HS
}


#this is likely not the most efficient way of coloring the scale but it works so using it for now, will hopefully change later...  
va_tot_education_bar








#black_professional_degrees <- county_stats1("B15002B_010", "B15002B_019", 2019)
#general_va_cutoff_professional_education <- va_stats("S1501_C01_012", "S1501_C01_006", 2019)
#black_professional_degrees <- black_professional_degrees  %>% 
#  mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
#  mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
#  ggplot(aes(x = NAME, y = pct_tot, fill = NAME)) + geom_col() + geom_hline(yintercept=general_va_cutoff_professional_education$pct_tot, linetype="dashed", color = "red") +
#  geom_hline(yintercept=mean(general_professional_degrees$pct_tot), linetype="dashed", color = "gray") + theme_minimal() + labs(title = "Virginia: Black Percentage of Graduate or Professional Degrees",                                                                                                                         y = "Percent (%)", x = "Hampton Roads", caption = "Source: ACS 5 Year Estimate") + theme(axis.text.x = element_text(angle = 40)
#black_professional_degrees


countyAndCities <- c("Chesapeake City Public Schools",
              "Franklin City Public Schools",
              "Hampton City Public Schools",
              "Newport News City Public Schools",
              "Norfolk City Public Schools",
              "Poquoson City Public Schools",
              "Portsmouth City Public Schools",
              "Suffolk City Public Schools",
              "Virginia Beach City Public Schools",
              "Williamsburg-James City County Public Schools",
              "Gloucester County Public Schools",
              "Isle of Wight County Public Schools",
              "Mathews County Public Schools",
              "Southampton County Public Schools",
              "York County Public Schools")

teacherByRace <- read_excel("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/teacherByRace.xlsx")
teacherByRace <- as.data.frame(teacherByRace) 
teacherByRace
teacherByRace$`Division Name`
teacherByRace <- teacherByRace[which(teacherByRace$`Division Name` %in% countyAndCities), ]
View(teacherByRace)



#keep.cols <- c("Division Name",'Total Counts','Black')
#teacherByRace <- teacherByRace[, names(teacherByRace) %in% keep.cols]
teacherByRace$BlackProportions <- (teacherByRace$Black/teacherByRace$`Total Counts`) * 100
teacherByRace$AsianProportions <- (teacherByRace$Asian/teacherByRace$`Total Counts`) * 100
teacherByRace$HispanicProportions <- (teacherByRace$Hispanic/teacherByRace$`Total Counts`) * 100
teacherByRace$WhiteProportions <- (teacherByRace$White/teacherByRace$`Total Counts`) * 100
teacherByRace$AmericanIndianProportions<- (teacherByRace$`American Indian`/teacherByRace$`Total Counts`) * 100
teacherByRace$TwoOrMoreRacesProportions <- (teacherByRace$`Two or More Races`/teacherByRace$`Total Counts`) * 100
teacherByRace$HawaiianProportions <- teacherByRace$`Hawaiian`/teacherByRace$`Total Counts`
teacherByRace <- teacherByRace %>% mutate(`Division Name` = str_remove(`Division Name`, "County Public Schools")) %>% mutate(`Division Name` = str_remove(`Division Name`, "City Public Schools")) %>% mutate(`Division Name` = str_remove(`Division Name`, "City"))
write_csv(teacherByRace, file = ("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/teacherByRacesBreakdown.csv"))
teacherByRace <- read.csv("C:/Users/victo/OneDrive/Documents/GitPractice/DSPG2021_HamptonRoads/shinyapp/data/teacherByRacesBreakdown.csv")
colnames(teacherByRace) <- c("Division Number", "Name", "Total Counts", "American Indian", "Asian", "Black", "Hispanic", "White","Hawaiian", "Two or More Races",  "Not Specified", "% of Black Teachers", "% of Asian Teachers", "% of Hispanic Teachers", "% of White Teachers", "% of American Indian Teachers", "% of Two Or More Races Teachers", "% of Hawaiian Teachers")

#teacherByRace$BlackProportions <- teacherByRace$Black/teacherByRace$`Total Counts`
#changing col names so it I can convert division name column without getting mutate character error 
#colnames(teacherByRace) <- c("DivisionName",'TotalCounts','Black', 'Proportions')
teacherByRace <- teacherByRace  %>% 
  ggplot(aes(x = Name, y = `% of Two Or More Races Teachers`, fill = Name)) + geom_col() +
  labs(title = "Two or More Races Teacher Breakdown", y = "Percentage (%)", x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40))  +  scale_color_viridis_d() +  scale_fill_viridis_d() + ylim(0, 25)
#adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
hide_legend(ggplotly(teacherByRace, tooltip=c("x", "y"))) %>% 
  layout(annotations = list(x = 1, y = -0.60, text = "Source: Virginia 2020-2021 Teacher Race Report", 
                            showarrow = F, xref='paper', yref='paper', 
                            xanchor='right', yanchor='auto', xshift=0, yshift=0,
                            font=list(size=10, color="black")), xaxis = list(autorange = FALSE),
                  yaxis = list(autorange = FALSE))




#adding caption from ggplot does not transfer to plotly so have to load in with plotly separately


