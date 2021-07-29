#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(leaflet.minicharts)
library(tidyverse)
library(sf)
library(ggthemes)
library(RColorBrewer)
library(sjmisc)
library(shinythemes)
library(DT)
library(data.table)
library(rsconnect)
library(shinycssloaders)
library(readxl)
library(readr)
library(gridExtra)
library(stringr)
library(shinyjs)
library(plotly)
library(ggrepel)
library(shinydashboard)
library(mapdata)
library(plotrix)
library(scatterpie)

prettyblue <- "#232D4B"
navBarBlue <- '#427EDC'
options(spinner.color = prettyblue, spinner.color.background = '#ffffff', spinner.size = 3, spinner.type = 7)

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")


#countyAndCities <- c("Chesapeake City Public Schools",
#                     "Franklin City Public Schools",
#                     "Hampton City Public Schools",
#                     "Newport News City Public Schools",
#                     "Norfolk City Public Schools",
#                    "Poquoson City Public Schools",
#                     "Portsmouth City Public Schools",
#                     "Suffolk City Public Schools",
#                     "Virginia Beach City Public Schools",
#                     "Williamsburg-James City County Public Schools",
#                     "Gloucester County Public Schools",
#                     "Isle of Wight County Public Schools",
#                     "Mathews County Public Schools",
#                     "Southampton County Public Schools",
#                     "York County Public Schools")

#teacherByRace <- read_excel("teacherByRace.xlsx")
#teacherByRace <- as.data.frame(teacherByRace) 
#teacherByRace$Division.Name
#teacherByRace <- teacherByRace[which(teacherByRace$Division.Name %in% countyAndCities), ]
#teacherByRace


# CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY
jscode <- "function getUrlVars() {
                var vars = {};
                var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) {
                    vars[key] = value;
                });
                return vars;
            }
           function getUrlParam(parameter, defaultvalue){
                var urlparameter = defaultvalue;
                if(window.location.href.indexOf(parameter) > -1){
                    urlparameter = getUrlVars()[parameter];
                    }
                return urlparameter;
            }
            var mytype = getUrlParam('type','Empty');
            function changeLinks(parameter) {
                links = document.getElementsByTagName(\"a\");
                for(var i = 0; i < links.length; i++) {
                   var link = links[i];
                   var newurl = link.href + '?type=' + parameter;
                   link.setAttribute('href', newurl);
                 }
            }
           var x = document.getElementsByClassName('navbar-brand');
           if (mytype != 'economic') {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html\">' +
                              '<img src=\"VTDSPG Logo.png\", alt=\"DSPG 2021 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a></div>';
             //changeLinks('dspg');
           } else {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights/case-studies\">' +
                              '<img src=\"AEMLogoGatesColorsBlack-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a></div>';
             //changeLinks('economic');
           }
           "

# user -------------------------------------------------------------
ui <- navbarPage(title = "Hampton Roads",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')),
                 useShinyjs(),
                 
                 # Project Overview -----------------------------------------------------------
                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   # br("", style = "padding-top:2px;"),
                                   # img(src = "VTDSPG Logo.png", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                   br(""),
                                   h1(strong("Tracking indicators of the economic and social mobility of the Black community in Hampton Roads"),
                                      br(""),
                                      h4("Data Science for the Public Good Program"),
                                      h4("Virginia Tech"),
                                      h4("Department of Agriculture"),
                                      br()
                                   )
                          ),
                          fluidRow(style = "margin: 6px;",
                                   column(4,
                                          h2(strong("Project Background")),
                                          p(strong("The problem."), "Systematic issues are preferential treatement given to certain groups of inidividuals. They have impeded minorities from being able to progress to share the same level of stature, accomplishment, and acceptance fellow peers with their similar backgrounds have achieved. The Black and African American (AA) community have been no exception to this injustice, resulting in several disadvantages across all facets of life.  This includes, but is certainly not limited to, various sectors of society like: 
                                          public health/healthcare, employment, education, government, and criminal justice. "),
                                          p(),
                                          p(strong("The setting."), a(href = "https://en.wikipedia.org/wiki/Hampton_Roads", "Hampton Roads", target = "_blank"), "is a an area that comprises of sixteen total cities and counties with a much higher Black percentage, have a per capita income and employment rate below the national and metropolitan statistical area (MSA) average. 
                                          Additionally, the graduation rate is lower than state average.  As a result, our team wants to investigate the nature of these disadvantages through  indicators for Hampton Roads, Virginia, from 2010 to 2020 according to four key domains: Education, Economic Conditions, Housing, and Healthcare."),
                                          
                                          p("The Hampton Roads Metropolitan Statistical Area (MSA) consists of ten cities and six counties in the Southeastern region of Virginia.  It is ranked as the 33rd largest MSA in the United States, the 8th largest metro area in the Southeast region, and the 2nd largest between Atlanta and Washington, DC. "),  
                                          p("The jurisdictions of Hampton Roads are the cities of Chesapeake, Franklin, Hampton, Newport News, Norfolk, Poquoson, Portsmouth, Suffolk, Virginia Beach, and Williamsburg, and the counties of Isle of Wright, James City, Matthews, Southampton, and York. "),
                                          p("The population of the Hampton Roads MSA has been growing over the last decade with an estimated population of approximately 1.7 million in 2020, a 3% increase from 2010. This region accounts for a large percentage – about 20% - of Virginia’s state population."), 
                                          p("Each jurisdiction in Hampton Roads has a separate municipal government, unlike some other metro areas. While there are consultations on regional issues, there are more than 20 elected independent municipal governing bodies. As such, it is imperative for our project to examine not only differences with the Virginia population but also within the localities of Hampton Roads."),
                                   ),
                                   column(4,
                                          h2(strong("Project Goals")),
                                          p("The primary goal of this project was to identify key economic and education indicators that collectively will produce an overview of the challenges facing the Black community in Hampton Roads.  
                                            Identifying emerging patterns in these key areas will allow our stakeholders to plan strategic policies and initiatives to improve the economic wellbeing of the Black community.  "),
                                          p(),
                                          p("This dashboard compiles our findings and allows extension professionals, stakeholders, and other users to explore the information interactively.")
                                   ),
                                   column(4,
                                          h2(strong("Our approach")),
                                          p("Guided by our meetings with the Black BRAND stakeholders and Claud Anderson’s PowerNomics model, our reach team identify two main pillars – education and economics – to measure the overall economic and social progress of the Black community in Hampton Roads")
                                   )
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2021'))))
                 ),
                 
                 
                 #Hampton Roads Overview & Demographics--------------------------------------------------
                     navbarMenu(title="Hampton Region",
                       tabPanel("Hampton Roads Localities", 
                              fluidRow(
                                       column(5,
                                       h3(strong("Counties and Cities of Hampton Roads"), align = "center"),
                                       withSpinner((plotOutput("hampton_counties_map", width ="100%", height = "600px")))
                                       ),
                                       
                                       column(5,
                                              h4(strong("What is Hampton Roads (HR)?")),
                                              p("City of Chesapeake"),
                                              p("City of Franklin"),
                                              p("City of Hampton"),
                                              p("City of Newport News"),
                                              p("City of Norfolk"),
                                              p("City of Poquoson"),
                                              p("City of Portsmouth"),
                                              p("City of Suffolk"),
                                              p("City of Virginia Beach"),
                                              p("City of Williamsburg"),
                                              p("Gloucester County"),
                                              p("Isle of Wight COunty"),
                                              p("James City County"),
                                              p("Mathews County"),
                                              p("Southampton County"),
                                              p("York County") )
                              )
                       ),
                       
                       tabPanel("Sociodemographics",
                                fluidRow(style = "margin: 6px;",
                                         h1(strong("Sociodemographic Characteristics" ), align = "center"),
                                         column(4,
                                         p("We used the American Community Census data (ACS) to better understand the population in Hampton Roads and Virginia.  The ACS is a yearly survey conducted by the U.S. Census Bureau provides detailed demographic information about American household. We collected the 5-year estimates over the period 2010-2019 to compute the percent of Hampton Roads residents in each locality by race and age.  This information is also presented for the state of Virginia."), 
                                         p("The black population accounts for about 30% of the total population in Hampton Roads. This is significantly greater than Virginia’s which is about 19%."),
                                         p("The population breakdown by age for Hampton Roads is relatively and representative of Virginia’s breakdown. However, there seems to be variation by localities, for example, there exists a large population of young adults (ages 18 to 29) in Southampton County, whereas Portsmouth has a greater population of seniors - individuals 65 years and older. This data suggests that such ages difference may play a role in differences in economic or education indicators across localities"), 
                                          ),
                                         column(8, 
                                                tabsetPanel(
                                                  # Tab race
                                                  tabPanel("Race",
                                                           fluidRow(
                                                             h1(strong("Racial Demographics"), align = "center"),
                                                             
                                                             column(12, 
                                                                    h4(strong("Race Demographic")) ),
                                                             column(6,
                                                                    h4("Hampton Roads"),
                                                                    
                                                                    selectInput("hampRaceYearDrop", "Select Year:", width = "100%", choices = c(
                                                                      "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"
                                                                    )),
                                                                    p(strong("Hampton Roads Population by Race")),
                                                                    withSpinner(plotOutput("hamp_pie")),
                                                                    p(tags$small("Data Source: ACS 5 Year Estimate Table B02001")),
                                                             ),
                                                             
                                                             
                                                             column(6,
                                                                    
                                                                    h4("Virginia"),
                                                                    
                                                                    selectInput("VaRaceYearDrop", "Select Year:", width = "100%", choices = c(
                                                                      "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"
                                                                    )),
                                                                    p(strong("Virginia")),
                                                                    withSpinner(plotOutput("va_pie")),
                                                                    p(tags$small("Data Source: ACS 5 Year Estimate Table B02001"))
                                                             )
                                                             
                                                           )
                                                  ),
                                                  #Tab age
                                                  tabPanel("Age",
                                                           fluidRow(
                                                             h1(strong("Age Composition of Hampton Roads"), align = "center"),
                                                             column(4,
                                                                    
                                                                    h4("Hampton Roads Age Breakdown"),
                                                                    selectInput("HampAgeYearDrop", "Select Year:", width = "100%", choices = c(
                                                                      "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"
                                                                    )),
                                                                    p(strong("Hampton Roads")),
                                                                    withSpinner(plotOutput("hamp_graph")),
                                                                    p(tags$small("Data Source: ACS 5 Year Estimate Table B01001"))
                                                             ),
                                                             
                                                             column(4,
                                                                    
                                                                    h4("Virginia Age Breakdown"),
                                                                    p(""),
                                                                    selectInput("VaAgeYearDrop", "Select Year:", width = "100%", choices = c(
                                                                      "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"
                                                                    )),
                                                                    p(strong("Virginia")),
                                                                    withSpinner(plotOutput("va_graph")),
                                                                    p(tags$small("Data Source: ACS 5 Year Estimate Table B01001"))
                                                             ),
                                                             
                                                             column(12,
                                                                    
                                                                    h4("Hampton Roads' Counties and Cities' Age Breakdown"),
                                                                    selectInput("HampCountAgeYearDrop", "Select Year:", width = "100%", choices = c(
                                                                      "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"
                                                                    )),
                                                                    p(strong("Hampton Roads Counties and Cities' Age Breakdown")),
                                                                    withSpinner(plotOutput("age_map", height="600px")),
                                                                    p(tags$small("Data Source: ACS 5 Year Estimate Table B01001"))
                                                             )
                                                             
                                                           )
                                                           
                                                  )
                                                ) 
                                         )    
                                )) 
                       
                       
                     ),
                     
                    
                 
                 

                 # Education Indicators ----------------------------------------------------
                 
                 
                 navbarMenu(title="Education",
                            tabPanel("Educational Attainment", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Percentage of Hampton Roads Population 25 years and older with highest educational attainment as Bachelor's degree or higher"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              column(4,
                                                     h4(strong("Why education?")),
                                                     p("Knowledge is power. Consequently, education is a pivotal sector of society that influences and interwines with many other pillars like economics of health care."),
                                                     p("Investigating trends in education can allow us to explore the general 'well-being' of an area. In particular, looking at educational patterns over time like those who have reached a
                                            certain level of educational attainment or high school dropout rates can help us see if systematic issues might be playing a role (if any) to the Hampton Roads region."),
                                                     p("If Black students are consistently underperforming and or have levels of educational attainment much lower than their contemporaries, we can speculate if issues like discrimination or lack of
                                            colored representation in education is in any way influencing the trends that we are noticing. Moreoever, comparing both
                                            the general Hampton region and the Virginia averages to the Black population specifically in Hampton Roads will allow us to see the impact of the Hampton region itself on its
                                            constitutents as well (to seperate the possibility of sysematic issues from just relative lack of opporturnity or unideal conditions present in Hampton Roads) how that affects the Black community present."),
                                                     p("In particular, researching those who are 25 years and older who have a Bachelor's degree or higher as their highest educational Attainment will help us investigate the general educational development of the
                                          Hampton community as most individuals 25 years and older are old enough to have graduated high school and finished some type of college education.")
                                              ),
                                              column(8,
                                                     tabsetPanel(
                                                       tabPanel("General Population",
                                                                p(""),
                                                                selectInput("genEdAttainmentYearDrop", "Select Year:", width = "100%", choices = c(
                                                                  "2019", "2018", "2017", "2016", "2015", "2014", "2013",
                                                                  "2012", "2011", "2010"
                                                                )
                                                                ),
                                                                p(strong("Percentage of Hampton Roads Population 25 years and older with highest educational attainment as Bachelor's degree or higher")),
                                                                withSpinner(plotlyOutput("genEdAttainmentPlots")),
                                                                p(tags$small("Note: Some year to year comparisions had very little variability in attainement percentages. Certain counties/cities may not have data for specific years."))
                                                       ),
                                                       tabPanel("Black Population",
                                                                p(""),
                                                                #selectInput("blackEdAttainmentYearDrop", "Select Year:", width = "100%", choices = c(
                                                                #   "2019","2018", "2017", "2016", "2015","2014",
                                                                #   "2013","2012", "2011", "2010")),
                                                                p(strong("Black Educational Attainment")),
                                                                sliderInput("blackEdAttainmentYearDrop", "Select Year:", value = 2019, min = 2010, max = 2019, sep = "", animate=animationOptions(interval = 1400)),
                                                                withSpinner(plotlyOutput("blackEdAttainmentPlots")),
                                                                p(tags$small("Note: Some year to year comparisions had very little variability in attainement percentages. Certain counties/cities only had partial data (like male or female only) or no data at all for specific years."))
                                                                #p(tags$small("Data Source: ACS 5 Year Estimate Table C15002B"))
                                                       )
                                                     )
                                              ))
                            ),
                            
                            tabPanel("Suspension",
                                     
                                     fluidRow(
                                       
                                       h1(strong("Suspension"), align = "center"),
                                       column(4, h4(strong("Suspension Rate")), ),
                                       column(8, 
                                              tabsetPanel(
                                                tabPanel("Short Term Suspension",
                                                  h4("Percent Students Suspended Short-Term", align = "center"),
                                                  withSpinner(plotlyOutput("suspension_line_graph")),
                                                  p(tags$small("Data Source: KIDS COUNT, Annie E. Casey Foundation"))
                                                ),
                                                # tabPanel("Suspension Rate by Race", p(""),
                                                #          selectInput("BWsuspensionYearDrop", "Select Year:", width = "100%", choices = c(
                                                #            "2018-2019", "AY 2017-2018", "AY 2016-2017", "AY 2015-2016", "AY 2014-2015")),
                                                #          withSpinner(plotOutput("BW_map", height = "700px")),
                                                #          p(tags$small("Data Source: KIDS COUNT, Annie E. Casey Foundation")),
                                                #          p(tags$small("Note: Black student data supressed for Mathews and Poquoson."))
                                                # ),
                                                tabPanel("Suspension Gap",
                                                         h4("Difference Between Black and White Student Short-Term Suspension", align = "center"),
                                                         withSpinner(plotlyOutput("suspensionGap", height = "700px")),
                                                         p(tags$small("Data Source: KIDS COUNT, Annie E. Casey Foundation"))
                                                )
                                                
                                              )
                                              
                                       )
                                       
                                     )
                                     
                            ), 
                            
                            #Teacher Demographics
                            tabPanel("Teacher Demographics",
                                     
                                   fluidRow(
                                       column(4,
                                              h4(strong("Teacher Race Breakdown")),
                                       ),
                                       column(8,
                                              tabsetPanel(
                                                tabPanel("Teacher Race Breakdown",
                                                         p(""),
                                                         selectInput("teacherRaceBreakdown", "Select Race:", width = "100%", choices = c(
                                                           "Black", "Asian", "American Indian", "Hispanic", "Two or More Races", "White", "Hawaiian"
                                                         )),
                                                         p(strong("Virginia: Teacher Breakdown By Race in Hampton Roads")),
                                                         withSpinner(plotlyOutput("teacherRacePlots")),
                                                         p(tags$small("Possible Suppression of Counts in Dataset for Counties/Cities with Zero Values Stated"))
                                                ))))
                            
                         ),
                         
                         
                         #Drop Out Rate
                         tabPanel("Dropout Rate",
                                  column(4,
                                         h4(strong("Dropout Rate")),
                                  ),
                                  column(8,
                                  fluidRow(
                                    h1(strong("High School Dropout Rates in Hampton Roads"), align = "center"),
                                    withSpinner(leafletOutput("dropout_map")),
                                    p(tags$small("Data Source: Virginia Department of Education")),
                                    box(title = "Select Year", width = 12,
                                        selectInput("DropoutDropdown", "Select Year:", width = "100%", choices = c("2020", "2019", "2018", "2017", "2016", "2015",
                                                                                                                   "2014", "2013", "2012", "2011", "2010"))
                                        
                                    ))
                                  )
                                  
                         )
                 
                 
                 
                 ),

                 navbarMenu(title="Economics",
                            #Median Income
                            # tabPanel("Median Income",
                            #          fluidRow(style = "margin: 6px;",
                            #                   #change this later to formatting that is desired
                            #                   column(4,
                            #                           h4(strong("Median Income")),
                            #                           p("In 2019, the Black community had a median annual income of $52,596, which is less than the median income of $67,387 of Hampton Road’s general population. This is consistent with the state trend.  Notably, the gap has been closing over the last decade as in 2010, it was $37,941 and $55,062, respectively. This suggests that despite the Black household having a lower median annual income in the region, there have been some improvements since 2010.")
                            #                   ),
                            #                   h1(strong("Median Income in Virginia and Hampton Roads"), align = "center"),
                            #                   column(8, 
                            #                          h4("Median Income from 2010 to 2019"),
                            #                          withSpinner(plotOutput("medianTimeGraph")),
                            #                          p(tags$small("Data Source: ACS 5 Year Estimates Table S1903"))
                            #                   ), 
                            #                   
                            #                   column(8,
                            #                          selectInput("MedianIncomeYearDrop", "Select Year:", width = "100%", choices = c(
                            #                            "2019","2018", "2017", "2016", "2015","2014",
                            #                            "2013","2012", "2011", "2010")),
                            #                          withSpinner(plotOutput("income_plot")),
                            #                          p(tags$small("Data Source: ACS 5 Year Estimates Table S1903"))
                            #                   )
                            #          )), 
                            tabPanel("Median Income",
                                     fluidRow(
                                       column(3),
                                       column(6,
                                              tags$br(),
                                              p(style = "text-align: justify;", "In 2019, the Black community had a median annual income of $52,596, which is less than the median income of $67,387 of Hampton Road’s general population. This is consistent with the state trend.  Notably, the gap has been closing over the last decade as in 2010, it was $37,941 and $55,062, respectively. This suggests that despite the Black household having a lower median annual income in the region, there have been some improvements since 2010."))
                                     ),
                                     fluidRow(style = "margin: 6px", align = "center",
                                              column(12, align = "center",
                                                     h4("Median Income from 2010 to 2019"),
                                                     selectInput("MedianIncomeYearDrop", "Select Year:", width = "100%", choices = c(
                                                                                  "2019","2018", "2017", "2016", "2015","2014",
                                                                                  "2013","2012", "2011", "2010")),
                                                                                withSpinner(plotOutput("income_plot"))
                                                      )

                                                    )
                                     
                                     , 
                                     
                                     
                                     fluidRow(style = "margin: 6px", align = "center",
                                              column(12, align = "center",
                                                     h4("Median Income from 2010 to 2019"),
                                                     withSpinner(plotOutput("medianTimeGraph")),
                                                     p(tags$small("Data Source: ACS 5 Year Estimates Table S1903"))
                                              )
                                              
                                     )
                                     ),
                           
                 
                                     
                            
                                     # fluidRow(style = "margin: 6px;",
                                     #          #change this later to formatting that is desired
                                     #          column(4,
                                     #                 h4(strong("Median Income")),
                                     #                 p("In 2019, the Black community had a median annual income of $52,596, which is less than the median income of $67,387 of Hampton Road’s general population. This is consistent with the state trend.  Notably, the gap has been closing over the last decade as in 2010, it was $37,941 and $55,062, respectively. This suggests that despite the Black household having a lower median annual income in the region, there have been some improvements since 2010.")
                                     #          ),
                                     #          h1(strong("Median Income in Virginia and Hampton Roads"), align = "center"),
                                     #          column(8, 
                                     #                 h4("Median Income from 2010 to 2019"),
                                     #                 withSpinner(plotOutput("medianTimeGraph")),
                                     #                 p(tags$small("Data Source: ACS 5 Year Estimates Table S1903"))
                                     #          ), 
                                     #          
                                     #          column(8,
                                     #                 selectInput("MedianIncomeYearDrop", "Select Year:", width = "100%", choices = c(
                                     #                   "2019","2018", "2017", "2016", "2015","2014",
                                     #                   "2013","2012", "2011", "2010")),
                                     #                 withSpinner(plotOutput("income_plot")),
                                     #                 p(tags$small("Data Source: ACS 5 Year Estimates Table S1903"))
                                     #          )
                                     #)
                                     
                                     #),
                            #Homeownership Map
                            tabPanel("Homeownership Map", 
                                     fluidPage(
                                       
                                       column(4, h1(strong("Homeownership in Hampton Roads"), align = "center")),
                                       column(8,
                                           withSpinner(leafletOutput("homeownership_map")),
                                           p(tags$small("Data Source: ACS 5 Year Estimates Table S2505")),
                                           sliderInput("HomeOwnSlider", "", value = 2019, min = 2010, max = 2019, sep = "", width = "100%", animate=animationOptions(interval = 1400))),
                            )),
                            
                            
                            #Unemployment Rate
                            tabPanel("Labor Market Characteristics", 
                                     fluidRow(
                                       column(4, h1(strong("Unemployment"), align = "center")),
                                       column(8,
                                              tabsetPanel(
                                                #Sector Employment
                                                tabPanel("Industry Employment", 
                                                         h1(strong("Top Two Highest Industry Sectors"), align = "center"),
                                                         fluidRow(p(""),
                                                                  selectInput("SectorEmploymentYearDrop", "Select Year:", width = "100%", choices = c(
                                                                    "2019","2018", "2017", "2016", "2015","2014",
                                                                    "2013","2012", "2011", "2010")),
                                                                  withSpinner(plotlyOutput("sector_plot")),
                                                                  p(tags$small("Note: Some year to year comparisions had very little variability in enrollement by industry sectors. Certain counties/cities may not have data for specific years."))
                                                           
                                                         )
                                                ),
                                                
                                                tabPanel("Unemployment Rate",
                                                         h1(strong("Unemployment Rate in Hampton Roads"), align = "center"),
                                                             withSpinner(plotlyOutput("unemployment_plot")),
                                                             p(tags$small("Data Source: ACS 5 Year Estimates Table S2301")),
                                                             sliderInput("UnemploymentRateSlider", "Select Year", value = 2019, min = 2010, max = 2019, sep = "", width = "100%"),
                                                ),
                                                tabPanel("Unemployment Over Time",
                                                  fluidRow(
                                                  box(width = 8, height = 800,
                                                      img(src="unemployment_plot.gif", height='750', width='700')),

                                                  box(width = 4, height = 550,
                                                      h3("Description: "),
                                                      p("Placeholder Text"))
                                                  ),

                                                  fluidRow(
                                                    box(width =12, height = 300,
                                                        h3("Trends: "),
                                                        p("Text"))
                                                  )

                                                )
                                                
                                            )
                                         )
                                       )
                                     ), 
                            
                            #Poverty Rate
                            tabPanel("Poverty Rates",
                              column(4, h1(strong("Poverty Rates"), align = "center")),
                              column(8,
                                     tabsetPanel(
                                       tabPanel("Poverty Rates in Hampton and Virginia",
                                                h4(strong("Poverty Rates in Virginia and Hampton Roads")),
                                                selectInput("PovertyYearDrop", "Select Year:", width = "100%", choices = c(
                                                "2019","2018", "2017", "2016", "2015","2014",
                                                "2013","2012")),
                                                withSpinner(plotOutput("pov_plot")),
                                                p(tags$small("Data Source: ACS 5 Year Estimates Table S1701"))
                                       ),
                                       tabPanel("Poverty Rates in Hampton Roads Counties and Cities",
                                                h1(strong("Poverty Rates in Hampton Roads Counties and Cities"), align = "center"), 
                                                selectInput("PovertyCountYearDrop", "Select Year:", width = "100%", choices = c(
                                                "2019","2018", "2017", "2016", "2015","2014",
                                                "2013","2012")),
                                                withSpinner(plotlyOutput("counties_pov")),
                                                p(tags$small("Data Source: ACS 5 Year Estimates Table S1701"))
                                        ), 
                                       
                                       tabPanel("Poverty Over Time", h1(strong("Poverty Over Time"), align = "center"),
                                                fluidRow(width=12, height=550,
                                                    img(src="poverty2.gif", height = "800", width="1200")
                                                    
                                                )
                                        )
                                                 
                                                
                                       )
                                       
                                     )       
                                     
                                     
                              ), 
                            
                              #Uninsured Rates
                              tabPanel("Uninsured Rates", 
                                       column(4, h1(strong("Uninsured Rates"), align = "center")),
                                       column(8,
                                         fluidPage(
                                          h1(strong("Health Insurance in Hampton Roads"), align = "center"),
                                          withSpinner(plotlyOutput("uninsured_plot")),
                                          p(tags$small("Data Source: ACS 5 Year Estimates Table S2701")),
                                          sliderInput("UninsuredPctSlider", "Select Year", value = 2019, min = 2010, max = 2019, sep = "", width = "100%")),
                                  )
                                  
                                ),
                            
                            
                             #Veteran Status
                            
                            tabPanel("Veteran Status", 
                                     column(4, h1(strong("Veteran Status"), align = "center")),
                                     column(8,
                                        withSpinner(leafletOutput("veteran_map")),
                                        p(tags$small("Data Source: ACS 5 Year Estimates Table S2101")),
                                        sliderInput("VeteranSlider", "Select Year:", value = 2019, min = 2010, max = 2019, sep = "",  width = "100%")),
                                     )
                                     
                            ),
                 
                 tabPanel("VT DSPG Team", value = "team",
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                                   h1(strong("Hampton Team"), align = "center"),
                                   br(),
                                   h4(strong("Virginia Tech Data Science for the Public Good")),
                                   p("The", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics/dspg-program', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"), 
                                     "is a summer immersive program held at the", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics', 'University of Virginia Biocomplexity Institute’s Social and Decision Analytics division (SDAD).'), 
                                     "In its seventh year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around 
                                          critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences 
                                          to determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program 
                                          highlights, how to apply, and our annual symposium, please visit", a(href = 'https://biocomplexity.virginia.edu/social-decision-analytics/dspg-program', 'the official Biocomplexity DSPG website.', target = "_blank")),
                                   p("", style = "padding-top:10px;")
                          ),
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                                   column(6, align = "center",
                                          h4(strong("DSPG Team Members")),
                                          img(src = "Avi_Seth_Headshot.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          p("Avi Seth"),
                                          img(src = "BurkholderHeadshot.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          p("Matthew Burkholder"),
                                          img(src = "VictorMukoraHeadshot.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          p("Victor Mukora"),
                                          img(src = "Christina_Prisbe_Headshot.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          p("Christina Prisbe"),
                                          img(src = "BurkholderHeadshot.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          p("Kwabena Boateng")
                                   ),
                                   column(6, align = "center",
                                          h4(strong("Virginia Tech Faculty Members")),
                                          img(src = "Dr_Holmes.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          p("Dr. Chanita Holmes"),
                                          img(src = "Dr_Bradburn.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          p("Dr. Isabel Bradburn")
                                   )
                          ),
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                                   h4(strong("Project Stakeholders")),
                                   p(""),
                                   p("")
                                   # p(a(href = 'https://www.linkedin.com/in/nancy-bell-aa293810/', 'Nancy Bell', target = '_blank'), "(Virginia Department of Health);",
                                   #   a(href = 'https://www.linkedin.com/in/terri-alt-3138b4101/', 'Terri Alt', target = '_blank'), "(Virginia Cooperative Extension, Patrick County at Virginia Tech)."),
                                   # p("", style = "padding-top:10px;"),
                                   # h4(strong("Acknowledgments")),
                                   # p("We would like to thank Healthy Patrick County, an association of concerned Patrick County residents, and Brandon Kramer for their input to this project.")
                          )
                 ),
                 
                 
                 inverse = T)

# server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  runjs(jscode)
  
  #hampton roads map of counties -------------------------------------------
  output$hampton_counties_map <- renderPlot({
    coord_data <- read_rds("data/TableB01001FiveYearEstimates/coordinates.rds")
    coord_data <- st_transform(coord_data)
    coordinates1 <- coord_data %>% group_by(NAME) %>% slice(1)
    coordinates2 <- coordinates1[,6]
    city <- c("Chesapeake", "Franklin", "Gloucester", "Hampton", "Isle of Wight", "James City", "Mathews", 
              "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton", "Suffolk", "Virginia Beach",
              "Williamsburg", "York")
    coordinates2 <- mutate(coordinates2, Loc = city)
    coordinates2$Loc[coordinates2$Loc == "Franklin"] <- "Franklin City"
    #Graph
    hampton_counties_map <- ggplot(coordinates2) +
      geom_sf() +
      geom_sf_label(aes(label=Loc,geometry = geometry), label.padding = unit(.5, "mm"), size =4) +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size=13),
            panel.background = element_blank()) 
    #plot
    hampton_counties_map
  
  })
  
  # hampton race plots -----------------------------------------------------
  var_hampRace <- reactive({
    input$hampRaceYearDrop
  })
  
  output$hamp_pie <- renderPlot({
    #pal
    vir_pal <- c("#33638DFF", "#1F968BFF", "#29AF7FFF", "#73D055FF", "#FDE725FF")
    
    if(var_hampRace() == 2019){
      hamp_data <- read.csv("data/TableB02001FiveYearEstimates/hamp_race2019.csv")
    }
    if(var_hampRace() == 2018){
      hamp_data <- read.csv("data/TableB02001FiveYearEstimates/hamp_race2018.csv")
    }
    if(var_hampRace() == 2017){
      hamp_data <- read.csv("data/TableB02001FiveYearEstimates/hamp_race2017.csv")
    }
    if(var_hampRace() == 2016){
      hamp_data <- read.csv("data/TableB02001FiveYearEstimates/hamp_race2016.csv")
    }
    if(var_hampRace() == 2015){
      hamp_data <- read.csv("data/TableB02001FiveYearEstimates/hamp_race2015.csv")
    }
    if(var_hampRace() == 2014){
      hamp_data <- read.csv("data/TableB02001FiveYearEstimates/hamp_race2014.csv")
    }
    if(var_hampRace() == 2013){
      hamp_data <- read.csv("data/TableB02001FiveYearEstimates/hamp_race2013.csv")
    }
    if(var_hampRace() == 2012){
      hamp_data <- read.csv("data/TableB02001FiveYearEstimates/hamp_race2012.csv")
    }
    if(var_hampRace() == 2011){
      hamp_data <- read.csv("data/TableB02001FiveYearEstimates/hamp_race2011.csv")
    }
    if(var_hampRace() == 2010){
      hamp_data <- read.csv("data/TableB02001FiveYearEstimates/hamp_race2010.csv")
    }
    hamp_data <- hamp_data[,2:6]
    #combining data from counties
    variable <- sample(c("B02001_001", "B02001_002","B02001_003", "B02001_004",
                         "B02001_005", "B02001_006", "B02001_007", "B02001_008",
                         "B02001_009", "B02001_010"),160, replace = TRUE)
    hamp_data <- hamp_data %>% group_by(variable) %>% summarize(sum(estimate))
    #select the column and rows we want
    hamp_races <- hamp_data[c(2:8),]
    #Now graphing with other = hawaiin/pi, america/alask naive. other
    hamp_races3 <- data.frame(t(hamp_data[c(2:8),2]))
    hamp_races3 <- mutate(hamp_races3, X8 = X3+X5+X6)
    hamp_races4 <- data.frame(t(hamp_races3[,c(1,2,4,7,8)]))
    colnames(hamp_races4) <- "estimate"
    total_pop = sum(hamp_races4$estimate)
    hamp_races4 <- mutate(hamp_races4, total = total_pop)
    hamp_races4 <- mutate(hamp_races4, pct = estimate/total*100)
    hamp_races4 <- mutate(hamp_races4, race = c("White", "Black", "Asian", "Two or more", "Other"))
    colnames(hamp_races4) <- c("estimate", "Total", "Percent of Population", "race")
    
    hamp_races5 <- hamp_races4 %>% 
      mutate(
        cs = rev(cumsum(rev(`Percent of Population`))), 
        pos = `Percent of Population`/2 + lead(cs, 1),
        pos = if_else(is.na(pos),`Percent of Population`/2, pos))
    
    hamp_pie <- ggplot(hamp_races5, aes(x = "" , y = `Percent of Population`, fill = fct_inorder(race))) +
      geom_col(width = 1) +
      coord_polar(theta = "y", start = 0 ) +
      geom_label_repel(aes(y = pos, label = paste0(round(`Percent of Population`, digits=2), "%")),
                       data = hamp_races5, size=4, show.legend = F, nudge_x = 1) +
      guides(fill = guide_legend(title = "Key")) +
      scale_fill_manual(values =vir_pal)+
      theme_void() +
      theme(legend.title = element_blank())
    #plot
    hamp_pie
  })
  
  
  # VA race plots -----------------------------------------------------
  var_VaRace <- reactive({
    input$VaRaceYearDrop
  })
  
  output$va_pie <- renderPlot({
    #pal
    vir_pal <- c("#33638DFF", "#1F968BFF", "#29AF7FFF", "#73D055FF", "#FDE725FF")
    if(var_VaRace() == 2019){
      races <- read.csv("data/TableB02001FiveYearEstimates/va_race2019.csv")
    }
    if(var_VaRace() == 2018){
      races <- read.csv("data/TableB02001FiveYearEstimates/va_race2018.csv")
    }
    if(var_VaRace() == 2017){
      races <- read.csv("data/TableB02001FiveYearEstimates/va_race2017.csv")
    }
    if(var_VaRace() == 2016){
      races <- read.csv("data/TableB02001FiveYearEstimates/va_race2016.csv")
    }
    if(var_VaRace() == 2015){
      races <- read.csv("data/TableB02001FiveYearEstimates/va_race2015.csv")
    }
    if(var_VaRace() == 2014){
      races <- read.csv("data/TableB02001FiveYearEstimates/va_race2014.csv")
    }
    if(var_VaRace() == 2013){
      races <- read.csv("data/TableB02001FiveYearEstimates/va_race2013.csv")
    }
    if(var_VaRace() == 2012){
      races <- read.csv("data/TableB02001FiveYearEstimates/va_race2012.csv")
    }
    if(var_VaRace() == 2011){
      races <- read.csv("data/TableB02001FiveYearEstimates/va_race2011.csv")
    }
    if(var_VaRace() == 2010){
      races <- read.csv("data/TableB02001FiveYearEstimates/va_race2010.csv")
    }
    races <- races[,2:6]
    total <- races[1,4]
    #Now graphing with other = hawaiin/pi, america/alask naive. other
    va_races <-  data.frame(t((races[c(2:8), 4])))
    va_races <- mutate(va_races, X8 = X3+X5+X6)
    va_races2 <- data.frame(t(va_races[,c(1,2,4,7,8)]))
    colnames(va_races2) <- "estimate"
    va_races2 <- mutate(va_races2, totl = total)
    va_races2 <- mutate(va_races2, pct = estimate/totl*100)
    va_races2 <- mutate(va_races2, race = c("White", "Black", "Asian", "Two or more", "Other"))
    va_races3 <- va_races2 %>% 
      mutate(
        cs = rev(cumsum(rev(pct))), 
        pos = pct/2 + lead(cs, 1),
        pos = if_else(is.na(pos), pct/2, pos))
    
    va_pie <- ggplot(va_races3, aes(x = "" , y = pct, fill = fct_inorder(race))) +
      geom_col(width = 1) +
      coord_polar(theta = "y", start = 0 ) +
      geom_label_repel(aes(y = pos, label = paste0(round(pct, digits=2), "%")),
                       data = va_races3, size=4, show.legend = F, nudge_x = 1) +
      guides(fill = guide_legend(title = "Key")) +
      scale_fill_manual(values=vir_pal)+
      theme_void()  +
      theme(legend.title = element_blank()) 
    #plot 
    va_pie
    
  })
  
  # Hampton age plot-------------------------------------------------
  var_hampAge <- reactive({
    input$HampAgeYearDrop
  })
  
  output$hamp_graph <- renderPlot({
    #pal
    vir_pal <- c("#33638DFF", "#1F968BFF", "#29AF7FFF", "#73D055FF", "#FDE725FF")
    if(var_hampAge() == 2019){
      hamp_ages  <- read.csv("data/TableB01001FiveYearEstimates/hamp_age2019.csv")
    }
    if(var_hampAge() == 2018){
      hamp_ages  <- read.csv("data/TableB01001FiveYearEstimates/hamp_age2018.csv")
    }
    if(var_hampAge() == 2017){
      hamp_ages  <- read.csv("data/TableB01001FiveYearEstimates/hamp_age2017.csv")
    }
    if(var_hampAge() == 2016){
      hamp_ages  <- read.csv("data/TableB01001FiveYearEstimates/hamp_age2016.csv")
    }
    if(var_hampAge() == 2015){
      hamp_ages  <- read.csv("data/TableB01001FiveYearEstimates/hamp_age2015.csv")
    }
    if(var_hampAge() == 2014){
      hamp_ages  <- read.csv("data/TableB01001FiveYearEstimates/hamp_age2014.csv")
    }
    if(var_hampAge() == 2013){
      hamp_ages  <- read.csv("data/TableB01001FiveYearEstimates/hamp_age2013.csv")
    }
    if(var_hampAge() == 2012){
      hamp_ages  <- read.csv("data/TableB01001FiveYearEstimates/hamp_age2012.csv")
    }
    if(var_hampAge() == 2011){
      hamp_ages  <- read.csv("data/TableB01001FiveYearEstimates/hamp_age2011.csv")
    }
    if(var_hampAge() == 2010){
      hamp_ages  <- read.csv("data/TableB01001FiveYearEstimates/hamp_age2010.csv")
    }
    hamp_ages <- hamp_ages[,2:6]
    #total population in hampton Roads (1713267)
    hamp_pop_tbl <- hamp_ages %>%
      group_by(NAME) %>%
      slice(1)
    hamp_pop <- colSums(hamp_pop_tbl[,4])
    #Getting male estimates for each age group (summing every county for that specific male age group)
    hamp_male <- hamp_ages %>%
      group_by(NAME) %>%
      slice(3:25)
    hamp_male2 <- hamp_male %>% group_by(variable) %>% summarize(sum(estimate, na.rm = TRUE))
    #Getting female estimates for each age group (summing every county for that specific female age group)
    hamp_female <- hamp_ages %>%
      group_by(NAME) %>%
      slice(27:49)
    hamp_female2 <- hamp_female %>% group_by(variable) %>% summarize(sum(estimate, na.rm = TRUE))
    hamp_gender <- cbind(hamp_male2, hamp_female2)
    hamp_gender <- hamp_gender[,c(2,4)]
    #hamp_gender2 <- data.frame(estimate = c(hamp_gender[,1], hamp_gender[,2]))
    colnames(hamp_gender) <- c("male", "female")
    hamp_gender <- mutate(hamp_gender, total = male+female)
    #transposing just the estimates
    hamp_ages2 <- data.frame(t(hamp_gender[,3]))
    #sorting into the age groups
    hamp_ages2 <- mutate(hamp_ages2, Under18 = X1 + X2 + X3 + X4)
    hamp_ages2 <- mutate(hamp_ages2, YoungAdult = X5 + X6 + X7 + X8 + X9)
    hamp_ages2 <- mutate(hamp_ages2, Adult = X10 + X11 + X12)
    hamp_ages2 <- mutate(hamp_ages2, MiddleAge = X13 + X14 + X15 + X16 + X17)
    hamp_ages2 <- mutate(hamp_ages2, Senior = X18 + X19 + X20 + X21 + X22 + X23)
    #using just the 5 age group data that was just sorted
    hamp_ages3 <- hamp_ages2[,24:28]
    row.names(hamp_ages3) <- "General Estimate"
    hamp_ages3 <- data.frame(t(hamp_ages3))
    #Getting the percentage
    hamp_ages3 <- mutate(hamp_ages3,TotalPopulation = hamp_pop)
    hamp_ages3 <- mutate(hamp_ages3, PctPop = General.Estimate/TotalPopulation*100)
    hamp_ages3 <- mutate(hamp_ages3, Labels =c("Under 18", "18 to 29", "30 to 44",
                                               "45 to 64","65 and Older"))
    #ordering the age grpups
    hamp_ages3$Labels <- factor(hamp_ages3$Labels, levels=c("Under 18", "18 to 29", "30 to 44",
                                                            "45 to 64","65 and Older"))
    #Graph
    
    
    hamp_graph <- ggplot(hamp_ages3 , aes(x="", y=PctPop, fill=Labels)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) + 
      theme_void() +
      theme(
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 13)) +
      geom_text(aes(label = paste0(round(PctPop), "%")), position = position_stack(vjust=0.5), size=5, color = "white")  +
      scale_fill_manual(values =vir_pal)
    #plot
    hamp_graph
    
  })
  
  # Va age plot----------------------------------------------------
  var_VaAge <- reactive({
    input$VaAgeYearDrop
  })
  
  output$va_graph <- renderPlot({
    #pal
    vir_pal <- c("#33638DFF", "#1F968BFF", "#29AF7FFF", "#73D055FF", "#FDE725FF")
    if(var_VaAge() == 2019){
      age1  <- read.csv("data/TableB01001FiveYearEstimates/va_age2019.csv")
    }
    if(var_VaAge() == 2018){
      age1  <- read.csv("data/TableB01001FiveYearEstimates/va_age2018.csv")
    }
    if(var_VaAge() == 2017){
      age1  <- read.csv("data/TableB01001FiveYearEstimates/va_age2017.csv")
    }
    if(var_VaAge() == 2016){
      age1  <- read.csv("data/TableB01001FiveYearEstimates/va_age2016.csv")
    }
    if(var_VaAge() == 2015){
      age1  <- read.csv("data/TableB01001FiveYearEstimates/va_age2015.csv")
    }
    if(var_VaAge() == 2014){
      age1  <- read.csv("data/TableB01001FiveYearEstimates/va_age2014.csv")
    }
    if(var_VaAge() == 2013){
      age1  <- read.csv("data/TableB01001FiveYearEstimates/va_age2013.csv")
    }
    if(var_VaAge() == 2012){
      age1  <- read.csv("data/TableB01001FiveYearEstimates/va_age2012.csv")
    }
    if(var_VaAge() == 2011){
      age1  <- read.csv("data/TableB01001FiveYearEstimates/va_age2011.csv")
    }
    if(var_VaAge() == 2010){
      age1  <- read.csv("data/TableB01001FiveYearEstimates/va_age2010.csv")
    }
    age1 <- age1[,2:6]
    va_total_pop<- age1[1,4]
    #Adds the female and male data together to get the population for each age group
    va_male_age <- age1[3:25,]
    va_female_age <- age1[27:49,]
    va_male_age <- tibble::rowid_to_column(va_male_age, "ID")
    va_female_age <- tibble::rowid_to_column(va_female_age, "ID")
    #adding the male and female estimates to get the total
    ages <- merge(va_female_age, va_male_age, by = "ID")
    ages <- mutate(ages, total = estimate.x + estimate.y)
    #Getting just the estimates for each age group and transposing it to combining rows easily
    va_ages1 <- data.frame(t(ages[,12]))
    #Groups: Under 18, 18-30, 30-45, 45-65, 65+
    va_ages1 <- mutate(va_ages1, Under18 = X1 + X2 + X3 + X4)
    va_ages1 <- mutate(va_ages1, YoungAdult = X5 + X6 + X7 + X8 + X9)
    va_ages1 <- mutate(va_ages1, Adult = X10 + X11 + X12)
    va_ages1 <- mutate(va_ages1, MiddleAge = X13 + X14 + X15 + X16 + X17)
    va_ages1 <- mutate(va_ages1, Senior = X18 + X19 + X20 + X21 + X22 + X23)
    #using the 5 age group data
    va_ages2 <- va_ages1[,24:28]
    row.names(va_ages2) <- "Estimate"
    va_ages2 <- data.frame(t(va_ages2))
    va_ages2 <- mutate(va_ages2,TotalPopulation = va_total_pop)
    #Make Percentage
    va_ages2 <- mutate(va_ages2, PctPop = Estimate/TotalPopulation*100)
    #labeling
    va_ages2 <- mutate(va_ages2, labels = c("Under 18", "18 to 29", "30 to 44",
                                            "45 to 64","65 and Older"))
    colnames(va_ages2) <- c("Estimate", "Total Population", "Percent of Population", "Labels")
    va_ages2[,4] <- factor(va_ages2[,4], levels = c("Under 18", "18 to 29", "30 to 44",
                                                    "45 to 64","65 and Older"))
    #Graph
    va_graph <- ggplot(va_ages2 , aes(x="", y=`Percent of Population`, fill=Labels)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) + 
      theme_void() +
      theme(
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 13)) +
      geom_text(aes(label = paste0(round(`Percent of Population`), "%")), position = position_stack(vjust=0.5), size=5, color = "white") +
      scale_fill_manual(values =vir_pal)
    #plot
    va_graph
    
  })
  
  
  # Hampton Counties Map ------------------------------------------
  var_hampCountiesAge <- reactive({
    input$HampCountAgeYearDrop
  })
  
  output$age_map <- renderPlot({
    if(var_hampCountiesAge() == 2019){
      hamp_ages  <- read.csv("data/TableB01001FiveYearEstimates/hamp_age2019.csv")
    }
    if(var_hampCountiesAge() == 2018){
      hamp_ages  <- read.csv("data/TableB01001FiveYearEstimates/hamp_age2018.csv")
    }
    if(var_hampCountiesAge() == 2017){
      hamp_ages  <- read.csv("data/TableB01001FiveYearEstimates/hamp_age2017.csv")
    }
    if(var_hampCountiesAge() == 2016){
      hamp_ages  <- read.csv("data/TableB01001FiveYearEstimates/hamp_age2016.csv")
    }
    if(var_hampCountiesAge() == 2015){
      hamp_ages  <- read.csv("data/TableB01001FiveYearEstimates/hamp_age2015.csv")
    }
    if(var_hampCountiesAge() == 2014){
      hamp_ages  <- read.csv("data/TableB01001FiveYearEstimates/hamp_age2014.csv")
    }
    if(var_hampCountiesAge() == 2013){
      hamp_ages  <- read.csv("data/TableB01001FiveYearEstimates/hamp_age2013.csv")
    }
    if(var_hampCountiesAge() == 2012){
      hamp_ages  <- read.csv("data/TableB01001FiveYearEstimates/hamp_age2012.csv")
    }
    if(var_hampCountiesAge() == 2011){
      hamp_ages  <- read.csv("data/TableB01001FiveYearEstimates/hamp_age2011.csv")
    }
    if(var_hampCountiesAge() == 2010){
      hamp_ages  <- read.csv("data/TableB01001FiveYearEstimates/hamp_age2010.csv")
    }
    hamp_ages <- hamp_ages[,2:6]
    county_pop <- hamp_ages %>% group_by(NAME) %>%
      slice(1)
    county_pop <- county_pop[,4]
    #Getting male estimates for each age group 
    county_male <- hamp_ages %>%
      group_by(NAME) %>%
      slice(3:25)
    #Getting female estimates for each age group (summing every county for that specific female age group)
    county_female <- hamp_ages %>%
      group_by(NAME) %>%
      slice(27:49)
    #assigning ID to merge female and male estimates to get overall estimates
    county_male <- tibble::rowid_to_column(county_male, "ID")
    county_female <- tibble::rowid_to_column(county_female, "ID")
    county_ages <- merge(county_female, county_male, by = "ID")
    county_ages <- mutate(county_ages, total = estimate.x + estimate.y)
    #get the estimates put in the age groups(map data)
    #under 18
    county_under <- county_ages %>%
      group_by(NAME.y) %>%
      slice(1:4)
    county_under2 <- county_under %>%
      group_by(NAME.y) %>%
      summarise(x=sum(total))
    county_under2<- county_under2[,2]
    #young adult
    county_ya <- county_ages %>%
      group_by(NAME.y) %>%
      slice(5:9)
    county_ya2 <- county_ya %>%
      group_by(NAME.y) %>%
      summarise(x=sum(total))
    county_ya2 <- county_ya2[,2]
    #adult
    county_adult <- county_ages %>%
      group_by(NAME.y) %>%
      slice(10:12)
    county_adult2 <- county_adult %>%
      group_by(NAME.y) %>%
      summarise(x=sum(total))
    county_adult2 <- county_adult2[,2]
    #middle age
    county_ma <- county_ages %>%
      group_by(NAME.y) %>%
      slice(13:17)
    county_ma2 <- county_ma %>%
      group_by(NAME.y) %>%
      summarise(x=sum(total))
    county_ma2 <- county_ma2[,2]
    #senior
    county_senior <- county_ages %>%
      group_by(NAME.y) %>%
      slice(18:23)
    county_senior2 <- county_senior %>%
      group_by(NAME.y) %>%
      summarise(x=sum(total))
    county_senior2 <- county_senior2[,2]
    counties_label <- c("Chesapeake", "Franklin", "Hampton", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Suffolk", "Virginia Beach", "Williamsburg", "Gloucester", "Isle of Wight", "James City", "Mathews", "Southampton", "York")
    #getting coordinates
    lat <- c(36.690473, 36.683540, 37.046933, 37.123232, 36.903378, 37.130348, 36.878493, 36.714941, 36.792042,      
             37.267284, 37.405450, 36.901637, 37.311197, 37.470724, 36.720152, 37.242246)
    lon <- c(-76.297654, -76.940148, -76.390236, -76.523771, -76.248186, -76.357799, -76.380289, -76.626346,
             -76.053855, -76.708205, -76.519133, -76.708161, -76.804677, -76.375820, -77.114512, -76.566393)
    #format
    general_county_alt <-cbind(county_under2, county_ya2, county_adult2, county_ma2, county_senior2, county_pop)
    colnames(general_county_alt) <- c("a", "b", "c", "d", "e", "total")
    general_county_alt <-  mutate(general_county_alt, under = a/total*100)
    general_county_alt <-  mutate(general_county_alt, ya = b/total*100)
    general_county_alt <-  mutate(general_county_alt, adult = c/total*100)
    general_county_alt <-  mutate(general_county_alt, ma = d/total*100)
    general_county_alt <-  mutate(general_county_alt, senior = e/total*100)
    general_county_alt2 <- general_county_alt[,7:11]
    general_county_alt2 <-  mutate(general_county_alt2, county = counties_label)
    general_county_alt2 <- cbind(general_county_alt2, lon, lat)
    colnames(general_county_alt2) <- c("A", "B","C","D", "E", "county", "lon", "lat")
    #Getting map data for counties in Hampton roads
    coord_data <- read_rds("data/TableB01001FiveYearEstimates/coordinates.rds")
    coord_data <- st_transform(coord_data)
    coordinates1 <- coord_data %>% group_by(NAME) %>% slice(1)
    coordinates2 <- coordinates1[,6]
    city <- c("Chesapeake", "Franklin", "Gloucester", "Hampton", "Isle of Wight", "James City", "Mathews", 
              "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton", "Suffolk", "Virginia Beach",
              "Williamsburg", "York")
    coordinates2 <- mutate(coordinates2, Loc = city)
    coordinates2$Loc[coordinates2$Loc == "Franklin"] <- "Franklin City"
    #Graph
    age_map <- ggplot(coordinates2) +
      geom_sf() +
      geom_scatterpie(aes(x=lon, y=lat, group=county, r =0.05), data=general_county_alt2,
                      cols=LETTERS[1:5]) +
      geom_sf_label(aes(label=Loc,geometry = geometry), label.padding = unit(.5, "mm"), size =4, nudge_x=0.05, nudge_y = 0.05) +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size=13)) +
      #geom_scatterpie(aes(x=lon, y=lat, group=county, r =0.05), data=general_county_alt2,
         #             cols=LETTERS[1:5]) + 
      scale_fill_viridis_d(labels = c("Under 18", "18 to 29", "30 to 44",
                                      "45 to 64","65 and Older")) 
    #plot
    age_map
    
  })
  
  # Total Population Educational Attainment ---------------------------------
  
  var_genEducationalAttainment <- reactive({
    input$genEdAttainmentYearDrop
  })
  
  output$genEdAttainmentPlots <- renderPlotly({
    if(var_genEducationalAttainment() == "2019") {
      generalEducationalAttainment2019 <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2019.csv")
      colnames(generalEducationalAttainment2019) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      va_tot_education_bar2019 <- generalEducationalAttainment2019 %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Name)) + geom_col() +
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2019)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2019, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.43, text = "Source: ACS 5 Year Estimate Table S1501", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if(var_genEducationalAttainment() == "2018") {
      generalEducationalAttainment2018 <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2018.csv")
      colnames(generalEducationalAttainment2018) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      va_tot_education_bar2018 <- generalEducationalAttainment2018  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Name)) + geom_col() +
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2018)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2018, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.43, text = "Source: ACS 5 Year Estimate Table S1501", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if(var_genEducationalAttainment() == "2017") {
      generalEducationalAttainment2017 <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2017.csv")
      colnames(generalEducationalAttainment2017) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      va_tot_education_bar2017 <- generalEducationalAttainment2017  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Name)) + geom_col() +
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2017)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2017, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.43, text = "Source: ACS 5 Year Estimate Table S1501", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if(var_genEducationalAttainment() == "2016") {
      generalEducationalAttainment2016 <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2016.csv")
      colnames(generalEducationalAttainment2016) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      va_tot_education_bar2016 <- generalEducationalAttainment2016  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Name)) + geom_col() +
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2016)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2016, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.43, text = "Source: ACS 5 Year Estimate Table S1501", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if(var_genEducationalAttainment() == "2015") {
      generalEducationalAttainment2015 <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2015.csv")
      colnames(generalEducationalAttainment2015) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      va_tot_education_bar2015 <- generalEducationalAttainment2015  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Name)) + geom_col() +
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2015)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2015, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.43, text = "Source: ACS 5 Year Estimate Table S1501", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if(var_genEducationalAttainment() == "2014") {
      generalEducationalAttainment2014 <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2014.csv")
      colnames(generalEducationalAttainment2014) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      va_tot_education_bar2014 <- generalEducationalAttainment2014  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Name)) + geom_col() +
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2014)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2014, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.43, text = "Source: ACS 5 Year Estimate Table S1501", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if(var_genEducationalAttainment() == "2013") {
      generalEducationalAttainment2013 <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2013.csv")
      colnames(generalEducationalAttainment2013) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      va_tot_education_bar2013 <- generalEducationalAttainment2013  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Name)) + geom_col() +
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2013)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2013, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.43, text = "Source: ACS 5 Year Estimate Table S1501", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if(var_genEducationalAttainment() == "2012") {
      generalEducationalAttainment2012 <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2012.csv")
      colnames(generalEducationalAttainment2012) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      va_tot_education_bar2012 <- generalEducationalAttainment2012  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Name)) + geom_col() +
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2012)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2012, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.43, text = "Source: ACS 5 Year Estimate Table S1501", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if(var_genEducationalAttainment() == "2011") {
      generalEducationalAttainment2011 <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2011.csv")
      colnames(generalEducationalAttainment2011) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      va_tot_education_bar2011 <- generalEducationalAttainment2011  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Name)) + geom_col() +
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2011)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2011, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.43, text = "Source: ACS 5 Year Estimate Table S1501", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    
    else if(var_genEducationalAttainment() == "2010") {
      generalEducationalAttainment2010 <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2010.csv")
      colnames(generalEducationalAttainment2010) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      va_tot_education_bar2010 <- generalEducationalAttainment2010  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Name)) + geom_col() +
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2012)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2010, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.43, text = "Source: ACS 5 Year Estimate Table S1501", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
  })
  
  
  # Black Population Educational Attainment ---------------------------------
  
  var_blackEducationalAttainment <- reactive({
    input$blackEdAttainmentYearDrop
  })
  
  
  output$blackEdAttainmentPlots  <- renderPlotly({
    
    if(var_blackEducationalAttainment() == "2019") {
      blackEducationalAttainment2019 <- read.csv("data/TableC15002BFiveYearEstimates/blackEducationalAttainment2019.csv")
      colnames(blackEducationalAttainment2019) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %", "Gender")
      va_tot_education_bar2019 <- blackEducationalAttainment2019  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Gender)) + geom_col() + 
        geom_bar (stat="identity", position = "dodge") + 
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2019)",
                               y = "Percent (%)",
                               x = "Hampton Roads") +  theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      va_tot_education_bar2019 #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2019, tooltip=c("x", "y", "Gender"))) %>% 
        layout(annotations = list(x = 1, y = -0.43, text = "Source: ACS 5 Year Estimate Table C15002B", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if(var_blackEducationalAttainment() == "2018") {
      blackEducationalAttainment2018 <- read.csv("data/TableC15002BFiveYearEstimates/blackEducationalAttainment2018.csv")
      colnames(blackEducationalAttainment2018) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %", "Gender")
      va_tot_education_bar2018 <- blackEducationalAttainment2018  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Gender)) + geom_col() +
        geom_bar (stat="identity", position = "dodge") + 
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2018)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2018, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.43, text = "Source: ACS 5 Year Estimate Table C15002B", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if(var_blackEducationalAttainment() == "2017") {
      blackEducationalAttainment2017 <- read.csv("data/TableC15002BFiveYearEstimates/blackEducationalAttainment2017.csv")
      colnames(blackEducationalAttainment2017) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %", "Gender")
      va_tot_education_bar2017 <- blackEducationalAttainment2017  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Gender)) + geom_col() +
        geom_bar (stat="identity", position = "dodge") + 
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2017)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2017, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.43, text = "Source: ACS 5 Year Estimate Table C15002B", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if(var_blackEducationalAttainment() == "2016") {
      blackEducationalAttainment2016 <- read.csv("data/TableC15002BFiveYearEstimates/blackEducationalAttainment2016.csv")
      colnames(blackEducationalAttainment2016) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %", "Gender")
      va_tot_education_bar2016 <- blackEducationalAttainment2016  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Gender)) + geom_col() +
        geom_bar (stat="identity", position = "dodge") + 
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2016)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2016, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.43, text = "Source: ACS 5 Year Estimate Table C15002B", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if(var_blackEducationalAttainment() == "2015") {
      blackEducationalAttainment2015 <- read.csv("data/TableC15002BFiveYearEstimates/blackEducationalAttainment2015.csv")
      colnames(blackEducationalAttainment2015) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %", "Gender")
      va_tot_education_bar2015 <- blackEducationalAttainment2015  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Gender)) + geom_col() +
        geom_bar (stat="identity", position = "dodge") + 
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2015)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2015, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.43, text = "Source: ACS 5 Year Estimate Table C15002B", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if(var_blackEducationalAttainment() == "2014") {
      blackEducationalAttainment2014 <- read.csv("data/TableC15002BFiveYearEstimates/blackEducationalAttainment2014.csv")
      colnames(blackEducationalAttainment2014) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %", "Gender")
      va_tot_education_bar2014 <- blackEducationalAttainment2014  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Gender)) + geom_col() +
        geom_bar (stat="identity", position = "dodge") + 
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2015)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2014, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.43, text = "Source: ACS 5 Year Estimate Table C15002B", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if(var_blackEducationalAttainment() == "2013") {
      blackEducationalAttainment2013 <- read.csv("data/TableC15002BFiveYearEstimates/blackEducationalAttainment2013.csv")
      colnames(blackEducationalAttainment2013) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %", "Gender")
      va_tot_education_bar2013 <- blackEducationalAttainment2013  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Gender)) + geom_col() +
        geom_bar (stat="identity", position = "dodge") + 
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2013)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2013, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.43, text = "Source: ACS 5 Year Estimate Table C15002B", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if(var_blackEducationalAttainment() == "2012") {
      blackEducationalAttainment2012 <- read.csv("data/TableC15002BFiveYearEstimates/blackEducationalAttainment2012.csv")
      colnames(blackEducationalAttainment2012) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %", "Gender")
      va_tot_education_bar2012 <- blackEducationalAttainment2012  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Gender)) + geom_col() +
        geom_bar (stat="identity", position = "dodge") + 
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2012)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2012, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.43, text = "Source: ACS 5 Year Estimate Table C15002B", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if(var_blackEducationalAttainment() == "2011") {
      blackEducationalAttainment2011 <- read.csv("data/TableC15002BFiveYearEstimates/blackEducationalAttainment2011.csv")
      colnames(blackEducationalAttainment2011) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %", "Gender")
      va_tot_education_bar2011 <- blackEducationalAttainment2011  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Gender)) + geom_col() +
        geom_bar (stat="identity", position = "dodge") + 
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2015)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2011, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.43, text = "Source: ACS 5 Year Estimate Table C15002B", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if(var_blackEducationalAttainment() == "2010") {
      blackEducationalAttainment2010 <- read.csv("data/TableC15002BFiveYearEstimates/blackEducationalAttainment2010.csv")
      colnames(blackEducationalAttainment2010) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %", "Gender")
      va_tot_education_bar2010 <- blackEducationalAttainment2010 %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Gender)) + geom_col() +
        geom_bar (stat="identity", position = "dodge") + 
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2010)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2010, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.43, text = "Source: ACS 5 Year Estimate Table C15002B", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
  })
  
  
  
  # Teacher Demographics------------------------------------------------------
  
  var_teacherRaces <- reactive({
    input$teacherRaceBreakdown
  })
  
  
  output$teacherRacePlots <- renderPlotly({
    if(var_teacherRaces() == "Black") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <- c("Division Number", "Name", "Total Counts", "American Indian", "Asian", "Black", "Hispanic", "White","Hawaiian", "Two or More Races",  "Not Specified", "% of Black Teachers", "% of Asian Teachers", "% of Hispanic Teachers", "% of White Teachers", "% of American Indian Teachers", "% of Two Or More Races Teachers", "% of Hawaiian Teachers")
      teacherByRace <- teacherByRace  %>% 
        ggplot(aes(x = Name, y = `% of Black Teachers`, fill = Name)) + geom_col() +
        labs(title = "Black Teacher Breakdown", y = "Percentage (%)", x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40))  +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.60, text = "Source: Virginia 2020-2021 Teacher Race Report", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if (var_teacherRaces() == "Asian") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <- c("Division Number", "Name", "Total Counts", "American Indian", "Asian", "Black", "Hispanic", "White","Hawaiian", "Two or More Races",  "Not Specified", "% of Black Teachers", "% of Asian Teachers", "% of Hispanic Teachers", "% of White Teachers", "% of American Indian Teachers", "% of Two Or More Races Teachers", "% of Hawaiian Teachers")
      teacherByRace <- teacherByRace  %>% 
        ggplot(aes(x = Name, y = `% of Asian Teachers`, fill = Name)) + geom_col() +
        labs(title = "Asian Teacher Breakdown", y = "Percentage (%)", x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40))  +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.60, text = "Source: Virginia 2020-2021 Teacher Race Report", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if (var_teacherRaces() == "White") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <- c("Division Number", "Name", "Total Counts", "American Indian", "Asian", "Black", "Hispanic", "White","Hawaiian", "Two or More Races",  "Not Specified", "% of Black Teachers", "% of Asian Teachers", "% of Hispanic Teachers", "% of White Teachers", "% of American Indian Teachers", "% of Two Or More Races Teachers", "% of Hawaiian Teachers")
      teacherByRace <- teacherByRace  %>% 
        ggplot(aes(x = Name, y = `% of White Teachers`, fill = Name)) + geom_col() +
        labs(title = "White Teacher Breakdown", y = "Percentage (%)", x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40))  +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.60, text = "Source: Virginia 2020-2021 Teacher Race Report", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if (var_teacherRaces() == "Hispanic") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <- c("Division Number", "Name", "Total Counts", "American Indian", "Asian", "Black", "Hispanic", "White","Hawaiian", "Two or More Races",  "Not Specified", "% of Black Teachers", "% of Asian Teachers", "% of Hispanic Teachers", "% of White Teachers", "% of American Indian Teachers", "% of Two Or More Races Teachers", "% of Hawaiian Teachers")
      teacherByRace <- teacherByRace  %>% 
        ggplot(aes(x = Name, y = `% of Hispanic Teachers`, fill = Name)) + geom_col() +
        labs(title = "Hispanic Teacher Breakdown", y = "Percentage (%)", x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40))  +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.60, text = "Source: Virginia 2020-2021 Teacher Race Report", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if (var_teacherRaces() == "American Indian") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <- c("Division Number", "Name", "Total Counts", "American Indian", "Asian", "Black", "Hispanic", "White","Hawaiian", "Two or More Races",  "Not Specified", "% of Black Teachers", "% of Asian Teachers", "% of Hispanic Teachers", "% of White Teachers", "% of American Indian Teachers", "% of Two Or More Races Teachers", "% of Hawaiian Teachers")
      teacherByRace <- teacherByRace  %>% 
        ggplot(aes(x = Name, y = `% of American Indian Teachers`, fill = Name)) + geom_col() +
        labs(title = "American Indian Teacher Breakdown", y = "Percentage (%)", x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40))  +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.60, text = "Source: Virginia 2020-2021 Teacher Race Report", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
    else if (var_teacherRaces() == "Two or More Races") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <- c("Division Number", "Name", "Total Counts", "American Indian", "Asian", "Black", "Hispanic", "White","Hawaiian", "Two or More Races",  "Not Specified", "% of Black Teachers", "% of Asian Teachers", "% of Hispanic Teachers", "% of White Teachers", "% of American Indian Teachers", "% of Two Or More Races Teachers", "% of Hawaiian Teachers")
      teacherByRace <- teacherByRace  %>% 
        ggplot(aes(x = Name, y = `% of Two Or More Races Teachers`, fill = Name)) + geom_col() +
        labs(title = "Two or More Races Teacher Breakdown", y = "Percentage (%)", x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40))  +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.60, text = "Source: Virginia 2020-2021 Teacher Race Report", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
      
    }
    
    else if (var_teacherRaces() == "Hawaiian") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <- c("Division Number", "Name", "Total Counts", "American Indian", "Asian", "Black", "Hispanic", "White","Hawaiian", "Two or More Races",  "Not Specified", "% of Black Teachers", "% of Asian Teachers", "% of Hispanic Teachers", "% of White Teachers", "% of American Indian Teachers", "% of Two Or More Races Teachers", "% of Hawaiian Teachers")
      teacherByRace <- teacherByRace  %>% 
        ggplot(aes(x = Name, y = `% of Hawaiian Teachers`, fill = Name)) + geom_col() +
        labs(title = "", y = "Percentage (%)", x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40))  +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip=c("x", "y"))) %>% 
        layout(annotations = list(x = 1, y = -0.60, text = "Data Source: Virginia 2020-2021 Teacher Race Report", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=10, color="black"))
        )
    }
    
  }
  )
  
  # VA suspension--------------------------------------------------------------
  # var_suspension <- reactive({
  #   input$suspensionYearDrop
  # })
  
  # output$graph_va19 <- renderPlot({
  #   # if(var_suspension() == "2018-2019"){
  #   #   year <- "2018-2019"
  #   # }
  #   # else if (var_suspension() == "AY 2017-2018") {
  #   #   year <- "AY 2017-2018"
  #   # }
  #   # else if (var_suspension() == "AY 2016-2017") {
  #   #   year <- "AY 2016-2017"
  #   # }
  #   # else if (var_suspension() == "AY 2015-2016") {
  #   #   year <- "AY 2015-2016"
  #   # }
  #   # else if (var_suspension() == "AY 2014-2015") {
  #   #   year <- "AY 2014-2015"
  #   # }
  #   year <- "2018-2019"
  #   suspension_data <- read_excel("data/suspension/kidsCountSuspension.xlsx")
  #   #using only  VA data for 2018-2019
  #   suspension_va <- suspension_data %>% filter(Location=="Virginia")%>% filter(TimeFrame == year)
  #   #VA percentage estimate for 2018-2019 (Black)
  #   va_blck <- suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
  #   #VA percentage estimate for 2018-2019 (Hispanic)
  #   va_hisp <- suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
  #   #VA percentage estimate for 2018-2019 (white)
  #   va_white <- suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
  #   #combining the three percentages(b;ack, hispanic, white)
  #   va_suspension_race <- rbind(va_blck[,6], va_hisp[,6], va_white[,6])
  #   va_suspension_race$Data <- as.numeric(va_suspension_race$Data)
  #   va_suspension_race <- mutate(va_suspension_race, Data = Data*100)
  #   va_suspension_race <- mutate(va_suspension_race, race = c("Black", "Hispanic", "White"))
  #   #Graph
  #   graph_va19 <- ggplot(data=va_suspension_race, aes(x=race, y=Data)) +
  #     geom_bar(stat="identity", fill ="#0072B2")+
  #     geom_text(aes(label=paste0(round(Data, digits = 2), "%")), vjust=1.6, color="white", size=10)+
  #     theme_minimal()+
  #     theme(axis.title.x = element_blank(),
  #           axis.title.y = element_blank(),
  #           axis.text.y = element_blank(),
  #           axis.text.x = element_text(size=20)) 
  #   #plot
  #   graph_va19
  #   
  # })
  
  # Black suspension map -----------------------------------------------------
  # 
  # var_Bsuspension <- reactive({
  #   input$BsuspensionYearDrop
  # })
  # 
  # output$black_map <- renderPlot({
  #   if(var_Bsuspension() == "2018-2019"){
  #     year <- "2018-2019"
  #   }
  #   else if (var_Bsuspension() == "AY 2017-2018") {
  #     year <- "AY 2017-2018"
  #   }
  #   else if (var_Bsuspension() == "AY 2016-2017") {
  #     year <- "AY 2016-2017"
  #   }
  #   else if (var_Bsuspension() == "AY 2015-2016") {
  #     year <- "AY 2015-2016"
  #   }
  #   else if (var_Bsuspension() == "AY 2014-2015") {
  #     year <- "AY 2014-2015"
  #   }
  #   coord_data <- read_rds("data/suspension/coordinates.rds")
  #   coord_data <- st_transform(coord_data)
  #   coordinates1 <- coord_data %>% group_by(NAME) %>% slice(1)
  #   coordinates2 <- coordinates1[,6]
  #   city <- c("Chesapeake", "Franklin City", "Gloucester", "Hampton", "Isle of Wight", "James City", "Mathews", 
  #             "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton", "Suffolk", "Virginia Beach",
  #             "Williamsburg", "York")
  #   coordinates2 <- mutate(coordinates2, Location = city)
  #   suspension_counties <-filter(suspension_data, Location %in% city)
  #   #using percentages instead of number estimates (black)
  #   suspension_pct<- suspension_counties %>% filter(Race=="Black") %>%
  #     filter(DataFormat=="Percent")
  #   suspension_pct2 <- suspension_pct %>% filter(TimeFrame==year)
  #   #make a table w/ NA a S
  #   display_tbl <- suspension_pct2 %>% filter(Data %in% c("NA", "S"))
  #   display_tbl <- display_tbl[,c(2,6)]
  #   suspension_pct2$Data[suspension_pct2$Data=="NA"] <- 0
  #   suspension_pct2$Data[suspension_pct2$Data=="S"] <- 0
  #   #convert data column to numeric so we can multiply by 100
  #   suspension_pct2$Data <- sapply(suspension_pct2$Data, as.numeric)
  #   suspension_pct2 <- mutate(suspension_pct2, pct = Data *100)
  #   #adding geometry column(coordinates)
  #   suspension_pct3 <- merge(suspension_pct2, coordinates2, by = "Location")
  #   suspension_pct4 <- suspension_pct3[,c(1,7,8)]
  #   #add back the NA (S will be NA. We have a table to clarify)
  #   suspension_pct4$pct <- na_if(suspension_pct4$pct,0.00000)
  #   as.numeric(suspension_pct4$pct, na.rm = TRUE)
  #   #Graph
  #   graph_blck <-
  #     suspension_pct4 %>%
  #     ggplot() +
  #     geom_sf(aes(fill = pct, geometry = geometry))+
  #     geom_sf_label(aes(label=Location,geometry = geometry), label.padding = unit(.5, "mm"), size =4) +
  #     theme(plot.title = element_text(hjust = 0.5),
  #           axis.title.x=element_blank(),
  #           axis.text.x=element_blank(),
  #           axis.ticks.x=element_blank(),
  #           axis.title.y=element_blank(),
  #           axis.text.y=element_blank(),
  #           axis.ticks.y=element_blank(),
  #           legend.title = element_blank(),
  #           legend.text = element_text(size=13)) +
  #     scale_fill_gradient(high = "#132B43",
  #                         low = "#56B1F7",
  #                         space = "Lab",
  #                         na.value = "grey50",
  #                         guide = "colourbar",
  #                         aesthetics = "fill") +
  #     guides(colour=guide_legend("No data", override.aes=list(colour="grey50")))
  #   #display table
  #   na_rows <- display_tbl %>% filter(Data == "NA")
  #   supr_rows <- display_tbl %>% filter(Data == "S")
  #   supr_rows <- mutate(supr_rows, Data = "Suppressed")
  #   display_tbl_final <- rbind(na_rows, supr_rows)
  #   table_plot <- tableGrob(display_tbl_final, rows = NULL)
  #   #plot together
  #   black_map <- grid.arrange(graph_blck, table_plot, nrow=2, heights=c(3,1))
  #   black_map
  # })
  
  # suspension line graph
  output$suspension_line_graph <- renderPlotly({
    year <- "2018-2019"
    suspension_data <- read_excel("data/suspension/kidsCountSuspension.xlsx")
    #using only  VA data for 2018-2019
    suspension_va <- suspension_data %>% filter(Location=="Virginia")%>% filter(TimeFrame == year)
    #VA percentage estimate for 2018-2019 (Black)
    va_blck <- suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (Hispanic)
    va_hisp <- suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (white)
    va_white <- suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
    #combining the three percentages(b;ack, hispanic, white)
    va_suspension_race19 <- rbind(va_blck[,6], va_hisp[,6], va_white[,6])
    va_suspension_race19$Data <- as.numeric(va_suspension_race19$Data)
    va_suspension_race19 <- mutate(va_suspension_race19, Data = Data*100)
    va_suspension_race19 <- mutate(va_suspension_race19, race = c("Black", "Hispanic", "White"))
    va_suspension_race19 <- mutate(va_suspension_race19, year = "2019")
    ####
    year <- "AY 2017-2018"
    suspension_data <- read_excel("data/suspension/kidsCountSuspension.xlsx")
    #using only  VA data for 2018-2019
    suspension_va <- suspension_data %>% filter(Location=="Virginia")%>% filter(TimeFrame == year)
    #VA percentage estimate for 2018-2019 (Black)
    va_blck <- suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (Hispanic)
    va_hisp <- suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (white)
    va_white <- suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
    #combining the three percentages(b;ack, hispanic, white)
    va_suspension_race18 <- rbind(va_blck[,6], va_hisp[,6], va_white[,6])
    va_suspension_race18$Data <- as.numeric(va_suspension_race18$Data)
    va_suspension_race18 <- mutate(va_suspension_race18, Data = Data*100)
    va_suspension_race18 <- mutate(va_suspension_race18, race = c("Black", "Hispanic", "White"))
    va_suspension_race18 <- mutate(va_suspension_race18, year = "2018")
    ###
    year <- "AY 2016-2017"
    suspension_data <- read_excel("data/suspension/kidsCountSuspension.xlsx")
    #using only  VA data for 2018-2019
    suspension_va <- suspension_data %>% filter(Location=="Virginia")%>% filter(TimeFrame == year)
    #VA percentage estimate for 2018-2019 (Black)
    va_blck <- suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (Hispanic)
    va_hisp <- suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (white)
    va_white <- suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
    #combining the three percentages(b;ack, hispanic, white)
    va_suspension_race17 <- rbind(va_blck[,6], va_hisp[,6], va_white[,6])
    va_suspension_race17$Data <- as.numeric(va_suspension_race17$Data)
    va_suspension_race17 <- mutate(va_suspension_race17, Data = Data*100)
    va_suspension_race17 <- mutate(va_suspension_race17, race = c("Black", "Hispanic", "White"))
    va_suspension_race17 <- mutate(va_suspension_race17, year = "2017")
    ###
    year <- "AY 2015-2016"
    suspension_data <- read_excel("data/suspension/kidsCountSuspension.xlsx")
    #using only  VA data for 2018-2019
    suspension_va <- suspension_data %>% filter(Location=="Virginia")%>% filter(TimeFrame == year)
    #VA percentage estimate for 2018-2019 (Black)
    va_blck <- suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (Hispanic)
    va_hisp <- suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (white)
    va_white <- suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
    #combining the three percentages(black, hispanic, white)
    va_suspension_race16 <- rbind(va_blck[,6], va_hisp[,6], va_white[,6])
    va_suspension_race16$Data <- as.numeric(va_suspension_race16$Data)
    va_suspension_race16 <- mutate(va_suspension_race16, Data = Data*100)
    va_suspension_race16 <- mutate(va_suspension_race16, race = c("Black", "Hispanic", "White"))
    va_suspension_race16 <- mutate(va_suspension_race16, year = "2016")
    ##
    year <- "AY 2014-2015"
    suspension_data <- read_excel("data/suspension/kidsCountSuspension.xlsx")
    #using only  VA data for 2018-2019
    suspension_va <- suspension_data %>% filter(Location=="Virginia")%>% filter(TimeFrame == year)
    #VA percentage estimate for 2018-2019 (Black)
    va_blck <- suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (Hispanic)
    va_hisp <- suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (white)
    va_white <- suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
    #combining the three percentages(b;ack, hispanic, white)
    va_suspension_race15 <- rbind(va_blck[,6], va_hisp[,6], va_white[,6])
    va_suspension_race15$Data <- as.numeric(va_suspension_race15$Data)
    va_suspension_race15 <- mutate(va_suspension_race15, Data = Data*100)
    va_suspension_race15 <- mutate(va_suspension_race15, race = c("Black", "Hispanic", "White"))
    va_suspension_race15 <- mutate(va_suspension_race15, year = "2015")
    ###########
    suspension_line <- rbind(va_suspension_race19, va_suspension_race18, va_suspension_race17, va_suspension_race16, va_suspension_race15)
    suspension_line_graph <- ggplot(suspension_line, aes(x=year, y=Data, group = race, color = race)) + 
      geom_line(position = "identity", size =1.5) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        legend.title = element_blank(),
        legend.text = element_text(size=13),
        axis.text = element_text(size=13)) +
      labs(y ="Percent (%)") +
      scale_color_viridis_d() +
      scale_y_continuous(limits = c(2,14), breaks = seq(0, 14, by =2))
    #plot
    ggplotly(suspension_line_graph) %>%
      layout(legend = list(y=0.5))
    
  })
  #suspension gap line graph ------------------------------------------------
  output$suspensionGap <- renderPlotly({
    gap_data <- read.csv("data/suspension/suspensionGap.csv")
    gap_data$year[gap_data$year == "2018-2019"] <- "2019"
    gap_data$year[gap_data$year == "2017-2018"] <- "2018"
    gap_data$year[gap_data$year == "2016-2017"] <- "2017"
    gap_data$year[gap_data$year == "2015-2016"] <- "2016"
    gap_data$year[gap_data$year == "2014-2015"] <- "2015"
    susGapPlot <- ggplot(gap_data, aes(x=year, y=gap, group = Location, color =Location)) + 
      geom_line(position = "identity", size =1.5) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        legend.title = element_blank(),
        legend.text = element_text(size=13),
        axis.text = element_text(siz=13)) +
      labs(y ="Percent Difference (%)") +
      scale_color_viridis_d() +
      ylim(0,16)
    
    suspensionGap <- ggplotly(susGapPlot) %>%
      layout(legend = list(y=0.5))
 
  })
  
  
  
  
  
  # Suspension for black and white (counties) ---------------------------------
  var_BWsuspension <- reactive({
    input$BWsuspensionYearDrop
  })
  
  output$BW_map <- renderPlot({
    
    if(var_BWsuspension() == "2018-2019"){
      year <- "2018-2019"
      suspension_data <- read_excel("data/suspension/kidsCountSuspension.xlsx")
      city <- c("Chesapeake", "Franklin City", "Gloucester", "Hampton", "Isle of Wight", "James City", "Mathews", 
                "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton", "Suffolk", "Virginia Beach",
                "Williamsburg", "York")
      suspension_counties <-filter(suspension_data, Location %in% city)
      
      pct_white<- suspension_counties %>% filter(Race=="White") %>%
        filter(DataFormat=="Percent")
      pct_white2 <- pct_white %>% filter(TimeFrame==year)
      pct_white2$Location[pct_white2$Location == "Williamsburg"] <- "Williamsburg-James City"
      pct_white2 <- pct_white2[c(1:5,7:16),]
      #putting NAs and Ss in a table
      #pct_white2$Data[is.na(pct_white2$Data)] <- "NA"
      display_tbl_white <- pct_white2 %>% filter(Data %in% c("NA", "S", "<", "*"))
      display_tbl_white2<- display_tbl_white[,c(2,3,6)]
      pct_white2$Data[pct_white2$Data=="NA"] <- 0
      pct_white2$Data[pct_white2$Data=="S"] <- 0
      pct_white2$Data[pct_white2$Data=="<"] <- 0
      pct_white2$Data[pct_white2$Data=="*"] <- 0
      #adding estimates by 100 (need to convert to numeric first)
      pct_white2$Data <- sapply(pct_white2$Data, as.numeric)
      pct_white2 <- mutate(pct_white2, pct = Data *100)
      pct_white2$pct <- na_if(pct_white2$pct,0.00000)
      as.numeric(pct_white2$pct, na.rm = TRUE)
      #labeling
      pct_white3 <- pct_white2[,c(2,7)]
      colnames(pct_white3) <- c("Location", "Percent (%)")
      #black data
      suspension_pct<- suspension_counties %>% filter(Race=="Black") %>%
        filter(DataFormat=="Percent")
      suspension_pct2 <- suspension_pct %>% filter(TimeFrame==year)
      suspension_pct2$Location[suspension_pct2$Location == "Williamsburg"] <- "Williamsburg-James City"
      suspension_pct2 <- suspension_pct2[c(1:5,7:16),]
      #make a table w/ NA a S
      suspension_pct2$Data[is.na(suspension_pct2$Data)] <- "NA"
      display_tbl_black <- suspension_pct2 %>% filter(Data %in% c("NA", "S","<", "*"))
      display_tbl_black2 <- display_tbl_black[,c(2,3,6)]
      suspension_pct2$Data[suspension_pct2$Data=="NA"] <- 0
      suspension_pct2$Data[suspension_pct2$Data=="S"] <- 0
      suspension_pct2$Data[suspension_pct2$Data=="<"] <- 0
      suspension_pct2$Data[suspension_pct2$Data=="*"] <- 0
      #convert data column to numeric so we can multiply by 100
      suspension_pct2$Data <- sapply(suspension_pct2$Data, as.numeric)
      suspension_pct2 <- mutate(suspension_pct2, pct = Data *100)
      suspension_pct2$pct <- na_if(suspension_pct2$pct,0.00000)
      as.numeric(suspension_pct2$pct, na.rm = TRUE)
      pct_blck <-suspension_pct2[,c(2,7)]
      colnames(pct_blck) <- c("Location", "Percent (%)")
      sus <- rbind(pct_blck,pct_white3)
      num <- nrow(sus)/2
      sus <- mutate(sus, race = c(rep("Black Students",num), rep("White Students", num)))
      #bar graph
      suspension_counties_plot <-
        ggplot(sus , aes(Location, y=`Percent (%)`, fill=race)) +
        geom_bar(stat="identity", position=position_dodge())+
        geom_text(aes(label=paste0(round(`Percent (%)`, digits=1), "%")), vjust=1.5, color="white",
                  position = position_dodge(0.9), size=3)+
        theme_minimal() + 
        theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'), 
              legend.key.height = unit(0.3, 'cm'), 
              legend.key.width = unit(0.3, 'cm'), 
              legend.title = element_blank(),
              legend.text = element_text(size=14),
              axis.text=element_text(size=15),
              #axis.text.x = element_text(size=10, face="bold"),
              axis.title=element_text(size=17),
              axis.title.x=element_blank()) +
        theme(axis.text.x = element_text(angle = 40, vjust = 0.95, hjust=1))+
        scale_fill_manual(values=c("#D55E00","#0072B2")) +
        labs(x = "Location") 
      #combining the tables
      # display_table <- rbind(display_tbl_white2, display_tbl_black2)
      # na_rows <- display_table %>% filter(Data == "NA")
      # supr_rows <- display_table %>% filter(Data == "S")
      # less_rows <- display_table %>% filter(Data == "<")
      # other_rows <- display_table %>% filter(Data == "*")
      # supr_rows <- mutate(supr_rows, Data = "Suppressed")
      # other_rows <- mutate(other_rows, Data = "Suppressed")
      # less_rows <- mutate(less_rows, Data = "less than 10")
      # display_table_final <- rbind(na_rows, supr_rows, less_rows, other_rows)
      # table_plot <- tableGrob(display_table_final, rows = NULL)
      #plot together
     
      
      
      # BW_map <- grid.arrange(suspension_counties_plot, table_plot, nrow=2, heights=c(4,1))
      # BW_map
      # 
    
      BW_map <- grid.arrange(suspension_counties_plot)
      BW_map
      
    }
    else if (var_BWsuspension() %in% c("AY 2017-2018", "AY 2016-2017", "AY 2015-2016", "AY 2014-2015")){
      if (var_BWsuspension() == "AY 2017-2018") {
        year <- "AY 2017-2018"
      }
      else if (var_BWsuspension() == "AY 2016-2017") {
        year <- "AY 2016-2017"
      }
      else if (var_BWsuspension() == "AY 2015-2016") {
        year <- "AY 2015-2016"
      }
      else if (var_BWsuspension() == "AY 2014-2015") {
        year <- "AY 2014-2015"
      }
      suspension_data <- read_excel("data/suspension/kidsCountSuspension.xlsx")
      city <- c("Chesapeake", "Franklin City", "Gloucester", "Hampton", "Isle of Wight", "James City", "Mathews", 
                "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton", "Suffolk", "Virginia Beach",
                "Williamsburg", "York")
      suspension_counties <-filter(suspension_data, Location %in% city)
      
      pct_white<- suspension_counties %>% filter(Race=="White") %>%
        filter(DataFormat=="Percent")
      pct_white2 <- pct_white %>% filter(TimeFrame==year)
      pct_white2$Location[pct_white2$Location == "James City"] <- "Williamsburg-James City"
      pct_white2$Location[pct_white2$Location == "Williamsburg"] <- "Williamsburg-James City"
      #putting NAs and Ss in a table
      pct_white2$Data[is.na(pct_white2$Data)] <- "NA"
      display_tbl_white <- pct_white2 %>% filter(Data %in% c("NA", "S", "<", "*"))
      display_tbl_white2<- display_tbl_white[,c(2,3,6)]
      pct_white2$Data[pct_white2$Data=="NA"] <- 0
      pct_white2$Data[pct_white2$Data=="S"] <- 0
      pct_white2$Data[pct_white2$Data=="<"] <- 0
      pct_white2$Data[pct_white2$Data=="*"] <- 0
      #adding estimates by 100 (need to convert to numeric first)
      pct_white2$Data <- sapply(pct_white2$Data, as.numeric)
      pct_white2 <- mutate(pct_white2, pct = Data *100)
      pct_white2$pct <- na_if(pct_white2$pct,0.00000)
      as.numeric(pct_white2$pct, na.rm = TRUE)
      #labeling
      pct_white3 <- pct_white2[,c(2,7)]
      colnames(pct_white3) <- c("Location", "Percentage of Students (%)")
      #black data
      suspension_pct<- suspension_counties %>% filter(Race=="Black") %>%
        filter(DataFormat=="Percent")
      suspension_pct2 <- suspension_pct %>% filter(TimeFrame==year)
      suspension_pct2$Location[suspension_pct2$Location == "James City"] <- "Williamsburg-James City"
      suspension_pct2$Location[suspension_pct2$Location == "Williamsburg"] <- "Williamsburg-James City"
      #make a table w/ NA a S
      suspension_pct2$Data[is.na(suspension_pct2$Data)] <- "NA"
      display_tbl_black <- suspension_pct2 %>% filter(Data %in% c("NA", "S","<", "*"))
      display_tbl_black2 <- display_tbl_black[,c(2,3,6)]
      suspension_pct2$Data[suspension_pct2$Data=="NA"] <- 0
      suspension_pct2$Data[suspension_pct2$Data=="S"] <- 0
      suspension_pct2$Data[suspension_pct2$Data=="<"] <- 0
      suspension_pct2$Data[suspension_pct2$Data=="*"] <- 0
      #convert data column to numeric so we can multiply by 100
      suspension_pct2$Data <- sapply(suspension_pct2$Data, as.numeric)
      suspension_pct2 <- mutate(suspension_pct2, pct = Data *100)
      suspension_pct2$pct <- na_if(suspension_pct2$pct,0.00000)
      as.numeric(suspension_pct2$pct, na.rm = TRUE)
      pct_blck <-suspension_pct2[,c(2,7)]
      colnames(pct_blck) <- c("Location", "Percentage of Students (%)")
      sus <- rbind(pct_blck,pct_white3)
      num <- nrow(sus)/2
      sus <- mutate(sus, race = c(rep("Black Students",num), rep("White Students", num)))
      #bar graph
      suspension_counties_plot <-
        ggplot(sus , aes(Location, y=`Percentage of Students (%)`, fill=race)) +
        geom_bar(stat="identity", position=position_dodge())+
        geom_text(aes(label=paste0(round(`Percentage of Students (%)`, digits=1), "%")), vjust=1.5, color="white",
                  position = position_dodge(0.9), size=3)+
        theme_minimal() + 
        theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'), 
              legend.key.height = unit(0.3, 'cm'), 
              legend.key.width = unit(0.3, 'cm'), 
              legend.title = element_blank(),
              legend.text = element_text(size=14),
              axis.text=element_text(size=15),
              #axis.text.x = element_text(size=10, face="bold"),
              axis.title=element_text(size=17),
              axis.title.x=element_blank()) +
        theme(axis.text.x = element_text(angle = 40, vjust = 0.95, hjust =1)) +
        scale_fill_manual(values=c("#D55E00","#0072B2")) +
        labs(x = "Location") 
      #combining the tables
      display_table <- rbind(display_tbl_white2, display_tbl_black2)
      na_rows <- display_table %>% filter(Data == "NA")
      supr_rows <- display_table %>% filter(Data == "S")
      less_rows <- display_table %>% filter(Data == "<")
      other_rows <- display_table %>% filter(Data == "*")
      supr_rows <- mutate(supr_rows, Data = "Suppressed")
      other_rows <- mutate(other_rows, Data = "Suppressed")
      less_rows <- mutate(less_rows, Data = "less than 10")
      display_table_final <- rbind(na_rows, supr_rows, less_rows, other_rows)
      table_plot <- tableGrob(display_table_final, rows = NULL)
      #plot together
      BW_map <- grid.arrange(suspension_counties_plot, table_plot, nrow=2, heights=c(4,1))
      BW_map
    }
  })
  
  
  # Dropout Rates -----------------------------------------------------------
  
  var_dropoutrate <- reactive({
    input$DropoutDropdown
  })
  
  output$dropout_map <- renderLeaflet({
    if(var_dropoutrate() == "2020") {
      dropout_20 <- read.csv("data/dropout2020.csv")
      mapping <- read.csv("data/dropoutmapdata.csv")
      colors <- c("#0072B2", "#D55E00")
      dropout_20_map <- leaflet() %>% 
        addProviderTiles("CartoDB.Voyager") %>% 
        addMinicharts(
          mapping$lon, mapping$lat,
          chartdata = dropout_20,
          colorPalette = colors,
          width = 45, height = 45
        )
    }
  })
  
  # Median Income plots: Working on it --------------------------------
  var_medianIncome <- reactive({
    input$MedianIncomeYearDrop
  })
  
  output$income_plot <- renderPlot({
    if(var_medianIncome() %in% c("2019", "2018", "2017")){
      if(var_medianIncome() == "2019") { 
        va_yr <- read.csv("data/TableS1903FiveYearEstimates/va_income2019.csv")
        hamp_yr <- read.csv("data/TableS1903FiveYearEstimates/hampton_income2019.csv")
      }
      else if(var_medianIncome() == "2018") { 
        va_yr <- read.csv("data/TableS1903FiveYearEstimates/va_income2018.csv")
        hamp_yr <- read.csv("data/TableS1903FiveYearEstimates/hampton_income2018.csv")
      }
      else if(var_medianIncome() == "2017") { 
        va_yr <- read.csv("data/TableS1903FiveYearEstimates/va_income2017.csv")
        hamp_yr <- read.csv("data/TableS1903FiveYearEstimates/hampton_income2017.csv")
      }
      va_yr <- va_yr[2:6]
      race_names <- c("Total", "Black")
      #VA income
      va_race_income_median <- data.frame(va_yr[c(81,83), 4])
      va_race_income <- data.frame(cbind(race_names, va_race_income_median))
      colnames(va_race_income) <- c("Race", "Median Income")
      #Hampton Income
      hamp_yr <- hamp_yr[2:6]
      #getting the name, variable and estimate
      hamp_income2 <- hamp_yr[,2:4]
      hamp_income3 <- hamp_income2 %>%
        group_by(NAME) %>%
        slice(c(81,83))
      variable <- sample(c("S1903_C03_001","S1903_C03_003"),32, replace = TRUE)
      hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
      #Putting them together
      median_income <- cbind(va_race_income, hamp_race_income_median)
      median_income <- median_income[, c(2,4)]
      #having all the estimates in the same column
      median_income2 <- data.frame(median = c(median_income[,1], median_income[,2]))
      #labeling
      median_income2 <- mutate(median_income2, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
      median_income2 <- mutate(median_income2, demo = rep(c("Total Population", "Black Population"),2))
      colnames(median_income2) <- c("Median Income (US Dollars)", "Location", "Demographic")
      #making them all numeric
      median_income2 <- transform(median_income2, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
      colnames(median_income2) <- c("Median Income (US Dollars)", "Location", "Demographic")
      median_income2$Location <- factor(median_income2$Location, levels= c("Hampton Roads","Virginia"))
      #Graph
      income_plot <- ggplot(median_income2, aes(x=Location, y=`Median Income (US Dollars)`, fill=Demographic)) +
        geom_bar(stat="identity", position=position_dodge())+
        geom_text(aes(label=paste0(round(`Median Income (US Dollars)`))), vjust=1.5, color="white",
                  position = position_dodge(0.9), size=5)+
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'),
              legend.title = element_blank(),
              legend.key.height = unit(1, 'cm'), 
              legend.key.width = unit(1, 'cm'),
              legend.text = element_text(size=14),
              axis.text=element_text(size=15),
              axis.title=element_text(size=17),
              axis.title.x=element_blank(),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      #plot
      income_plot
    }
    else if (var_medianIncome() %in% c("2016", "2015", "2014", "2013", "2012", "2011", "2010")){
      
      if(var_medianIncome() == "2016") { 
        va_yr <- read.csv("data/TableS1903FiveYearEstimates/va_income2016.csv")
        hamp_yr <- read.csv("data/TableS1903FiveYearEstimates/hampton_income2016.csv")
      }
      else if(var_medianIncome() == "2015") { 
        va_yr <- read.csv("data/TableS1903FiveYearEstimates/va_income2015.csv")
        hamp_yr <- read.csv("data/TableS1903FiveYearEstimates/hampton_income2015.csv")
      }
      else if(var_medianIncome() == "2014") { 
        va_yr <- read.csv("data/TableS1903FiveYearEstimates/va_income2014.csv")
        hamp_yr <- read.csv("data/TableS1903FiveYearEstimates/hampton_income2014.csv")
      }
      else if(var_medianIncome() == "2013") { 
        va_yr <- read.csv("data/TableS1903FiveYearEstimates/va_income2013.csv")
        hamp_yr <- read.csv("data/TableS1903FiveYearEstimates/hampton_income2013.csv")
      }
      else if(var_medianIncome() == "2012") { 
        va_yr <- read.csv("data/TableS1903FiveYearEstimates/va_income2012.csv")
        hamp_yr <- read.csv("data/TableS1903FiveYearEstimates/hampton_income2012.csv")
      }
      else if(var_medianIncome() == "2011") { 
        va_yr <- read.csv("data/TableS1903FiveYearEstimates/va_income2011.csv")
        hamp_yr <- read.csv("data/TableS1903FiveYearEstimates/hampton_income2011.csv")
      }
      else if(var_medianIncome() == "2010") { 
        va_yr <- read.csv("data/TableS1903FiveYearEstimates/va_income2010.csv")
        hamp_yr <- read.csv("data/TableS1903FiveYearEstimates/hampton_income2010.csv")
      }
      
      va_yr <- read.csv("data/TableS1903FiveYearEstimates/va_income2016.csv")
      va_yr <- va_yr[,2:6]
      #income by Race
      race_names <- c("Total", "Black")
      #median income
      va_race_income_median <- data.frame(va_yr[c(31,33), 4])
      va_race_income <- data.frame(cbind(race_names, va_race_income_median))
      colnames(va_race_income) <- c("Race", "Median Income")
      #Hampton Income
      #getting the name, variable and estimate
      hamp_income2 <- hamp_yr[,2:6]
      hamp_income3 <- hamp_income2 %>%
        group_by(NAME) %>%
        slice(c(31,33))
      variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
      hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
      #Putting them in the same dataset
      median_income <- cbind(va_race_income, hamp_race_income_median)
      median_income <- median_income[, c(2,4)]
      #having all the estimates in the same column
      median_income2 <- data.frame(median = c(median_income[,1], median_income[,2]))
      median_income2 <- mutate(median_income2, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
      median_income2 <- mutate(median_income2, demo = rep(c("Total Population", "Black Population"),2))
      colnames(median_income2) <- c("Median Income (US Dollars)", "Location", "Demographic")
      #making them all numeric
      median_income2 <- transform(median_income2, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
      colnames(median_income2) <- c("Median Income (US Dollars)", "Location", "Demographic")
      median_income2$Location <- factor(median_income2$Location, levels= c("Hampton Roads","Virginia"))
      #Hampton and VA graph
      income_plot <- ggplot(median_income2, aes(x=Location, y=`Median Income (US Dollars)`, fill=Demographic)) +
        geom_bar(stat="identity", position=position_dodge())+
        geom_text(aes(label=paste0(round(`Median Income (US Dollars)`))), vjust=1.5, color="white",
                  position = position_dodge(0.9), size=5)+
        theme_minimal()+ 
        theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'),
              legend.title = element_blank(),
              legend.key.height = unit(1, 'cm'), 
              legend.key.width = unit(1, 'cm'),
              legend.text = element_text(size=14),
              axis.text=element_text(size=15),
              axis.title=element_text(size=17),
              axis.title.x=element_blank(),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      #plot
      income_plot
    }
    
    
  })
  
  # Median Income line plots -------------------------------------------------
  
  output$medianTimeGraph <- renderPlot ({
    va_yr <- read.csv("data/TableS1903FiveYearEstimates/va_income2019.csv")
    va_yr <- va_yr[2:6]
    race_names <- c("Total", "Black")
    #median income
    va_race_income_median <- data.frame(va_yr[c(81,83), 4])
    va_race_income <- data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/TableS1903FiveYearEstimates/hampton_income2019.csv")
    hamp_yr <- hamp_yr[2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[,2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(81,83))
    #This give us overall hampton overall and black median income
    variable <- sample(c("S1903_C03_001","S1903_C03_003"),32, replace = TRUE)
    hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2,4)]
    #having all the estimates in the same column
    median_income19 <- data.frame(median = c(median_income[,1], median_income[,2]))
    #labeling
    median_income19 <- mutate(median_income19, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
    median_income19 <- mutate(median_income19, demo = rep(c("Total Population", "Black Population"),2))
    colnames(median_income19) <- c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income19 <- transform(median_income19, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income19) <- c("Median Income (US Dollars)", "Location", "Demographic")
    median_income19 <- mutate(median_income19, Year = "2019")
    ############################################################################2018
    va_yr <- read.csv("data/TableS1903FiveYearEstimates/va_income2018.csv")
    va_yr <- va_yr[2:6]
    race_names <- c("Total", "Black")
    #median income
    va_race_income_median <- data.frame(va_yr[c(81,83), 4])
    va_race_income <- data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/TableS1903FiveYearEstimates/hampton_income2018.csv")
    hamp_yr <- hamp_yr[2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[,2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(81,83))
    #This give us overall hampton overall and black median income
    variable <- sample(c("S1903_C03_001","S1903_C03_003"),32, replace = TRUE)
    hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2,4)]
    #having all the estimates in the same column
    median_income18 <- data.frame(median = c(median_income[,1], median_income[,2]))
    #labeling
    median_income18 <- mutate(median_income18, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
    median_income18 <- mutate(median_income18, demo = rep(c("Total Population", "Black Population"),2))
    colnames(median_income18) <- c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income18 <- transform(median_income18, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income18) <- c("Median Income (US Dollars)", "Location", "Demographic")
    median_income18 <- mutate(median_income18, Year = "2018")
    ############################################################################2017
    va_yr <- read.csv("data/TableS1903FiveYearEstimates/va_income2017.csv")
    va_yr <- va_yr[2:6]
    race_names <- c("Total", "Black")
    #median income
    va_race_income_median <- data.frame(va_yr[c(81,83), 4])
    va_race_income <- data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/TableS1903FiveYearEstimates/hampton_income2017.csv")
    hamp_yr <- hamp_yr[2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[,2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(81,83))
    #This give us overall hampton overall and black median income
    variable <- sample(c("S1903_C03_001","S1903_C03_003"),32, replace = TRUE)
    hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2,4)]
    #having all the estimates in the same column
    median_income17 <- data.frame(median = c(median_income[,1], median_income[,2]))
    #labeling
    median_income17 <- mutate(median_income17, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
    median_income17 <- mutate(median_income17, demo = rep(c("Total Population", "Black Population"),2))
    colnames(median_income17) <- c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income17 <- transform(median_income17, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income17) <- c("Median Income (US Dollars)", "Location", "Demographic")
    median_income17 <- mutate(median_income17, Year = "2017")
    ###########################################################################2016
    va_yr <- read.csv("data/TableS1903FiveYearEstimates/va_income2016.csv")
    va_yr <- va_yr[,2:6]
    race_names <- c("Total", "Black")
    va_race_income_median <- data.frame(va_yr[c(31,33), 4])
    va_race_income <- data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/TableS1903FiveYearEstimates/hampton_income2016.csv")
    hamp_yr <- hamp_yr[,2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[,2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(31,33))
    variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
    hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2,4)]
    #having all the estimates in the same column
    median_income16 <- data.frame(median = c(median_income[,1], median_income[,2]))
    #labeling
    median_income16 <- mutate(median_income16, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
    median_income16 <- mutate(median_income16, demo = rep(c("Total Population", "Black Population"),2))
    colnames(median_income16) <- c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income16 <- transform(median_income16, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income16) <- c("Median Income (US Dollars)", "Location", "Demographic")
    median_income16 <- mutate(median_income16, Year = "2016")
    ###########################################################################2016
    va_yr <- read.csv("data/TableS1903FiveYearEstimates/va_income2015.csv")
    va_yr <- va_yr[,2:6]
    race_names <- c("Total", "Black")
    va_race_income_median <- data.frame(va_yr[c(31,33), 4])
    va_race_income <- data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/TableS1903FiveYearEstimates/hampton_income2015.csv")
    hamp_yr <- hamp_yr[,2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[,2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(31,33))
    variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
    hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2,4)]
    #having all the estimates in the same column
    median_income15 <- data.frame(median = c(median_income[,1], median_income[,2]))
    #labeling
    median_income15 <- mutate(median_income15, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
    median_income15 <- mutate(median_income15, demo = rep(c("Total Population", "Black Population"),2))
    colnames(median_income15) <- c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income15 <- transform(median_income15, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income15) <- c("Median Income (US Dollars)", "Location", "Demographic")
    median_income15 <- mutate(median_income15, Year = "2015")
    ###########################################################################2014
    va_yr <- read.csv("data/TableS1903FiveYearEstimates/va_income2014.csv")
    va_yr <- va_yr[,2:6]
    race_names <- c("Total", "Black")
    va_race_income_median <- data.frame(va_yr[c(31,33), 4])
    va_race_income <- data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/TableS1903FiveYearEstimates/hampton_income2014.csv")
    hamp_yr <- hamp_yr[,2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[,2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(31,33))
    variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
    hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2,4)]
    #having all the estimates in the same column
    median_income14 <- data.frame(median = c(median_income[,1], median_income[,2]))
    #labeling
    median_income14 <- mutate(median_income14, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
    median_income14 <- mutate(median_income14, demo = rep(c("Total Population", "Black Population"),2))
    colnames(median_income14) <- c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income14 <- transform(median_income14, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income14) <- c("Median Income (US Dollars)", "Location", "Demographic")
    median_income14 <- mutate(median_income14, Year = "2014")
    ###########################################################################2013
    va_yr <- read.csv("data/TableS1903FiveYearEstimates/va_income2013.csv")
    va_yr <- va_yr[,2:6]
    race_names <- c("Total", "Black")
    va_race_income_median <- data.frame(va_yr[c(31,33), 4])
    va_race_income <- data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/TableS1903FiveYearEstimates/hampton_income2013.csv")
    hamp_yr <- hamp_yr[,2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[,2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(31,33))
    variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
    hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2,4)]
    #having all the estimates in the same column
    median_income13 <- data.frame(median = c(median_income[,1], median_income[,2]))
    #labeling
    median_income13 <- mutate(median_income13, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
    median_income13 <- mutate(median_income13, demo = rep(c("Total Population", "Black Population"),2))
    colnames(median_income13) <- c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income13 <- transform(median_income13, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income13) <- c("Median Income (US Dollars)", "Location", "Demographic")
    median_income13 <- mutate(median_income13, Year = "2013")
    ############################################################################2012
    va_yr <- read.csv("data/TableS1903FiveYearEstimates/va_income2012.csv")
    va_yr <- va_yr[,2:6]
    race_names <- c("Total", "Black")
    va_race_income_median <- data.frame(va_yr[c(31,33), 4])
    va_race_income <- data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/TableS1903FiveYearEstimates/hampton_income2012.csv")
    hamp_yr <- hamp_yr[,2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[,2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(31,33))
    variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
    hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2,4)]
    #having all the estimates in the same column
    median_income12 <- data.frame(median = c(median_income[,1], median_income[,2]))
    #labeling
    median_income12 <- mutate(median_income12, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
    median_income12 <- mutate(median_income12, demo = rep(c("Total Population", "Black Population"),2))
    colnames(median_income12) <- c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income12 <- transform(median_income12, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income12) <- c("Median Income (US Dollars)", "Location", "Demographic")
    median_income12 <- mutate(median_income12, Year = "2012")
    ###########################################################################2011
    va_yr <- read.csv("data/TableS1903FiveYearEstimates/va_income2011.csv")
    va_yr <- va_yr[,2:6]
    race_names <- c("Total", "Black")
    va_race_income_median <- data.frame(va_yr[c(31,33), 4])
    va_race_income <- data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/TableS1903FiveYearEstimates/hampton_income2011.csv")
    hamp_yr <- hamp_yr[,2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[,2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(31,33))
    variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
    hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2,4)]
    #having all the estimates in the same column
    median_income11 <- data.frame(median = c(median_income[,1], median_income[,2]))
    #labeling
    median_income11 <- mutate(median_income11, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
    median_income11 <- mutate(median_income11, demo = rep(c("Total Population", "Black Population"),2))
    colnames(median_income11) <- c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income11 <- transform(median_income11, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income11) <- c("Median Income (US Dollars)", "Location", "Demographic")
    median_income11 <- mutate(median_income11, Year = "2011")
    ###########################################################################2010
    va_yr <- read.csv("data/TableS1903FiveYearEstimates/va_income2010.csv")
    va_yr <- va_yr[,2:6]
    race_names <- c("Total", "Black")
    va_race_income_median <- data.frame(va_yr[c(31,33), 4])
    va_race_income <- data.frame(cbind(race_names, va_race_income_median))
    colnames(va_race_income) <- c("Race", "Median Income")
    #Hampton Income
    hamp_yr <- read.csv("data/TableS1903FiveYearEstimates/hampton_income2010.csv")
    hamp_yr <- hamp_yr[,2:6]
    #getting the name, variable and estimate
    hamp_income2 <- hamp_yr[,2:4]
    hamp_income3 <- hamp_income2 %>%
      group_by(NAME) %>%
      slice(c(31,33))
    variable <- sample(c("S1903_C02_001","S1903_C02_003"),32, replace = TRUE)
    hamp_race_income_median <- hamp_income3 %>% group_by(variable) %>% summarize(median(estimate, na.rm = TRUE))
    #Va and Hampton Roads
    median_income <- cbind(va_race_income, hamp_race_income_median)
    median_income <- median_income[, c(2,4)]
    #having all the estimates in the same column
    median_income10 <- data.frame(median = c(median_income[,1], median_income[,2]))
    #labeling
    median_income10 <- mutate(median_income10, location = c(rep("Virginia",2), rep("Hampton Roads",2)))
    median_income10 <- mutate(median_income10, demo = rep(c("Total Population", "Black Population"),2))
    colnames(median_income10) <- c("Median Income (US Dollars)", "Location", "Demographic")
    #making them all numeric
    median_income10 <- transform(median_income10, `Median Income (US Dollars)` = as.numeric(`Median Income (US Dollars)`))
    colnames(median_income10) <- c("Median Income (US Dollars)", "Location", "Demographic")
    median_income10 <- mutate(median_income10, Year = "2010")
    ###############################################################################Combined
    income_years <- rbind(median_income19, median_income18, median_income17, median_income16,
                          median_income15, median_income14, median_income13, median_income12,
                          median_income11, median_income10)
    #VA line graph
    va_years <- income_years %>% filter(Location=="Virginia")
    #graph
    va_line <- ggplot(va_years, aes(x=Year, y=`Median Income (US Dollars)`, group = Demographic, color = Demographic)) + 
      geom_line(position = "identity", size =1.3) +
      theme_minimal() +
      ggtitle("Virginia") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank()) +
      theme(legend.title = element_blank()) +
      labs(y ="Median Income (US Dollars)") +
      scale_color_manual(values = c("#D55E00", "#0072B2")) +
      ylim(35000, 75000)
    
    #hamp line graph
    hamp_years <- income_years %>% filter(Location == "Hampton Roads")
    #graph
    hamp_line <- ggplot(hamp_years, aes(x=Year, y=`Median Income (US Dollars)`, group = Demographic, color = Demographic)) + 
      geom_line(position = "identity", size =1.3 ) +
      scale_color_manual(values = c("#D55E00", "#0072B2")) +
      theme_minimal() +
      ggtitle("Hampton Roads")+
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x=element_blank(),
            legend.title = element_blank())+
      labs(y ="Median Income (US Dollars)") +
      ylim(35000, 75000)
    
    medianTimeGraph  <- grid.arrange(hamp_line, va_line, ncol=2)
    medianTimeGraph 
  })
  # Employment By Sector ----------------------------------------------------
  var_sectorEmployment <- reactive({
    input$SectorEmploymentYearDrop
  })
  
  output$sector_plot <- renderPlotly({
    if(var_sectorEmployment() == "2019") {
      sectors2019 <- read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2019.csv")
      colnames(sectors2019) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
      sectors2019 <- sectors2019  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  + 
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "Hampton Roads") +  theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      sectors2019  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2019 , tooltip=c("x", "y", "Sector"))) %>% 
        layout(annotations = list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table DP03", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=15, color="black"))
        )
    }
    
    else if (var_sectorEmployment() == "2018") {
      sectors2018 <- read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2018.csv")
      colnames(sectors2018) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
      sectors2018 <- sectors2018  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  + 
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "Hampton Roads") +  theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      sectors2018  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2018 , tooltip=c("x", "y", "Sector"))) %>% 
        layout(annotations = list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table DP03", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=15, color="black"))
        )
    }
    
    else if (var_sectorEmployment() == "2017") {
      sectors2017 <- read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2017.csv")
      colnames(sectors2017) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
      sectors2017 <- sectors2017  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  + 
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "Hampton Roads") +  theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      sectors2017  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2017 , tooltip=c("x", "y", "Sector"))) %>% 
        layout(annotations = list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table DP03", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=15, color="black"))
        )
    }
    
    else if (var_sectorEmployment() == "2016") {
      sectors2016 <- read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2016.csv")
      colnames(sectors2016) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
      sectors2016 <- sectors2016  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  + 
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "Hampton Roads") +  theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      sectors2016  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2016, tooltip=c("x", "y", "Sector"))) %>% 
        layout(annotations = list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table DP03", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=15, color="black"))
        )
    }
    
    else if (var_sectorEmployment() == "2015") {
      sectors2015 <- read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2015.csv")
      colnames(sectors2015) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
      sectors2015 <- sectors2015  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  + 
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "Hampton Roads") +  theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      sectors2015  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2015, tooltip=c("x", "y", "Sector"))) %>% 
        layout(annotations = list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table DP03", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=15, color="black"))
        )
    }
    
    else if (var_sectorEmployment() == "2014") {
      sectors2014 <- read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2014.csv")
      colnames(sectors2014) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
      sectors2014 <- sectors2014  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  + 
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "Hampton Roads") +  theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      sectors2014  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2014, tooltip=c("x", "y", "Sector"))) %>% 
        layout(annotations = list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table DP03", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=15, color="black"))
        )
    }
    
    else if (var_sectorEmployment() == "2013") {
      sectors2013 <- read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2013.csv")
      colnames(sectors2013) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
      sectors2013 <- sectors2013  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  + 
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "Hampton Roads") +  theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      sectors2013  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2013, tooltip=c("x", "y", "Sector"))) %>% 
        layout(annotations = list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table DP03", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=15, color="black"))
        )
    }
    
    else if (var_sectorEmployment() == "2012") {
      sectors2012 <- read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2012.csv")
      colnames(sectors2012) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
      sectors2012 <- sectors2012  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  + 
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "Hampton Roads") +  theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      sectors2012  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2012, tooltip=c("x", "y", "Sector"))) %>% 
        layout(annotations = list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table DP03", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=15, color="black"))
        )
    }
    
    else if (var_sectorEmployment() == "2011") {
      sectors2011 <- read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2011.csv")
      colnames(sectors2011) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
      sectors2011 <- sectors2011  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  + 
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "Hampton Roads") +  theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      sectors2011  #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2011, tooltip=c("x", "y", "Sector"))) %>% 
        layout(annotations = list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table DP03", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=15, color="black"))
        )
    }
    
    else if (var_sectorEmployment() == "2010") {
      sectors2010 <- read.csv("data/TableDP03FiveYearEstimates/top2employmentSectors2010.csv")
      colnames(sectors2010) <- c("Name", "Variable", "Number of People Employed in Sector", "Sector")
      sectors2010 <- sectors2010  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Number of People Employed in Sector`, fill = Sector)) + geom_col()  + 
        theme_minimal() + labs(title = "",
                               y = "Total Number of People Employed",
                               x = "Hampton Roads") +  theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      sectors2010 #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(sectors2010, tooltip=c("x", "y", "Sector"))) %>% 
        layout(annotations = list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table DP03", 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=15, color="black"))
        )
    }
  })
  
  # Unemployment Rate -------------------------------------------------------
  
  
  var_unemploymentRate <- reactive({
    input$UnemploymentRateSlider
  })
  
  output$unemployment_plot <- renderPlotly({
    if(var_unemploymentRate() == "2019") {
      unemp_19 <- read.csv("data/TableS2301FiveYearEstimates/unemployment2019.csv") 
      va_unemp_19 <- read.csv("data/TableS2301FiveYearEstimates/vaunemployment2019.csv")
      unemployment_2019 <- unemp_19 %>% 
        mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
        mutate(NAME = str_remove(NAME, "city, Virginia")) %>%
        arrange(desc(NAME)) %>% 
        ggplot(aes(fill = variable, y = estimate, x = NAME)) +
        geom_bar(position = "dodge", stat = "identity", aes(text = paste0("</br> Locality: ", NAME,
                                                                          "</br> Percent Uninsured: ", estimate, "%",
                                                                          "</br> Population: ", variable))) +
        geom_hline(yintercept = va_unemp_19$estimate, linetype="dashed", color = "red", show.legend = TRUE) +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Unemployment Rate (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2301") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2019)
      
      
    }
    
    else if(var_unemploymentRate() == "2018") {
      unemp_18 <- read.csv("data/TableS2301FiveYearEstimates/unemployment2018.csv") 
      va_unemp_18 <- read.csv("data/TableS2301FiveYearEstimates/vaunemployment2018.csv")
      unemployment_2018 <- unemp_18 %>% 
        mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
        mutate(NAME = str_remove(NAME, "city, Virginia")) %>%
        arrange(desc(NAME)) %>% 
        ggplot(aes(fill = variable, y = estimate, x = NAME)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(yintercept = va_unemp_18$estimate, linetype="dashed", color = "red", show.legend = TRUE) +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Unemployment Rate (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2301") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2018)
      
      
    }
    
    else if(var_unemploymentRate() == "2017") {
      unemp_17 <- read.csv("data/TableS2301FiveYearEstimates/unemployment2017.csv") 
      va_unemp_17 <- read.csv("data/TableS2301FiveYearEstimates/vaunemployment2017.csv")
      unemployment_2017 <- unemp_17 %>% 
        mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
        mutate(NAME = str_remove(NAME, "city, Virginia")) %>%
        arrange(desc(NAME)) %>% 
        ggplot(aes(fill = variable, y = estimate, x = NAME)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(yintercept = va_unemp_17$estimate, linetype="dashed", color = "red", show.legend = TRUE) +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Unemployment Rate (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2301") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2017)
      
      
    }
    
    else if(var_unemploymentRate() == "2016") {
      unemp_16 <- read.csv("data/TableS2301FiveYearEstimates/unemployment2016.csv") 
      va_unemp_16 <- read.csv("data/TableS2301FiveYearEstimates/vaunemployment2016.csv")
      unemployment_2016 <- unemp_16 %>% 
        mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
        mutate(NAME = str_remove(NAME, "city, Virginia")) %>%
        arrange(desc(NAME)) %>% 
        ggplot(aes(fill = variable, y = estimate, x = NAME)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(yintercept = va_unemp_16$estimate, linetype="dashed", color = "red", show.legend = TRUE) +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Unemployment Rate (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2301") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2016)
      
      
    }
    
    else if(var_unemploymentRate() == "2015") {
      unemp_15 <- read.csv("data/TableS2301FiveYearEstimates/unemployment2015.csv") 
      va_unemp_15 <- read.csv("data/TableS2301FiveYearEstimates/vaunemployment2015.csv")
      unemployment_2015 <- unemp_15 %>% 
        mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
        mutate(NAME = str_remove(NAME, "city, Virginia")) %>%
        arrange(desc(NAME)) %>% 
        ggplot(aes(fill = variable, y = estimate, x = NAME)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(yintercept = va_unemp_15$estimate, linetype="dashed", color = "red", show.legend = TRUE) +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Unemployment Rate (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2301") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2015)
      
      
    }
    
    else if(var_unemploymentRate() == "2014") {
      unemp_14 <- read.csv("data/TableS2301FiveYearEstimates/unemployment2014.csv") 
      va_unemp_14 <- read.csv("data/TableS2301FiveYearEstimates/vaunemployment2014.csv")
      unemployment_2014 <- unemp_14 %>% 
        mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
        mutate(NAME = str_remove(NAME, "city, Virginia")) %>%
        arrange(desc(NAME)) %>% 
        ggplot(aes(fill = variable, y = estimate, x = NAME)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(yintercept = va_unemp_14$estimate, linetype="dashed", color = "red", show.legend = TRUE) +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Unemployment Rate (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2301") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2014)
      
      
    }
    
    else if(var_unemploymentRate() == "2013") {
      unemp_13 <- read.csv("data/TableS2301FiveYearEstimates/unemployment2013.csv") 
      va_unemp_13 <- read.csv("data/TableS2301FiveYearEstimates/vaunemployment2013.csv")
      unemployment_2013 <- unemp_13 %>% 
        mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
        mutate(NAME = str_remove(NAME, "city, Virginia")) %>%
        arrange(desc(NAME)) %>% 
        ggplot(aes(fill = variable, y = estimate, x = NAME)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(yintercept = va_unemp_13$estimate, linetype="dashed", color = "red", show.legend = TRUE) +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Unemployment Rate (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2301") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2013)
      
      
    }
    
    else if(var_unemploymentRate() == "2012") {
      unemp_12 <- read.csv("data/TableS2301FiveYearEstimates/unemployment2012.csv") 
      va_unemp_12 <- read.csv("data/TableS2301FiveYearEstimates/vaunemployment2012.csv")
      unemployment_2012 <- unemp_12 %>% 
        mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
        mutate(NAME = str_remove(NAME, "city, Virginia")) %>%
        arrange(desc(NAME)) %>% 
        ggplot(aes(fill = variable, y = estimate, x = NAME)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(yintercept = va_unemp_12$estimate, linetype="dashed", color = "red", show.legend = TRUE) +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Unemployment Rate (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2301") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2012)
      
      
    }
    
    else if(var_unemploymentRate() == "2011") {
      unemp_11 <- read.csv("data/TableS2301FiveYearEstimates/unemployment2011.csv") 
      va_unemp_11 <- read.csv("data/TableS2301FiveYearEstimates/vaunemployment2011.csv")
      unemployment_2011 <- unemp_11 %>% 
        mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
        mutate(NAME = str_remove(NAME, "city, Virginia")) %>%
        arrange(desc(NAME)) %>% 
        ggplot(aes(fill = variable, y = estimate, x = NAME)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(yintercept = va_unemp_11$estimate, linetype="dashed", color = "red", show.legend = TRUE) +
      theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Unemployment Rate (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2301") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2011)
      
      
    }
    
    else if(var_unemploymentRate() == "2010") {
      unemp_10 <- read.csv("data/TableS2301FiveYearEstimates/unemployment2010.csv") 
      va_unemp_10 <- read.csv("data/TableS2301FiveYearEstimates/vaunemployment2010.csv")
      unemployment_2010 <- unemp_10 %>% 
        mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
        mutate(NAME = str_remove(NAME, "city, Virginia")) %>%
        arrange(desc(NAME)) %>% 
        ggplot(aes(fill = variable, y = estimate, x = NAME)) +
        geom_bar(position = "dodge", stat = "identity") +
        geom_hline(yintercept = va_unemp_10$estimate, linetype="dashed", color = "red", show.legend = TRUE) +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Unemployment Rate (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2301") +
        theme(axis.text.x = element_text(angle = 40)) +
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(unemployment_2010)
      
      
    }
  })
  
  # Poverty Rates in VA and Hampton Roads-------------------------------------
  var_poverty <- reactive ({
    input$PovertyYearDrop
  })
  
  output$pov_plot <- renderPlot({
    if(var_poverty() %in% c("2019", "2018", "2017", "2016", "2015")){
      if(var_poverty() == "2019") { 
        va_pov <- read.csv("data/TableS1701FiveYearEstimates/va_poverty2019.csv")
        hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2019.csv")
      }
      else if(var_poverty() == "2018") { 
        va_pov <- read.csv("data/TableS1701FiveYearEstimates/va_poverty2018.csv")
        hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2018.csv")
      }
      else if(var_poverty() == "2017") { 
        va_pov <- read.csv("data/TableS1701FiveYearEstimates/va_poverty2017.csv")
        hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2017.csv")
      }
      else if(var_poverty() == "2016") { 
        va_pov <- read.csv("data/TableS1701FiveYearEstimates/va_poverty2016.csv")
        hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2016.csv")
      }
      else if(var_poverty() == "2015") { 
        va_pov <- read.csv("data/TableS1701FiveYearEstimates/va_poverty2015.csv")
        hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2015.csv")
      }
      va_pov <- va_pov[,2:6]
      va_pct_pov <- va_pov[123,4]
      va_pct_pov_blck <- va_pov[136,4]
      va_pov_vector <- rbind(va_pct_pov, va_pct_pov_blck)
      hamp_pov <- hamp_pov[,2:6]
      #General
      hamp_total <- hamp_pov %>%
        group_by(NAME) %>%
        slice(c(1))
      hamp_total2 <- colSums(hamp_total[,4])
      hamp_pov2 <- hamp_pov %>%
        group_by(NAME) %>%
        slice(c(62))
      hamp_pov3 <- colSums(hamp_pov2[,4])
      hamp_overall_pov <- hamp_pov3/hamp_total2 *100
      #Black
      hamp_total_blck <- hamp_pov %>%
        group_by(NAME) %>%
        slice(14)
      hamp_total_blck2 <- colSums(hamp_total_blck[,4]) 
      hamp_pov_blck <- hamp_pov %>%
        group_by(NAME) %>%
        slice(75)
      hamp_pov_blck2 <- colSums(hamp_pov_blck[,4])
      hamp_blck_overall_pov <- hamp_pov_blck2/hamp_total_blck2*100
      hamp_pov_vector <- rbind(hamp_overall_pov, hamp_blck_overall_pov)
      #combing hampton and VA
      pov_pct <- data.frame(va_pov_vector, hamp_pov_vector)
      pov_pct2 <- data.frame(Ratio = unlist(pov_pct, use.names=FALSE))
      pov_pct2 <- mutate(pov_pct2, Location = c("Virginia", "Virginia", "Hampton Roads", "Hampton Roads"))
      pov_pct2 <- mutate(pov_pct2, Demographic = c("Total Population", "Black Population", "Total Population",
                                                   "Black Population"))
      colnames(pov_pct2) <- c("Percentage (%)", "Location", "Demographic")
      pov_pct2$Location <- factor(pov_pct2$Location, levels = c("Hampton Roads", "Virginia"))
      #Graph for just hampton roads and VA
      pov_plot <- ggplot(pov_pct2, aes(x=Location, y=`Percentage (%)`, fill=Demographic)) +
        geom_bar(stat="identity", position=position_dodge())+
        geom_text(aes(label=paste0(round(`Percentage (%)`, digits=2), "%")), vjust=1.5, color="white",
                  position = position_dodge(0.9), size=5)+
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'),
              legend.title = element_blank(),
              legend.key.height = unit(1, 'cm'), 
              legend.key.width = unit(1, 'cm'),
              legend.text = element_text(size=14),
              axis.text=element_text(size=15),
              axis.title=element_text(size=17),
              axis.title.x=element_blank(),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
        scale_fill_manual(values=c("#D55E00","#0072B2")) 
      #plot
      pov_plot
    }
    #when table changes
    else if (var_poverty() %in% c("2014", "2013", "2012")){
      if(var_poverty() == "2014") { 
        va_pov <- read.csv("data/TableS1701FiveYearEstimates/va_poverty2014.csv")
        hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2014.csv")
      }
      else if(var_poverty() == "2013") { 
        va_pov <- read.csv("data/TableS1701FiveYearEstimates/va_poverty2013.csv")
        hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2013.csv")
      }
      else if(var_poverty() == "2012") { 
        va_pov <- read.csv("data/TableS1701FiveYearEstimates/va_poverty2012.csv")
        hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2012.csv")
      }
      va_pov <- va_pov[,2:6]
      #General
      va_pct_pov <- va_pov[93,4]
      #Black
      va_pct_pov_blck <- va_pov[102,4]
      va_pov_vector <- rbind(va_pct_pov, va_pct_pov_blck)
      #Hampton Roads
      hamp_pov <- hamp_pov[,2:6]
      #General
      hamp_total <- hamp_pov %>%
        group_by(NAME) %>%
        slice(c(1))
      hamp_total2 <- colSums(hamp_total[,4])
      hamp_pov2 <- hamp_pov %>%
        group_by(NAME) %>%
        slice(c(47))
      hamp_pov3 <- colSums(hamp_pov2[,4])
      hamp_overall_pov <- hamp_pov3/hamp_total2 *100
      #Black
      hamp_total_blck <- hamp_pov %>%
        group_by(NAME) %>%
        slice(10)
      hamp_total_blck2 <- colSums(hamp_total_blck[,4]) 
      hamp_pov_blck <- hamp_pov %>%
        group_by(NAME) %>%
        slice(56)
      hamp_pov_blck2 <- colSums(hamp_pov_blck[,4])
      hamp_blck_overall_pov <- hamp_pov_blck2/hamp_total_blck2*100
      hamp_pov_vector <- rbind(hamp_overall_pov, hamp_blck_overall_pov)
      #combing hampton and VA
      pov_pct <- data.frame(va_pov_vector, hamp_pov_vector)
      pov_pct2 <- data.frame(Ratio = unlist(pov_pct, use.names=FALSE))
      pov_pct2 <- mutate(pov_pct2, Location = c(rep("Virginia",2), rep("Hampton Roads",2)))
      pov_pct2 <- mutate(pov_pct2, Demographic = rep(c("Total Population", "Black Population"),2))
      colnames(pov_pct2) <- c("Percentage (%)", "Location", "Demographic")
      pov_pct2$Location <- factor(pov_pct2$Location, levels = c("Hampton Roads", "Virginia"))
      #Graph for just hampton roads and VA
      pov_plot <- ggplot(pov_pct2, aes(x=Location, y=`Percentage (%)`, fill=Demographic)) +
        geom_bar(stat="identity", position=position_dodge())+
        geom_text(aes(label=paste0(round(`Percentage (%)`, digits=2), "%")), vjust=1.5, color="white",
                  position = position_dodge(0.9), size=5)+
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'),
              legend.title = element_blank(),
              legend.key.height = unit(1, 'cm'), 
              legend.key.width = unit(1, 'cm'),
              legend.text = element_text(size=14),
              axis.text=element_text(size=15),
              axis.title=element_text(size=17),
              axis.title.x=element_blank(),
              axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
        scale_fill_manual(values=c("#D55E00","#0072B2"))
      #plot
      pov_plot
    }
    
    
  })
  
  # Hampton counties poverty -------------------------------------------------------------
  var_povertyCount <- reactive ({
    input$PovertyCountYearDrop
  })
  
  output$counties_pov <- renderPlotly({
    if( var_povertyCount() %in% c("2019", "2018", "2017", "2016", "2015")){
      if( var_povertyCount() == "2019") { 
        hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2019.csv")
      }
      else if( var_povertyCount() == "2018") { 
        hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2018.csv")
      }
      else if( var_povertyCount() == "2017") { 
        hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2017.csv")
      }
      else if( var_povertyCount() == "2016") { 
        hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2016.csv")
      }
      else if( var_povertyCount() == "2015") { 
        hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2015.csv")
      }
      hamp_pov <- hamp_pov[,2:6]
      hamp_pov_tbl <- hamp_pov %>%
        group_by(NAME) %>%
        slice(123)
      hamp_pov_blck_tbl <- hamp_pov %>%
        group_by(NAME) %>%
        slice(136)
      hamp_pov_tbl %>% ungroup()
      hamp_pov_blck_tbl %>% ungroup()
      hamp_pctG <- hamp_pov_tbl[,4]
      hamp_pctB <- hamp_pov_blck_tbl[,4] 
      hamp_comb <- rbind(hamp_pctG, hamp_pctB)
      colnames(hamp_comb) <- "Ratio"
      hamp_comb <- mutate(hamp_comb, Location =  rep(c("Chesapeake", "Franklin City", "Gloucester", "Hampton", "Isle of Wight", "James City",
                                                       "Mathews", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton",
                                                       "Suffolk", "Virginia Beach", "Williamsburg", "York"),2))
      hamp_comb <- mutate(hamp_comb, Demographic = c(rep("Total Population", 16),
                                                     rep("Black Population",16)))
      colnames(hamp_comb) <- c("Percentage (%)", "Location", "Demographic")
      hamp_comb <- hamp_comb %>% filter(hamp_comb[,2] != "Poquoson")
      #Graph 
      counties_pov <-  ggplot(hamp_comb, aes(Location, y=`Percentage (%)`, fill=Demographic)) +
        geom_bar(stat="identity", position=position_dodge())+
        #geom_text(aes(label=paste0(round(`Percentage (%)`, digits=2), "%")), vjust=1.5, color="white",
                  #position = position_dodge(0.9), size=3)+
        theme_minimal() +
        scale_fill_manual(values=c("#D55E00","#0072B2")) +
        theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'), 
              #legend.key.height = unit(0.5, 'cm'), 
              #legend.key.width = unit(0.5, 'cm'), 
              legend.title = element_blank(),
              #legend.text = element_text(size=14),
              #axis.text=element_text(size=15),
              #axis.text.x = element_text(size=8, face="bold"),
              #axis.title=element_text(size=17),
              axis.title.x=element_blank()) +
        theme(axis.text.x = element_text(angle=40, vjust=0.95, hjust=1)) +
        labs(caption = "Source: ACS 5 Year Estimate Table S1701")
      #plot
      counties_pov <- ggplotly(counties_pov)
    }
    #when table changes
    else if (var_povertyCount() %in% c("2014", "2013", "2012")){
      if( var_povertyCount() == "2014") { 
        hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2014.csv")
      }
      else if( var_povertyCount() == "2013") { 
        hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2013.csv")
      }
      else if( var_povertyCount() == "2012") { 
        hamp_pov <- read.csv("data/TableS1701FiveYearEstimates/hamp_poverty2012.csv")
      }
      hamp_pov <- hamp_pov[,2:6]
      hamp_pov_tbl <- hamp_pov %>%
        group_by(NAME) %>%
        slice(93)
      hamp_pov_blck_tbl <- hamp_pov %>%
        group_by(NAME) %>%
        slice(102)
      hamp_pov_tbl %>% ungroup()
      hamp_pov_blck_tbl %>% ungroup()
      hamp_pctG <- hamp_pov_tbl[,4]
      hamp_pctB <- hamp_pov_blck_tbl[,4] 
      hamp_comb <- rbind(hamp_pctG, hamp_pctB)
      colnames(hamp_comb) <- "Ratio"
      hamp_comb <- mutate(hamp_comb, Location = c(rep(c("Chesapeake", "Franklin City", "Gloucester", "Hampton", "Isle of Wight", "James City",
                                                        "Mathews", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton",
                                                        "Suffolk", "Virginia Beach", "Williamsburg", "York"),2)))
      hamp_comb <- mutate(hamp_comb, Demographic = c(rep("Total Population", 16),
                                                     rep("Black Population",16)))
      colnames(hamp_comb) <- c("Percentage (%)", "Location", "Demographic")
      hamp_comb <- hamp_comb %>% filter(hamp_comb[,2] != "Poquoson")
      #Graph 
      
      counties_pov <-  ggplot(hamp_comb, aes(Location, y=`Percentage (%)`, fill=Demographic)) +
        geom_bar(stat="identity", position=position_dodge())+
        #geom_text(aes(label=paste0(round(`Percentage (%)`, digits=2), "%")), vjust=1.5, color="white",
                  #position = position_dodge(0.9), size=3)+
        theme_minimal() +
        scale_fill_manual(values=c("#D55E00","#0072B2")) +
        theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'), 
              legend.title = element_blank(),
              #legend.text = element_text(size=14),
             # axis.text=element_text(size=15),
              #axis.text.x = element_text(size=10, face="bold"),
              #axis.title=element_text(size=17),
              axis.title.x=element_blank())+
        theme(axis.text.x = element_text(angle=40, vjust=0.95, hjust=1)) +
        labs(caption = "Source: ACS 5 Year Estimate Table S1701")
      
      #plot
      counties_pov <- ggplotly(counties_pov)
    }
    
    
  })
  
  
  
  # Uninsured Population ----------------------------------------------------
  
  var_uninsuredpct <- reactive({
    input$UninsuredPctSlider
  })
  
  output$uninsured_plot <- renderPlotly({
    if(var_uninsuredpct() == "2019") {
      unins_19 <- read.csv("data/TableS2701FiveYearEstimates/uninsured2019.csv")
      uninsured_2019 <- unins_19 %>% 
        mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
        mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
        ggplot(aes(fill = variable, y = estimate, x = NAME)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent Uninsured (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2701") +
        theme(axis.text.x = element_text(angle = 40)) + 
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2019)
      
    }
    
    else if(var_uninsuredpct() == "2018") {
      unins_18 <- read.csv("data/TableS2701FiveYearEstimates/uninsured2018.csv")
      uninsured_2018 <- unins_18 %>% 
        mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
        mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
        ggplot(aes(fill = variable, y = estimate, x = NAME)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent Uninsured (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2701") +
        theme(axis.text.x = element_text(angle = 40)) + 
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2018)
    }
    
    else if(var_uninsuredpct() == "2017") {
      unins_17 <- read.csv("data/TableS2701FiveYearEstimates/uninsured2017.csv")
      uninsured_2017 <- unins_17 %>% 
        mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
        mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
        ggplot(aes(fill = variable, y = estimate, x = NAME)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent Uninsured (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2701") +
        theme(axis.text.x = element_text(angle = 40)) + 
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2017)
    }
    
    else if(var_uninsuredpct() == "2016") {
      unins_16 <- read.csv("data/TableS2701FiveYearEstimates/uninsured2016.csv")
      uninsured_2016 <- unins_16 %>% 
        mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
        mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
        ggplot(aes(fill = variable, y = estimate, x = NAME)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent Uninsured (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2701") +
        theme(axis.text.x = element_text(angle = 40)) + 
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2016)
    }
    
    else if(var_uninsuredpct() == "2015") {
      unins_15 <- read.csv("data/TableS2701FiveYearEstimates/uninsured2015.csv")
      uninsured_2015 <- unins_15 %>% 
        mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
        mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
        ggplot(aes(fill = variable, y = estimate, x = NAME)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent Uninsured (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2701") +
        theme(axis.text.x = element_text(angle = 40)) + 
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2015)
    }
    
    else if(var_uninsuredpct() == "2014") {
      unins_14 <- read.csv("data/TableS2701FiveYearEstimates/uninsured2014.csv")
      uninsured_2014 <- unins_14 %>% 
        mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
        mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
        ggplot(aes(fill = variable, y = estimate, x = NAME)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent Uninsured (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2701") +
        theme(axis.text.x = element_text(angle = 40)) + 
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2014)
    }
    
    else if(var_uninsuredpct() == "2013") {
      unins_13 <- read.csv("data/TableS2701FiveYearEstimates/uninsured2013.csv")
      uninsured_2013 <- unins_13 %>% 
        mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
        mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
        ggplot(aes(fill = variable, y = estimate, x = NAME)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent Uninsured (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2701") +
        theme(axis.text.x = element_text(angle = 40)) + 
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2013)
    }
    
    else if(var_uninsuredpct() == "2012") {
      unins_12 <- read.csv("data/TableS2701FiveYearEstimates/uninsured2012.csv")
      uninsured_2012 <- unins_12 %>% 
        mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
        mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
        ggplot(aes(fill = variable, y = estimate, x = NAME)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent Uninsured (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2701") +
        theme(axis.text.x = element_text(angle = 40)) + 
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2012)
    }
    
    else if(var_uninsuredpct() == "2011") {
      unins_11 <- read.csv("data/TableS2701FiveYearEstimates/uninsured2011.csv")
      uninsured_2011 <- unins_11 %>% 
        mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
        mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
        ggplot(aes(fill = variable, y = estimate, x = NAME)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent Uninsured (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2701") +
        theme(axis.text.x = element_text(angle = 40)) + 
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2011)
    }
    
    else if(var_uninsuredpct() == "2010") {
      unins_10 <- read.csv("data/TableS2701FiveYearEstimates/uninsured2010.csv")
      uninsured_2010 <- unins_10 %>% 
        mutate(NAME = str_remove(NAME, "County, Virginia")) %>% 
        mutate(NAME = str_remove(NAME, "city, Virginia")) %>% 
        ggplot(aes(fill = variable, y = estimate, x = NAME)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal() +
        theme(legend.title = element_blank()) +
        labs(title = "",
             y = "Percent Uninsured (%)",
             x = "",
             caption = "Source: ACS 5 Year Estimate Table S2701") +
        theme(axis.text.x = element_text(angle = 40)) + 
        scale_fill_manual(values = c("#D55E00", "#0072B2"))
      
      ggplotly(uninsured_2010)
    }
  })
  
  
  # Veteran Status ----------------------------------------------------------
  var_veteran <- reactive({
    input$VeteranSlider
  })
  
  output$veteran_map <- renderLeaflet({
    if(var_veteran() == "2019") {
      vet_19 <- read_rds("data/TableS2101FiveYearEstimates/bveteran2019.rds")
      military_bases <- read_rds("data/TableS2101FiveYearEstimates/militarybases.rds")
      pal <- colorNumeric(palette = "viridis", domain = vet_19$Percent, reverse = TRUE)
      veteran_19 <- vet_19 %>% 
        leaflet(options = leafletOptions(minZoom = 8)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Veterans: ", Percent, "%"), group = "Veteran Status") %>% 
        addMarkers(data = military_bases, popup = ~paste0("Base: ", base_name, " Branch: ", branch), group = "Military Bases") %>% 
        addLayersControl(
          baseGroups = c("Veteran Status"),
          overlayGroups = c("Military Bases"),
          options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup("Military Bases") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Black Veterans",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_veteran() == "2018") {
      vet_18 <- read_rds("data/TableS2101FiveYearEstimates/bveteran2018.rds")
      pal <- colorNumeric(palette = "viridis", domain = vet_18$Percent, reverse = TRUE)
      veteran_18 <- vet_18 %>% 
        leaflet(options = leafletOptions(minZoom = 8)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Veterans: ", Percent, "%"), group = "Veteran Status") %>% 
        addMarkers(data = military_bases, popup = ~paste0("Base: ", base_name, " Branch: ", branch), group = "Military Bases") %>% 
        addLayersControl(
          baseGroups = c("Veteran Status"),
          overlayGroups = c("Military Bases"),
          options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup("Military Bases") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Black Veterans",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_veteran() == "2017") {
      vet_17 <- read_rds("data/TableS2101FiveYearEstimates/bveteran2017.rds")
      pal <- colorNumeric(palette = "viridis", domain = vet_17$Percent, reverse = TRUE)
      veteran_17 <- vet_17 %>% 
        leaflet(options = leafletOptions(minZoom = 8)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Veterans: ", Percent, "%"), group = "Veteran Status") %>% 
        addMarkers(data = military_bases, popup = ~paste0("Base: ", base_name, " Branch: ", branch), group = "Military Bases") %>% 
        addLayersControl(
          baseGroups = c("Veteran Status"),
          overlayGroups = c("Military Bases"),
          options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup("Military Bases") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Black Veterans",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_veteran() == "2016") {
      vet_16 <- read_rds("data/TableS2101FiveYearEstimates/bveteran2016.rds")
      pal <- colorNumeric(palette = "viridis",domain = vet_16$Percent, reverse = TRUE)
      veteran_16 <- vet_16 %>% 
        leaflet(options = leafletOptions(minZoom = 8)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Veterans: ", Percent, "%"), group = "Veteran Status") %>% 
        addMarkers(data = military_bases, popup = ~paste0("Base: ", base_name, " Branch: ", branch), group = "Military Bases") %>% 
        addLayersControl(
          baseGroups = c("Veteran Status"),
          overlayGroups = c("Military Bases"),
          options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup("Military Bases") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Black Veterans",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_veteran() == "2015") {
      vet_15 <- read_rds("data/TableS2101FiveYearEstimates/bveteran2015.rds")
      pal <- colorNumeric(palette = "viridis", domain = vet_15$Percent, reverse = TRUE)
      veteran_15 <- vet_15 %>% 
        leaflet(options = leafletOptions(minZoom = 8)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Veterans: ", Percent, "%"), group = "Veteran Status") %>% 
        addMarkers(data = military_bases, popup = ~paste0("Base: ", base_name, " Branch: ", branch), group = "Military Bases") %>% 
        addLayersControl(
          baseGroups = c("Veteran Status"),
          overlayGroups = c("Military Bases"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup("Military Bases") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Black Veterans",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_veteran() == "2014") {
      vet_14 <- read_rds("data/TableS2101FiveYearEstimates/bveteran2014.rds")
      pal <- colorNumeric(palette = "viridis", domain = vet_14$Percent, reverse = TRUE)
      veteran_14 <- vet_14 %>% 
        leaflet(options = leafletOptions(minZoom = 8)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Veterans: ", Percent, "%"), group = "Veteran Status") %>% 
        addMarkers(data = military_bases, popup = ~paste0("Base: ", base_name, " Branch: ", branch), group = "Military Bases") %>% 
        addLayersControl(
          baseGroups = c("Veteran Status"),
          overlayGroups = c("Military Bases"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup("Military Bases") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Black Veterans",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_veteran() == "2013") {
      vet_13 <- read_rds("data/TableS2101FiveYearEstimates/bveteran2013.rds")
      pal <- colorNumeric(palette = "viridis", domain = vet_13$Percent, reverse = TRUE)
      veteran_13 <- vet_13 %>% 
        leaflet(options = leafletOptions(minZoom = 8)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Veterans: ", Percent, "%"), group = "Veteran Status") %>% 
        addMarkers(data = military_bases, popup = ~paste0("Base: ", base_name, " Branch: ", branch), group = "Military Bases") %>% 
        addLayersControl(
          baseGroups = c("Veteran Status"),
          overlayGroups = c("Military Bases"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup("Military Bases") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Black Veterans",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_veteran() == "2012") {
      vet_12 <- read_rds("data/TableS2101FiveYearEstimates/bveteran2012.rds")
      pal <- colorNumeric(palette = "viridis", domain = vet_12$Percent, reverse = TRUE)
      veteran_12 <- vet_12 %>% 
        leaflet(options = leafletOptions(minZoom = 8)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Veterans: ", Percent, "%"), group = "Veteran Status") %>% 
        addMarkers(data = military_bases, popup = ~paste0("Base: ", base_name, " Branch: ", branch), group = "Military Bases") %>% 
        addLayersControl(
          baseGroups = c("Veteran Status"),
          overlayGroups = c("Military Bases"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup("Military Bases") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Black Veterans",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_veteran() == "2011") {
      vet_11 <- read_rds("data/TableS2101FiveYearEstimates/bveteran2011.rds")
      pal <- colorNumeric(palette = "viridis", domain = vet_11$Percent, reverse = TRUE)
      veteran_11 <- vet_11 %>% 
        leaflet(options = leafletOptions(minZoom = 8)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Veterans: ", Percent, "%"), group = "Veteran Status") %>% 
        addMarkers(data = military_bases, popup = ~paste0("Base: ", base_name, " Branch: ", branch), group = "Military Bases") %>% 
        addLayersControl(
          baseGroups = c("Veteran Status"),
          overlayGroups = c("Military Bases"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup("Military Bases") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Black Veterans",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_veteran() == "2010") {
      vet_10 <- read_rds("data/TableS2101FiveYearEstimates/bveteran2010.rds")
      pal <- colorNumeric(palette = "viridis", domain = vet_10$Percent, reverse = TRUE)
      veteran_10 <- vet_10 %>% 
        leaflet(options = leafletOptions(minZoom = 8)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Veterans: ", Percent, "%"), group = "Veteran Status") %>% 
        addMarkers(data = military_bases, popup = ~paste0("Base: ", base_name, " Branch: ", branch), group = "Military Bases") %>% 
        addLayersControl(
          baseGroups = c("Veteran Status"),
          overlayGroups = c("Military Bases"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup("Military Bases") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Black Veterans",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
  })
  
  
  # Home Ownership Map -------------------------------------------------------
  var_hmown <- reactive({
    input$HomeOwnSlider
  })
  
  
  output$homeownership_map <- renderLeaflet({
    if(var_hmown() == "2019") {
      b_hm_19 <- read_rds("data/TableS2502FiveYearEstimates/bhmown2019.rds")
      tot_hm_19 <- read_rds("data/TableS2502FiveYearEstimates/tothmown2019.rds")
      pal <- colorNumeric(palette = "viridis", domain = b_hm_19$Percent, reverse = TRUE)
      b_hmown_leaf_19 <- b_hm_19 %>%
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(data = b_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Homeowners: ", Percent, "%"), group = "Black Home Owners") %>% 
        addPolygons(data = tot_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Total Homeowners: ", Percent, "%"), group = "Total Home Owners") %>% 
        addLayersControl(
          baseGroups = c("Total Home Owners"),
          overlayGroups = c("Black Home Owners"),
          options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup("Black Home Owners") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Home Owners",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_hmown() == "2018") {
      b_hm_18 <- read_rds("data/TableS2502FiveYearEstimates/bhmown2018.rds")
      tot_hm_18 <- read_rds("data/TableS2502FiveYearEstimates/tothmown2018.rds")
      pal <- colorNumeric(palette = "viridis", domain = b_hm_18$Percent, reverse = TRUE)
      b_hmown_leaf_18 <- b_hm_18 %>%
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(data = b_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Homeowners: ", Percent, "%"), group = "Black Home Owners") %>% 
        addPolygons(data = tot_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Total Homeowners: ", Percent, "%"), group = "Total Home Owners") %>% 
        addLayersControl(
          baseGroups = c("Total Home Owners"),
          overlayGroups = c("Black Home Owners"),
          options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup("Black Home Owners") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Home Owners",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_hmown() == "2017") {
      b_hm_17 <- read_rds("data/TableS2502FiveYearEstimates/bhmown2017.rds")
      tot_hm_17 <- read_rds("data/TableS2502FiveYearEstimates/tothmown2017.rds")
      pal <- colorNumeric(palette = "viridis", domain = b_hm_17$Percent, reverse = TRUE)
      b_hmown_leaf_17 <- b_hm_17 %>%
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(data = b_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Homeowners: ", Percent, "%"), group = "Black Home Owners") %>% 
        addPolygons(data = tot_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Total Homeowners: ", Percent, "%"), group = "Total Home Owners") %>% 
        addLayersControl(
          baseGroups = c("Total Home Owners"),
          overlayGroups = c("Black Home Owners"),
          options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup("Black Home Owners") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Home Owners",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_hmown() == "2016") {
      b_hm_16 <- read_rds("data/TableS2502FiveYearEstimates/bhmown2016.rds")
      tot_hm_16 <- read_rds("data/TableS2502FiveYearEstimates/tothmown2016.rds")
      pal <- colorNumeric(palette = "viridis", domain = b_hm_16$Percent, reverse = TRUE)
      b_hmown_leaf_16 <- b_hm_16 %>%
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(data = b_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Homeowners: ", Percent, "%"), group = "Black Home Owners") %>% 
        addPolygons(data = tot_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Total Homeowners: ", Percent, "%"), group = "Total Home Owners") %>% 
        addLayersControl(
          baseGroups = c("Total Home Owners"),
          overlayGroups = c("Black Home Owners"),
          options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup("Black Home Owners") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Home Owners",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_hmown() == "2015") {
      b_hm_15 <- read_rds("data/TableS2502FiveYearEstimates/bhmown2015.rds")
      tot_hm_15 <- read_rds("data/TableS2502FiveYearEstimates/tothmown2015.rds")
      pal <- colorNumeric(palette = "viridis", domain = b_hm_15$Percent, reverse = TRUE)
      b_hmown_leaf_15 <- b_hm_15 %>%
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(data = b_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Homeowners: ", Percent, "%"), group = "Black Home Owners") %>% 
        addPolygons(data = tot_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Total Homeowners: ", Percent, "%"), group = "Total Home Owners") %>% 
        addLayersControl(
          baseGroups = c("Total Home Owners"),
          overlayGroups = c("Black Home Owners"),
          options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup("Black Home Owners") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Home Owners",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_hmown() == "2014") {
      b_hm_14 <- read_rds("data/TableS2502FiveYearEstimates/bhmown2014.rds")
      tot_hm_14 <- read_rds("data/TableS2502FiveYearEstimates/tothmown2014.rds")
      pal <- colorNumeric(palette = "viridis", domain = b_hm_14$Percent, reverse = TRUE)
      b_hmown_leaf_14 <- b_hm_14 %>%
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(data = b_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Homeowners: ", Percent, "%"), group = "Black Home Owners") %>% 
        addPolygons(data = tot_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Total Homeowners: ", Percent, "%"), group = "Total Home Owners") %>% 
        addLayersControl(
          baseGroups = c("Total Home Owners"),
          overlayGroups = c("Black Home Owners"),
          options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup("Black Home Owners") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Home Owners",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_hmown() == "2013") {
      b_hm_13 <- read_rds("data/TableS2502FiveYearEstimates/bhmown2013.rds")
      tot_hm_13 <- read_rds("data/TableS2502FiveYearEstimates/tothmown2013.rds")
      pal <- colorNumeric(palette = "viridis", domain = b_hm_13$Percent, reverse = TRUE)
      b_hmown_leaf_13 <- b_hm_13 %>%
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(data = b_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Homeowners: ", Percent, "%"), group = "Black Home Owners") %>% 
        addPolygons(data = tot_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Total Homeowners: ", Percent, "%"), group = "Total Home Owners") %>% 
        addLayersControl(
          baseGroups = c("Total Home Owners"),
          overlayGroups = c("Black Home Owners"),
          options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup("Black Home Owners") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Home Owners",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_hmown() == "2012") {
      b_hm_12 <- read_rds("data/TableS2502FiveYearEstimates/bhmown2012.rds")
      tot_hm_12 <- read_rds("data/TableS2502FiveYearEstimates/tothmown2012.rds")
      pal <- colorNumeric(palette = "viridis", domain = b_hm_12$Percent, reverse = TRUE)
      b_hmown_leaf_12 <- b_hm_12 %>%
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(data = b_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Homeowners: ", Percent, "%"), group = "Black Home Owners") %>% 
        addPolygons(data = tot_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Total Homeowners: ", Percent, "%"), group = "Total Home Owners") %>% 
        addLayersControl(
          baseGroups = c("Total Home Owners"),
          overlayGroups = c("Black Home Owners"),
          options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup("Black Home Owners") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Home Owners",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_hmown() == "2011") {
      b_hm_11 <- read_rds("data/TableS2502FiveYearEstimates/bhmown2011.rds")
      tot_hm_11 <- read_rds("data/TableS2502FiveYearEstimates/tothmown2011.rds")
      pal <- colorNumeric(palette = "viridis", domain = b_hm_11$Percent, reverse = TRUE)
      b_hmown_leaf_11 <- b_hm_11 %>%
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(data = b_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Homeowners: ", Percent, "%"), group = "Black Home Owners") %>% 
        addPolygons(data = tot_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Total Homeowners: ", Percent, "%"), group = "Total Home Owners") %>% 
        addLayersControl(
          baseGroups = c("Total Home Owners"),
          overlayGroups = c("Black Home Owners"),
          options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup("Black Home Owners") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Home Owners",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
    
    else if(var_hmown() == "2010") {
      b_hm_10 <- read_rds("data/TableS2502FiveYearEstimates/bhmown2010.rds")
      tot_hm_10 <- read_rds("data/TableS2502FiveYearEstimates/tothmown2010.rds")
      pal <- colorNumeric(palette = "viridis", domain = b_hm_10$Percent, reverse = TRUE)
      b_hmown_leaf_10 <- b_hm_10 %>%
        leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15, drag = FALSE)) %>% 
        addProviderTiles("CartoDB.PositronNoLabels") %>% 
        addPolygons(data = b_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Black Homeowners: ", Percent, "%"), group = "Black Home Owners") %>% 
        addPolygons(data = tot_hm_19, color = ~ pal(Percent), weight = 0.5, fillOpacity = 0.7, smoothFactor = 0, 
                    highlightOptions = highlightOptions(bringToFront = TRUE, opacity = 1.5, weight = 3), 
                    label = ~paste0(NAME,  " Total Homeowners: ", Percent, "%"), group = "Total Home Owners") %>% 
        addLayersControl(
          baseGroups = c("Total Home Owners"),
          overlayGroups = c("Black Home Owners"),
          options = layersControlOptions(collapsed = FALSE)) %>% 
        hideGroup("Black Home Owners") %>% 
        addLegend("topleft",
                  pal = pal,
                  values = ~ Percent,
                  title = "Home Owners",
                  labFormat = labelFormat(suffix = "%"),
                  opacity = 1)
    }
  })
  
  
  
}

shinyApp(ui = ui, server = server)