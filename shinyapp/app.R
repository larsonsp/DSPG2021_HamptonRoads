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
                 # main -----------------------------------------------------------
                 # tabPanel("Home", value = "home",
                 #          fluidRow(style = "margin: 6px;",
                 #                   align = "center",
                 #                   br("", style = "padding-top:10px;"),
                 #                   img(src = "VTDSPG Logo.png", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                 #                   br(""),
                 #                   h2(strong("Addressing and Tracking the Impact of Systematic Issues in Hampton Roads"),
                 #                   br(""),
                 #                   h4("Data Science for the Public Good Program"),
                 #                   h4("Virginia Tech"),
                 #                   h4("Department of Agriculture"),
                 #                   br(),
                 #                   br(),
                 #                   br(),
                 #                   br(),
                 #                   br(),
                 #                   p(tags$small(em('Last updated: July 2021')))
                 #                   )
                 #          )
                 # ),
                 
                 # main -----------------------------------------------------------
                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   # br("", style = "padding-top:2px;"),
                                   # img(src = "VTDSPG Logo.png", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                   br(""),
                                   h1(strong("Developing indicators to track the economic and social mobility of Black families in Hampton Roads"),
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
                                          p(),
                                          p(strong("The project."), "This Virginia Tech", a(href = "https://biocomplexity.virginia.edu/social-decision-analytics", "Biocomplexity Institute", target = "_blank"), 
                                            "Data Science for Public Good (DSPG) project will be aimed at analyzing questions like: * How do the black unemployment rates in Hampton compare to the Hampton and Virginia mean? 
* How do the black poverty rates in Hampton contrast with Hampton and Virginia rates? Our motivation is that the dashboard will help us see **potential** ways sysematic issues are (or are not) affecting the Black community in **Hampton Roads** and what/how we can overcome those challenges. This project is under the much broader program of Virginia Tech Data Science for Public Good program, at the very gracious request of stakeholder  Mallory Tuttle. Additionally, we would like to give our immense credit and appreciation to the creators of R-Shiny-App-Graduate-Employment-Singapore, as their dashboard and code provided a great template and blueprint for forming our own Hampon-Roads-Shiny-App. .")
                                   ),
                                   column(4,
                                          h2(strong("Our Work")),
                                          p("Our research team worked closely with Patrick County Extension Office, Virginia Department of Health, and Healthy Patrick County coalition stakeholders 
                                            to identify the county’s priority challenges in the area of health. The research team reviewed a prior", a(href = "https://www.vdh.virginia.gov/west-piedmont/2020/05/27/patrick-county-health-needs-improvement-plan-completed/", 
                                                                                                                                                       "community health assessment,", target = "blank"), a(href = "https://www.pubs.ext.vt.edu/VCE/VCE-596/VCE-596-75/VCE-1002-75.html", "situation analysis", target = "_blank"),
                                            "relevant funding applications, and held a listening meeting with stakeholders to identify these challenges. Lack of 
                                            data on health care access, food access as related to diabetes and heart disease prevalence, older adult health, and digital connectivity that would facilitate 
                                            access to telemedicine emerged as key problems where providing actionable insights could address barriers to Patrick County residents’ health."),
                                          p(),
                                          p("We implemented the", a(href = "https://doi.org/10.1162/99608f92.2d83f7f5", "data science framework", target = "_blank"), "and identified, acquired, profiled, and used 
                                            publicly available data to provide Patrick County with data-driven resources in each of the four priority areas. We:"),
                                          tags$li("Provided census tract- and census block group-level maps of Patrick County residents'", strong("sociodemographic and socioeconomic characteristics,"), " highlighting underprivileged areas."),
                                          tags$li("Created census tract-level maps on", strong("older adult health"), "to show the geographic distribution of older adults in the county by gender and
                                                  type of disability, identifying areas where providing telehealth or travelling preventive care services may be particularly important."),
                                          tags$li("Mapped residents'", strong("computing device and internet access"), "at census block group level, and constructed 10- and 15-minute isochrones (areas of equal travel time) from households to free
                                                  wifi hotspots to highlight internet gaps that could suggest where new wi-fi hotspots could be optimally placed to provide internet access to more residents."),
                                          tags$li("Calculated and mapped", strong("emergency medical service (EMS) station coverage"), "of households within 8-, 10-, and 12-minute travel times, identifying areas difficult to reach within 
                                                   standard EMS travel thresholds."),
                                          tags$li("Constructed", strong("food access"), "maps by census tract, 10- and 15-minute isochrones from households to grocery stores and farmers markets, and maps of food security resources in the county,
                                                highlighting food deserts and areas that could benefit from programs facilitating access to fresh produce."),
                                          p(),
                                          p("This dashboard compiles our findings and allows extension professionals, stakeholders, and other users to explore the information interactively.")
                                   ),
                                   column(4,
                                          h2(strong("Dashboard Aims")),
                                          p("Our dashboard is aimed at:"),
                                          p(strong("Patrick County extension professionals and the communities they serve."), "Information available through the interface helps extension 
                                            agents identify areas where residents may not have access to internet, or areas with a high smartphone ownership share, suggesting what channels agents may 
                                            want to use to disseminate health-related information most effectively. Information on older adult populations and grocery store access can help extension agents 
                                            better understand where underserved populations live and how to advocate on their behalf."),
                                          p(strong("Local health-related agencies and departments seeking data insights to inform their decision-making."), "For local stakeholders, identifying broadband 
                                            access gaps that limit access to telemedicine, grocery store access gaps, and areas with high proportions of older adults with independent living difficulty can suggest 
                                            optimal locations for placing free wifi hotspots, providing grocery delivery services, devising mobile health unit routes, or can inform other solutions that would benefit 
                                            a broad population base."),
                                          p(strong("State government representatives in the Virginia Department of Health and the State Office of Rural Health."), "These and similar stakeholders may 
                                            need small or rural area-specific insights that Centers for Disease Control and other county-level datasets cannot provide.")
                                   )
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2020'))))
                 ),
                 
                 #Hampton Overview
                 
                 tabPanel("Hampton Roads", value = "makeup",
                          dashboardPage(
                            skin = 'black',
                            dashboardHeader(
                              title = 'Hampton Roads Composition'
                            ),
                            
                            
                            dashboardSidebar(
                              sidebarMenu(
                                menuItem(
                                  "Counties and Cities",
                                  tabName = 'makeup'
                                ),
                                menuItem(
                                  "Race",
                                  tabName = "race"
                                ),
                                menuItem(
                                  "Age",
                                  tabName = "age"
                                )
                              )
                            ),
                            
                            dashboardBody(tabItems(
                              ## First Sidebar ----------------------------
                              tabItem(
                                tabName = "makeup",
                                fluidRow(style = "margin: 6px;",
                                         h1(strong("Counties and Cities of Hampton Roads"), align = "center")
                                         
                                         
                                         
                                         )
                                ),
                              
                              tabItem(tabName = "race",
                                      fluidRow(
                                        h1(strong("Race Composition of Hampton Roads"), align = "center"),
                                        column(5, width = 12,
                                               h4(strong("Race Demographic"))
                                               ),
                                        column(6,
                                                 
                                                    h4("Hampton Roads Breakdown"),
                                                           
                                                             selectInput("hampRaceYearDrop", "Select Year:", width = "100%", choices = c(
                                                               "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"
                                                             )),
                                                             p(strong("Hampton Roads")),
                                                             withSpinner(plotOutput("hamp_pie")),
                                                             p(tags$small("Data Source: ACS 5 Year Estimate Table B02001"))
                                                    ),
                                                   
                                        
                                        column(6,
                                            
                                                 h4("Virginia Breakdown"),
                                                        
                                                          selectInput("VaRaceYearDrop", "Select Year:", width = "100%", choices = c(
                                                            "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"
                                                          )),
                                                          p(strong("Virginia")),
                                                          withSpinner(plotOutput("va_pie")),
                                                          p(tags$small("Data Source: ACS 5 Year Estimate Table B02001"))
                                                
                                                
                                                )
                                        
                                        
                                        
                                        
                                      )
                              ),
                              
                              tabItem(tabName = "age",
                                      fluidRow(
                                        h1(strong("Age Composition of Hampton Roads"), align = "center"),
                                        column(6,
                                            
                                                 h4("Hampton Roads Age Breakdown"),
                                                        
                                                          selectInput("HampAgeYearDrop", "Select Year:", width = "100%", choices = c(
                                                            "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"
                                                          )),
                                                          p(strong("Hampton Roads")),
                                                          withSpinner(plotOutput("hamp_graph")),
                                                          p(tags$small("Data Source: ACS 5 Year Estimate Table B01001"))
                                                ),
                                               
                                        column(6,
                                               
                                                 h4("Virginia Age Breakdown"),
                                                          p(""),
                                                          selectInput("VaAgeYearDrop", "Select Year:", width = "100%", choices = c(
                                                            "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"
                                                          )),
                                                          p(strong("Virginia")),
                                                          withSpinner(plotOutput("va_graph")),
                                                          p(tags$small("Data Source: ACS 5 Year Estimate Table B01001"))
                                                 ),
                                               
                                        column(6, width=12,
                                           
                                                 h4("Hampton Roads' County and City Breakdown"),
                                                          selectInput("HampCountAgeYearDrop", "Select Year:", width = "100%", choices = c(
                                                            "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010"
                                                          )),
                                                          p(strong("Hampton Roads Counties and Cities' Age Breakdown")),
                                                          withSpinner(plotOutput("age_map")),
                                                          p(tags$small("Data Source: ACS 5 Year Estimate Table B01001"))
                                                 ),
                                               
                                        
                                      
                                      )
                              )
                              
                              )))),
                 
                 #testing out tabs for education (I don't want to mess with victors code until I know the tabs code works)
                 tabPanel("Education", value = "edu",
                          dashboardPage(
                            skin = 'black',
                            dashboardHeader(
                              title = ''
                            ),
                            
                            
                            dashboardSidebar(
                              sidebarMenu(
                                menuItem(
                                  "Suspension",
                                  tabName = 'suspension'
                                )
                              )
                            ),
                            
                            dashboardBody(tabItems(
                              ## First Sidebar ----------------------------
                              tabItem(tabName = "suspension",
                                      fluidRow(
                                        h1(strong("Short Term Suspension"), align = "center"),
                                        column(5,
                                                 h4("Virgina Suspensions"),
                                                          
                                                          selectInput("suspensionYearDrop", "Select Year:", width = "100%", choices = c(
                                                            "2018-2019", "AY 2017-2018", "AY 2016-2017", "AY 2015-2016", "AY 2014-2015")),
                                                          p(strong("Virgina Suspensions")),
                                                          withSpinner(plotOutput("graph_va")),
                                                          p(tags$small("Data Source: Kids Count Data Center"))
                                                 ),
                                        column(5,
                                               h4("Academic Punishments")
                                               ),
                                        column(7, width = 12,
                                               h4("Percent of Black Students Suspended"),
                                               selectInput("BsuspensionYearDrop", "Select Year:", width = "100%", choices = c(
                                                 "2018-2019", "AY 2017-2018", "AY 2016-2017", "AY 2015-2016", "AY 2014-2015")),
                                               withSpinner(plotOutput("black_map")),
                                               p(tags$small("Data Source: Kids Count Data Center"))
                                               ),
                                        column(7, width = 12,
                                               h4("Percent of Black and White Students Suspended in Hampton Roads"),
                                               selectInput("BWsuspensionYearDrop", "Select Year:", width = "100%", choices = c(
                                                 "2018-2019", "AY 2017-2018", "AY 2016-2017", "AY 2015-2016", "AY 2014-2015")),
                                               withSpinner(plotOutput("BW_map")),
                                               p(tags$small("Data Source: Kids Count Data Center"))
                                        ),
                                        
                                        
                                                
                                        
                                        
               
                                        
                                      )
                              )
                              
                            )))),
                 
                 
                 
                 
                 
                 
                 
                 # Education-----------------------------------------------------------
                 tabPanel("Education", value =  "education",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Percentage of Hampton Roads Population 25 years and older with highest educational attainment as Bachelor's degree or higher"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(5,
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
                                   column(7,
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
                                                     p(tags$small("Note: Some year to year comparisions had very little variability in attainement percentages"))
                                            ),
                                            tabPanel("Black Population",
                                                     p(""),
                                                     selectInput("blackEdAttainmentYearDrop", "Select Year:", width = "100%", choices = c(
                                                       "2019","2018", "2017", "2016", "2015","2014",
                                                       "2013","2012", "2011", "2010")),
                                                     p(strong("Black Educational Attainment")),
                                                     withSpinner(plotlyOutput("blackEdAttainmentPlots")),
                                                     p(tags$small("Note: Some year to year comparisions had very little variability in attainement percentages"))
                                                     #p(tags$small("Data Source: ACS 5 Year Estimate Table C15002B"))
                                            )
                                          )
                                   ), 
                                   
                                   column(5,
                                          h4(strong("Teacher Race Breakdown")),
                                   ),
                                   column(7,
                                          tabsetPanel(
                                            tabPanel("Teacher Race Breakdown",
                                                     p(""),
                                                     selectInput("teacherRaceBreakdown", "Select Race:", width = "100%", choices = c(
                                                       "Black", "Asian", "American Indian", "Hispanic", "Two or More Races", "White", "Hawaiian"
                                                     )),
                                                     p(strong("Virginia: Teacher Breakdown By Race in Hampton Roads")),
                                                     withSpinner(plotlyOutput("teacherRacePlots")),
                                                     p(tags$small("Possible Suppression of Counts in Dataset for Counties/Cities with Zero Values Stated"))
                                            )
                                   ), 
                          )
                 )
              ),
                 
                 
                 #unemployment rates
                 # tabPanel("Economics", value = "econ",
                 #          fluidRow(style = "margin: 6px;",
                 #                   h1(strong("Black and General Population Unemployment Rates"), align = "center"),
                 #                   p("", style = "padding-top:10px;"),
                 #                   column(5,
                 #                          h4(strong("Unemployment in the Black Population"))
                 #                   ),
                 #                   column(7,
                 #                          #sliderInput("MedianIncomeYearSlider", "", value = 2019, min =2010, max=2020),
                 #                          selectInput("UnemploymentRateYearDrop", "Select Year:", width = "100%", choices = c(
                 #                            "2019","2018", "2017", "2016", "2015","2014",
                 #                            "2013","2012", "2011", "2010")),
                 #                          p(strong("Unemployment Rate")),
                 #                          withSpinner(plotlyOutput("unemployment_plot")),
                 #                          p(tags$small("Data Source: ACS 5 Year Estimate Table S2301"))
                 #                   )
                 #          )
                 # ),
                 
                 # socio -----------------------------------------------------------
                 tabPanel("Sociodemographics", value = "socio",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Patrick County Residents' Sociodemographic Characteristics"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(4,
                                          h4(strong("Who does Patrick County Serve?")),
                                          p("We examined Patrick County population sociodemographic and socioeconomic characteristics to better understand the 
                                            residents that the county serves."),
                                          p("We retrieved American Community Survey (ACS) data to calculate this information at census block group and census 
                                            tract levels. ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households to compile 1-year and 5-year datasets. We used 
                                            the most recently available 5-year estimates from 2014/18 to compute percent Patrick County residents in a given block group or tract by age, race, ethnicity, 
                                            employment, health insurance coverage, and other relevant characteristics."),
                                          p("Our interactive plots visualize census block-group level sociodemographic characteristics of Patrick County residents.")),
                                   column(8,
                                          h4(strong("Map of Resident Socioeconomic Characteristics by Census Tract or Block Group")),
                                          selectInput("sociodrop", "Select Variable:", width = "100%", choices = c(
                                            "Percent Population Age 65 and Older" = "age65",
                                            "Percent Population Age 18 and Younger" = "under18",
                                            "Percent Population Black" = "black",
                                            "Percent Population Hispanic" = "hispanic",
                                            "Percent Population Without Bachelor's Degree" = "noba",
                                            "Percent Population In Labor Force Unemployed" = "unempl",
                                            "Percent Population Without Health Insurance" = "nohealthins2",
                                            "Percent Population With Private Health Insurance" = "privateins",
                                            "Percent Population With Public Health Insurance" = "publicins",
                                            "Percent Population in Poverty" = "inpov",
                                            "Percent Population Receiving SNAP Benefits or Public Assistance" = "snap",
                                            "Total Population by Census Block Group" = "totalpop_bgrp",
                                            "Total Population by Census Tract" = "totalpop_trct")
                                          ),
                                          withSpinner(leafletOutput("socioplot")),
                                          p(tags$small("Data Source: American Community Survey 2014/18 5-Year Estimates."))
                                   ))
                 ),
                 
                 # older -----------------------------------------------------------
                 tabPanel("Older Adults", value = "older",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Older Adults in Patrick County"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(4,
                                          h4(strong("Who are the Patrick County Older Adults?")),
                                          p("The US population is aging, and in Patrick County, over 30% of residents are older adults aged 65 years and over. This represents more than 5,000
                                           individuals with varying health conditions that may benefit from locally accessible health care and social services resources. However, access to 
                                           health care resources is limited in rural areas, particularly for older adults in need of assistance with activities of daily life."),
                                          p("To help Patrick County better understand their older adult population, we used American Community Survey (ACS) data and obtained census tract 
                                           level information for the age group. ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households to compile 
                                           1-year and 5-year estimates of population sociodemographic and socioeconomic characteristics. We used the most recently available 5-year data
                                           from 2014/18 to calculate the percentage of the Patrick County older adults with different types of disability, as well as provided information 
                                           on their living arrangements and socioeconomic status. We provided all information at census tract level and by gender."),
                                          p("These insights on the health and socioeconomic status of older adults in Patrick County can assist the county in identifying areas of high need 
                                          for health care resources.")
                                   ),
                                   column(8,
                                          h4(strong("Map of Older Adult Characteristics by Census Tract")),
                                          tabsetPanel(
                                            tabPanel("Older Adult Characteristics",
                                                     p(""),
                                                     column(6,
                                                            selectInput("olddrop", "1. Select Variable:", width = "100%", choices = c(
                                                              "Percent with Vision Difficulty" = "visdiff",
                                                              "Percent with Ambulatory Difficulty" = "ambdiff",
                                                              "Percent with Self-Care Difficulty" = "carediff",
                                                              "Percent with Cognitive Difficulty" = "cogdiff",
                                                              "Percent with Independent Living Difficulty" = "ildiff",
                                                              "Percent with Any Disability" = "disab",
                                                              "Percent in Poverty" = "inpov",
                                                              "Percent in Labor Force" = "labfor")
                                                            )),
                                                     column(6,
                                                            selectInput("oldspecdrop", "2. Select Group:", width = "100%", choices = c(
                                                              "Total",
                                                              "Female" = "_f",
                                                              "Male" = "_m")
                                                            )),
                                                     withSpinner(leafletOutput("oldplot")),
                                                     p(tags$small("Data Source: American Community Survey 2014/18 5-Year Estimates."))
                                            ),
                                            tabPanel("Older Adult Household Characteristics",
                                                     p(""),
                                                     selectInput("hhdrop", "Select Variable:", width = "100%", choices = c(
                                                       "Percent Married Couple Households with one or more 60+ Member" = "hhsixty_married",
                                                       "Percent Households with one or more 60+ Members" = "hhsixty_total",
                                                       "Percent Single (no partner present) Households with one or more 60+ Member" = "hhsixty_nonfam",
                                                       "Percent Households with one or more Male 60+ Members" = "hhsixty_mhh",
                                                       "Households with one or more Female 60+ Members" = "hhsixty_fhh")),
                                                     withSpinner(leafletOutput("householdplot")),
                                                     p(tags$small("Data Source: American Community Survey 2014/18 5-Year Estimates."))
                                            )
                                          )
                                   )
                          )
                 ),
                 
                 
                 # Potential code for sidebar ----------------------------------------------
                 
                 
                 tabPanel("Economics", value = "economics",
                          dashboardPage(
                            skin = 'black',
                            dashboardHeader(
                              title = 'Economics Indicators'
                            ),
                            
                            
                            dashboardSidebar(
                              sidebarMenu(
                                menuItem(
                                  "Unemployment",
                                  tabName = 'unemp'
                                ),
                                menuItem(
                                  "Health Insurance",
                                  tabName = "unins"
                                ),
                                menuItem(
                                  "Veteran Status",
                                  tabName = "vet"
                                ),
                                menuItem(
                                  "Homeownership",
                                  tabName = "hmown"
                                ),
                                menuItem(
                                  "Median Income",
                                  tabName = 'median'
                                ),
                                menuItem(
                                  "Poverty Rate",
                                  tabName = 'poverty'
                                )
                              )
                            ),
                            
                            dashboardBody(tabItems(
                              ## First Sidebar ----------------------------
                              tabItem(
                                tabName = "unemp",
                                fluidRow(style = "margin: 6px;",
                                         h1(strong("Unemployment in Hampton Roads"), align = "center"),
                                         #sliderInput("MedianIncomeYearSlider", "", value = 2019, min =2010, max=2020),
                                         selectInput("UnemploymentRateYearDrop", "Select Year:", width = "100%", choices = c(
                                           "2019","2018", "2017", "2016", "2015","2014",
                                           "2013","2012", "2011", "2010")),
                                         p(strong("Unemployment Rate")),
                                         withSpinner(plotlyOutput("unemployment_plot")),
                                         p(tags$small("Data Source: ACS 5 Year Estimates Table S2301"))
                                         
                                         
                                )),
                              
                              tabItem(tabName = "unins",
                                      fluidRow(
                                        h1(strong("Health Insurance in Hampton Roads"), align = "center"),
                                        withSpinner(plotlyOutput("uninsured_plot")),
                                        p(tags$small("Data Source: ACS 5 Year Estimates Table S2701")),
                                        box(title = "Select Year:", width = 12,
                                            sliderInput("UninsuredPctSlider", "", value = 2019, min = 2010, max = 2019))
                                        
                                        
                                      )
                              ),
                              
                              tabItem(tabName = "vet",
                                      fluidRow(
                                        h1(strong("Veteran Status in Hampton Roads"), align = "center"),
                                        withSpinner(leafletOutput("veteran_map")),
                                        p(tags$small("Data Source: ACS 5 Year Estimates Table S2101")),
                                        box(title = "Select Year:", width = 12,
                                            sliderInput("VeteranSlider", "", value = 2019, min = 2010, max = 2019))
                                      )
                              ),
                              
                              tabItem(tabName = "hmown",
                                      fluidRow(
                                        h1(strong("Homeownership in Hampton Roads"), align = "center"),
                                        withSpinner(leafletOutput("homeownership_map")),
                                        p(tags$small("Data Source: ACS 5 Year Estimates Table S2505")),
                                        box(title = "Select Year:", width = 12,
                                            sliderInput("HomeOwnSlider", "", value = 2019, min = 2010, max = 2019))
                                      )),
                              
                              tabItem(
                                tabName = "median",
                                fluidRow(style = "margin: 6px;",
                                         h1(strong("Median Income in Virginia and Hampton Roads"), align = "center"),
                                         column(9,
                                         selectInput("MedianIncomeYearDrop", "Select Year:", width = "100%", choices = c(
                                           "2019","2018", "2017", "2016", "2015","2014",
                                           "2013","2012", "2011", "2010")),
                                         withSpinner(plotOutput("income_plot")),
                                         p(tags$small("Data Source: ACS 5 Year Estimates Table S1903"))
                                         ),
                                         column(12,
                                                h4("Median Income from 2010 to 2019"),
                                                withSpinner(plotOutput("medianTimeGraph")),
                                                p(tags$small("Data Source: ACS 5 Year Estimates Table S1903"))
                                                )
                                         
                                )
                              ),
                              tabItem(
                                tabName = "poverty",
                                fluidRow(style = "margin: 6px;",
                                         h1(strong("Poverty Rates in Virginia and Hampton Roads"), align = "center"),
                                         column(7, 
                                                h4(strong("Poverty Rates in Virginia and Hampton Roads")),
                                                selectInput("PovertyYearDrop", "Select Year:", width = "100%", choices = c(
                                                  "2019","2018", "2017", "2016", "2015","2014",
                                                  "2013","2012")),
                                                withSpinner(plotOutput("pov_plot")),
                                                p(tags$small("Data Source: ACS 5 Year Estimates Table S1701"))
                                                ),
                                         column(7, width = 12,
                                                h4(strong("Poverty Rates in Hampton Roads Counties and Cities")),
                                                selectInput("PovertyCountYearDrop", "Select Year:", width = "100%", choices = c(
                                                  "2019","2018", "2017", "2016", "2015","2014",
                                                  "2013","2012")),
                                                withSpinner(plotOutput("counties_pov")),
                                                p(tags$small("Data Source: ACS 5 Year Estimates Table S1701"))
                                                
                                         )
                                         
                              
                                )
                              )
                              
                              
                              )))),
                 
                 
                 # wifi-----------------------------------------------------------
                 tabPanel("Connectivity", value = "connectivity",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Digital Connectivity in Patrick County"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(6,
                                          h4(strong("Computing Device Ownership and Internet Access Type")),
                                          p("Internet connection and computing devices are key for access to health information and participation in online health-related services like 
                                             telemedicine. Rural areas frequently lack broadband access, experience low internet speeds, and have fewer internet providers available 
                                             than urban areas. It is crucial to consider digital connectivity in improving health care access. We examined digital connectivity in Patrick County in two ways to 
                                             provide the county with insights on where increasing connectivity would facilitate communicating health information and improve online health service access."),
                                          p("We first examined access to computing devices and internet connection types in Patrick County. We used American Community Survey (ACS) data to 
                                            obtain this information at census block group level. ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households 
                                            to compile 1-year and 5-year estimates of population sociodemographic and socioeconomic characteristics. We used the most 
                                            recently available 5-year data from 2014/18 to calculate the percentage of the Patrick County residents with access to devices
                                            and internet by census block group."),
                                          br(),
                                          selectInput("devicedrop", "Select Variable:", width = "100%", choices = c(
                                            "Percent Households with No Computer" = "nocomputer",
                                            "Percent Households with Laptop" = "laptop",
                                            "Percent Households with Smartphone" = "smartphone",
                                            "Percent Households with Tablet" = "tablet",
                                            "Percent Households without Internet" = "nointernet",
                                            "Percent Households with Satellite Internet" = "satellite",
                                            "Percent Households with Cellular Internet" = "cellular",
                                            "Percent Households with Broadband Internet" = "broadband")
                                          ),
                                          p(strong("Map of Access by Census Block Group")),
                                          withSpinner(leafletOutput("deviceplot")),
                                          p(tags$small("Data Source: American Community Survey 2014/18 5-Year Estimates."))),
                                   column(6,
                                          h4(strong("Free WiFi Hotspot Access")),
                                          p("To understand internet access at a more granular level, we examined access to free wi-fi hotspots in the county."),
                                          p("We obtained wifi hotspot locations using the Virginia Tech and CommonwealthConnect hotspot map. CommonwealthConnect identifies where people can connect to 
                                            the internet for free, decreasing constraints placed on families that do not have internet access at home. We retrieved free internet locations in Patrick 
                                            County from the data. We extracted locations of Patrick County residential properties from 2019 CoreLogic, a proprietary dataset for US real estate that 
                                            includes information on building characteristics. Finally, we used the TravelTime Application Programming Interface (API) to calculate 10- and 15-minute 
                                            car travel time isochrones—areas of equal travel time given a departure time and mode of transportation—from wifi hotspots. TravelTime API aggregates data 
                                            from Open Street Maps, transport timetables and speed profiles to generate isochrones. Isochrones allowed us to identify wifi gaps, or clusters of 
                                            residential properties that cannot reach a free internet location within a selected travel time range."),
                                          p("This information equips extension agents with knowledge on how best to reach their constituents, as well as identifies internet gaps that suggest where 
                                            new wi-fi hotspots could be optimally placed to provide internet access to more residents."),
                                          br(),
                                          tabsetPanel(
                                            tabPanel("Explore Hotspot Coverage",
                                                     p(""),
                                                     selectInput("wifidrop", "Select Free Wifi Location:", width = "100%", choices = c(
                                                       "Meadows of Dan Elementary School",
                                                       "Woolwine Elementary School",
                                                       "Patrick Springs Primary School",
                                                       "Blue Ridge Elementary School",
                                                       "Patrick County High School",
                                                       "Stuart Elementary School",
                                                       "Patrick County Branch Library",
                                                       "Hardin Reynolds Memorial School",
                                                       "Stuart Baptist Church",                        
                                                       "Patrick Henry Community College Stuart Campus")),
                                                     p(strong("Percent Residential Properties Covered")),
                                                     withSpinner(tableOutput("wifitable")),
                                                     p(strong("Map of Coverage")),
                                                     withSpinner(leafletOutput("wifiplot")),
                                                     p(tags$small("Data Sources: CommonwealthConnect, 2020; CoreLogic, 2019; TravelTime API."))
                                            ),
                                            tabPanel("Explore 'Deserts'",
                                                     p(""),
                                                     p(strong("Percent Residential Properties Covered")),
                                                     withSpinner(tableOutput("allwifitable")),
                                                     p(strong("Map of Free Wi-Fi Deserts")),
                                                     withSpinner(leafletOutput("allwifi")),
                                                     p(tags$small("Data Sources: CommonwealthConnect, 2020; CoreLogic, 2019; TravelTime API."))
                                            )
                                          )
                                   )
                          )
                 ),
                 
                 # ems -----------------------------------------------------------
                 tabPanel("Health Care Access", value = "ems",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Health Care Access in Patrick County"), align = "center"),
                                   p("", style = "padding-top:10px;"),
                                   column(4,
                                          h4(strong("Accessing Emergency Medical Service Stations")),
                                          p("Access to health care services in rural areas is limited by a lack of transportation and a shortage of healthcare professionals. Compared to urban 
                                            counterparts, rural residents must travel farther to obtain both preventive and specialty care. Patrick County’s general practitioner, dentist, and mental health
                                            provider-to-patient ratios fall below state averages, and the county recently experienced the closure of its only hospital. Its residents often rely on emergency
                                            medical services (EMS) stations to obtain care and transportation to other health care facilities."),
                                          p("To better understand health service access limitations in the county, we examined residents’ access to EMS stations. We obtained EMS locations using Homeland 
                                            Infrastructure Foundation-Level Data (HIFLD) collected by the Department of Homeland Security. HIFLD is a public source dataset with information on a range of 
                                            facilities; we used the data to retrieve EMS station latitude and longitude. We extracted locations of Patrick County residential 
                                            properties from 2019 CoreLogic, a proprietary dataset for US real estate that includes information on building characteristics. Finally, we used the TravelTime
                                            Application Programming Interface (API) to calculate 8-, 10-, and 12- minute drive time isochrones—areas of equal travel time given a departure time and 
                                            mode of transportation—from EMS stations. TravelTime API aggregates data from Open Street Maps, transport timetables and speed profiles to generate isochrones. 
                                            Isochrones allowed us to identify EMS coverage gaps, or clusters of residential properties that cannot be reached from an EMS location within a selected travel 
                                            time range. We selected 8-, 10-, and 12-minute thresholds as EMS are expected to reach distressed individuals within 8 minutes. However, this threshold is 
                                            frequently exceeded by 20% to 40% in rural areas.")
                                   ),
                                   column(8,
                                          tabsetPanel(
                                            tabPanel("Explore Coverage",
                                                     p(""),
                                                     selectInput("emsdrop", "Select EMS Location:", width = "100%", choices = c(
                                                       "Stuart Volunteer Fire Department" = "STUART VOLUNTEER FIRE DEPARTMENT",
                                                       "Moorefield Store Volunteer Fire Department" = "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT",                                                         
                                                       "Blue Ridge Volunteer Rescue Squad" = "BLUE RIDGE VOLUNTEER RESCUE SQUAD",                                                                   
                                                       "Vesta Rescue Squad" = "VESTA RESCUE SQUAD",                                                                                           
                                                       "Ararat Rescue Squad" = "ARARAT RESCUE SQUAD",                                                                                          
                                                       "Five Forks Volunteer Fire and Rescue Station 1" = "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS",
                                                       "Five Forks Volunteer Fire and Rescue Station 2"= "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2",
                                                       "Jeb Stuart Rescue Squad" = "JEB STUART RESCUE SQUAD",                                                                                      
                                                       "Smith River Rescue Squad" = "SMITH RIVER RESCUE SQUAD"                                                                                     
                                                     )),
                                                     p(strong("Percent Residents Covered")),
                                                     withSpinner(tableOutput("emstable")),
                                                     p(strong("Map of Coverage")),
                                                     withSpinner(leafletOutput("emsplot")),
                                                     p(tags$small("Data Sources: Homeland Infrastructure Foundation-Level Data, 2010; CoreLogic, 2019; TravelTime API."))
                                            ),
                                            tabPanel("Explore 'Deserts'",
                                                     p(""),
                                                     p(strong("Percent Residents Covered")),
                                                     withSpinner(tableOutput("allemstable")),
                                                     p(strong("Map of Coverage Deserts")),
                                                     withSpinner(leafletOutput("allems")),
                                                     p(tags$small("Data Sources: Homeland Infrastructure Foundation-Level Data, 2010; CoreLogic, 2019; TravelTime API.")))
                                          )
                                   )
                          )
                 ),
                 
                 # data -----------------------------------------------------------
                 tabPanel("Employment", value = "data",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Data and Measures"), align = "center"),
                                   br()
                          ),
                          tabsetPanel(
                            tabPanel("Data Sources",
                                     h3("", align = "center"),
                                     br(""),
                                     column(4, 
                                            img(src = "data-hifld.png", style = "display: inline; float: left;", width = "100px"),
                                            p(strong("Homeland Infrastructure Foundation-Level Data."), "Homeland Infrastructure Foundation-Level Data (HIFLD) is a collection of public 
                                              source datasets at property level provided by the Department of Homeland Security. Since 2002, this HIFLD has provided quarterly 
                                              updated layers on topics from education to energy, including on health care facilities. We used HIFLD emergency medical services 
                                              station data at the latitude and longitude geographic level in our analyses."),
                                            br(""),
                                            img(src = "data-gmaps.png", style = "display: inline; float: left;", width = "130px"),
                                            p(strong("Google Maps."), "Google Maps is a comprehensive web mapping service created by Google. Its goal is to provide an interactive map
                                              of all the geographical contents of the world. This resource has a variety of uses, ranging from examining all service locations within 
                                              a city to finding the quickest route between locations. It provides data at latitude and longitude level. We used Google Maps to locate 
                                              all supermarkets, convenience stores, and farmers’ markets in Patrick County, and subsequently employed the information in calculating 
                                              grocery access and coverage isochrones.")
                                     ),
                                     column(4,
                                            img(src = "data-acs.png", style = "display: inline; float: left;", width = "200px"),
                                            p(strong("American Community Survey."), "The American Community Survey (ACS) is an ongoing yearly survey conducted by the U.S Census 
                                            Bureau. ACS samples households to compile 1-year and 5-year datasets providing information on population sociodemographic and 
                                            socioeconomic characteristics including employment, disability, and health insurance coverage. We used AC 2014/18 5-year 
                                            estimates to obtain census tract and census block group-level to explore Patrick County resident characteristics."),
                                            br(""),
                                            img(src = "data-connect.png", style = "display: inline; float: left;", width = "150px"),
                                            p(strong("CommonwealthConnect."), "The Virginia Tech CommonwealthConnect Wi-Fi Hotspot Map is an interactive map of free, publicly 
                                           available wi-fi hotspots in Virginia. Its goal is to provide an easily accessible map of areas where individuals can connect to the 
                                           internet for free, decreasing the constraints placed on families that do not have internet access at home. We used the 2020 wi-fi 
                                           hotspot map data to retrieve hotspot locations in Patrick County and subsequently employed the information in calculating hotspot 
                                           coverage isochrones."),
                                            br(""),
                                            img(src = "data-corelogic.png", style = "display: inline; float: left;", width = "120px"),
                                            p(strong("CoreLogic."), "CoreLogic is a supplier of proprietary US real estate and specialized business data at the property level. 
                                           This company provides data spanning over 50 years at the latitude and longitude level. Information available in the dataset includes 
                                           property characteristics, mortgage, foreclosures and performance. We used 2019 CoreLogic data to obtain the locations of all residential
                                           properties in Patrick County.")
                                     ),
                                     column(4,
                                            img(src = "data-traveltime.png", style = "display: inline; float: left;", width = "140px"),
                                            p(strong("TravelTime."), "TravelTime Application Programming Interface (API) aggregates data from OpenStreetMap, transport timetables and
                                           speed profiles to generate isochrones. An isochrone is a shape covering all locations that can be reached within the same timeframe 
                                           given a start location, departure time, and a mode of transportation. We used the TravelTime API to produce isochrones of 10- and 
                                           15-minute drive time interval from supermarkets, farmers' markets, and free wi-fi hotspots, and of 8-, 10-, and 12-minute drive 
                                           time intervals from all emergency medical service stations in Patrick County."),
                                            br(""),
                                            img(src = "data-ers.png", style = "display: inline; float: left;", width = "120px"),
                                            p(strong("Food Access Research Atlas."), "The United State Department of Agriculture Food Access Research Atlas is a data resource 
                                          created by the Economic Research Service that provides information on food access indicators at census tract level. The data allows 
                                          individuals to understand food access in communities based on factors like age and socioeconomic status. We used the 2017 Food Access
                                          Research Atlas to examine Patrick County residents’ food access at multiple distance thresholds and by resident characteristics.")
                                     )
                            ),
                            tabPanel("Measures",  
                                     h3(strong(""), align = "center"),
                                     selectInput("topic", "Select Topic:", width = "100%", choices = c(
                                       "All Measures",
                                       "Sociodemographic Measures",
                                       "Older Adult Population Measures",
                                       "Connectivity Measures",
                                       "Food Access Measures",
                                       "Health Care Access Measures")
                                     ),
                                     withSpinner(DTOutput("datatable"))
                            )
                          )
                 ),
                 
                 # contact -----------------------------------------------------------
                 tabPanel("Contact", value = "contact",
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                                   h1(strong("Contact"), align = "center"),
                                   br(),
                                   h4(strong("UVA Data Science for the Public Good")),
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
                                          img(src = "team-morgan.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-tasfia.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-isabel.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = 'https://www.linkedin.com/in/morgan-stockham/', 'Morgan Stockham', target = '_blank'), "(Claremont Graduate University, Applied Microeconomics);",
                                            a(href = 'https://www.linkedin.com/in/tasfia-chowdhury-89005a1b2/', 'Tasfia Chowdhury', target = '_blank'), "(Indiana University Bloomington, Political Science);",
                                            a(href = 'https://www.linkedin.com/in/igomez-3099/', 'Isabel Gomez', target = '_blank'), "(Smith College, Statistical and Data Science)."),
                                          p("", style = "padding-top:10px;")
                                   ),
                                   column(6, align = "center",
                                          h4(strong("UVA SDAD Team Members")),
                                          img(src = "team-teja.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-brandon.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-sallie.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = "https://www.linkedin.com/in/tejapristavec/", 'Teja Pristavec', target = '_blank'), "(Project Lead, Research Assistant Professor);",
                                            a(href = "https://biocomplexity.virginia.edu/brandon-kramer", 'Brandon Kramer', target = '_blank'), "(Postdoctoral Research Associate);",
                                            a(href = 'https://biocomplexity.virginia.edu/sallie-keller', 'Sallie Keller', target = '_blank'), "(Division Director and Distinguished Professor)."),
                                          p("", style = "padding-top:10px;")
                                   )
                          ),
                          fluidRow(style = "margin-left: 300px; margin-right: 300px;",
                                   h4(strong("Project Stakeholders")),
                                   p(a(href = 'https://www.linkedin.com/in/nancy-bell-aa293810/', 'Nancy Bell', target = '_blank'), "(Virginia Department of Health);",
                                     a(href = 'https://www.linkedin.com/in/terri-alt-3138b4101/', 'Terri Alt', target = '_blank'), "(Virginia Cooperative Extension, Patrick County at Virginia Tech)."),
                                   p("", style = "padding-top:10px;"),
                                   h4(strong("Acknowledgments")),
                                   p("We would like to thank Healthy Patrick County, an association of concerned Patrick County residents, and Brandon Kramer for their input to this project.")
                          )
                 ),
                 
                 tabPanel("Dropout Rate", value = "dropout",
                          fluidRow(
                            h1(strong("Dropout Rates in Hampton Roads"), align = "center"),
                            withSpinner(leafletOutput("dropout_map")),
                            p(tags$small("Data Source: Virginia Department of Education")),
                            box(title = "Select Year", width = 12,
                                selectInput("DropoutDropdown", "Select Year:", width = "100%", choices = c("2020", "2019", "2018", "2017", "2016", "2015", 
                                                                                                           "2014", "2013", "2012", "2011", "2010"))
                            
                        
                          ))),
                 inverse = T)

# server -----------------------------------------------------------
server <- function(input, output, session) {
  # Run JavaScript Code
  runjs(jscode)
  
  # hampton race plots -----------------------------------------------------
  var_hampRace <- reactive({
    input$hampRaceYearDrop
  })
  
  output$hamp_pie <- renderPlot({
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
    #pal
    vir_pal <- c("#404788FF", "#2D708EFF", "#29AF7FFF", "#73D055FF", "#FDE725FF")
    
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
    #pal
    vir_pal <- c("#404788FF", "#2D708EFF", "#29AF7FFF", "#73D055FF", "#FDE725FF")
    
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
      scale_fill_manual(values =vir_pal)+
      theme_void()  +
      theme(legend.title = element_blank()) 
    #plot 
    va_pie
    
  })
  
  #Hampton age plot-------------------------------------------------
  var_hampAge <- reactive({
    input$HampAgeYearDrop
  })
  
  output$hamp_graph <- renderPlot({
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
    hamp_ages3 <- mutate(hamp_ages3, Labels =c("Under 18: 17 and Younger", "Young Adult: 18 to 29", "Adult: 30 to 44",                                       "Middle Age: 45 to 64","Senior: 65 and Older"))
    #ordering the age grpups
    hamp_ages3$Labels <- factor(hamp_ages3$Labels, levels=c("Under 18: 17 and Younger", "Young Adult: 18 to 29", "Adult: 30 to 44",
                                                            "Middle Age: 45 to 64","Senior: 65 and Older"))
    #Graph
    hamp_graph <- ggplot(hamp_ages3 , aes(x="", y=PctPop, fill=Labels)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) + 
      theme(
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size = 13)) +
      geom_text(aes(label = paste0(round(PctPop), "%")), position = position_stack(vjust=0.5), size=5, color = "white") +
      theme_void() +
      scale_fill_viridis_d()
    #plot
    hamp_graph
    
  })
  
  #Va age plot----------------------------------------------------
  var_VaAge <- reactive({
    input$VaAgeYearDrop
  })
  
  output$va_graph <- renderPlot({
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
    va_ages2 <- mutate(va_ages2, labels = c("Under 18: 17 and Younger", "Young Adult: 18 to 29", "Adult: 30 to 44",
                                            "Middle Age: 45 to 64","Senior: 65 and Older"))
    colnames(va_ages2) <- c("Estimate", "Total Population", "Percent of Population", "Labels")
    va_ages2[,4] <- factor(va_ages2[,4], levels = c("Under 18: 17 and Younger", "Young Adult: 18 to 29", "Adult: 30 to 44",
                                                    "Middle Age: 45 to 64","Senior: 65 and Older"))
    #Graph
    va_graph <- ggplot(va_ages2 , aes(x="", y=`Percent of Population`, fill=Labels)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) + 
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
      theme_void() +
      scale_fill_viridis_d()  
    #plot
    va_graph
    
  })
  
  
  #hampton counties map ------------------------------------------
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
    #Graph
    age_map <- ggplot(coordinates2) +
      geom_sf() +
      geom_sf_label(aes(label=Loc,geometry = geometry), label.padding = unit(.5, "mm"), size =4, nudge_x=0.05, nudge_y = 0.1) +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size=13)) +
      geom_scatterpie(aes(x=lon, y=lat, group=county, r =0.05), data=general_county_alt2,
                      cols=LETTERS[1:5]) + 
      scale_fill_viridis_d(labels = c("Under 18", "Young Adult: 18 to 29", "Adult: 30 to 44",
                                      "Middle Age: 45 to 64","Senior: 65 and Older")) 
    #plot
    age_map
 
  })
  
  
  #educational attainment plots working on it....................
  var_genEducationalAttainment <- reactive({
    input$genEdAttainmentYearDrop
  })
  
  output$genEdAttainmentPlots <- renderPlotly({
    if(var_genEducationalAttainment() == "2019") {
      generalEducationalAttainment2019 <- read.csv("data/TableS1501FiveYearEstimates/generalEducationalAttainment2019.csv")
      colnames(generalEducationalAttainment2019) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      va_tot_education_bar2019 <- generalEducationalAttainment2019  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Name)) + geom_col() +
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2019)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2019, tooltip=c("x", "y"))) %>% 
        layout(annotations = 
                 list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table S1501", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
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
        layout(annotations = 
                 list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table S1501", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
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
        layout(annotations = 
                 list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table S1501", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
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
        layout(annotations = 
                 list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table S1501", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
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
        layout(annotations = 
                 list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table S1501", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
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
        layout(annotations = 
                 list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table S1501", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
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
        layout(annotations = 
                 list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table S1501", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
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
        layout(annotations = 
                 list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table S1501", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
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
        layout(annotations = 
                 list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table S1501", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
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
        layout(annotations = 
                 list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table S1501", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
        )
    }
    
  })
  
  
  var_blackEducationalAttainment <- reactive({
    input$blackEdAttainmentYearDrop
  })
  
  
  output$blackEdAttainmentPlots  <- renderPlotly({
    
    if(var_blackEducationalAttainment() == "2019") {
      blackEducationalAttainment2019 <- read.csv("data/TableC15002BFiveYearEstimates/blackEducationalAttainment2019.csv")
      colnames(blackEducationalAttainment2019) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      va_tot_education_bar2019 <- blackEducationalAttainment2019  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Name)) + geom_col() +
        geom_bar(position = "dodge", stat = "identity") + 
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2019)",
                               y = "Percent (%)",
                               x = "Hampton Roads") +  theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
     va_tot_education_bar2019 #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
     hide_legend(ggplotly(va_tot_education_bar2019, tooltip=c("x", "y"))) %>% 
       layout(annotations = 
                list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table C15002B", 
                     showarrow = F, xref='paper', yref='paper', 
                     xanchor='right', yanchor='auto', xshift=0, yshift=0,
                     font=list(size=15, color="black"))
       )
    }
    
    else if(var_blackEducationalAttainment() == "2018") {
      blackEducationalAttainment2018 <- read.csv("data/TableC15002BFiveYearEstimates/blackEducationalAttainment2018.csv")
      colnames(blackEducationalAttainment2018) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      va_tot_education_bar2018 <- blackEducationalAttainment2018  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Name)) + geom_col() +
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2018)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2018, tooltip=c("x", "y"))) %>% 
        layout(annotations = 
                 list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table C15002B", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
        )
    }
    
    else if(var_blackEducationalAttainment() == "2017") {
      blackEducationalAttainment2017 <- read.csv("data/TableC15002BFiveYearEstimates/blackEducationalAttainment2017.csv")
      colnames(blackEducationalAttainment2017) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      va_tot_education_bar2017 <- blackEducationalAttainment2017  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Name)) + geom_col() +
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2017)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2017, tooltip=c("x", "y"))) %>% 
        layout(annotations = 
                 list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table C15002B", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
        )
    }
    
    else if(var_blackEducationalAttainment() == "2016") {
      blackEducationalAttainment2016 <- read.csv("data/TableC15002BFiveYearEstimates/blackEducationalAttainment2016.csv")
      colnames(blackEducationalAttainment2016) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      va_tot_education_bar2016 <- blackEducationalAttainment2016  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Name)) + geom_col() +
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2016)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2016, tooltip=c("x", "y"))) %>% 
        layout(annotations = 
                 list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table C15002B", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
        )
    }
    
    else if(var_blackEducationalAttainment() == "2015") {
      blackEducationalAttainment2015 <- read.csv("data/TableC15002BFiveYearEstimates/blackEducationalAttainment2015.csv")
      colnames(blackEducationalAttainment2015) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      va_tot_education_bar2015 <- blackEducationalAttainment2015  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Name)) + geom_col() +
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2015)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2015, tooltip=c("x", "y"))) %>% 
        layout(annotations = 
                 list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table C15002B", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
        )
    }
    
    else if(var_blackEducationalAttainment() == "2014") {
      blackEducationalAttainment2014 <- read.csv("data/TableC15002BFiveYearEstimates/blackEducationalAttainment2014.csv")
      colnames(blackEducationalAttainment2014) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      va_tot_education_bar2014 <- blackEducationalAttainment2014  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Name)) + geom_col() +
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2015)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2014, tooltip=c("x", "y"))) %>% 
        layout(annotations = 
                 list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table C15002B", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
        )
    }
    
    else if(var_blackEducationalAttainment() == "2013") {
      blackEducationalAttainment2013 <- read.csv("data/TableC15002BFiveYearEstimates/blackEducationalAttainment2013.csv")
      colnames(blackEducationalAttainment2013) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      va_tot_education_bar2013 <- blackEducationalAttainment2013  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Name)) + geom_col() +
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2013)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2013, tooltip=c("x", "y"))) %>% 
        layout(annotations = 
                 list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table C15002B", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
        )
    }
    
    else if(var_blackEducationalAttainment() == "2012") {
      blackEducationalAttainment2012 <- read.csv("data/TableC15002BFiveYearEstimates/blackEducationalAttainment2012.csv")
      colnames(blackEducationalAttainment2012) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      va_tot_education_bar2012 <- blackEducationalAttainment2012  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Name)) + geom_col() +
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2012)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2012, tooltip=c("x", "y"))) %>% 
        layout(annotations = 
                 list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table C15002B", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
        )
    }
    
    else if(var_blackEducationalAttainment() == "2011") {
      blackEducationalAttainment2011 <- read.csv("data/TableC15002BFiveYearEstimates/blackEducationalAttainment2011.csv")
      colnames(blackEducationalAttainment2011) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      va_tot_education_bar2011 <- blackEducationalAttainment2011  %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Name)) + geom_col() +
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2015)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2011, tooltip=c("x", "y"))) %>% 
        layout(annotations = 
                 list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table C15002B", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
        )
    }
    
    else if(var_blackEducationalAttainment() == "2010") {
      blackEducationalAttainment2010 <- read.csv("data/TableC15002BFiveYearEstimates/blackEducationalAttainment2010.csv")
      colnames(blackEducationalAttainment2010) <- c("Name", "Variable", "Bachelor or Higher as Highest Attainment %")
      va_tot_education_bar2010 <- blackEducationalAttainment2010 %>% 
        mutate(Name = str_remove(Name, "County, Virginia")) %>% 
        mutate(Name = str_remove(Name, "city, Virginia")) %>% 
        ggplot(aes(x = Name, y = `Bachelor or Higher as Highest Attainment %`, fill = Name)) + geom_col() +
        theme_minimal() + labs(title = "Bachelor's Degree or Higher as Highest Attainment (2010)",
                               y = "Percent (%)",
                               x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40)) +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(va_tot_education_bar2010, tooltip=c("x", "y"))) %>% 
        layout(annotations = 
                 list(x = 1, y = -0.4, text = "Source: ACS 5 Year Estimate Table C15002B", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
        )
    }
    
  })
  
  
  #teacher Race plots working on it....................
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
      labs(title = "", y = "Percentage (%)", x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40))  +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip=c("x", "y"))) %>% 
        layout(annotations = 
                 list(x = 1, y = -0.6, text = "Data Source: Virginia 2020-2021 Teacher Race Report", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
        )
    }
    
    else if (var_teacherRaces() == "Asian") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
           colnames(teacherByRace) <- c("Division Number", "Name", "Total Counts", "American Indian", "Asian", "Black", "Hispanic", "White","Hawaiian", "Two or More Races",  "Not Specified", "% of Black Teachers", "% of Asian Teachers", "% of Hispanic Teachers", "% of White Teachers", "% of American Indian Teachers", "% of Two Or More Races Teachers", "% of Hawaiian Teachers")
      teacherByRace <- teacherByRace  %>% 
        ggplot(aes(x = Name, y = `% of Asian Teachers`, fill = Name)) + geom_col() +
        labs(title = "", y = "Percentage (%)", x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40))  +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip=c("x", "y"))) %>% 
        layout(annotations = 
                 list(x = 1, y = -0.6, text = "Data Source: Virginia 2020-2021 Teacher Race Report", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
        )
    }
    
    else if (var_teacherRaces() == "White") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
           colnames(teacherByRace) <- c("Division Number", "Name", "Total Counts", "American Indian", "Asian", "Black", "Hispanic", "White","Hawaiian", "Two or More Races",  "Not Specified", "% of Black Teachers", "% of Asian Teachers", "% of Hispanic Teachers", "% of White Teachers", "% of American Indian Teachers", "% of Two Or More Races Teachers", "% of Hawaiian Teachers")
      teacherByRace <- teacherByRace  %>% 
        ggplot(aes(x = Name, y = `% of White Teachers`, fill = Name)) + geom_col() +
        labs(title = "", y = "Percentage (%)", x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40))  +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip=c("x", "y"))) %>% 
        layout(annotations = 
                 list(x = 1, y = -0.6, text = "Data Source: Virginia 2020-2021 Teacher Race Report", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
        )
    }
    
    else if (var_teacherRaces() == "Hispanic") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
           colnames(teacherByRace) <- c("Division Number", "Name", "Total Counts", "American Indian", "Asian", "Black", "Hispanic", "White","Hawaiian", "Two or More Races",  "Not Specified", "% of Black Teachers", "% of Asian Teachers", "% of Hispanic Teachers", "% of White Teachers", "% of American Indian Teachers", "% of Two Or More Races Teachers", "% of Hawaiian Teachers")
      teacherByRace <- teacherByRace  %>% 
        ggplot(aes(x = Name, y = `% of Hispanic Teachers`, fill = Name)) + geom_col() +
        labs(title = "", y = "Percentage (%)", x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40))  +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip=c("x", "y"))) %>% 
        layout(annotations = 
                 list(x = 1, y = -0.6, text = "Data Source: Virginia 2020-2021 Teacher Race Report", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
        )
    }
    
    else if (var_teacherRaces() == "American Indian") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
      colnames(teacherByRace) <- c("Division Number", "Name", "Total Counts", "American Indian", "Asian", "Black", "Hispanic", "White","Hawaiian", "Two or More Races",  "Not Specified", "% of Black Teachers", "% of Asian Teachers", "% of Hispanic Teachers", "% of White Teachers", "% of American Indian Teachers", "% of Two Or More Races Teachers", "% of Hawaiian Teachers")
      teacherByRace <- teacherByRace  %>% 
        ggplot(aes(x = Name, y = `% of American Indian Teachers`, fill = Name)) + geom_col() +
        labs(title = "", y = "Percentage (%)", x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40))  +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip=c("x", "y"))) %>% 
        layout(annotations = 
                 list(x = 1, y = -0.6, text = "Data Source: Virginia 2020-2021 Teacher Race Report", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
        )
    }
    
    else if (var_teacherRaces() == "Two or More Races") {
      teacherByRace <- read.csv("data/teacherByRacesBreakdown.csv")
      #renaming column names to make division name to name for readability purposes and also made sure it was consistent mapping of columns to the excel spreadsheet
      #original column names for reference: Division No.	Division Name	Total Counts	American Indian	Asian	Black	Hispanic	White	Hawaiian	Two or More Races	Not Specified	BlackProportions	AsianProportions	HispanicProportions	WhiteProportions	AmericanIndianProportions	TwoOrMoreRacesProportions	HawaiianProportions
           colnames(teacherByRace) <- c("Division Number", "Name", "Total Counts", "American Indian", "Asian", "Black", "Hispanic", "White","Hawaiian", "Two or More Races",  "Not Specified", "% of Black Teachers", "% of Asian Teachers", "% of Hispanic Teachers", "% of White Teachers", "% of American Indian Teachers", "% of Two Or More Races Teachers", "% of Hawaiian Teachers")
      teacherByRace <- teacherByRace  %>% 
        ggplot(aes(x = Name, y = `% of Two Or More Races Teachers`, fill = Name)) + geom_col() +
        labs(title = "", y = "Percentage (%)", x = "Hampton Roads") + theme(axis.text.x = element_text(angle = 40))  +  scale_color_viridis_d() +  scale_fill_viridis_d()
      #adding caption from ggplot does not transfer to plotly so have to load in with plotly separately
      hide_legend(ggplotly(teacherByRace, tooltip=c("x", "y"))) %>% 
        layout(annotations = 
                 list(x = 1, y = -0.6, text = "Data Source: Virginia 2020-2021 Teacher Race Report", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
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
        layout(annotations = 
                 list(x = 1, y = -0.6, text = "Data Source: Virginia 2020-2021 Teacher Race Report", 
                      showarrow = F, xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', xshift=0, yshift=0,
                      font=list(size=15, color="black"))
        )
    }
    
  }
)
  
  
  # VA suspension--------------------------------------------------------------
  var_suspension <- reactive({
    input$suspensionYearDrop
  })
  
  output$graph_va <- renderPlot({
    if(var_suspension() == "2018-2019"){
      year <- "2018-2019"
    }
    else if (var_suspension() == "AY 2017-2018") {
      year <- "AY 2017-2018"
    }
    else if (var_suspension() == "AY 2016-2017") {
      year <- "AY 2016-2017"
    }
    else if (var_suspension() == "AY 2015-2016") {
      year <- "AY 2015-2016"
    }
    else if (var_suspension() == "AY 2014-2015") {
      year <- "AY 2014-2015"
    }
    suspension_data <- read_excel("data/suspension/shortTermSuspension.xlsx")
    #using only  VA data for 2018-2019
    suspension_va <- suspension_data %>% filter(Location=="Virginia")%>% filter(TimeFrame == year)
    #VA percentage estimate for 2018-2019 (Black)
    va_blck <- suspension_va %>% filter(Race == "Black") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (Hispanic)
    va_hisp <- suspension_va %>% filter(Race == "Hispanic") %>% filter(DataFormat == "Percent")
    #VA percentage estimate for 2018-2019 (white)
    va_white <- suspension_va %>% filter(Race == "White") %>% filter(DataFormat == "Percent")
    #combining the three percentages(b;ack, hispanic, white)
    va_suspension_race <- rbind(va_blck[,6], va_hisp[,6], va_white[,6])
    va_suspension_race$Data <- as.numeric(va_suspension_race$Data)
    va_suspension_race <- mutate(va_suspension_race, Data = Data*100)
    va_suspension_race <- mutate(va_suspension_race, race = c("Black", "Hispanic", "White"))
    #Graph
    graph_va <- ggplot(data=va_suspension_race, aes(x=race, y=Data)) +
      geom_bar(stat="identity", fill ="#0072B2")+
      geom_text(aes(label=paste0(round(Data, digits = 2), "%")), vjust=1.6, color="white", size=10)+
      theme_minimal()+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size=20)) 
    #plot
    graph_va
    
  })
  
  # Black suspension map -----------------------------------------------------
  
  var_Bsuspension <- reactive({
    input$BsuspensionYearDrop
  })
  
  output$black_map <- renderPlot({
    if(var_Bsuspension() == "2018-2019"){
      year <- "2018-2019"
    }
    else if (var_Bsuspension() == "AY 2017-2018") {
      year <- "AY 2017-2018"
    }
    else if (var_Bsuspension() == "AY 2016-2017") {
      year <- "AY 2016-2017"
    }
    else if (var_Bsuspension() == "AY 2015-2016") {
      year <- "AY 2015-2016"
    }
    else if (var_Bsuspension() == "AY 2014-2015") {
      year <- "AY 2014-2015"
    }
    coord_data <- read_rds("data/suspension/coordinates.rds")
    coord_data <- st_transform(coord_data)
    coordinates1 <- coord_data %>% group_by(NAME) %>% slice(1)
    coordinates2 <- coordinates1[,6]
    city <- c("Chesapeake", "Franklin City", "Gloucester", "Hampton", "Isle of Wight", "James City", "Mathews", 
              "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton", "Suffolk", "Virginia Beach",
              "Williamsburg", "York")
    coordinates2 <- mutate(coordinates2, Location = city)
    suspension_counties <-filter(suspension_data, Location %in% city)
    #using percentages instead of number estimates (black)
    suspension_pct<- suspension_counties %>% filter(Race=="Black") %>%
      filter(DataFormat=="Percent")
    suspension_pct2 <- suspension_pct %>% filter(TimeFrame==year)
    #make a table w/ NA a S
    display_tbl <- suspension_pct2 %>% filter(Data %in% c("NA", "S"))
    display_tbl <- display_tbl[,c(2,6)]
    suspension_pct2$Data[suspension_pct2$Data=="NA"] <- 0
    suspension_pct2$Data[suspension_pct2$Data=="S"] <- 0
    #convert data column to numeric so we can multiply by 100
    suspension_pct2$Data <- sapply(suspension_pct2$Data, as.numeric)
    suspension_pct2 <- mutate(suspension_pct2, pct = Data *100)
    #adding geometry column(coordinates)
    suspension_pct3 <- merge(suspension_pct2, coordinates2, by = "Location")
    suspension_pct4 <- suspension_pct3[,c(1,7,8)]
    #add back the NA (S will be NA. We have a table to clarify)
    suspension_pct4$pct <- na_if(suspension_pct4$pct,0.00000)
    as.numeric(suspension_pct4$pct, na.rm = TRUE)
    #Graph
    graph_blck <-
      suspension_pct4 %>%
      ggplot() +
      geom_sf(aes(fill = pct, geometry = geometry))+
      geom_sf_label(aes(label=Location,geometry = geometry), label.padding = unit(.5, "mm"), size =4) +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            legend.title = element_blank(),
            legend.text = element_text(size=13)) +
      scale_fill_gradient(high = "#132B43",
                          low = "#56B1F7",
                          space = "Lab",
                          na.value = "grey50",
                          guide = "colourbar",
                          aesthetics = "fill") +
    guides(colour=guide_legend("No data", override.aes=list(colour="grey50")))
    #display table
    na_rows <- display_tbl %>% filter(Data == "NA")
    supr_rows <- display_tbl %>% filter(Data == "S")
    supr_rows <- mutate(supr_rows, Data = "Suppressed")
    display_tbl_final <- rbind(na_rows, supr_rows)
    table_plot <- tableGrob(display_tbl_final)
    #plot together
    black_map <- grid.arrange(graph_blck, table_plot, nrow=2, heights=c(3,1))
    black_map
    })
  
  # suspension for black and white (counties) ---------------------------------
  var_BWsuspension <- reactive({
    input$BWsuspensionYearDrop
  })
  
  output$BW_map <- renderPlot({
    if(var_Bsuspension() == "2018-2019"){
      year <- "2018-2019"
    }
    else if (var_BWsuspension() == "AY 2017-2018") {
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
    suspension_data <- read_excel("data/suspension/shortTermSuspension.xlsx")
    city <- c("Chesapeake", "Franklin City", "Gloucester", "Hampton", "Isle of Wight", "James City", "Mathews", 
              "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton", "Suffolk", "Virginia Beach",
              "Williamsburg", "York")
    suspension_counties <-filter(suspension_data, Location %in% city)
    pct_white<- suspension_counties %>% filter(Race=="White") %>%
      filter(DataFormat=="Percent")
    pct_white2 <- pct_white %>% filter(TimeFrame==year)
    #putting NAs and Ss in a table
    display_tbl_white <- suspension_pct2 %>% filter(Data %in% c("NA", "S"))
    display_tbl_white2<- display_tbl_white[,c(2,6)]
    pct_white2$Data[pct_white2$Data=="NA"] <- 0
    pct_white2$Data[pct_white2$Data=="S"] <- 0
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
    #make a table w/ NA a S
    display_tbl_black <- suspension_pct2 %>% filter(Data %in% c("NA", "S"))
    display_tbl_black2 <- display_tbl_black[,c(2,6)]
    suspension_pct2$Data[suspension_pct2$Data=="NA"] <- 0
    suspension_pct2$Data[suspension_pct2$Data=="S"] <- 0
    #convert data column to numeric so we can multiply by 100
    suspension_pct2$Data <- sapply(suspension_pct2$Data, as.numeric)
    suspension_pct2 <- mutate(suspension_pct2, pct = Data *100)
    suspension_pct2$pct <- na_if(suspension_pct2$pct,0.00000)
    as.numeric(suspension_pct2$pct, na.rm = TRUE)
    pct_blck <-suspension_pct2[,c(2,7)]
    colnames(pct_blck) <- c("Location", "Percentage of Students (%)")
    sus <- rbind(pct_blck,pct_white3)
    sus <- mutate(sus, race = c(rep("Black Students",16), rep("White Students", 16)))
    #bar graph
    suspension_counties_plot <-
      ggplot(sus , aes(Location, y=`Percentage of Students (%)`, fill=race)) +
      geom_bar(stat="identity", position=position_dodge())+
      geom_text(aes(label=paste0(round(`Percentage of Students (%)`, digits=2), "%")), vjust=1.5, color="white",
                position = position_dodge(0.9), size=2)+
      theme_minimal() + 
      theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'), 
            legend.key.height = unit(0.3, 'cm'), 
            legend.key.width = unit(0.3, 'cm'), 
            legend.title = element_blank(),
            legend.text = element_text(size=14),
            axis.text=element_text(size=15),
            axis.text.x = element_text(size=8, face="bold"),
            axis.title=element_text(size=17),
            axis.title.x=element_blank()) +
      scale_fill_manual(values=c("#D55E00","#0072B2")) +
      labs(x = "Location") 
    #combining the tables
    display_table <- rbind(display_tbl_white2, display_tbl_black2)
    na_rows <- display_table %>% filter(Data == "NA")
    supr_rows <- display_table %>% filter(Data == "S")
    supr_rows <- mutate(supr_rows, Data = "Suppressed")
    display_table_final <- rbind(na_rows, supr_rows)
    table_plot <- tableGrob(display_table_final)
    #plot together
    BW_map <- grid.arrange(suspension_counties_plot, table_plot, nrow=2, heights=c(3,1))
    BW_map
  })
  
  
  
  # Unemployment Rate -------------------------------------------------------
  
  
  var_unemploymentRate <- reactive({
    input$UnemploymentRateYearDrop
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
        geom_bar(position = "dodge", stat = "identity") +
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
        geom_hline(yintercept = va_unemp_11$estimate, linetype="dashed", color = "red", show.legend = TRUE) 
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
  
  
  # Homeownership Map -------------------------------------------------------
  var_hmown <- reactive({
    input$HomeOwnSlider
  })
  
  
  output$homeownership_map <- renderLeaflet({
    if(var_hmown() == "2019") {
      b_hm_19 <- read_rds("data/TableS2502FiveYearEstimates/bhmown2019.rds")
      tot_hm_19 <- read_rds("data/TableS2502FiveYearEstimates/tothmown2019.rds")
      pal <- colorNumeric(palette = "viridis", domain = b_hm_19$Percent, reverse = TRUE)
      b_hmown_leaf_19 <- b_hm_19 %>%
        leaflet(options = leafletOptions(minZoom = 8.5)) %>% 
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
        leaflet(options = leafletOptions(minZoom = 8.5)) %>% 
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
        leaflet(options = leafletOptions(minZoom = 8.5)) %>% 
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
        leaflet(options = leafletOptions(minZoom = 8.5)) %>% 
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
        leaflet(options = leafletOptions(minZoom = 8.5)) %>% 
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
        leaflet(options = leafletOptions(minZoom = 8.5)) %>% 
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
        leaflet(options = leafletOptions(minZoom = 8.5)) %>% 
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
        leaflet(options = leafletOptions(minZoom = 8.5)) %>% 
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
        leaflet(options = leafletOptions(minZoom = 8.5)) %>% 
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
        leaflet(options = leafletOptions(minZoom = 8.5)) %>% 
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
  
  
  #Median Income plots: Working on it --------------------------------
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
  
  #median income line plots -------------------------------------------------
  
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
      ggtitle("Virginia") +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x = element_blank()) +
      theme(legend.title = element_blank()) +
      labs(y ="Median Income (US Dollars") +
      theme_minimal() +
      scale_color_manual(values = c("#D55E00", "#0072B2"))
    
    #hamp line graph
    hamp_years <- income_years %>% filter(Location == "Hampton Roads")
    #graph
    hamp_line <- ggplot(hamp_years, aes(x=Year, y=`Median Income (US Dollars)`, group = Demographic, color = Demographic)) + 
      geom_line(position = "identity", size =1.3 ) +
      scale_color_manual(values = c("#D55E00", "#0072B2")) +
      ggtitle("Hampton Roads")+
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.x=element_blank(),
            legend.title = element_blank(),
            legend.position = "none") +
      labs(y ="Median Income (US Dollars)") +
      theme_minimal() 
    
    medianTimeGraph  <- grid.arrange(hamp_line, va_line, ncol=2)
    medianTimeGraph 
  })
  
  #poverty Rates in VA and Hampton Roads-------------------------------------
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
  
  #hampton counties poverty -------------------------------------------------------------
  var_povertyCount <- reactive ({
    input$PovertyCountYearDrop
  })
  
  output$counties_pov <- renderPlot({
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
      pov_pct3 <- data.frame(pov_pct2[,1]) 
      colnames(pov_pct3) <- "Ratio"
      hamp_pct2 <- rbind(pov_pct3, hamp_comb)
      hamp_pct2 <- mutate(hamp_pct2, Location = c(rep("Virginia", 2), rep("Hampton Roads", 2), rep(c("Chesapeake", "Franklin", "Gloucester", "Hampton", "Isle of Wight", "James City",
                                                                                                     "Mathews", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton",
                                                                                                     "Suffolk", "Virginia Beach", "Williamsburg", "York"),2)))
      hamp_pct2 <- mutate(hamp_pct2, Demographic = c(rep(c("Total Population", "Black Population"),2), rep("Total Population", 16),
                                                     rep("Black Population",16)))
      colnames(hamp_pct2) <- c("Ratio (%)", "Location", "Demographic")
      #Graph for just the Hampton Counties
      hamp_pct3 <- hamp_pct2[5:36,]
      colnames(hamp_pct3) <- c("Percentage (%)", "Location", "Demographic")
      counties_pov <-  ggplot(hamp_pct3, aes(Location, y=`Percentage (%)`, fill=Demographic)) +
        geom_bar(stat="identity", position=position_dodge())+
        geom_text(aes(label=paste0(round(`Percentage (%)`, digits=2), "%")), vjust=1.5, color="white",
                  position = position_dodge(0.9), size=3)+
        theme_minimal() +
        scale_fill_manual(values=c("#D55E00","#0072B2")) +
        theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'), 
              legend.key.height = unit(0., 'cm'), 
              legend.key.width = unit(0.5, 'cm'), 
              legend.title = element_blank(),
              legend.text = element_text(size=14),
              axis.text=element_text(size=15),
              axis.text.x = element_text(size=8, face="bold"),
              axis.title=element_text(size=17),
              axis.title.x=element_blank()) 
      #plot
      counties_pov 
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
      pov_pct3 <- data.frame(pov_pct2[,1]) 
      colnames(pov_pct3) <- "Ratio"
      hamp_pct2 <- rbind(pov_pct3, hamp_comb)
      hamp_pct2 <- mutate(hamp_pct2, Location = c(rep("Virginia", 2), rep("Hampton Roads", 2), rep(c("Chesapeake", "Franklin", "Gloucester", "Hampton", "Isle of Wight", "James City",
                                                                                                     "Mathews", "Newport News", "Norfolk", "Poquoson", "Portsmouth", "Southampton",
                                                                                                     "Suffolk", "Virginia Beach", "Williamsburg", "York"),2)))
      hamp_pct2 <- mutate(hamp_pct2, Demographic = c(rep(c("Total Population", "Black Population"),2), rep("Total Population", 16),
                                                     rep("Black Population",16)))
      colnames(hamp_pct2) <- c("Ratio (%)", "Location", "Demographic")
      #Graph for just the Hampton Counties
      hamp_pct3 <- hamp_pct2[5:36,]
      colnames(hamp_pct3) <- c("Percentage (%)", "Location", "Demographic")
      
      counties_pov <-  ggplot(hamp_pct3, aes(Location, y=`Percentage (%)`, fill=Demographic)) +
        geom_bar(stat="identity", position=position_dodge())+
        geom_text(aes(label=paste0(round(`Percentage (%)`, digits=2), "%")), vjust=1.5, color="white",
                  position = position_dodge(0.9), size=3)+
        theme_minimal() +
        scale_fill_manual(values=c("#D55E00","#0072B2")) +
        theme(plot.title = element_text(hjust = 0.5, size=25), legend.key.size = unit(1, 'cm'), 
              legend.key.height = unit(1, 'cm'), 
              legend.key.width = unit(1, 'cm'), 
              legend.title = element_blank(),
              legend.text = element_text(size=14),
              axis.text=element_text(size=15),
              axis.text.x = element_text(size=10, face="bold"),
              axis.title=element_text(size=17),
              axis.title.x=element_blank()) 
      #plot
      counties_pov
    }
    
    
  })

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # socio plots: done -----------------------------------------------------
  
  var <- reactive({
    input$sociodrop
  })
  #age65
  output$socioplot <- renderLeaflet({
    if(var() == "age65") {
      
      pal <- colorQuantile("Blues", domain = socdem_block$age65, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population age 65 or over:</strong>",
              round(socdem_block$age65, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$age65), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$age65),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
      #under18
    }else if(var() == "under18"){
      pal <- colorQuantile("Blues", domain = socdem_block$under18, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population age 18 or under: </strong>",
              round(socdem_block$under18, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$under18), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$under18),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
      #population-tract
    }else if(var() == "totalpop_trct"){
      pal <- colorQuantile("Blues", domain = socdem_tract$totalpop_trct, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>Total population: </strong>",
              formatC(socdem_tract$totalpop_trct, format = "f", big.mark =",", digits = 0)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$totalpop_trct), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$totalpop_trct),
                  title = "Total Population<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 0), " &ndash; ", round(cuts[-1], 0), ")")
                  })
      #population-block group
    }else if(var() == "totalpop_bgrp"){
      pal <- colorQuantile("Blues", domain = socdem_block$totalpop_bgrp, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>Total population: </strong>",
              formatC(socdem_block$totalpop_bgrp, format = "f", big.mark =",", digits = 0)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$totalpop_bgrp), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$totalpop_bgrp),
                  title = "Total Population<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 0), " &ndash; ", round(cuts[-1], 0), ")")
                  })
    }else if(var() == "black"){
      pal <- colorQuantile("Blues", domain = socdem_block$black, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population Black: </strong>",
              round(socdem_block$black, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$black), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$black),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "noba"){
      pal <- colorQuantile("Blues", domain = socdem_block$noba, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population without BA degree: </strong>",
              round(socdem_block$noba, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$noba), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$noba),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "unempl"){
      pal <- colorQuantile("Blues", domain = socdem_block$unempl, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population in labor force unemployed: </strong>",
              round(socdem_block$unempl, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$unempl), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$unempl),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "nohealthins2"){
      pal <- colorQuantile("Blues", domain = socdem_block$nohealthins2, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population without health insurance: </strong>",
              round(socdem_block$nohealthins2, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$nohealthins2), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$nohealthins2),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "snap"){
      pal <- colorQuantile("Blues", domain = socdem_block$snap, probs = seq(0, 1, length = 6), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_block$NAME.y,
              "<br />",
              "<strong>% Population receiving public assistance or SNAP benefits: </strong>",
              round(socdem_block$snap, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_block, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_block$snap), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_block$snap),
                  title = "Percent by<br>Quintile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "inpov"){
      pal <- colorQuantile("Blues", domain = socdem_tract$inpov, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% Population in poverty: </strong>",
              round(socdem_tract$inpov, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$inpov), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$inpov),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "hispanic"){
      pal <- colorQuantile("Blues", domain = socdem_tract$hispanic, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% Population Hispanic or Latino: </strong>",
              round(socdem_tract$hispanic, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$hispanic), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$hispanic),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var() == "privateins"){
      pal <- colorQuantile("Blues", domain = socdem_tract$privateins, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% Population with private health insurance: </strong>",
              round(socdem_tract$privateins, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$privateins), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$privateins),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else{
      pal <- colorQuantile("Blues", domain = socdem_tract$publicins, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              socdem_tract$NAME.y,
              "<br />",
              "<strong>% Population with public health insurance: </strong>",
              round(socdem_tract$publicins, 2)),
        htmltools::HTML
      )
      
      leaflet(data = socdem_tract, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(socdem_tract$publicins), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(socdem_tract$publicins),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  
  # old plots - snap -----------------------------------------------
  var_old <- reactive({
    input$olddrop
  })
  var_hh <- reactive({
    input$hhdrop
  })
  output$oldplot <- renderLeaflet({
    # healthins wasn't coded properly so it's just all zeroes
    if(var_old() == "visdiff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$visdiff,
                     "_f" = olderadults$visdiff_f,
                     "_m" = olderadults$visdiff_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with vision difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "ambdiff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$ambdiff,
                     "_f" = olderadults$ambdiff_f,
                     "_m" = olderadults$ambdiff_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with ambulatory difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "cogdiff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$cogdiff,
                     "_f" = olderadults$cogdiff_f,
                     "_m" = olderadults$cogdiff_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with cognitive difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "carediff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$carediff,
                     "_f" = olderadults$carediff_f,
                     "_m" = olderadults$carediff_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with self-care difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "ildiff") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$ildiff,
                     "_f" = olderadults$ildiff_f,
                     "_m" = olderadults$ildiff_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with independent living difficulties: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "disab") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$disab,
                     "_f" = olderadults$disab_f,
                     "_m" = olderadults$disab_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults with any disability: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_old() == "inpov") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$inpov,
                     "_f" = olderadults$inpov_f,
                     "_m" = olderadults$inpov_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults in poverty: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else 
      # if(var_old() == "labfor")
    {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$labfor,
                     "_f" = olderadults$labfor_f,
                     "_m" = olderadults$labfor_m)
      
      pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Older adults in the labor force: </strong>",
              round(data, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(data), 
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(data),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  output$householdplot <- renderLeaflet({
    if(var_hh() == "hhsixty_total") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_total,
                     "_f" = olderadults$hhsixty_total,
                     "_m" = olderadults$hhsixty_total)
      
      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_total, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Housholds with a 60+ member: </strong>",
              round(olderadults$hhsixty_total, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_total),
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_total),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_hh() == "hhsixty_fhh") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_fhh,
                     "_f" = olderadults$hhsixty_fhh,
                     "_m" = olderadults$hhsixty_fhh)
      
      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_fhh, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Housholds with a female 60+ member:</strong>",
              round(olderadults$hhsixty_fhh, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_fhh),
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_fhh),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_hh() == "hhsixty_mhh") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_mhh,
                     "_f" = olderadults$hhsixty_mhh,
                     "_m" = olderadults$hhsixty_mhh)
      
      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_mhh, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Housholds with a male 60+ member: </strong>",
              round(olderadults$hhsixty_mhh, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_mhh),
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_mhh),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else if(var_hh() == "hhsixty_nonfam") {
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_nonfam,
                     "_f" = olderadults$hhsixty_nonfam,
                     "_m" = olderadults$hhsixty_nonfam)
      
      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_nonfam, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Single housholds with a 60+ member: </strong>",
              round(olderadults$hhsixty_nonfam, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_nonfam),
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_nonfam),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }else{
      data <- switch(input$oldspecdrop,
                     "Total" = olderadults$hhsixty_marr,
                     "_f" = olderadults$hhsixty_marr,
                     "_m" = olderadults$hhsixty_marr)
      
      pal <- colorQuantile("Blues", domain = olderadults$hhsixty_marr, probs = seq(0, 1, length = 5), right = TRUE)
      
      labels <- lapply(
        paste("<strong>Area: </strong>",
              olderadults$NAME.y,
              "<br />",
              "<strong>% Married households with a 60+ member: </strong>",
              round(olderadults$hhsixty_marr, 2)),
        htmltools::HTML
      )
      
      leaflet(data = olderadults, options = leafletOptions(minZoom = 10))%>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(fillColor = ~pal(olderadults$hhsixty_marr),
                    fillOpacity = 0.7, 
                    stroke = TRUE, weight = 0.5, color = "#202020",
                    label = labels,
                    labelOptions = labelOptions(direction = "bottom",
                                                style = list(
                                                  "font-size" = "12px",
                                                  "border-color" = "rgba(0,0,0,0.5)",
                                                  direction = "auto"
                                                ))) %>%
        addLegend("bottomleft",
                  pal = pal,
                  values =  ~(olderadults$hhsixty_marr),
                  title = "Percent by<br>Quartile Group",
                  opacity = 0.7,
                  labFormat = function(type, cuts, p) {
                    n = length(cuts)
                    paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                  })
    }
  })
  
  
  # data and measures table: done ----------------------------------------
  var_topic <- reactive({
    input$topic
  })
  output$datatable <- renderDataTable({
    if(var_topic() == "All Measures"){
      table <- as.data.frame(measures_table)
      datatable(table, rownames = FALSE, options = list(pageLength = 15)) %>% formatStyle(0, target = 'row', lineHeight = '80%')
    }
    else{
      data <- switch(input$topic,
                     "Connectivity Measures" = "connectivity",
                     "Sociodemographic Measures" = "demographics",
                     "Food Access Measures" = "food access",
                     "Health Care Access Measures" = "health",
                     "Older Adult Population Measures" = "older adults")
      table <- subset(measures_table, Topic == data)
      table <- as.data.frame(table)
      datatable(table, rownames = FALSE, options = list(pageLength = 15)) %>% formatStyle(0, target = 'row', lineHeight = '80%')
    }
  })
  
  # device: done ---------------------------------------------------------
  
  output$deviceplot <- renderLeaflet({
    data <- switch(input$devicedrop,
                   "nocomputer" = connectivity$nocomputer,
                   "laptop" = connectivity$laptop,
                   "smartphone" = connectivity$smartphone,
                   "tablet" = connectivity$tablet, 
                   "nointernet" = connectivity$nointernet,
                   "satellite" = connectivity$satellite,
                   "cellular" = connectivity$cellular,
                   "broadband" = connectivity$broadband)
    
    device_spec <- switch(input$devicedrop,
                          "nocomputer" = "no computer",
                          "laptop" = "laptop",
                          "smartphone" = "smartphone",
                          "tablet" = "tablet", 
                          "nointernet" = "no internet access",
                          "satellite" = "satellite internet",
                          "cellular" = "cellular internet",
                          "broadband" = "broadband internet")
    
    pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 6), right = TRUE)
    
    labels <- lapply(
      paste("<strong>Area: </strong>",
            connectivity$NAME.y,
            "<br />",
            "<strong>% Households with",
            device_spec,
            "access: </strong>",
            round(data, 2)),
      htmltools::HTML
    )
    
    leaflet(data = connectivity, options = leafletOptions(minZoom = 10))%>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(data), 
                  fillOpacity = 0.7, 
                  stroke = TRUE, weight = 0.5, color = "#202020",
                  label = labels,
                  labelOptions = labelOptions(direction = "bottom",
                                              style = list(
                                                "font-size" = "12px",
                                                "border-color" = "rgba(0,0,0,0.5)",
                                                direction = "auto"
                                              ))) %>%
      addLegend("bottomleft",
                pal = pal,
                values =  ~(data),
                title = "Percent by<br>Quintile Group",
                opacity = 0.7,
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                })
  })
  
  
  # wifi: done -----------------------------------------------------------
  
  # Iso selector
  output$wifiplot <- renderLeaflet({
    colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
    
    wifi_iso10 <- switch(input$wifidrop,
                         "Meadows of Dan Elementary School" = wifi_iso_10_1,
                         "Woolwine Elementary School" = wifi_iso_10_2,
                         "Patrick Springs Primary School" = wifi_iso_10_3,
                         "Blue Ridge Elementary School" = wifi_iso_10_4,
                         "Patrick County High School" = wifi_iso_10_5,
                         "Stuart Elementary School" = wifi_iso_10_6,
                         "Patrick County Branch Library" = wifi_iso_10_7,
                         "Hardin Reynolds Memorial School" = wifi_iso_10_8,
                         "Stuart Baptist Church" = wifi_iso_10_9,                       
                         "Patrick Henry Community College Stuart Campus" = wifi_iso_10_10)
    
    wifi_iso15 <- switch(input$wifidrop,
                         "Meadows of Dan Elementary School" = wifi_iso_15_1,
                         "Woolwine Elementary School" = wifi_iso_15_2,
                         "Patrick Springs Primary School" = wifi_iso_15_3,
                         "Blue Ridge Elementary School" = wifi_iso_15_4,
                         "Patrick County High School" = wifi_iso_15_5,
                         "Stuart Elementary School" = wifi_iso_15_6,
                         "Patrick County Branch Library" = wifi_iso_15_7,
                         "Hardin Reynolds Memorial School" = wifi_iso_15_8,
                         "Stuart Baptist Church" = wifi_iso_15_9,                       
                         "Patrick Henry Community College Stuart Campus" = wifi_iso_15_10)
    
    data <- switch(input$wifidrop,
                   "Meadows of Dan Elementary School" = 1,
                   "Woolwine Elementary School" = 2,
                   "Patrick Springs Primary School" = 3,
                   "Blue Ridge Elementary School" = 4,
                   "Patrick County High School" = 5,
                   "Stuart Elementary School" = 6,
                   "Patrick County Branch Library" = 7,
                   "Hardin Reynolds Memorial School" = 8,
                   "Stuart Baptist Church" = 9,                       
                   "Patrick Henry Community College Stuart Campus" = 10)
    
    labels <- lapply(
      paste("<strong>Name: </strong>",
            wifi_latlong[data, ]$name,
            "<br />",
            "<strong>Address:</strong>",
            wifi_latlong[data, ]$fulladdress,
            "<br />",
            "<strong>Notes:</strong>",
            wifi_latlong[data, ]$notes),
      htmltools::HTML
    )
    
    m1 <- leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential, 
                 fillColor = colors[5],
                 fillOpacity = .8, 
                 stroke = FALSE, 
                 group = "Residential Properties") %>%
      addPolygons(data = wifi_iso10, 
                  fillColor = colors[1],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrone") %>%
      addPolygons(data = wifi_iso15,
                  fillColor = colors[2],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrone") %>%
      addMarkers(data = wifi_latlong, ~longitude[data], ~latitude[data],
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto")))  %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("10 Minute Isochrone",
                          "15 Minute Isochrone",
                          "Residential Properties"),
        options = layersControlOptions(collapsed = FALSE))
    m1 
  })
  
  # Coverage table
  output$wifitable <- renderTable({
    data <- switch(input$wifidrop,
                   "Meadows of Dan Elementary School" = 1,
                   "Woolwine Elementary School" = 2,
                   "Patrick Springs Primary School" = 3,
                   "Blue Ridge Elementary School" = 4,
                   "Patrick County High School" = 5,
                   "Stuart Elementary School" = 6,
                   "Patrick County Branch Library" = 7,
                   "Hardin Reynolds Memorial School" = 8,
                   "Stuart Baptist Church" = 9,                       
                   "Patrick Henry Community College Stuart Campus" = 10)
    
    table <- read.csv(paste0("data/isochrones/tables/wifi_iso_table_",data,".csv"))
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  # Wifi deserts
  output$allwifi <- renderLeaflet({
    
    labels <- lapply(
      paste("<strong>Name: </strong>",
            wifi_latlong$name,
            "<br />",
            "<strong>Address:</strong>",
            wifi_latlong$fulladdress,
            "<br />",
            "<strong>Notes:</strong>",
            wifi_latlong$notes),
      htmltools::HTML
    )
    
    leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential, 
                 fillColor = colors[5],
                 fillOpacity = .5, 
                 stroke = FALSE, 
                 group = "Residential Properties") %>%
      addPolygons(data = wifi_iso_10_1, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_2, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_3, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_4, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_5, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_6, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_7, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_8, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_10_9, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_1, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_2, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_3, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_4, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_5, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_6, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_7, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_8, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_9, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = wifi_iso_15_10, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addMarkers(data = wifi_latlong,
                 group = "Free Wi-Fi Locations",
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto")))  %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("Free Wi-Fi Locations",
                          "Residential Properties"),
        baseGroups = c("10 Minute Isochrones",
                       "15 Minute Isochrones"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  output$allwifitable <- renderTable({
    table <- read.csv("data/isochrones/tables/wifi_iso_table.csv")
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  # ems: done ------------------------------------------------------------
  
  output$emsplot <- renderLeaflet({
    colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
    
    ems_iso8 <- switch(input$emsdrop,
                       "STUART VOLUNTEER FIRE DEPARTMENT" = ems_iso_8_1,
                       "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = ems_iso_8_2,                                                         
                       "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = ems_iso_8_3,                                                                   
                       "VESTA RESCUE SQUAD" = ems_iso_8_4,                                                                                           
                       "ARARAT RESCUE SQUAD" = ems_iso_8_5,                                                                                          
                       "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = ems_iso_8_6,
                       "JEB STUART RESCUE SQUAD" = ems_iso_8_7,                                                                                      
                       "SMITH RIVER RESCUE SQUAD" = ems_iso_8_8,                                                                                     
                       "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = ems_iso_8_9)
    
    ems_iso10 <- switch(input$emsdrop,
                        "STUART VOLUNTEER FIRE DEPARTMENT" = ems_iso_10_1,
                        "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = ems_iso_10_2,                                                         
                        "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = ems_iso_10_3,                                                                   
                        "VESTA RESCUE SQUAD" = ems_iso_10_4,                                                                                           
                        "ARARAT RESCUE SQUAD" = ems_iso_10_5,                                                                                          
                        "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = ems_iso_10_6,
                        "JEB STUART RESCUE SQUAD" = ems_iso_10_7,                                                                                      
                        "SMITH RIVER RESCUE SQUAD" = ems_iso_10_8,                                                                                     
                        "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = ems_iso_10_9)
    
    ems_iso12 <- switch(input$emsdrop,
                        "STUART VOLUNTEER FIRE DEPARTMENT" = ems_iso_12_1,
                        "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = ems_iso_12_2,                                                         
                        "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = ems_iso_12_3,                                                                   
                        "VESTA RESCUE SQUAD" = ems_iso_12_4,                                                                                           
                        "ARARAT RESCUE SQUAD" = ems_iso_12_5,                                                                                          
                        "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = ems_iso_12_6,
                        "JEB STUART RESCUE SQUAD" = ems_iso_12_7,                                                                                      
                        "SMITH RIVER RESCUE SQUAD" = ems_iso_12_8,                                                                                     
                        "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = ems_iso_12_9)
    
    data <- switch(input$emsdrop,
                   "STUART VOLUNTEER FIRE DEPARTMENT" = 1,
                   "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = 2,                                                         
                   "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = 3,                                                                   
                   "VESTA RESCUE SQUAD" = 4,                                                                                           
                   "ARARAT RESCUE SQUAD" = 5,                                                                                          
                   "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = 6,
                   "JEB STUART RESCUE SQUAD" = 7,                                                                                      
                   "SMITH RIVER RESCUE SQUAD" = 8,                                                                                     
                   "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = 9)
    
    labels <- lapply(
      paste("<strong>Name: </strong>",
            str_to_title(ems[data, ]$NAME),
            "<br />",
            "<strong>Address:</strong>",
            str_to_title(ems[data, ]$ADDRESS), ",", str_to_title(ems[data, ]$CITY), ", VA", ems[data, ]$ZIP,
            "<br />",
            "<strong>Type:</strong>",
            str_to_title(ems[data, ]$NAICSDESCR)),
      htmltools::HTML
    )
    
    m1 <- leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential, 
                 fillColor = colors[5],
                 fillOpacity = .8, 
                 stroke = FALSE, 
                 group = "Residential Properties") %>%
      addPolygons(data = ems_iso8, 
                  fillColor = colors[1],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrone") %>%
      addPolygons(data = ems_iso10,
                  fillColor = colors[2],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrone") %>%
      addPolygons(data = ems_iso12,
                  fillColor = colors[2],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrone") %>%
      addMarkers(data = ems, ~LONGITUDE[data], ~LATITUDE[data],
                 group = "EMS Locations",
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto"))) %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("8 Minute Isochrone",
                          "10 Minute Isochrone",
                          "12 Minute Isochrone",
                          "Residential Properties"),
        options = layersControlOptions(collapsed = FALSE))
    m1 
  })
  
  output$emstable <- renderTable({
    data <- switch(input$emsdrop,
                   "STUART VOLUNTEER FIRE DEPARTMENT" = 1,
                   "MOOREFIELD STORE VOLUNTEER FIRE DEPARTMENT" = 2,                                                         
                   "BLUE RIDGE VOLUNTEER RESCUE SQUAD" = 3,                                                                   
                   "VESTA RESCUE SQUAD" = 4,                                                                                           
                   "ARARAT RESCUE SQUAD" = 5,                                                                                          
                   "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 1 - HEADQUARTERS" = 6,
                   "JEB STUART RESCUE SQUAD" = 7,                                                                                      
                   "SMITH RIVER RESCUE SQUAD" = 8,                                                                                     
                   "COLLINSTOWN - CLAUDVILLE - DRYPOND - FIVE FORKS VOLUNTEER FIRE AND RESCUE DEPARTMENT STATION 2" = 9)
    
    
    table <- read.csv(paste0("data/isochrones/tables/ems_iso_table_",data,".csv"))
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  # EMS deserts
  output$allems <- renderLeaflet({
    
    labels <- lapply(
      paste("<strong>Name: </strong>",
            str_to_title(ems$NAME),
            "<br />",
            "<strong>Address:</strong>",
            paste0(str_to_title(ems$ADDRESS), ", ", str_to_title(ems$CITY), ", VA ", ems$ZIP),
            "<br />",
            "<strong>Type:</strong>",
            str_to_title(ems$NAICSDESCR)),
      htmltools::HTML
    )
    
    leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential, 
                 fillColor = colors[5],
                 fillOpacity = .5, 
                 stroke = FALSE, 
                 group = "Residential Properties") %>%
      addPolygons(data = ems_iso_8_1, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_2, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_3, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_4, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_5, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_6, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_7, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_8, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_8_9, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "8 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_1, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_2, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_3, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_4, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_5, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_6, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_7, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_8, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_10_9, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_1, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_2, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_3, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_4, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_5, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_6, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_7, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_8, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addPolygons(data = ems_iso_12_9, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "12 Minute Isochrones") %>%
      addMarkers(data = ems,
                 group = "EMS Locations",
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto"))) %>%
      addLayersControl(
        position = "topright",
        baseGroups = c("8 Minute Isochrones",
                       "10 Minute Isochrones",
                       "12 Minute Isochrones"),
        overlayGroups = c("EMS Locations",
                          "Residential Properties"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  output$allemstable <- renderTable({
    table <- read.csv("data/isochrones/tables/ems_iso_table.csv")
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  
  # usda - lahunv10share  -----------------------------------------------------------
  var_usda <- reactive({
    input$usdadrop
  })
  output$usdaplot <- renderLeaflet({
    data <- switch(input$usdadrop,
                   "lakids1share" = usda$lakids1share,
                   "lakids10share" = usda$lakids10share,
                   "lalowi1share" = usda$lalowi1share,
                   "lalowi10share" = usda$lalowi10share,
                   "lapop1share" = usda$lapop1share,  
                   "lapop10share" = usda$lapop10share,
                   "laseniors1share" = usda$laseniors1share,
                   "laseniors10share" = usda$laseniors10share)
    
    usda_spec <- switch(input$usdadrop,
                        "lakids1share" = "low food access for children at 1 mile",
                        "lakids10share" = "low food access for children at 10 miles",
                        "lalowi1share" = "low food access for low income population at 1 mile",
                        "lalowi10share" = "low food access for low income population at 10 miles",
                        "lapop1share" = "low food access at 1 mile",  
                        "lapop10share" = "low food access at 10 miles",
                        "laseniors1share" = "low food access for seniors at 1 mile",
                        "laseniors10share" = "low food access for seniors at 10 miles")
    
    pal <- colorQuantile("Blues", domain = data, probs = seq(0, 1, length = 5), right = TRUE)
    
    labels <- lapply(
      paste("<strong>Area: </strong>",
            usda$NAME.y,
            "<br />",
            "<strong>% Population with",
            usda_spec,
            round(data, 2)),
      htmltools::HTML
    )
    
    leaflet(data = usda, options = leafletOptions(minZoom = 10))%>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(data), 
                  fillOpacity = 0.7, 
                  stroke = TRUE, weight = 0.5, color = "#202020",
                  label = labels,
                  labelOptions = labelOptions(direction = "bottom",
                                              style = list(
                                                "font-size" = "12px",
                                                "border-color" = "rgba(0,0,0,0.5)",
                                                direction = "auto"
                                              ))) %>%
      addLegend("bottomleft",
                pal = pal,
                values =  ~(data),
                title = "Percent by<br>Quartile Group",
                opacity = 0.7,
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0("[", round(cuts[-n], 2), " &ndash; ", round(cuts[-1], 2), ")")
                })
  })
  
  # grocery --------------------------------------------------------
  
  # Iso selector
  output$grocplot <- renderLeaflet({
    colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")
    
    groc_iso10 <- switch(input$grocdrop,
                         "Mountain Meadow Farm and Craft Market" = grc_iso_10_1,
                         "Lowes Foods of Stuart" = grc_iso_10_2,
                         "Patrick County Local Farmers Market" = grc_iso_10_3,
                         "Stuart Farmers Market" = grc_iso_10_4,                
                         "W & W Produce" = grc_iso_10_5,
                         "Walmart Supercenter" = grc_iso_10_6,
                         "Poor Farmers Farm" = grc_iso_10_7)
    
    groc_iso15 <- switch(input$grocdrop,
                         "Mountain Meadow Farm and Craft Market" = grc_iso_15_1,
                         "Lowes Foods of Stuart" = grc_iso_15_2,
                         "Patrick County Local Farmers Market" = grc_iso_15_3,
                         "Stuart Farmers Market" = grc_iso_15_4,                
                         "W & W Produce" = grc_iso_15_5,
                         "Walmart Supercenter" = grc_iso_15_6,
                         "Poor Farmers Farm" = grc_iso_15_7)
    
    data <- switch(input$grocdrop,
                   "Mountain Meadow Farm and Craft Market" = 1,
                   "Lowes Foods of Stuart" = 2,
                   "Patrick County Local Farmers Market" = 3,
                   "Stuart Farmers Market" = 4,                
                   "W & W Produce" = 5,
                   "Walmart Supercenter" = 6,
                   "Poor Farmers Farm" = 7)
    
    labels <- lapply(
      paste("<strong>Name: </strong>",
            groceries_latlong[data, ]$name,
            "<br />",
            "<strong>Address:</strong>",
            groceries_latlong[data, ]$fulladdress,
            "<br />",
            "<strong>Type:</strong>",
            groceries_latlong[data, ]$type),
      htmltools::HTML
    )
    
    m1 <- leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential, 
                 fillColor = colors[5],
                 fillOpacity = .8, 
                 stroke = FALSE, 
                 group = "Residential Properties") %>%
      addPolygons(data = groc_iso10, 
                  fillColor = colors[1],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrone") %>%
      addPolygons(data = groc_iso15,
                  fillColor = colors[2],
                  fillOpacity = .8, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrone") %>%
      addMarkers(data = groceries_latlong, ~longitude[data], ~latitude[data],
                 group = "Fresh Food Location",
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto"))) %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("15 Minute Isochrone",
                          "10 Minute Isochrone",
                          "Residential Properties",
                          "Fresh Food Location"),
        options = layersControlOptions(collapsed = FALSE))
    m1 
  })
  
  # Grocery table
  output$groctable <- renderTable({
    data <- switch(input$grocdrop,
                   "Mountain Meadow Farm and Craft Market" = 1,
                   "Lowes Foods of Stuart" = 2,
                   "Patrick County Local Farmers Market" = 3,
                   "Stuart Farmers Market" = 4,                
                   "W & W Produce" = 5,
                   "Walmart Supercenter" = 6,
                   "Poor Farmers Farm" = 7)
    
    table <- read.csv(paste0("data/isochrones/tables/grc_iso_table_",data,".csv"))
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  # Food deserts
  output$allgroc <- renderLeaflet({
    
    labels <- lapply(
      paste("<strong>Name: </strong>",
            groceries_latlong$name,
            "<br />",
            "<strong>Address:</strong>",
            groceries_latlong$fulladdress,
            "<br />",
            "<strong>Type:</strong>",
            groceries_latlong$type),
      htmltools::HTML
    )
    
    leaflet(options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircles(data = residential, 
                 fillColor = colors[5],
                 fillOpacity = .5, 
                 stroke = FALSE, 
                 group = "Residential Properties") %>%
      addPolygons(data = grc_iso_10_1, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_2, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_3, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_4, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_5, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_6, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_10_7, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "10 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_1, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_2, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_3, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_4, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_5, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_6, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addPolygons(data = grc_iso_15_7, 
                  fillColor = colors[1],
                  fillOpacity = .5, 
                  stroke = FALSE, 
                  group = "15 Minute Isochrones") %>%
      addMarkers(data = groceries_latlong,
                 group = "Fresh Food Locations",
                 label = labels,
                 labelOptions = labelOptions(direction = "bottom",
                                             style = list(
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)",
                                               direction = "auto")))  %>%
      addLayersControl(
        position = "topright",
        baseGroups = c("10 Minute Isochrones",
                       "15 Minute Isochrones"),
        overlayGroups = c("Residential Properties",
                          "Fresh Food Locations"),
        options = layersControlOptions(collapsed = FALSE))
  })
  
  # Other food resources
  output$othermap <- renderLeaflet({
    
    pal <- colorFactor(c("#0E879C", "#D9E12B", "#E6A01D"), domain = otherfood$type)
    
    labels <- lapply(
      paste("<strong>Name: </strong>",
            otherfood$name,
            "<br />",
            "<strong>Address:</strong>",
            otherfood$fulladdress,
            "<br />",
            "<strong>Type:</strong>",
            otherfood$type,
            "<br />",
            "<strong>Open to:</strong>",
            otherfood$audience,
            "<br />",
            "<strong>Notes:</strong>",
            otherfood$notes),
      htmltools::HTML
    )
    
    leaflet(data = otherfood,
            options = leafletOptions(minZoom = 10)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = patrickborder, stroke = T, weight = 2, color = "grey", fillOpacity = 0) %>%
      addCircleMarkers(data = otherfood,
                       stroke = FALSE,
                       fillOpacity = 1,
                       color = ~pal(type),
                       radius = 7,
                       opacity = 1,
                       label = labels,
                       labelOptions = labelOptions(direction = "bottom",
                                                   style = list(
                                                     "font-size" = "12px",
                                                     "border-color" = "rgba(0,0,0,0.5)",
                                                     direction = "auto"))) %>%
      addLegend("bottomleft",
                pal = pal,
                values =  ~type,
                title = "Type",
                opacity = 0.9)
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
  

  output$allgrctable <- renderTable({
    table <- read.csv("data/isochrones/tables/grc_iso_table.csv")
    table$Coverage <- paste0(round(table$Coverage, 2), " %")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
}

shinyApp(ui = ui, server = server)