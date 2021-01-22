# Load all libraries
library(xml2)
library(rvest)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(leaflet)
library(highcharter)
library(tidyverse)
library(tmap)
library(sf)
library(RColorBrewer)
library(cartogram)
library(DT)
library(forcats)
library(shinycssloaders)

# Read all required data and shapefiles
df <- readRDS("Rodent_Inspection_clean_R1.rds")
nycnew <- st_read("nyu_2451_34509.shp")

# Convert data types
df$INSPECTION_TYPE <- as.factor(df$INSPECTION_TYPE)
df$BOROUGH <- as.factor(df$BOROUGH)
df$INSPECTION_DATE <- as.Date(df$INSPECTION_DATE)
df$RESULT <- as.factor(df$RESULT)
df$YEAR <- as.factor(df$YEAR)
df$MONTH <- as.factor(df$MONTH)
df$DAY <- as.integer(df$DAY)

# Make a copy of data frame
df_copy <- data.frame(df)

# New datatable data frame for outputting render data table
dt_df <-
  df[, c(
    'JOB_ID',
    'JOB_PROGRESS',
    'STREET_NAME',
    'INSPECTION_TYPE',
    'RESULT',
    "INSPECTION_DATE",
    "BOROUGH",
    "YEAR",
    "MONTH"
  )]

# Change the levels of the df_copy
df_copy$RESULT <-
  fct_relevel(
    df_copy$RESULT,
    "Rat Activity",
    "Failed for Other R",
    "Cleanup done",
    "Passed",
    "Monitoring visit",
    "Bait applied",
    "Stoppage done"
  )

# Create a month vector
months <-
  c(
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December"
  )

# Create 2 dataframes for group by of Passed & Rat Activity
top_10_p <- df %>%
  filter(RESULT == "Passed") %>%
  group_by(BOROUGH, STREET_NAME, RESULT) %>%
  summarise(count = n()) %>%
  arrange(-count)
top_10_p <- top_10_p[1:10,]

top_10_r <- df %>%
  filter(RESULT == "Rat Activity") %>%
  group_by(BOROUGH, STREET_NAME, RESULT) %>%
  summarise(count = n()) %>%
  arrange(-count)
top_10_r <- top_10_r[1:10,]

# Separate the data for graphs use - Inspection Type (Tab 3)
dfInCo <- df %>%
  filter(INSPECTION_TYPE == 'Initial' |
           INSPECTION_TYPE == 'Compliance')
dfActions <- df %>%
  filter(INSPECTION_TYPE != 'Initial' &
           INSPECTION_TYPE != 'Compliance')

############### UI for this application ###############
ui <- dashboardPage(
  title = "Tracking Down Rats At Your Convenience",
  
  ############### Header ###############
  dashboardHeader(title = tagList(
    tags$img(
      class = "logo-mini",
      src = "logoSmall.PNG",
      height = "50px"
    ),
    tags$img(
      class = "logo-lg",
      src = "logo.PNG",
      height = "50px",
      width = "235px"
    )
  )),
  
  ############### Side Bar - Menu ###############
  dashboardSidebar(
    sidebarMenu(
      menuItem("Application", tabName = "application", icon = icon("gear")),
      menuItem("User Guide", tabName = "userGuide", icon = icon("book")),
      menuItem(
        "Source Code",
        href = "https://github.com/waizhen/WQD7001_Project-NYC_Rodent_App",
        newtab = FALSE,
        icon = icon("code")
      ),
      menuItem(
        "Data Source",
        href = "https://data.cityofnewyork.us/Health/Rodent-Inspection/p937-wjvj",
        newtab = FALSE,
        icon = icon("database")
      ),
      br(),
      menuItem(
        "About Us",
        icon = icon("user"),
        img(src = "Ratatouille.png", width = "200px"),
        div(style = "font-size: 20px;", "-by Team Ratatouille")
      )
    )
  ),
  
  ############### Body - Content ###############
  dashboardBody(tags$script(
    HTML("$('body').addClass('sidebar-mini');")
  ),
  tabItems(
    tabItem(
      tabName = "application",
      tags$style(
        HTML(
          "
        .tabbable > .nav > li > a   {background-color: #2F7BE7;  color:white; width : 300px; font-size: 16px;}
        .tabbable > .nav > li[class=active]    > a {background-color: #E7962F; color:white; width: 300px; font-size: 16px;}
        .checkbox {
        line-height: 30px;
        }
        input[type='checkbox']{
        width: 20px;
        height: 20px;
        line-height: 10px;
        }
        .pretty {
        line-height: 13px;
        }
        input[type='radio']{
        width: 20px;
        height: 20px;
        line-height: 10px;
        }
        span {
          font-size: 20px;
          padding: 5px;
        }
        .selectize-input { font-size: 20px; line-height: 22px;}
        .selectize-dropdown { font-size: 20px; line-height: 20px; }
        .bootstrap-select .dropdown-toggle .filter-option-inner-inner { font-size: 16px; }
        .leaflet-tooltip {color:black !important; background-color: white !important;}
        #reset{ margin-top: 28px; }
        #year{ font-size:20px; }
        "
        )
      ),
      tags$head(tags$style(
        HTML(
          "
          .shiny-output-error-validation {
          color: black;
          padding: 100px;
          text-align: left;
          font-size: 20px;
          }
          .myClass {
          font-size: 30px;
          font-weight: bold;
          line-height: 50px;
          text-align: left;
          font-family: Palatino Linotype;
          padding: 0 15px;
          overflow: hidden;
          color: white;
          }
          .skin-blue .main-header .logo {
          background-color: white;
          padding: 0px;
          }
          .skin-blue .main-header .logo:hover {
          background-color: white;
          }
          .skin-blue .main-header .navbar {
          background-color: #001D72;
          }
          .skin-blue .main-header .navbar .sidebar-toggle:hover{
          background-color: #001D72;
          }
          .skin-blue .main-sidebar {
          background-color: #001D72;
          }
          .content-wrapper, .right-side {
          background-image: linear-gradient(to bottom, white , #FFE8BE);
          }
          .sidebar-mini.sidebar-collapse .main-header .logo>.logo-mini {
          margin-left: 0px;
          margin-right: 0px;
          }
          "
        )
      )),
      tags$script(
        HTML(
          '
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Tracking Down Rats At Your Convenience </span>\');
      })
     '
        )
      ),
      tabsetPanel(
        type = "tabs",
        tabPanel("Rat Detection and Inspection Progress",
                 fluidRow(
                   mainPanel(
                     width = 12,
                     h2("Rat Detection & Inspection Progress"),
                     fluidRow(column(6, strong(
                       htmlOutput("date"),
                       tags$head(tags$style("#date{
                         font-size: 22px;
                         }"))
                     ), ),
                     column(6, strong(
                       div(style = "font-size:28px;",
                           "Rat Inspector's Progress:")
                     ))),
                     fluidRow(
                       column(
                         6,
                         withSpinner(
                           leafletOutput("map", height = "850px"),
                           type = 8,
                           color = "#1870d5",
                           hide.ui = FALSE
                         )
                       ),
                       absolutePanel(
                         top = 205,
                         left = 35,
                         selectInput(
                           "borough",
                           div(style = "font-size:22px;", "Borough"),
                           as.list(unique(as.character(df$BOROUGH))),
                           selected = "Manhattan"
                         ),
                         numericInput(
                           'year',
                           div(style = "font-size:22px;", "Year"),
                           2020,
                           min = 2015,
                           max = 2020,
                           step = 1
                         ),
                         sliderInput(
                           'month',
                           div(style = "font-size:22px;", "Month"),
                           min = 1,
                           max = 12,
                           value = 1
                         )
                       ),
                       column(
                         6,
                         br(),
                         htmlOutput("outputStreet"),
                         tags$head(tags$style("#outputStreet{
                                      font-size: 23px;
                                      }")),
                         br(),
                         switchInput(
                           inputId = "alt_table",
                           label = "Show all rows related to the street",
                           value = FALSE,
                           labelWidth = "150px"
                         ),
                         br(),
                         br(),
                         DT::dataTableOutput("table")
                       )
                     ),
                   )
                 )),
        tabPanel("Inspection Overview",
                 fluidRow(
                   mainPanel(
                     width = 12,
                     h2("Geographical Heat Map of Inspection Count Distribution"),
                     column(
                       6,
                       withSpinner(
                         tmapOutput("mapT", height = "850px"),
                         type = 8,
                         color = "#1870d5",
                         hide.ui = FALSE
                       )
                     ),
                     column(
                       6,
                       fluidRow(column(
                         5, div(
                           style = "font-size:15px;",
                           prettyRadioButtons(
                             "type",
                             div(style = "font-size:22px;", "Inspection Type"),
                             as.list(unique(as.character(df$INSPECTION_TYPE))),
                             selected = "Initial",
                             status = "primary",
                             shape = "round",
                             outline = TRUE,
                             fill = FALSE,
                             thick = TRUE,
                             animation = "pulse",
                             icon = NULL,
                             plain = FALSE,
                             bigger = TRUE,
                             inline = FALSE,
                             width = "350px",
                           )
                         )
                       ),
                       column(
                         7,
                         strong(div(style = "font-size:24px;",
                                    "Note on Inspection Result:")),
                         br(),
                         div(
                           style = "font-size:21px;",
                           "Bait applied, Monitoring visit, Cleanup done and Stoppage done are actions taken by rodent specialists."
                         ),
                       )),
                       br(),
                       fluidRow(column(
                         5,
                         pickerInput(
                           "result",
                           div(style = "font-size:18px;", "Inspection Result"),
                           as.list(unique(as.character(df$RESULT))),
                           choicesOpt = list(
                             icon = c(
                               "glyphicon-exclamation-sign",
                               "glyphicon-remove-sign",
                               "glyphicon-ok-sign",
                               "glyphicon-user",
                               "hlyphicon-wrench",
                               "glyphicon-trash",
                               "glyphicon-compressed"
                             )
                           ),
                           options = list(style = "btn-primary")
                         )
                       ),
                       column(
                         7,
                         actionBttn(
                           "reset",
                           label = "Reset Map",
                           icon = icon("refresh", lib = "glyphicon"),
                           style = "gradient",
                           color = "primary",
                           size = "md",
                           block = FALSE,
                           no_outline = TRUE
                         )
                       )),
                       br(),
                       h3("Horizontal Bar Charts of Top 10 Streets for Rat Activity & Passed"),
                       br(),
                       fluidRow(tabBox(
                         width = 12,
                         tabPanel('Rat Activity', highchartOutput("hcrat")),
                         tabPanel('Passed', highchartOutput("hcpass"))
                       ))
                     )
                   )
                 )),
        tabPanel("Inspection Break Down",
                 fluidRow(
                   mainPanel(
                     width = 12,
                     h2('Inspection Results'),
                     column(
                       3,
                       selectInput(
                         'boroughBD',
                         div(style = "font-size:22px;", "Borough"),
                         as.list(unique(as.character(df$BOROUGH)))
                       ),
                       br(),
                       checkboxInput("incStr", "Activate Street Name", FALSE),
                       br(),
                       uiOutput('streetName')
                     ),
                     
                     column(
                       9,
                       fluidRow(
                         strong(div(style = "font-size:24px;", "Inspection Type (General):")),
                         fluidRow(column(
                           3, checkboxInput("chkIni", "Initial", TRUE)
                         ),
                         column(
                           3, checkboxInput("chkCom", "Compliance", TRUE)
                         )),
                         strong(div(style = "font-size:22px;", "Lifetime Value")),
                         fluidRow(
                           valueBoxOutput('RA1', width = 4),
                           valueBoxOutput('PA1', width = 4),
                           valueBoxOutput('FA1', width = 4)
                         ),
                         fluidRow(
                           tabBox(
                             id = "tabset2",
                             width = 12,
                             tabPanel("Rat Activity", highchartOutput('graph1')),
                             tabPanel("Passed", highchartOutput('graph2')),
                             tabPanel("Failed by Other R", highchartOutput('graph3'))
                           )
                         ),
                         strong(div(style = "font-size:24px;", "Inspection Type (Actions):")),
                         strong(div(style = "font-size:22px;", "Lifetime Value")),
                         fluidRow(
                           valueBoxOutput('BA1', width = 3),
                           valueBoxOutput('BM1', width = 3),
                           valueBoxOutput('CU1', width = 3),
                           valueBoxOutput('ST1', width = 3)
                         ),
                         fluidRow(
                           tabBox(
                             id = "tabset3",
                             width = 12,
                             tabPanel("Bait - Bait Applied", highchartOutput('graph4')),
                             tabPanel("Bait - Monitoring Visit", highchartOutput('graph5')),
                             tabPanel("Clean Ups", highchartOutput('graph6')),
                             tabPanel("Stoppage", highchartOutput('graph7'))
                           )
                         )
                       )
                     )
                   )
                 ))
      )
    ),
    tabItem(
      tabName = "userGuide",
      HTML(
        '
               <style>
               #t1, #t1 th, #t1 td {
               border: 1px solid black;
               border-collapse: collapse;
               }
               #t1 th {
               font-size: 20px;
               }
               #t1 td {
               font-size: 18px;
               }
               #t1 th, #t1 td {
               padding: 5px;
               text-align: left;
               }
               p, li {
               padding: 5px;
               font-size: 20px;
               text-align: justify;
               }
               #link {
               font-size: 15px;
               }
               .imgUG {
               border: 2px solid black;
               width: 80%;
               height: 80%;
               display: block;
               margin-left: auto;
               margin-right: auto;
               }
               </style>

               <p> Finding a rat while you are spending quality time with your family or when you are cooking as a chef in a restaurant can be a traumatic experience due to the harm and diseases that these little creatures bring upon us. Therefore, we, team Ratatouille, are here to help you curb this issue. </p>
               <p><b> What are the benefits that you could reap as a user (i.e. society and government)? </b></p>
               <ul>
               <li> Ability to monitor closely on location of rats, contaminated areas, rat-free and cleaned-up areas. </li>
               <li> Ability to track rodent inspectors efficiency by monitoring their progress in having initial or follow-up inspections, applying and monitoring baits, performing clean-ups, or applying stoppages. </li>
               <li> Ability to observe overall and monthly trends on inspection results and inspection types along with its actions. </li>
               <li>Ability to perform better resource planning by deploying more rodent inspectors at locations with stagnant progresses or higher occurrence of rat activities.</li>
               </ul>
               <h3> STEPS IN USING THIS WHOLESOME APP:- </h3><br />
               <p><b>PRIOR:</b> As a citizen in a society, you may contact the government to request for a regular inspection or file a complaint if you spotted any rats in your area. They would issue you a Job ID and starting date of inspection upon request. You may also skip this procedure and check for the presence of rats or other statuses in other areas on different months. </p><br />
               <img src ="SS1.PNG" class="imgUG"><br />
               <p><b>STEP 1: </b>Click on the <b><u>Rat Detection & Inspection Progress</b></u> tab.</p>
               <p><b>STEP 2: </b>Choose your borough.</p>
               <p><b>STEP 3: </b>Choose your year and month.</p>
               <p><b>STEP 4: </b>Click on the checkbox for the statuses that you would like to view only (i.e. Rat Activity, Failed for other R, Passed, Cleanup Done).</p>
               <p><b>STEP 5: </b>You will see all details related to the rodent inspector&#39;s progress in a tabular format based on the year or month selected.</p>
               <p><b>STEP 6: </b>Click on any of the markers in the map to view the details in the table based on street name.</p>
               <p><b>STEP 7: </b>If there are multiple Job IDs, you may key-in your desired Job ID in the search box to filter accordingly and monitor the progress of inspectors (i.e. job progress, inspection type, results, and dates). Also, if you wish to see all rows of inspections for other months, do set the toggle button of <b>"Show all rows related to the street"</b> to <b>ON</b>.</p>
               <p><b>STEP 8: </b>If you did not click on the marker, skip <b><u>STEP 6</b></u> and proceed to <b><u>STEP 7</b></u>.</p>
               <p><b>STEP 9: </b>Refer to the table below to further understand the terms used by the government or rodent inspectors in regards to inspection types and results.</p>
               <p><b> INSPECTION TYPE: </b></p>
               <table id="t1">
               <tr>
                <th>Inspection</th>
                <th>Inspection Type</th>
                <th>Description</th>
               </tr>
               <tr>
                <td rowspan=2>Regular</td>
                <td>Initial</td>
                <td>Initial inspection due to complaints or indexing programme in neighbourhood</td>
               </tr>
               <tr>
                <td>Compliance</td>
                <td>Following up from initial inspection if a property failed in the initial inspection</td>
               </tr>
               <tr>
                <td rowspan=3>Action</td>
                <td>Bait</td>
                <td>Applying rodenticide bait or monitor bait applied via visits by government pest experts</td>
               </tr>
               <tr>
                <td>Cleanup</td>
                <td>Cleaning up rubbish and clutter by the Health Department</td>
               </tr>
               <tr>
                <td>Stoppage</td>
                <td>Installing rodent proofing</td>
               </tr>
               </table><br />
               <p><b> INSPECTION RESULT: </b></p>
               <table id="t1">
               <tr>
                <th>Inspection Result</th>
                <th>Description</th>
               </tr>
               <tr>
                <td>Rat activity</td>
                <td>Sightings of rats (i.e. fresh tracks, droppings, gnawing marks; active burrows,  runways and rub marks; live rats)</td>
               </tr>
               <tr>
                <td>Failed for other R</td>
                <td>Other problems related to poor or contaminated environment conditions such as tons of garbage and harborage which potentially attract rats</td>
               </tr>
               <tr>
                <td>Bait applied</td>
                <td>Applying rodenticides or rat poison</td>
               </tr>
               <tr>
                <td>Monitoring visit</td>
                <td>Visiting to monitor progress by the Health Department Pest Control Professional after applying bait</td>
               </tr>
               <tr>
                <td>Passed</td>
                <td>No signs of rodents in property upon inspection</td>
               </tr>
               <tr>
                <td>Cleanup done</td>
                <td>Cleaning is completed and installation of stoppages (rodent proofing) are done</td>
               </tr>
               </table>
               <p id="link"><b><i>Source: <a href="https://sitecompli.com/blog/reviewing-nyc-rodent-inspection-data/">https://sitecompli.com/blog/reviewing-nyc-rodent-inspection-data/</a></i></b></p>
               <p><b>STEP 10: </b>Based on your observation, you may contact the government to file a complaint if no actions were taken by the rodent inspectors to curb the rat issue.</p>
               <p><b>STEP 11: </b>If you are the government, you may monitor the efficiency of your rodent inspectors in performing inspections related to baiting, performing clean ups, or applying stoppages to clear the rats from that particular area.</p><br />
               <img src ="SS2.PNG" class="imgUG"><br />
               <p><b>STEP 12: </b>Click on the <b><u>Inspections Overview</b></u> tab to view the overall counts for inspection results and inspection types as well as the top 10 streets in regards to the count of inspection results, <b><i>"Rat Activity"</b></i> and <b><i>"Passed"</b></i>.</p><br />
               <img src ="SS3.PNG" class="imgUG"><br />
               <p><b>STEP 13: </b>Click on the <b><u>Inspections Break Down</b></u> tab to view the lifetime values and monthly trends for inspection results and inspection types (actions) by cities and/or streets.</p>
               <p><b>STEP 14: </b>If you are the government, as a suggestion, you may further plan your strategies by deploying more rodent inspectors at higher risk cities or streets.</p>
               <p>Feel free to reach out to us if you have any enquiries in regards to this app.</p>
               <p><b>Thank you and have a great day! =)</b></p>
                '
      )
    )
  ))
)