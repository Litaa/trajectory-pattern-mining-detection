library(shinythemes)
library(leaflet)


shinyUI(
  div(fluidPage(style = 'height:5500px;', theme = shinytheme("cerulean"),
      headerPanel(h1("Haze Trajectory Pattern in Australia", align = "center", theme = shinytheme("slate")),
          windowTitle = "Haze Trajectory Pattern Mining"),
          tags$style(type = "text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }"
            ),
  tabsetPanel(type = "pills",
      tabPanel("Haze Trajectory", hr(),
        tabsetPanel(type = "tab",
          tabPanel('Initial point from hotspot sequence',sidebarLayout(fluid = TRUE,
                  sidebarPanel(h5('Initial point HYSPLIT Simulation'),
                  selectInput('date_Period', 'Hotspot Sequence', 
                  choices = c(Choose = '',
                            'Australia, October 2019' = 'aus_oct',
                            'Australia, November 2019' = 'aus_nov',
                            'Australia, December 2019' = 'aus_dec'), selectize = TRUE),
                  actionButton('go_sim', 'Start Simulation'),width = 3),
                  mainPanel(fluid = TRUE,h3(textOutput('caption_traj'),uiOutput("popup_about")),
                  h5('Description and visualization of trajectory simulation results'),
                  leafletOutput('showplotleaflet', height = 500),
                  dataTableOutput('simulation_traj')))),
                                                 
          tabPanel("Initial point from user's input",sidebarLayout(fluid = TRUE,
                  sidebarPanel(h5('Initial point HYSPLIT Simulation'),
                  selectInput('date_PeriodInput', 'Hotspot Sequence', 
                  choices = c(Choose = '', 
                              'Australia, October 2019' = 'aus_oct',
                              'Australia, November 2019' = 'aus_nov',
                              'Australia, December 2019' = 'aus_dec'), selectize = TRUE),
                   uiOutput("numInput"),
                   uiOutput("Dynamic"),                
                   actionButton('go_sim2', 'Start Simulation'),
                   uiOutput("Dynamic2"),width = 3),
                   mainPanel(fluid = TRUE,h3(textOutput('caption_traj2')),
                   h5('Description and visualization of trajectory simulation results'),
                   leafletOutput('showplotleafletinput', height = 500),
                   dataTableOutput('simulation_trajinput')))),
                                                 
          tabPanel('Initial point from CSV file',sidebarLayout(fluid = TRUE,
                  sidebarPanel(h5('Initial point HYSPLIT Simulation'),
                  fileInput('datafilecsv', 'Upload the file'),
                  br(),
                  actionButton('go_sim3', 'Start Simulation'),width = 3),
                  mainPanel(fluid = TRUE,h3(textOutput('caption_traj3')),
                  h5('Description and visualization of trajectory simulation results'),
                  leafletOutput('showplotleafletcsv', height = 500),
                  h4(textOutput('titlecheck')),
                  dataTableOutput('checkstatus'),
                  h4(textOutput('titleSimulation')),
                  dataTableOutput('simulation_trajcsv'))))
                
                )))
  )))