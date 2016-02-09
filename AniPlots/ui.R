#Libraries
library("shiny")
library("shinyFiles")

shinyUI(fluidPage(
  titlePanel("Bioclinic"),
  hr(),
  br(),
  sidebarLayout(
    
    sidebarPanel(
      h3("Analysis")
      # , shinyFilesButton("analysis_file", "Load File", "Please select a file", FALSE)
      , fileInput('analysis_file', 'Choose file to upload',
                  accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    'text/tab-separated-values',
                    'text/plain',
                    '.csv',
                    '.tsv'
                  ))
      , hr()
      , h4("Select your variable to plot")
      , uiOutput("variables_list")
      , uiOutput("interval_age")
      , uiOutput("interval_imc")
      , helpText("Underweight:  <18.5, Normal Range: 18.5-24.99, Moderately obsese: >25, Severely Obese: >30")
      
    ),
    mainPanel(
      plotOutput("boxplot")
    )
  )
))
