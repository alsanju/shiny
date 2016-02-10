#Libraries
library("shiny")
library("shinyFiles")

shinyUI(fluidPage(
  titlePanel("Bioclinic"),
  hr(),
  br(),
  sidebarLayout(
    
    sidebarPanel(
      conditionalPanel(
        'input.dataset == "index"',
          h3("General info")
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
      ),
      conditionalPanel(
        'input.dataset == "boxplot"',
        h3("Boxplot Analysis"),
        # , shinyFilesButton("analysis_file", "Load File", "Please select a file", FALSE)
        
        h4("Select your variable to plot")
        , uiOutput("variables_list")
        , uiOutput("interval_age")
        , uiOutput("interval_imc")
        , radioButtons("sex", 'Sex', choices = c("All", "Male", "Female"), selected = c("All"))
        , helpText("Underweight:  <18.5, Normal Range: 18.5-24.99, Moderately obsese: >25, Severely Obese: >30")
      ),
      conditionalPanel(
        'input.dataset == "scatterplot"',
          h3("Select your variables to compare in the scatterplot")
        , uiOutput("x_axis")
        , uiOutput("y_axis")
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel('index', textOutput("Blah")),
        tabPanel('boxplot', plotOutput("boxplot")),
        tabPanel('scatterplot', plotOutput("scatterplot"))
      )
    )
  )
))
