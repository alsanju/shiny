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
        'input.dataset == "index"'
        , h3("General info")
        , hr()
        , fileInput('analysis_file', 'Choose file to upload',
                    accept = c(
                      'text/csv',
                      'text/comma-separated-values',
                      'text/tab-separated-values',
                      'text/plain',
                      '.csv',
                      '.tsv'
                    ))
      ),
      conditionalPanel(
        'input.dataset == "boxplot"'
        , h3("Boxplot Analysis")
        , hr()
        , h4("Select your variable to plot")
        , uiOutput("variables_list")
        , uiOutput("interval_age")
        , uiOutput("interval_imc")
        , uiOutput("sex_option")
        , uiOutput("helpmessage")
      ),
      conditionalPanel(
        'input.dataset == "scatterplot"'
        , h3("Scatterplot Analysis")
        , hr()
        , h4("Select your variables to compare in the scatterplot")
        , uiOutput("x_axis")
        , uiOutput("y_axis")
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel('index', textOutput("index_message")),
        tabPanel('boxplot', plotOutput("boxplot")),
        tabPanel('scatterplot', plotOutput("scatterplot"))
      )
    )
  )
))
