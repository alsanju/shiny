library(shiny)

shinyUI(fluidPage(
  titlePanel("Variants Filtering and Prioritization"),
  br(),
  sidebarLayout(
  
    sidebarPanel(
      helpText("Filter your data in order to do the variant prioritization. The file must be separated by tabulations. Limit size of 200MB."),
      fileInput("file1", "Upload file:"),
      checkboxInput('header', 'Header', TRUE),
      downloadButton('downloadData', 'Download filtered data')),
    
    mainPanel(
      DT::dataTableOutput('dataTable')
    )
  )
))
