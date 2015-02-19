library(shiny)
library(ggplot2)
library(datasets)

shinyUI(fluidPage(
  
  titlePanel("Puromycin Explorer"),
  br(),
  sidebarLayout(  
    sidebarPanel(
      
      conditionalPanel(
        'input.dataset === "Filtering data"',
        sliderInput("rate_threshold", 
                    label = "Rate",
                    min=0, max= max(Puromycin$rate, na.rm = T),
                    value=0,
                    step=1, round=0
                    ),
        sliderInput("conc_threshold", 
                    label = "Concentration",
                    min=0, max= 1.2,
                    value=c(min(Puromycin$conc), max(Puromycin$conc, na.rm = T)),
                    step=0.01, round=0),
        checkboxGroupInput("checkGroup", 
                           label = "State", 
                           choices = c("Treated" = "treated", "Untreated" = "untreated"),
                           selected = c("treated","untreated")),
        downloadButton('downloadData', 'Download'),
        helpText("Download your filtered data table")
        ),
      
      conditionalPanel(
        'input.dataset === "Plot"',
        selectInput('x', 'X-axis', names(Puromycin), 'rate'),
        selectInput('y', 'Y-axis', names(Puromycin), 'conc'),
        selectInput('group', 'Group', c("None", names(Puromycin)), "state"),
        selectInput('wrap', 'Facet', c("None", "Wrap"), "None"),
        selectInput('regresion', 'Line regresion', c("None", "Yes"), "None")
        )
      ),
    
    mainPanel(
      tabsetPanel(
        id='dataset',
        tabPanel("Filtering data", dataTableOutput("data_table")),
        tabPanel("Plot", plotOutput("plot"))
        )
      )
    )
))
