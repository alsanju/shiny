shinyUI(fluidPage(
  
  titlePanel("GeneCov"),
  br(),
  sidebarLayout(  
    sidebarPanel(
      
      conditionalPanel(
        'input.dataset === "Gene data"',
        radioButtons("filter", 
                     label = h3("Filtering by..."),
                     choices = list("HGNC symbol" = 1, "Ensembl ID" = 2), 
                     selected = 1
        ),
        textInput("gene", 
                  label = h3("Gene name"), 
                  value = ""
        ),
        helpText("E.g. LDLR or ENSG00000130164"),
        radioButtons("version", 
                     label = h3("Ensembl build"),
                     choices = list("GRCh37" = 1, "GRCh38" = 2), 
                     selected = 1
        ),
        downloadButton('downloadData', 'Download'),
        helpText("Download your filtered data table")
      ),
      
      conditionalPanel(
        'input.dataset === "PlotTrack"',
        
        checkboxGroupInput('tracks_to_plot',
                           'Tracks to plot:',
                           c("IdeogramTrack", "GenomeAxisTrack", "GeneRegionTrack", "PlotTrack"), 
                           selected =  c("IdeogramTrack", "GenomeAxisTrack", "GeneRegionTrack")
                           ),
        uiOutput("conditional_plotTrack")
      )
    ),
    mainPanel(
      tabsetPanel(
        id='dataset',
        tabPanel("Gene data", dataTableOutput("data_table")),
        tabPanel("PlotTrack", plotOutput("plot"))
      )
    )
  )
))
