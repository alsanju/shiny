library("shiny")
library("shinyFiles")

shinyUI(fluidPage(
  titlePanel("Sequence masking"),
  hr(),
  br(),
  sidebarLayout( 
    
    sidebarPanel(
      h3("Gene list")
      , shinyFilesButton("file", "Load File", "Please select a file", FALSE)
      , hr()
      , conditionalPanel("output.fileUploaded",
                         
                         h3("Filter conditions"),
                         
                         radioButtons("type"
                                      , "Type of filter"
                                      , choices = c("Ensembl ID" = "ensembl_id"
                                                    , "Gene Name" = "gene_name"
                                                    , "Variation Name" = "variation_name"
                                      )
                         ),
                         
                         textInput("project_name", 
                                   label = "Project Name", 
                                   value = ""
                                   
                         ),
                         
                         helpText("E.g. Methylation_001"),
                         
                         shinyDirButton("output_dir"
                                        , label = "Output Directory"
                                        , title = "Please choose your output Directory"
                                        , buttonType = "default"
                                        , class = NULL
                         ),

                         helpText("E.g. /path/to/directory"),
                         
                         conditionalPanel("input.type == 'ensembl_id' | input.type == 'gene_name'",
                                          sliderInput("maf", "MAF threshold:", 
                                                      min=0, max=0.05, value=0.001),
                                          
                                          sliderInput("fiveutr", "5'UTR Flanking Region:", 
                                                      min=0, max=10000, value=5000, step = 100),
                                          
                                          sliderInput("threeutr", "3'UTR Flanking Region:", 
                                                      min=0, max=2000, value=1000, step = 50),
                                          
                                          sliderInput("intronic", "Intronic Flanking Region:", 
                                                      min=0, max=800, value=400, step = 50)
                         ),
                         
                         conditionalPanel("input.type == 'variation_name'",
                                          sliderInput("maf", "MAF threshold:", 
                                                      min=0, max=0.05, value=0.001),
                                          
                                          sliderInput("intronic", "Intronic Flanking Region:", 
                                                      min=0, max=800, value=400, step = 50)
                         ),
                         actionButton("submit", "Submit"),
                         hr(), 
                         h3("Results"),
                         uiOutput("gene_file"),
                         actionButton("view", "View")
                         
      )
    ),
    mainPanel(
      textOutput("result")  
    )
  )
))
