library(shiny)
library(DT)

##MAX size of 200 MB
options(shiny.maxRequestSize=200*1024^2) 

shinyServer(
  function(input,output,session){
    
    dataFile <- reactive({
      if(is.null(input$file1)) return(NULL)
      read.delim(input$file1$datapath, header = input$header, sep = "\t")
    })
    
    output$dataTable <- DT::renderDataTable(
      dataFile(), filter = 'top', server = TRUE, rownames = FALSE,
      options = list(autoWidth = TRUE)
    )
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("filtering-", Sys.Date(), '.txt')
      },
      content = function(file) {
        filteredContent = input$dataTable_rows_all
        write.table(filteredContent, file, quote = F, sep = "\t", header = T)}
    )
  }
)
