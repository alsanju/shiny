library(shiny)
library(ggplot2)
library(datasets)

shinyServer(
  function(input,output){
        
    data_puromycin <- reactive({
      
      if(!("treated" %in% input$checkGroup) & "untreated" %in% input$checkGroup){
        subset <- Puromycin[Puromycin$state == "untreated",]
      }else if("treated" %in% input$checkGroup & !("untreated" %in% input$checkGroup)){
        subset <- Puromycin[Puromycin$state == "treated",]
      }else if(!("treated" %in% input$checkGroup) & !("untreated" %in% input$checkGroup)){
        subset <- stop("Non selected state")
      }else{
        subset <- Puromycin  
      } 
      
      subset[  subset$rate >= input$rate_threshold & 
               !is.na(subset$rate) &
               input$conc_threshold[1] <= subset$conc &
               input$conc_threshold[2] >= subset$conc &
               !is.na(subset$conc)
            ,]   
    })
    
    output$data_table <- renderDataTable({
      data_puromycin()
    }, options = list(lengthMenu = c(10, 30, 50), pageLength = 10))
    
    output$plot <- renderPlot({    
      p <- ggplot(data_puromycin(), aes_string(x= input$x, y = input$y))
      if(input$group == "None"){
        p1 <- p + geom_point() + geom_line()
      }else{
        p1 <- p + geom_point(aes_string(group = input$group, color = input$group)) + geom_line(aes_string(group = input$group, color = input$group))
      }
      if(input$wrap == "None"){
        p2 <- p1 
      }else{
        p2 <- p1 + facet_wrap(~state)
      }
      if(input$regresion == "None"){
        p3 <- p2
      }else{
        p3 <- p2 + geom_smooth(method = 'lm', se = TRUE)
      }  
      p3
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('puromycin-filtrated-', Sys.Date(), '.txt')
        },
      content = function(file) {
        write.table(data_puromycin(), file, quote = F, sep = "\t")
      }
    )
})
