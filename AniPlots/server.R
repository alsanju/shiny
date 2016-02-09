##Libraries
library("shiny")
library("shinyFiles")
library("ggplot2")

shinyServer(
  function(input,output, session){
    
    #roots <- c(wd='/home')
    # roots <- c(wd='/home/asj/winbioinfo/users/asj/projects/bioclinic/')
    # shinyFileChoose(input, 'analysis_file', session = session, roots = roots, filetypes=('csv'))
    
    getData <- reactive({
      #file_path  <- parseFilePaths(roots, input$analysis_file)
      #read.csv(as.character(file_path$datapath), header = T)
      if (is.null(input$analysis_file))
        return(NULL)
      read.csv(input$analysis_file$datapath, header = T)
    })
    
    output$variables_list <- renderUI({
      if (is.null(getData())) return(NULL)
      selectInput("variable", "Variables:", as.vector(names(getData())))
    })
    
    output$interval_age <- renderUI({
      if (is.null(getData())) return(NULL)
      min_age <- min(getData()$Edad)
      max_age <- max(getData()$Edad)
      sliderInput("age", "Interval of age:", min= min_age, max=max_age, value=c(min_age,max_age))
    })
    
    output$interval_imc <- renderUI({
      if (is.null(getData())) return(NULL)
      min_imc=min(getData()$Imc)
      max_imc=max(getData()$Imc)
      sliderInput("imc", "Body mass index", min=min_imc, max=max_imc, value=c(min_imc, max_imc), step = 0.01)
      
    })
    
    output$boxplot <- renderPlot({

      if (is.null(getData())) return(NULL)
      #DMbio <- read.csv("/home/asj/winbioinfo/users/asj/projects/bioclinic/cleaning_and_EDA_DMbio/tidy_data/DMbio.csv", header = T)
      DMbio <- getData()
      
      DMbio$Time_points <- as.factor(DMbio$Time_points)
      DMbio$Sexo <- as.factor(DMbio$Sexo)
      DMbio$Colors <- ifelse(DMbio$Sexo == 0, 0, 1)
      
      DMbio[DMbio$Edad <= min(input$age) | DMbio$Edad >= max(input$age),]$Colors <- 9
      DMbio[DMbio$Imc <= min(input$imc) | DMbio$Imc >= max(input$imc),]$Colors <- 9
      
      p <- ggplot(DMbio, aes_string("Time_points", input$variable)) + facet_grid(Sample_Group~.)
      ##input$variable needs aes_string
      
      p_box <- p + geom_violin() + geom_point(aes(size = 1, color=factor(Colors)), alpha = 0.7) +
        scale_colour_manual(values = c("#20B2AA", "#FF0000", "#D3D3D3"))
      
      print(p_box)
      
    })
  }
)
