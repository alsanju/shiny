##Libraries
library("shiny")
library("shinyFiles")
library("ggplot2")

`%then%` <- shiny:::`%OR%`

shinyServer(
  function(input,output, session){
    
    getData <- reactive({
      analysis_file <- input$analysis_file
      #file_path  <- parseFilePaths(roots, input$analysis_file)
      #read.csv(as.character(file_path$datapath), header = T)
      if (is.null(analysis_file))return(NULL)
      read.csv(analysis_file$datapath, header = T)
    })
    
    output$index_message <- renderText({
      validate(need(getData() != "", label = "Data set"))
      "Now you can perform your data exploration!!"
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
    
    output$sex_option <- renderUI({
      if (is.null(getData())) return(NULL)
      radioButtons("sex", 'Sex', choices = c("All", "Male", "Female"), selected = c("All"))
    })
    
    output$helpmessage <- renderUI({
      if (is.null(getData())) return(NULL)
      helpText("Underweight:  <18.5, Normal Range: 18.5-24.99, Moderately obsese: >25, Severely Obese: >30")
    })
    
    output$boxplot <- renderPlot({
      DMbio <- getData()
      sex <- input$sex
      age <- input$age
      imc <- input$imc
      variable <- input$variable
      
      validate(need(DMbio != "", "Please, select a data set") %then%
               need(input$variable != "", "Please, wait") ##TODO with time
               )
      
      #DMbio <- read.csv("/home/asj/winbioinfo/users/asj/projects/bioclinic/cleaning_and_EDA_DMbio/tidy_data/DMbio.csv", header = T)
      
      #Time points and sex are factors
      DMbio$Time_points <- as.factor(DMbio$Time_points)
      DMbio$Sexo <- as.factor(DMbio$Sexo)
      
      #Adding new column with colour info by sex
      ##levels(DMbio) <- c(0, 1, 9)
      DMbio$Colors <- ifelse(DMbio$Sexo == 0, 0, 1)
      
      #If sex is not selected, the color is grey
      if (sex == "Male"){
        #Grey colour for females
        DMbio[DMbio$Sexo == 1,]$Colors <- 9
      }else if(sex == "Female"){
        #Grey colour for males
        DMbio[DMbio$Sexo == 0,]$Colors <- 9
      }
      
      #Grey colour for samples out of ranges of age and imc
      if(nrow(DMbio[DMbio$Edad < min(age) | DMbio$Edad > max(age),]) > 0){
        DMbio[DMbio$Edad < min(age) | DMbio$Edad > max(age),]$Colors <- 9
      }
      
      if(nrow(DMbio[DMbio$Imc < min(imc) | DMbio$Imc > max(imc),]) > 0){
        DMbio[DMbio$Imc < min(imc) | DMbio$Imc > max(imc),]$Colors <- 9
      }
      
      #Create a custom color scale
      myColors <- c("#20B2AA", "#FF6347", "#C0C0C0")
      names(myColors) <- c(0, 1, 9)
      colScale <- scale_colour_manual(name = "Colors", values = myColors)
      
      p <- ggplot(DMbio, aes_string("Time_points", variable)) + facet_grid(Sample_Group~.) ##variable needs aes_string
      p_box <- p + geom_violin() + geom_point(aes(size = 1, color=factor(Colors)), alpha = 0.7) + colScale
      
      print(p_box)
      
    })
    
    output$x_axis <- renderUI({
      if (is.null(getData())) return(NULL)
      selectInput("x_axis", "X axis variable:", as.vector(names(getData())))
    })
    
    output$y_axis <- renderUI({
      if (is.null(getData())) return(NULL)
      selectInput("y_axis", "Y axis variable:", as.vector(names(getData())))
    })
    
    output$scatterplot <- renderPlot({
      DMbio <- getData()
      validate(need(DMbio != "", "Please, select a data set") %then%
               need(input$x_axis != "", "Please, wait") ##TODO with time
      )
      
      x_axis <- input$x_axis
      y_axis <- input$y_axis
      
      #We cut HOMA values into classes
      cutpoints_homa <- quantile(DMbio$HOMA, seq(0,1,length=5), na.rm = T)
      
      DMbiobis <- DMbio
      DMbiobis$HOMA_cut <- cut(DMbio$HOMA, cutpoints_homa)
      
      #Plotting
      gluco_insu_homa <- ggplot(DMbiobis, aes_string(x_axis, y_axis)) + facet_grid(HOMA_cut ~ Time_points, margins = T)
      gluco_insu_homascatter <- gluco_insu_homa + geom_point(aes(colour = Sample_Group), size = 2)
      
      print(gluco_insu_homascatter)
      
    })
  })
