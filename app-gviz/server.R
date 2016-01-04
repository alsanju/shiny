library(shiny)
library(Gviz)
library(biomaRt) 

## File max size of 200 MB
options(shiny.maxRequestSize=200*1024^2) 

shinyServer(
  function(input,output){
    
    ##Filtering the data by gene name
    final_filter <- reactive({
      if (input$filter == 1){
        final_filter <- "hgnc_symbol"
      }else{
        final_filter <- "ensembl_gene_id"
      }
    })
    
    ##Ensembl build selection
    ensembl <- reactive({
      if (input$version == 1){
        ensembl <- useMart(biomart="ENSEMBL_MART_ENSEMBL", host="grch37.ensembl.org", path="/biomart/martservice" ,dataset="hsapiens_gene_ensembl")
      }else{
        ensembl <- useMart(biomart = "ENSEMBL_MART_ENSEMBL", host = "www.ensembl.org", dataset = "hsapiens_gene_ensembl")
      }
    })
    
    ##Getting data from biomart
    gene_data <- reactive({
      biomart_output <- getBM (
        attributes = c(  "ensembl_gene_id"
                         , "start_position"
                         , "end_position"
                         , "chromosome_name"
                         , "strand"
                         , "exon_chrom_start"
                         , "exon_chrom_end"
                         , "ensembl_exon_id"
                         , "ensembl_transcript_id"
        ),
        filters = final_filter(), 
        values = input$gene,
        mart = ensembl()
      )
      biomart_output <- biomart_output [grep("LRG", biomart_output$chromosome_name, invert=TRUE),]
      width <- as.numeric(biomart_output$exon_chrom_end - biomart_output$exon_chrom_start)
      biomart_output <- cbind(biomart_output, width)
      biomart_output$ensembl_gene_name <- rep(input$gene,nrow(biomart_output))
      biomart_output <- biomart_output[,c(1,11,2:10)]
      names(biomart_output) <- c(
        "gene",
        "symbol",
        "start_gene", 
        "end_gene", 
        "chromosome", 
        "strand", 
        "start", 
        "end", 
        "exon", 
        "transcript", 
        "width")
      biomart_output$strand <- ifelse(biomart_output$strand==1,  "+", "-")
      return(biomart_output)
    })
    
    ##Output the data table
    output$data_table <- renderDataTable({
      validate(
        need(input$gene, 'Please write a gene name')
      )
      withProgress(message = 'Loading data', detail = "0 %", value = 0, {
        for (i in 1:10) {
          incProgress(0.1, detail = paste0(i, "0%"))
          Sys.sleep(0.1)
        }
        gene_data()
      })
    }, options = list(lengthMenu = c(10, 30, 50), pageLength = 10))
    
    ##Download your data table
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0(gene, '-info-', Sys.Date(), '.txt')
      },
      content = function(file) {
        write.table(gene_data(), file, quote = F, sep = "\t")
      }
    )
    
    ##Conditional bamfile input 
    output$conditional_plotTrack <- renderUI({
      if ("PlotTrack" %in% input$tracks_to_plot){
        textInput("bamfile", "/path/to/bamfile", value = "")
      }
    })
    
    ##PlotTrack output
    output$plot <- renderPlot({
      
      validate(
        need(input$gene, 'Please write a gene name')
      )
      
      validate(
        need(input$tracks_to_plot, 'Please select a track')
      )
      
      query_result <- gene_data()
      chr <- unique(query_result$chromosome)
      start_gene <- unique(query_result$start_gene)
      end_gene <- unique(query_result$end_gene)
      
      start <- start_gene - 1500
      end <- end_gene + 1500   
      
      list_tracks <- list()
      
      if ("IdeogramTrack" %in% input$tracks_to_plot){          
        Track <- IdeogramTrack(      genome = "hg38"
                                     , chromosome = chr
                                     , showBandId = TRUE
                                     , cex.bands = 0.5
        )
        list_tracks <- c(list_tracks, Track)
      }
      
      if ("GenomeAxisTrack" %in% input$tracks_to_plot){
        Track <- GenomeAxisTrack(    add53 = T
                                     , add35 = T
        )
        list_tracks <- c(list_tracks, Track)
      }
      
      if ("GeneRegionTrack" %in% input$tracks_to_plot){
        Track <- GeneRegionTrack (    query_result
                                      , chromosome = chr
                                      , end = end
                                      , start = start
                                      , transcriptAnnotation = "transcript"
                                      , showId = TRUE 
                                      , name = "ENSEMBL"
        )
        list_tracks <- c(list_tracks, Track)
      }
      
      if ("PlotTrack" %in% input$tracks_to_plot){
        validate(
          need(input$bamfile, 'Please write the path of your bam file')
        )
        CovTrack <- DataTrack (    range = bam ##input$bamfile
                                   , genome = genome
                                   , type = c("l","g")
                                   , name = basename(input$bamfile)
                                   , chromosome = chr
                                   , from = start
                                   , to = end
        )
        list_tracks <- c(list_tracks, CovTrack)
      }      
      
      plotTrack <- plotTracks(  list_tracks
                                , from = start
                                , to = end
                                , main = input$gene
      )
      
      #Creating progress bar
      withProgress(message = 'Creating plot', detail = "0 %", value = 0, {
        for (i in 1:10) {
          incProgress(0.1, detail = paste0(i, "0%"))
          Sys.sleep(0.1)
        }
      })
      print(plotTrack)
    })
  }
)
