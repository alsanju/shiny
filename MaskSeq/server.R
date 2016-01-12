# Loading required libraries
library("shiny")
library("RSQLite")
library("data.table")
library("shinyFiles")

`%then%` <- shiny:::`%OR%`

# Setting up env variable
BEDTOOLS_BINARY <- "/softbio/bedtools_current/bin/bedtools"
REF_SEQ <- "/references/human/1kg/hs37d5.fa"
con <- dbConnect(RSQLite::SQLite(), dbname = "/references/human/ens_dumps/ens_dumps_v75/ensembl_v75.db")

##Function to get gene info from sqlite database
GetGeneInfo <- function(type, query){
  
  if (type == "ensembl_id"){
    
    info_query_genes.df <- NULL
    
    info_query_genes.df <- dbGetQuery(  con
                                        , paste0('select * from ens_exon where gene_id == \"', query, "\";")
    )
    return(info_query_genes.df)
  }else if(type == "gene_name"){
    
    info_query_genes.df <- NULL
    
    info_query_genes.df <- dbGetQuery(  con
                                        , paste0('select * from ens_exon where gene_name == \"', query, "\";")
    )
    return(info_query_genes.df)
    
  }
}

##Function to get variants info from sqlite database
GetVariantsInfo <- function(type, query){
  
  if (type == "chromosomal_region"){
    
    chr <- as.numeric(strsplit(query, ":")[[1]][1])
    start <- as.numeric(strsplit(query, ":")[[1]][2])
    end <- as.numeric(strsplit(query, ":")[[1]][3])
    
    info_variants.df <- NULL
    
    info_variants.df <- dbGetQuery(  con
                                     , paste0('select * from ens_vf where chr == ', chr, ' and start >= ',  start, ' and end <= ', end
                                              , ";")                        
    )
    return(info_variants.df)
    
  }else if (type == "variation_name"){
    info_variants.df <- NULL
    
    info_variants.df <- dbGetQuery(  con
                                     , paste0('select * from ens_vf where variation_name = \"', query, "\";")
    )
    return(info_variants.df)
  }
}

##Function to get fasta files with masked sequences by gene
GetFastaFromGene <- function(type
                             , query
                             , outputdir
                             , project
                             , MAF
                             , fiveUTRFlankingRegion
                             , threeUTRFlankingRegion
                             , intronFlankingRegion){
  
  if (type == "gene_name" | type == "ensembl_id"){
    
    gene_info <- GetGeneInfo(type = type, query = query) 
    
    ##Getting gene info in bed data format
    gene_name <- unique(gene_info$gene_name)
    gene_description <- paste0(project, "-", gene_name, "-m", MAF,"-f", fiveUTRFlankingRegion, "-t", threeUTRFlankingRegion, "-i", intronFlankingRegion)
    
    exon_id <- paste0(gene_info$gene_name, "-", gene_info$transcript_id, "-", gene_info$rank, "-", gene_info$exon_id)
    bed_data_format <- cbind(gene_info[,c(5:7)], exon_id, gene_info[,c(2,8)])
    
    # If opposite strand, 5UTR is at the end and 3UTR is at the start
    if (unique(bed_data_format$strand) == -1) {
      tmp <- threeUTRFlankingRegion
      threeUTRFlankingRegion <- fiveUTRFlankingRegion
      fiveUTRFlankingRegion <- tmp
    }
    
    # Adding specific flanking regions to start and end (compensating the previous intronFlankingRegion adding)
    bed_data_format[1, ]$start <- bed_data_format[1, ]$start - fiveUTRFlankingRegion + intronFlankingRegion
    bed_data_format[nrow(bed_data_format), ]$end <- bed_data_format[nrow(bed_data_format), ]$end + threeUTRFlankingRegion - intronFlankingRegion
    
    # Adding intronFlankingRegion to each exon extreme
    bed_data_format$start <- bed_data_format$start - intronFlankingRegion
    bed_data_format$end <- bed_data_format$end + intronFlankingRegion
    write.table(bed_data_format, file.path(outputdir, paste0(gene_description, ".bed")), sep = "\t", quote = FALSE, col.names = FALSE, row.names = FALSE)
    
    ##Collapse exons
    cmd_collapse <- paste0(BEDTOOLS_BINARY, " merge -i ", file.path(outputdir, paste0(gene_description, ".bed")), " -c 4,5,6 -o collapse,distinct,distinct -delim '--'")
    #cmd_collapse <- paste0(BEDTOOLS_BINARY, " merge -nms -i ", file.path(outputdir, paste0(gene_description, ".bed")))
    collapsedBed <- fread(cmd_collapse, header = FALSE)
    
    # Writing collapsed exons BED
    setnames(collapsedBed, c("chr", "start", "end", "regionId", "geneName", "strand"))
    #setnames(collapsedBed, c("chr", "start", "end", "regionId"))
    write.table(collapsedBed, file = file.path(outputdir, paste0(gene_description, "_collapsed.bed")), sep = "\t", quote = FALSE, col.names = FALSE, row.names = FALSE)
    
    ##GetFasta from reference sequence
    cmd_get_fasta <- paste0(BEDTOOLS_BINARY, " getfasta -fi ", REF_SEQ, " -bed ", file.path(outputdir, paste0(gene_description, "_collapsed.bed")), " -name -fo stdout")
    fasta_seqs <- fread(cmd_get_fasta, header = FALSE, verbose = T)
    
    ##Transforming fasta file to dataframe
    complete_info_fasta.df <- data.frame(  regionId = grep(">", fasta_seqs$V1, value = T)
                                           , sequence = grep(">", fasta_seqs$V1, value = T, invert = T)
    )
    
    #Avoiding '>' symbol from data frame
    complete_info_fasta.df$regionId <- sapply(complete_info_fasta.df$regionId, function(x){strsplit(as.character(x), split = '>')[[1]][2]})
    
    ##Merging data frames
    merged_info_fasta.df <- merge(complete_info_fasta.df, as.data.frame(collapsedBed), by = "regionId")
    merged_info_fasta.df$masked_sequence <- rep(NA, nrow(merged_info_fasta.df))
    
    ##Getting info for variants from ensembl db
    region <- paste0(chr <- unique(merged_info_fasta.df$chr)
                     , ":"
                     , seq_start <- min(merged_info_fasta.df$start)
                     , ":"
                     , seq_end <- max(merged_info_fasta.df$start)
    )
    
    ##Variants info from database for all the gene
    variants_info <- GetVariantsInfo(type = "chromosomal_region", query = region)
    info_maf_variants.df <- variants_info[variants_info$minor_allele_freq >= MAF,]
    
    ##Sequence masking
    fasta_file <- NULL
    
    for (row in seq(nrow(merged_info_fasta.df))){
      ##Getting variants coordinates
      seq_exon_start <- merged_info_fasta.df[row,]$start
      seq_exon_end <- merged_info_fasta.df[row,]$end
      
      sequence_variants.df <- subset(info_maf_variants.df, start >= seq_exon_start & end <= seq_exon_end)
      
      variants_pos <- sequence_variants.df$start
      rel_variants_pos <- variants_pos - seq_exon_start + 1
      splited_sequence <- strsplit(as.character(merged_info_fasta.df[row,]$sequence), split='')[[1]]
      
      ##Masking all variant with one N by default
      splited_sequence[rel_variants_pos] = 'N'
      
      
      ##If is insertion, masking with 5 Ns
      if(length(grep("^-", sequence_variants.df$allele_string)) > 1){  
        
        splited_sequence[rel_variants_pos[grep("^-", sequence_variants.df$allele_string)]] = "NNNNN"
        
      }
      
      ##If is deletion, masking with same name of Ns than the lenght_deletion
      if(length(grep("^[ACTG]{1}/-$", sequence_variants.df$allele_string)) > 1){  
        
        for (deletion in grep("^[ACTG]{1}/-$", sequence_variants.df$allele_string)){
          lenght_deletion <- length(strsplit(sequence_variants.df[deletion,]$allele_string, split = "/")[[1]][1])
          splited_sequence[rel_variants_pos[deletion]] = paste(rep('N', lenght_deletion), collapse = '')
        }
      }
      
      ##Adding collapsed sequences to dataframe
      merged_info_fasta.df[row,]$masked_sequence <- paste(splited_sequence, collapse = '')
      
      ##Adding entries to the fasta file
      fasta_file <- append(fasta_file, paste0('>'
                                              , paste0(gene_name
                                                       , "  |  Chromosome="
                                                       , chr
                                                       , "  |  Seq_exon_start="
                                                       , seq_exon_start
                                                       , "  |  Seq_exon_end="
                                                       , seq_exon_end
                                                       , "  |  "
                                                       , merged_info_fasta.df[row,]$regionId
                                              )
      ))
      fasta_file <- append(fasta_file, merged_info_fasta.df[row,]$masked_sequence)
      
    }    
    
    ##Writting fasta file
    write.table(fasta_file, file.path(outputdir, paste0(gene_description,'_masked.fasta')), sep = "\t", quote = FALSE, col.names = FALSE, row.names = FALSE)
    #     return(fasta_file)
    
    
  }else if (type == "variation_name"){
    
    
    variant_info <- GetVariantsInfo(type = type, query = query)
    variant_description <- paste0(project, "-", query, "-m", MAF, "-i", intronFlankingRegion)
    
    bed_data_format <- cbind(variant_info[,c(2,3,4,1)], gene = query, strand = variant_info$strand)
    
    bed_data_format$start <- bed_data_format$start - intronFlankingRegion
    bed_data_format$end <- bed_data_format$end + intronFlankingRegion
    write.table(bed_data_format, file.path(outputdir, paste0(variant_description, ".bed")), sep = "\t", quote = FALSE, col.names = FALSE, row.names = FALSE)
    
    ##GetFasta from reference sequence
    cmd_get_fasta <- paste0(BEDTOOLS_BINARY, " getfasta -fi ", REF_SEQ, " -bed ", file.path(outputdir, paste0(variant_description, ".bed")), " -name -fo stdout")
    fasta_seqs <- fread(cmd_get_fasta, header = FALSE, verbose=TRUE)
    
    
    ##Transforming fasta file to dataframe
    complete_info_fasta.df <- data.frame(  regionId = grep(">", fasta_seqs$V1, value = T)
                                           , sequence = grep(">", fasta_seqs$V1, value = T, invert = T)
    )
    
    #Avoiding '>' symbol from data frame
    complete_info_fasta.df$regionId <- sapply(complete_info_fasta.df$regionId, function(x){strsplit(as.character(x), split = '>')[[1]][2]})
    complete_info_fasta.df$masked_sequence <- rep(NA, nrow(cosesmplete_info_fasta.df))
    
    
    ##Variants info for the rest of the region
    chr <- bed_data_format$chr
    seq_start <- bed_data_format$start
    seq_end <- bed_data_format$end
    region <- paste0(chr, ":", seq_start, ":", seq_end)
    
    all_variants <- GetVariantsInfo(type = "chromosomal_region", query = region)
    
    info_maf_variants.df <- all_variants[all_variants$minor_allele_freq >= MAF,]
    
    ##Sequence masking
    fasta_file <- NULL
    
    for (row in seq(nrow(complete_info_fasta.df))){
      
      variants_pos <- info_maf_variants.df$start
      rel_variants_pos <- variants_pos - seq_start + 1
      splited_sequence <- strsplit(as.character(complete_info_fasta.df$sequence), split='')[[1]]
      
      ##Masking all variant with one N by default
      splited_sequence[rel_variants_pos] = 'N'
      
      
      ##If is insertion, masking with 5 Ns
      if(length(grep("^-", info_maf_variants.df$allele_string)) > 1){  
        
        splited_sequence[rel_variants_pos[grep("^-", info_maf_variants.df$allele_string)]] = "NNNNN"
        
      }
      
      ##If is deletion, masking with same name of Ns than the lenght_deletion
      if(length(grep("^[ACTG]{1}/-$", info_maf_variants.df$allele_string)) > 1){  
        
        for (deletion in grep("^[ACTG]{1}/-$", info_maf_variants.df$allele_string)){
          lenght_deletion <- length(strsplit(info_maf_variants.df[deletion,]$allele_string, split = "/")[[1]][1])
          splited_sequence[rel_variants_pos[deletion]] = paste(rep('N', lenght_deletion), collapse = '')
        }
      }
      
      
      ##Adding collapsed sequences to dataframe
      complete_info_fasta.df$masked_sequence <- paste(splited_sequence, collapse = '')
      
      ##Adding entries to the fasta file
      fasta_file <- append(fasta_file, paste0('>', complete_info_fasta.df[row,]$regionId))
      fasta_file <- append(fasta_file, complete_info_fasta.df[row,]$masked_sequence)
    }
    
    
    ##Writting fasta file
    write.table(fasta_file, file.path(outputdir, paste0(variant_description,'_masked.fasta')), sep = "\t", quote = FALSE, col.names = FALSE, row.names = FALSE)
    #     return(fasta_file)
  }
}

shinyServer(
  function(input,output, session){
    
    roots <- c(wd='/home/ugdg_user/apps/masking_sequences/projects')
    shinyFileChoose(input, 'file', session = session, roots = roots, filetypes=('txt'))
    shinyDirChoose(input, 'output_dir', roots = roots, session = session, restrictions=system.file(package='base'))
    
    getData <- reactive({
      file_path  <- parseFilePaths(roots, input$file)
      read.delim(as.character(file_path$datapath), header = F, sep = '', quote = '\t')
    })
    
    output$fileUploaded <- renderTable({
      if (is.null(getData())) return(NULL)
      return(getData())
    })
    
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    
    output$gene_file <- renderUI({
      if (is.null(getData())) return(NULL)
      selectizeInput("genes", "Gene:", as.vector(getData()$V1))
    })
    
    observeEvent(input$submit,{
      out_path  <- parseDirPath(roots, input$output_dir)
      for (i in as.vector(getData()$V1)){
        GetFastaFromGene(type = input$type
                         , query = i
                         , outputdir = out_path[1]
                         , project = input$project_name
                         , MAF = input$maf
                         , fiveUTRFlankingRegion = input$fiveutr
                         , threeUTRFlankingRegion = input$threeutr
                         , intronFlankingRegion = input$intronic
        )
      }
      withProgress(message = 'Getting data', detail = "0 %", value = 0, {
        for (i in 1:10) {
          incProgress(0.1, detail = paste0(i, "0%"))
          Sys.sleep(0.1)
        }
      })
    })
    
    mydata <- reactive({
      if(is.null(input$file)) return(NULL)
      out_path  <- parseDirPath(roots, input$output_dir)
      
      if(input$type == "ensembl_id" | input$type == "gene_name"){
        masked_file <- paste0(input$project_name, "-", input$genes, "-m", input$maf,"-f", input$fiveutr, "-t", input$threeutr, "-i", input$intronic, "_masked.fasta")
      }else if(input$type == "variation_name"){
        masked_file <- paste0(input$project_name, "-", input$genes, "-m", input$maf, "-i", input$intronic, "_masked.fasta")
      }
      read.delim(file.path(out_path, masked_file), header = F, sep = '\t')
    })
    
    observeEvent(input$view, {
      output$result <- renderPrint({
        mydata()$V1
      })
    })
    
  }
)
