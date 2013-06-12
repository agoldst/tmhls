prune_filelist <- function(files,metadata,aquo,adquem,types="fla\t") {
    # apply cutoff dates
    keep_ids <- metadata$id[metadata$date >= as.Date(aquo) &
                            metadata$date <= as.Date(adquem) &
                            metadata$type %in% types]
    files[as.id(files) %in% keep_ids]
}

                     
get_counts <- function(dirs,
                       aquo,adquem,
                       itemtypes) {

    message("Loading metadata...")

    metadata <- read_metadata(file.path(dirs,"citations.CSV"))
    metadata$date <- pubdate_Date(metadata$pubdate)

    message("Read ",nrow(metadata)," metadata entries")

    globs <- file.path(dirs,"wordcounts","wordcounts*.CSV")
    files <- prune_filelist(files=Sys.glob(globs),
                            metadata=metadata,
                            aquo=aquo,adquem=adquem,
                            types=itemtypes)

    message("Importing ",length(files)," wordcount.CSV files")

    read_dfr(files=files)
}
 
rare_token_report <- function(overall,
                              freq_threshold=NULL,rank_threshold=NULL,
                              plotsfile="freqplots.png") {

    total <- sum(overall)
    ovf <- as.data.frame(overall)
    if(!is.null(freq_threshold)) { 
        count_threshold <- freq_threshold * total
    }
    else {
        if(is.null(rank_threshold)) {
            stop("No threshold supplied")
        }

        count_threshold <- sort(overall,decreasing=T)[rank_threshold]
    }
            
    ovf$keep <- ovf$Freq >= count_threshold
    
    message("Constructing plots...")

    png(plotsfile,width=800,height=600)
    grid.newpage()
    pushViewport(viewport(layout=grid.layout(1,2)))
  
    freqdist <- qplot(Freq / total,data=ovf,geom="bar",log="x",fill=keep,
                      xlab="frequency",
                      ylab="number of word types",
                      main="Types") +
        theme(legend.position="none")
    
    tokdist <- freqdist + geom_bar(aes(weight=Freq)) +
        ylab("token count") + ggtitle("Tokens")

    print(freqdist,vp=viewport(layout.pos.row=1,layout.pos.col=1))
    print(tokdist,vp=viewport(layout.pos.row=1,layout.pos.col=2))
    dev.off()
    
    message("Plots saved to ",plotsfile)
    
    types_frac <- 1 - ecdf(overall)(count_threshold)
    types_total <- length(overall)
    types_msg <- sprintf("%.0f of %.0f types (%.3f)",
                         types_frac * types_total,types_total,types_frac)
    
    tokens_count <- sum(overall[overall >= count_threshold])
    tokens_msg <- sprintf("%.0f of %.0f tokens (%.3f)",
                         tokens_count,total,tokens_count / total)
    message("A frequency threshold of ",count_threshold / total,
            " or > ",floor(count_threshold)," tokens\n",
            "leaves ",types_msg," and ",tokens_msg)
    
    freqdist
}

stopword_report <- function(overall,stoplist_file) {
    stopwords <- scan(stoplist_file,what=character(),sep="\n",quiet=T)
    stopwords <- unique(stopwords)

    total <- sum(overall)
    stopcount <- sum(overall[stopwords],na.rm=T)

    message("The ",length(stopwords)," unique stopwords from ",
            stoplist_file,"\n",
            "correspond to ",stopcount, " of ",total," tokens (",
            sprintf("%.3f",stopcount / total),") in the corpus")
}

# main()

make_instance_main <- function() {
    # "includes"
    source("~/Developer/dfr-analysis/source_all.R")
    library(ggplot2)
    library(grid)
    pwd <- getwd()
    setwd("~/Documents/research/20c/hls/tmhls")

    # parameters
    dfr_data_root <- "/Users/agoldst/Documents/research/20c/hls/tmhls/dfr-data"
    journal_dirs <- c("elh_ci_all",
                  "mlr1905-1970",
                  "mlr1971-2013",
                  "modphil_all",
                  "nlh_all",
                  "pmla_all",
                  "res1925-1980",
                  "res1981-2012")
    dfr_dirs <- file.path(dfr_data_root,journal_dirs)

    outdir <- "/Users/agoldst/Documents/research/20c/hls/tmhls/models/out130612/"
    # or for faster testing, uncomment:
    # dfr_dirs <- "~/Developer/dfr-analysis/test_data/pmla_sample"
    # outdir <- "/Users/agoldst/Documents/research/20c/hls/tmhls/models/test"

    aquo <- as.Date("1905-01-01")
    adquem <- as.Date("2004-12-31")
    itemtypes <- "fla\t"
    
    # corpus-trimming parameters
    # this should be pretty moderate
    # freq_threshold <- 1e-7
    #rank_threshold <- NULL
    # this would be more aggressive
    freq_threshold <- NULL
    rank_threshold <- 10000

    

    plotfile <- file.path(outdir,"freqplots.png")
    outfile <- file.path(outdir,"journals.mallet")

    message("regenerating stoplist_final.txt") 
    system("python stoplist_final.py",ignore.stdout=T,ignore.stderr=T)

    stoplist_file <- "stoplist_final.txt"

    # main script: commands

    if(!file.exists(outdir)) {
      dir.create(outdir)
    }

    counts <- get_counts(dfr_dirs,
                         aquo,
                         adquem,
                         itemtypes)

    message("Read ",nrow(counts)," rows") 
    message("Aggregating token counts into overall counts")

    overall <- overall_counts(counts) 
    rare_token_report(overall,freq_threshold,rank_threshold,plotfile) 
    stopword_report(overall,stoplist_file)

    message("Removing infrequent word types...")

    counts <- remove_rare(counts,freq_threshold,rank_threshold,
                          .overall=overall)

    message(nrow(counts)," rows remain.") 
    message("Making MALLET instance...") 
    inst <- make_instances(docs_frame(counts),stoplist_file)

    write_instances(inst,outfile) 
    message("Instance saved to ",outfile)

    setwd(pwd)
}

# execution

make_instance_main()
