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
 
rare_token_report <- function(counts,freq_threshold,plotsfile="freqplots.png") {

    message("Aggregating token counts into overall counts")

    overall <- with(counts,table(rep(WORDCOUNTS,times=WEIGHT)))

    total <- sum(overall)
    ovf <- as.data.frame(overall)
    ovf$keep <- ovf$Freq / total > freq_threshold
    
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
    
    types_frac <- 1 - ecdf(overall)(freq_threshold * total)
    types_total <- length(overall)
    types_msg <- sprintf("%.0f of %.0f types (%.3f)",
                         types_frac * types_total,types_total,types_frac)
    
    tokens_count <- sum(overall[overall >= freq_threshold * total])
    tokens_msg <- sprintf("%.0f of %.0f tokens (%.3f)",
                         tokens_count,total,tokens_count / total)
    message("A frequency threshold of ",freq_threshold,
            " or > ",floor(freq_threshold * total)," tokens\n",
            "leaves ",types_msg," and ",tokens_msg)
    
    freqdist
}


# main()

make_instance_main <- function() {
    # "includes"
    source("~/Developer/dfr-analysis/source_all.R")
    library(ggplot2)
    library(grid)
    pwd <- getwd()
    setwd("~/Documents/research/20c/hls/dfr-data")

    # parameters
    dfr_dirs <- c("elh_ci_all",
                  "mlr1905-1970",
                  "mlr1971-2013",
                  "modphil_all",
                  "nlh_all",
                  "pmla_all",
                  "res1925-1980",
                  "res1981-2012")
    # or for faster testing, uncomment:
    # dfr_dirs <- "~/Developer/dfr-analysis/test_data/pmla_sample"

    aquo <- as.Date("1905-01-01")
    adquem <- as.Date("2004-12-31")
    itemtypes <- "fla\t"
    
    # this should be pretty moderate
    freq_threshold=1e-7
    # this would be more aggressive
    # freq_threshold=1e-5

    outdir <- "out"

    plotfile <- file.path(outdir,"freqplots.png")
    outfile <- file.path(outdir,"journals.mallet")
    stoplist_file <- "../external-repo/tmhls/stoplist_final.txt"

    # main script: commands

    if(!file.exists(outdir)) {
      dir.create(outdir)
    }

    counts <- get_counts(dfr_dirs,
                         aquo,
                         adquem,
                         itemtypes)

    rare_token_report(counts,freq_threshold,plotfile)

    message("Read ",nrow(counts)," rows\n",
            "Removing word types with corpus frequency < ",freq_threshold)

    counts <- remove_rare(counts,freq_threshold)

    message("Making MALLET instance...")

    inst <- make_instances(docs_frame(counts),stoplist_file)
    write_instances(inst,outfile)

    message("Instance saved to ",outfile)

    setwd(pwd)
}

# execution

make_instance_main()
