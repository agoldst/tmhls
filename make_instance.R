source("~/Developer/dfr-analysis/source_all.R")

setwd("~/Documents/research/20c/hls/dfr-data")

prune_filelist <- function(files,metadata,aquo,adquem,types="fla\t") {
    # apply cutoff dates
    keep_ids <- with(metadata,
                     id[date >= as.Date(aquo)
                        && date <= as.Date(adquem)
                        && type %in% types])
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

    counts <- read_dfr(files=files)

}
 
rare_token_report <- function(counts,freq_threshold,
                              plot_filename="wordfreq_dist.png") {

    overall <- with(counts,table(rep(WORDCOUNTS,times=WEIGHT)))

    total <- sum(overall)
    freq_threshold <- 1e-5
    freqdist <- qplot(Freq / total,data=as.data.frame(overall),
                      stat="ecdf",geom="step",log="x",
                      xlab="frequency",
                      ylab="number of word types",
                      main="Cumulative distribution of total word frequencies") 
    freqdist <- freqdist +
        geom_vline(xintercept=freq_threshold,color="red") +
        geom_text(label="frequency cutoff",x=freq_threshold,y=0.5,hjust=0)
    ggsave("wordfreq_dist.png",plot=freqdist)

    message("A frequency threshold of ",freq_threshold,
            " or > ",floor(freq_threshold * total)," tokens\n",
            "leaves ",1 - ecdf(overall)(freq_threshold * total),
            " of word types and ",
            sum(overall[overall >= freq_threshold * total]) / total,
            " of word tokens")
}
# main script: parameters

dfr_dirs <- c("elh_ci_all",
              "mlr1905-1970",
              "mlr1971-2013",
              "modphil_all",
              "nlh_all",
              "pmla_all",
              "res1925-1980",
              "res1981-2012")


aquo <- as.Date("1905-01-01")
adquem <- as.Date("2004-12-31")
itemtypes <- "fla\t"
freq_threshold=1e-5

outfile <- "journals.mallet"
stoplist_file <- "../external-repo/tmhls/stoplist_final.txt"

# main script: commands

counts <- get_counts(dfr_dirs,
                     aquo,
                     adquem,
                     itemtypes)

rare_token_report(counts,freq_threshold)

message("Read ",nrow(counts)," rows\n",
        "Removing word types with corpus frequency < ",freq_threshold)

counts <- remove_rare(counts,freq_threshold)

message("Making MALLET instance...")

inst <- make_instances(docs_frame(counts),stoplist_file)
write_instances(inst,outfile)

message("Instance saved to ",outfile)

