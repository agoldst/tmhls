import_model <- function(path) { 
    wkf <- read.csv(file.path(path,"keys.csv"))
    doctops <- read.csv(file.path(path,"doc_topics.csv"),as.is=T)

    list(wkf=wkf,
         doctops=doctops) 
    # topic words?
}

# main()
analyze_model_main <- function() {
    source("~/Developer/dfr-analysis/source_all.R")

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

    model_dir <- "/Users/agoldst/Documents/research/20c/hls/tmhls/models/out130612/"

    testing <- F
    # uncomment to run on small test set
    # testing <- T

    if(testing) {
        model_dir <- "/Users/agoldst/Documents/research/20c/hls/tmhls/models/test/"
    }

    report_file <- file.path(model_dir,"topic_report.pdf")
    boxplot_time <- "10 years"


    # do the analysis

    message("Loading metadata")

    metadata <- read_metadata(file.path(dfr_dirs,"citations.CSV"))

    message("Loading modeling results")

    model <- import_model(model_dir)

    message("Converting document-topic matrix to long form") 

    model$dtl <- doc_topics_long(model$doctops,metadata,
                                 meta_keep="pubdate")

    message("Generating report...")
    topic_report(model$dtl,model$wkf,time_breaks=boxplot_time,
                 filename=report_file)

    message("Report saved as ",report_file)

    # allow results to persist
    list(model=model,metadata=metadata)
}

# execution

results <- analyze_model_main()

