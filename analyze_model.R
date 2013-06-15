import_model <- function(keys,doctops) { 
    wkf <- read.csv(keys)
    doctops <- read.csv(doctops,as.is=T)

    list(wkf=wkf,
         doctops=doctops) 
    # topic words?
}

# main()
analyze_model_main <- function(model="test") {
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

    if(model=="130612") { 
        model_dir <- "/Users/agoldst/Documents/research/20c/hls/tmhls/models/out130612/"
        doctops_file <- file.path(model_dir,"doc_topics.csv")
        keys_file <- file.path(model_dir,"keys.csv")
    } else if(model=="130611") { 
        model_dir <- "/Users/agoldst/Documents/research/20c/hls/tmhls/models/out130611/"
        # slightly different filename on 130611 model
        doctops_file <- gzfile(file.path(model_dir,"topics.csv.gz"))
        keys_file <- file.path(model_dir,"keys.csv")
    } else if(model=="test") {
        model_dir <- "/Users/agoldst/Documents/research/20c/hls/tmhls/models/test/"
        keys_file <- file.path(model_dir,"keys.csv")
        doctops_file <- file.path(model_dir,"topics.csv")
    }
    else {
        stop("Specify a model to analyze.")
    }

    report_dir <- file.path(model_dir,"report")
    if(!file.exists(report_dir)) {
        dir.create(report_dir)
    }

    boxplot_time <- "10 years"

    # do the analysis

    message("Loading metadata")

    metadata <- read_metadata(file.path(dfr_dirs,"citations.CSV"))

    message("Loading modeling results")

    model <- import_model(keys_file,doctops_file)

    message("Converting document-topic matrix to long form") 

    model$dtl <- doc_topics_long(model$doctops,metadata,
                                 meta_keep="pubdate")

    message("Generating report...")
    topic_report(model$dtl,model$wkf,time_breaks=boxplot_time,
                 filename_base=report_dir)

    message("Reports saved to ",report_dir)

    # allow results to persist
    list(model=model,metadata=metadata)
}

# execution
# choose which model (file paths specified in main() subroutine)

results <- analyze_model_main(
                              model="130612"
                              # model="130611"
                              # model="test"
                              )

