# model_files
#
# convenience function collecting analyze_model parameters
#
# usage: do.call(analyze_model,model_files("130612"))

model_files <- function(model) {
    if(model=="130612") { 
        model_dir <- "/Users/agoldst/Documents/research/20c/hls/tmhls/models/out130612/"
        list(model_dir=model_dir,
             doctops_file=file.path(model_dir,"doc_topics.csv"),
             keys_file=file.path(model_dir,"keys.csv"))
    } else if(model=="130611") { 
        model_dir <- "/Users/agoldst/Documents/research/20c/hls/tmhls/models/out130611/"
        # slightly different filename on 130611 model
        list(model_dir=model_dir, 
             doctops_file=gzfile(file.path(model_dir,"topics.csv.gz")),
             keys_file=file.path(model_dir,"keys.csv"))
    } else if(model=="test") {
        model_dir <- "/Users/agoldst/Documents/research/20c/hls/tmhls/models/test/"
        list(model_dir=model_dir,
             keys_file=file.path(model_dir,"keys.csv"),
             doctops_file=file.path(model_dir,"doc_topics.csv"),
             dfr_data_root="~/Developer/dfr-analysis/test_data",
             journal_dirs="pmla_sample",
             model_smoothed=F,
             model_normalized=F)
    } else if(model=="ahr_k100_v20000") {
        model_dir <- "~/Documents/research/20c/hls/tmhls/models/ahr_k100_v20000/"
        list(model_dir=model_dir,
             journal_dirs="AHR",
             model_smoothed=F,
             model_normalized=F)
    } else if(model=="hls_k100_v100000") {
        model_dir <- "~/Documents/research/20c/hls/tmhls/models/hls_k100_v100000/"

        list(model_dir=model_dir,
             doctops_file=gzfile(file.path(model_dir,"doc_topics.csv.gz")))
    } else if(model=="hls_k200_v105") {
        list(model_dir="~/Documents/research/20c/hls/tmhls/models/HLSk200v105/",
             model_smoothed=F,
             model_normalized=F)
    } else if(model=="hls_k48_v100K") {
        list(model_dir="~/Documents/research/20c/hls/tmhls/models/hls_k48_v100K/",
             model_smoothed=T)
    } else if(model=="hls_k150_v100K") {
        model_dir <- "~/Documents/research/20c/hls/tmhls/models/hls_k150_v100K/"
        list(model_dir=model_dir,
             keys_file=file.path(model_dir,"wkf_nosmooth.csv"),
             model_smoothed=F,
             model_normalized=F,
             generate_report=F,
             instances_file=path.expand("~/Documents/research/20c/hls/tmhls/models/hls_k48_v100K/journals.mallet"),
             generate_tym=T)
    } 
    else {
        stop("Specify a model to analyze.")
    }
}

# import_model
#
# called by analyze_model

import_model <- function(keys,doctops) { 
    wkf <- read.csv(keys)
    doctops <- read.csv(doctops,as.is=T)

    list(wkf=wkf,
         doctops=doctops) 
    # topic words?
}

# analyze_model
#
# main function

analyze_model <- function(
        model_dir,
        doctops_file=file.path(model_dir,"doc_topics.csv"),
        keys_file=file.path(model_dir,"keys.csv"),
        report_dir=file.path(model_dir,"report"),
        keys_summary_file=file.path(report_dir,"keys_summary.csv"),
        generate_tym=F,
        instances_file=file.path(model_dir,"journals.mallet"),
        tym_result_file=file.path(report_dir,"tym_result.rda"), 
        dfr_analysis_root="~/Developer/dfr-analysis",
        dfr_analysis_source=file.path(dfr_analysis_root,"source_all.R"),
        dfr_data_root="~/Documents/research/20c/hls/tmhls/dfr-data" ,
        journal_dirs=c("elh_ci_all",
                       "mlr1905-1970",
                       "mlr1971-2013",
                       "modphil_all",
                       "nlh_all",
                       "pmla_all",
                       "res1925-1980",
                       "res1981-2012"),
        dfr_dirs=file.path(dfr_data_root,journal_dirs),
        generate_report=T,
        boxplot_time="10 years",
        model_smoothed=T,
        model_normalized=T,
        log_scale=model_smoothed) {

    analyze_model_wd <- getwd()
    setwd(dfr_analysis_root)
    source(dfr_analysis_source)
    topics_rmallet_setup()
    setwd(analyze_model_wd)

    # parameters



    if(!file.exists(report_dir)) {
        dir.create(report_dir)
    }

    # do the analysis

    message("Loading metadata")

    metadata <- read_metadata(file.path(dfr_dirs,"citations.CSV"))

    message("Loading modeling results")

    model <- import_model(keys_file,doctops_file)

    message("Converting document-topic matrix to long form") 

    model$dtl <- doc_topics_long(model$doctops,metadata,
                                 meta_keep="pubdate")
    dt_wide <- merge(model$doctops,metadata[,c("pubdate","id")],by="id")

    if(generate_report) {
        message("Generating report...")

        topic_report(dt_long=model$dtl,
                     wkf=model$wkf,
                     dt_wide=dt_wide,
                     time_breaks=boxplot_time,
                     log_scale=log_scale,
                     raw_counts=!model_normalized && !model_smoothed,
                     filename_base=report_dir)
        message("Reports saved to ",report_dir)
    }
    else {
        message("Skipping report generation")
    }

    write.csv(wkf_kf(model$wkf),keys_summary_file,
              quote=F,row.names=F)

    message("Saved summary of top key words to ",keys_summary_file)

    result <- list(model=model,metadata=metadata)

    if(generate_tym) {
        message("Reloading instances from ",instances_file)
        instances <- read_instances(instances_file)
        message("Generating term-document matrix")
        tdm <- instances_tdm(instances,big=T)

        message("Calculating term-year matrix")
        tym_result <- term_year_matrix(metadata=metadata,
                                       tdm=tdm,
                                       big=T,
                                       instances=instances)
        message("Saving result list to ",tym_result_file) 
        save(tym_result,file=tym_result_file) 

        result$tym_result <- tym_result 
    }

    # allow results to persist

    result
}

