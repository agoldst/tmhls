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
             generate_report=F)
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
# main function for loading results of the topic modeling run. Also
# saves a file with just the head words for topics
#
# if generate_report=T (default), also generates a folder of plots
# giving topic overviews

analyze_model <- function(
        model_dir,
        doctops_file=file.path(model_dir,"doc_topics.csv"),
        keys_file=file.path(model_dir,"keys.csv"),
        report_dir=file.path(model_dir,"report"),
        keys_summary_file=file.path(report_dir,"keys_summary.csv"),
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

    # allow results to persist
    result
}

# generate_tym
#
# Save the term-year matrix for the corpus that was modeled to a file, in 
# native R format.

generate_tym <- function(
        instances_file,
        tym_result_file="tym.rda",
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
        dfr_analysis_root="~/Developer/dfr-analysis",
        dfr_analysis_source=file.path(dfr_analysis_root,"source_all.R")) {

    analyze_model_wd <- getwd()
    setwd(dfr_analysis_root)
    source(dfr_analysis_source)
    topics_rmallet_setup()
    setwd(analyze_model_wd)

    message("Reading metadata") 
    metadata <- read_metadata(file.path(dfr_dirs,"citations.CSV"))
    message("Reloading instances from ",instances_file)
    instances <- read_instances(instances_file)
    message("Generating term-document matrix")
    tdm <- instances_tdm(instances,big=T,verbose=T)

    message("Calculating term-year matrix")
    tym_result <- term_year_matrix(metadata=metadata,
                                   tdm=tdm,
                                   big=T,
                                   instances=instances)
    message("Saving result list to ",tym_result_file) 
    save(tym_result,file=tym_result_file) 
}

write_corpus_info_files <- function(model_dir,
        instances_file=file.path(model_dir,"journals.mallet"),
        vocab_file=file.path(model_dir,"vocab.txt"),
        id_map_file=file.path(model_dir,"id_map.txt")) {

    message("Reloading instances from ",instances_file)
    instances <- read_instances(instances_file)

    id_map <- instances_ids(instances)
    vocab <- instances_vocabulary(instances)
    instances <- read_instances(instances_file)

    message("Saving vocabulary to ",vocab_file)
    writeLines(vocab,vocab_file)

    message("Saving document id list to ",id_map_file)
    writeLines(id_map,id_map_file)
}

read_corpus_info_files <- function(model_dir,
        vocab_file=file.path(model_dir,"vocab.txt"),
        id_map_file=file.path(model_dir,"id_map.txt")) {
    vocab <- readLines(vocab_file)
    id_map <- readLines(id_map_file)

    list(vocab=vocab,id_map=id_map)
}


# generate_tytm
#
# Save matrices for occurrences of words in a given topic per year, in R
# native format.
#
# topics: the function will save one file per topic, holding a single
# list called tytm_result. The data *should* be in sparseMatrices,
# so these shouldn't be enormous files. Go ahead and set topics=1:k.
# But if you load more than one of the resultant files into the same
# environment you'll clobber the tytm_result objects and be left only
# with the last one you loaded. arRgh.

generate_tytm <- function(
        topics,
        ss,         # the "simplified state"--see load_ss() below
        model_dir,
        tytm_file_template=file.path(model_dir,"tytm%03d.rda"), # for sprintf
        vocab_file=file.path(model_dir,"vocab.txt"),
        id_map_file=file.path(model_dir,"id_map.txt"),
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
        dfr_analysis_root="~/Developer/dfr-analysis",
        dfr_analysis_source=file.path(dfr_analysis_root,"source_all.R")) {

    analyze_model_wd <- getwd()
    setwd(dfr_analysis_root)
    source(dfr_analysis_source)
    topics_rmallet_setup()
    setwd(analyze_model_wd)

    message("Reading metadata") 
    metadata <- read_metadata(file.path(dfr_dirs,"citations.CSV"))
    id_map <- readLines(id_map_file)
    vocab <- readLines(vocab_file)


    for(topic in topics) {
        tytm_file <- sprintf(tytm_file_template,topic)

        message("Getting the term-year matrix for topic ",topic)
        tytm_result <- term_year_topic_matrix(topic,ss,id_map,metadata,vocab)
        
        save(tytm_result,file=tytm_file) 
        message("Saved the results in R native format to ",tytm_file)
    }
}

# load_ss
#
# load the "simplified state," i.e. the Gibbs sampling state aggregated into
# doc-word-topic counts. Returns a reference to a big.matrix.

load_ss <- function(model_dir=".",
        ss_file=file.path(model_dir,"state_simple.csv"),
        state_file=file.path(model_dir,"mallet_state.gz"),
        ss_script="python/simplify_state.py") {

    if(!file.exists(ss_file)) {
        message("Simplified state file ",ss_file," not found. It will be regenerated, then loaded.")
        generate_ss <- T
    }
    else {
        generate_ss <- F
    }

    ss <- read_simplified_state(infile=ss_file,
                                generate_file=generate_ss,
                                state_file=state_file,
                                simplifier=ss_script,
                                big=T)
    ss
}
