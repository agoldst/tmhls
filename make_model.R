
make_model_main <- function() {
    source("~/Developer/dfr-analysis/source_all.R")

    output_dir <- "/spare2/ag978/130612/"

    modeling_params <- list(
        instances="/spare2/ag978/130612/journals.mallet",
        num.topics=150,
        alpha.sum=5,
        beta=0.01 ,
        n.iters=500,
        n.max.iters=10,
        n.hyper.iters=20,
        n.burn.in=50,
        threads=16L)

    testing <- F 
    # uncomment to use fast test settings
    # testing <- T
    if(testing) {
        modeling_params$instances <- "/Users/agoldst/Documents/research/20c/hls/tmhls/models/test/journals.mallet"
        modeling_params$n.iters <- 200
        modeling_params$num.topics <- 10
        modeling_params$threads <- 2L
        output_dir <- "/Users/agoldst/Documents/research/20c/hls/tmhls/models/test"
    }


    doctopics_file <- file.path(output_dir,"topics.csv")
    state_file <- file.path(output_dir,"mallet_state.gz")

    # how many "top key words" for each topic?
    num.top.words <- 50
    wk_file <- file.path(output_dir,"keys.csv")

    # give a filename to save this one
    # weights_file <- NULL
    weights_file <- file.path(output_dir,"weights.tsv")

    message("Beginning mallet train-topics run...")

    trainer <- do.call(train_model,modeling_params)

    message("mallet run complete.")
    message("Saving document topics to ",doctopics_file)
    doctopics <- topic_frame(trainer)
    write.table(doctopics,
              doctopics_file,
              quote=F,sep=",",
              row.names=F,
              col.names=T)

    message("Saving state to ",state_file)
    write_mallet_state(trainer,outfile=state_file)

    message("Saving weighted keys to ",wk_file)
    wkf <- weighted_keys_frame(trainer,num.top.words=num.top.words)
    write.table(wkf,
              wk_file,
              quote=F,sep=",",
              row.names=F,
              col.names=T)

    # This is not as useful, so by default we won't make this huge file

    if(!is.null(weights_file)) {
        message("Saving topic word weights to ",weights_file)

        write_topic_words(trainer,outfile=weights_file)
    }

    trainer
}

# execution: should allow the trainer object to persist

trainer <- make_model_main()

# TODO diagnostics
