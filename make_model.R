
make_model_main <- function() {
    source("~/Developer/dfr-analysis/source_all.R")

    modeling_params <- list(
        instances="/spare2/ag978/journals130611.mallet",
        num.topics=150,
        alpha.sum=5,
        beta=0.01 ,
        n.iters=200,
        n.max.iters=10,
        n.hyper.iters=20,
        n.burn.in=50,
        threads=4L)
    # uncomment to use fast test settings
    modeling_params$instances <- "/Users/agoldst/Documents/research/20c/hls/dfr-data/out/journals.mallet"
    modeling_params$num.topics <- 10

    doctopics_file <- "models/topics.csv"
    state_file <- "models/mallet_state.gz"
    num.top.words <- 20
    wk_file <- "models/keys.csv"

    # give a filename to save this one
    weights_file <- NULL
    # weights_file <- "models/weights.tsv"

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
}

# execution

make_model_main()
