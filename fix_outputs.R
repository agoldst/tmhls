# setwd("~/Documents/research/20c/hls/tmhls")

to_fix <- c("hls_k100_v100000",
            "hls_k48_v100K")

prompt_overwrite <- function(f) {
    if(file.exists(f)) {
        message(f," already exists.")
        response <- readline("Overwrite? (yes/no) > \n")
        if(response != "yes") {
            message("Skipping...")
            return(F)
        }
        else {
            return(T)
        }
    }
    else {
        return(T)
    }
}

for(m in file.path("models",to_fix)) {
    message("Working on ",m)

    statefile <- file.path(m,"mallet_state.gz")
    stopifnot(file.exists(statefile))

    twfile <- file.path(m,"topic_words_fixed.csv")
    if(prompt_overwrite(twfile)) {
        message("Generating ",twfile)
        system(paste("python python/topic_words.py",
                     statefile,
                     ">",
                     twfile))
        message("Done.")
    }

    ssfile <- file.path(m,"state_simple.csv")
    if(prompt_overwrite(ssfile)) {
        message("Generating ",ssfile)
        system(paste("python python/simplify_state.py",
                     statefile,
                     ">",
                     ssfile))
        message("Done.")
    }


    dtfile <- file.path(m,"doc_topics_fixed.csv")
    if(prompt_overwrite(dtfile)) {
        n_topics <- substr(m,13,nchar(m))
        n_topics <- sub('_.*',"",n_topics)
        message("Generating ",dtfile)
        stopifnot(file.exists(ssfile))
        system(paste("python python/doc_topics.py",
                     ssfile,
                     n_topics,
                     ">",
                     dtfile))
        message("Done.")
    }


    wkfile <- file.path(m,"wkf_fixed.csv")
    if(prompt_overwrite(wkfile)) {
        message("Generating ",wkfile)
        vfile <- file.path(m,"vocab.txt")
        stopifnot(file.exists(vfile))
        kfile <- file.path(m,"keys.csv")
        stopifnot(file.exists(kfile))

        system(paste("python python/fix_wkf.py",
                     twfile,kfile,vfile,
                     ">",
                     wkfile))
        message("Done.")
    }
}



