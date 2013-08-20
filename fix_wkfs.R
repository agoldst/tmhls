# setwd("~/Documents/research/20c/hls/tmhls")
source("source_dfr_analysis.R")
source("analyze_model.R")

to_fix <- c("AHRk150v105",
            "HLSk200v105",
            "ahr_k100_v20000",
            "hls_k100_v100000",
            "hls_k200_v105_again",
            "hls_k300_v12000",
            "hls_k48_v100K")

for(m in file.path("models",to_fix)) {
    message("Working on ",m)
    kfile <- file.path(m,"keys.csv")
    stopifnot(file.exists(kfile))
    twfile <- file.path(m,"topic_words.csv")
    stopifnot(file.exists(twfile))
    vfile <- file.path(m,"vocab.txt")
    stopifnot(file.exists(vfile))

    out_file <- file.path(m,"wkf_fixed.csv")
    if(file.exists(out_file)) {
        message(out_file," already exists.")
        response <- readline("Overwrite? (yes/no) > \n")
        if(response != "yes") {
            message("Skipping...")
            next
        }
    }

    command <- "python python/fix_wkf.py"
    command <- paste(command,twfile,kfile,vfile)
    command <- paste(command,">",out_file)
    message("Executing ",command)

    result <- system(command)
    if(result != 0) {
        message("Script run unsuccessful")
    }
    else {
        message("Wrote ",out_file)
    }
}



