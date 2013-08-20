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

for(m in file.path("model",to_fix)) {
    kfile <- file.path(m,"keys.csv")
    stopifnot(file.exists(kfile))
    twfile <- file.path(m,"topic_words.csv")
    stopifnot(file.exists(twfile))
    vfile <- file.path(m,"vocab.txt")
    stopifnot(file.exists(vfile))

    out_file <- file.path(m,"wkf_reconstructed.csv")
    if(file.exists(out_file)) {
        stop(paste(out_file,"already exists."))
    }


    message("Working on ",to_fix)

    wkf <- reconstruct_wkf(kfile,twfile,vfile)

    write.table(wkf,
              out_file,
              quote=F,sep=",",
              row.names=F,
              col.names=T)
    
    message("Wrote ",out_file)
}



