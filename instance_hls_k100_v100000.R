pwd <- getwd()
setwd("/Users/agoldst/Documents/research/20c/hls/tmhls/")
source("make_instance.R")

result <- make_instance(
        outdir="models/hls_k100_v100000",
        aquo=as.Date("1880-01-01"),
        adquem=as.Date("2013-12-31"),
        itemtypes="fla\t",
        exclude=mlr_review_exclude,                     # a function
        length_min=1000,                                # words
        freq_threshold=NULL,
        rank_threshold=100000)

setwd(pwd)
