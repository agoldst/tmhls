make_topic_table <- function(wkfile="models/hls_k150_v100K/wkf_nosmooth.csv",
                            outfile="essay/topic_table.tex",
                            n_words=10) {
    library(plyr)
    library(xtable)

    wkf <- read.csv(wkfile,as.is=T)

    labeler <- function(d,n) {
        data.frame(label=paste(d$word[order(d$weight,decreasing=T)][1:n],
              collapse=" "),stringsAsFactors=F)
    }

    tlabels <- ddply(wkf,.(topic),labeler,n=n_words)

    sink(outfile)
    print(xtable(tlabels),include.rownames=F,
          tabular.environment='longtable',
          floating=F)
    sink()
    message("Wrote ",outfile)
}

# main program
make_topic_table()
