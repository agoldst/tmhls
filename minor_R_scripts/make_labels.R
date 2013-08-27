
setwd("~/Documents/research/20c/hls/tmhls")
library(Matrix)
source("analyze_model.R")
m <- do.call(analyze_model,model_files("hls_k150_v100K"))
# tym_result:
load("models/hls_k150_v100K/tym.rda")
m$n <- length(unique(m$wkf$topic))


# just a wrapper for cor on corresponding rows of 2 matrices
word_topic_cor <- function (n,m1,m2) {
    suppressWarnings(cor(m1[n,],m2[n,]))
}

top_corr_words <- function(topic,tym_m,norm_m,n=10,
                           threshold=0.25) {
    load(sprintf("models/hls_k150_v100K/tytm/%03d.rda",topic))
    if(!is.null(norm_m))  {
        tytm_m <- as.matrix(tytm_result$tym %*% norm_m)
    }
    else {
        tytm_m <- as.matrix(tytm_result$tym)
    }

    wkf <- m$wkf[m$wkf$topic==topic,]
    if(!is.null(threshold)) {
        cutoff <- sum(m$doctops[,topic]) * threshold
        # assume wkf is in decreasing order by weight
        j <- match(T,cumsum(wkf$weight) > cutoff)
        if(!is.na(j)) {
            keywords <- wkf$word[1:j]
        }
        else {
            keywords <- wkf$word
        }
    }
    else {
        keywords <- wkf$word
    }
    message("Choosing among top ", length(keywords)," words")
    if(length(keywords) < n) {
        n <- length(keywords)
    }

    words <- match(keywords,m$vocab)

    wtcs <- sapply(words,word_topic_cor,
                   m1=tym_m,m2=tytm_m)
    o <- order(wtcs,decreasing=T)[1:n]
    data.frame(topic=topic,word=m$vocab[words[o]],r=wtcs[o])
}

make_label_frame <- function(n_words=10,normalize=T,
                             threshold=0.25) {
    lst <- vector("list",m$n)
    if(normalize) { 
        norm_m <- Diagonal(x=1 / colSums(tym_result$tym))
        tym_m <- as.matrix(tym_result$tym %*% norm_m)
    }
    else {
        norm_m <- NULL
        tym_m <- as.matrix(tym_result$tym)
    }

    for(i in seq(m$n)) {
      message("Working on topic ",i)
      lst[[i]] <- top_corr_words(topic=i,
                                 n=n_words,
                                 tym_m=tym_m,
                                 norm_m=norm_m,
                                 threshold=threshold)
    }
    do.call(rbind,lst)
}

n_label_words <- 10
lfrm <- make_label_frame(n_words=n_label_words,
                         normalize=T,
                         threshold=0.25)
write.csv(lfrm,"models/hls_k150_v100K/tytm_corr_labels.csv",quote=F)
labelings <- sapply(seq(m$n),function(i) {
                    paste(lfrm$word[lfrm$topic==i],collapse=" ")
                   })
labelings <- paste(sprintf("%03d",seq(m$n)),labelings)
writeLines(labelings,"models/hls_k150_v100K/tytm_corr_labels.txt")

