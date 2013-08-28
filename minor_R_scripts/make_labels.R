# DEPRECATED

top_corr_words <- function(topic,topic_yearly,norm_m,n=10,
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

    wtcs <- sapply(words,function(w) {
                       suppressWarnings(cor(topic_yearly,tytm_m[w,]))
                   })
                   
    o <- order(wtcs,decreasing=T)[1:n]
    data.frame(topic=topic,word=m$vocab[words[o]],r=wtcs[o])
}

make_label_frame <- function(n_words=10,normalize=T,
                             threshold=0.25) {
    lst <- vector("list",m$n)
    if(normalize) { 
        norm_m <- Diagonal(x=1 / colSums(m$yrly))
        topic_ym <- as.matrix(m$yrly %*% norm_m)
    }
    else {
        norm_m <- NULL
        topic_ym <- m$yrly
    }

    for(i in seq(m$n)) {
      message("Working on topic ",i)
      lst[[i]] <- top_corr_words(topic=i,
                                 n=n_words,
                                 topic_yearly=topic_ym[i,],
                                 norm_m=norm_m,
                                 threshold=threshold)
    }
    do.call(rbind,lst)
}

# words_totals_scatterplot()
#
# not called below, but useful for looking at the meanings of
# correlations interactively after you do labelings.
#
# Compares yearly counts of words to that topic's yearly totals
#
# The baseline is always m$yrly[topic,], the total number of words in
# that topic each year
#
# The comparison is drawn with a term_year_matrix. Try it with either
# a term-year- topic-matrix (load tytm/NNN.rda) or the corpus-wide
# term-year-matrix (load tym.rda).


words_totals_scatterplot <- function(words,topic,
                                     term_year_matrix)  {
    w <- match(words,m$vocab)
    yearly_totals <- m$yrly[topic,]
    to.plot <- data.frame(count=as.vector(term_year_matrix[w,]),
                          word=rep(words,times=length(yearly_totals)),
                          yearly_total=rep(yearly_totals,each=length(words)))
    ggplot(to.plot,aes(yearly_total,count,color=word)) +
        geom_point() + geom_smooth(aes(group=word),method="lm")
}



# main program

setwd("~/Documents/research/20c/hls/tmhls")
library(Matrix)
source("analyze_model.R")
m <- do.call(analyze_model,model_files("hls_k150_v100K"))
m$n <- length(unique(m$wkf$topic))
m$dtw <- merge(m$doctops,m$metadata[,c("id","pubdate")],by="id")
m$yrly <- tm_yearly_totals(tm_wide=m$dtw)

n_label_words <- 10
lfrm <- make_label_frame(n_words=n_label_words,
                         normalize=T,
                         threshold=0.25)

# save dataframe
lfrm_file <- "models/hls_k150_v100K/tytm_corr_labels_data.csv"
write.csv(lfrm,lfrm_file,quote=F)
message("Saved data frame with correlated key words to ",lfrm_file)

# and also generate a shadow keys-summary file
m$a <- m$wkf$alpha[seq(from=1,to=149*50 + 1,by=50)]
stopifnot(all(m$a==unique(m$wkf$alpha)))

labelings <- sapply(seq(m$n),function(i) {
                    paste(lfrm$word[lfrm$topic==i],collapse=" ")
                   })
labelings <- paste(seq(m$n),
                   m$a,
                   labelings,
                   sep=",")
label_file <- "models/hls_k150_v100K/tytm_corr_labels_short.csv"

writeLines(labelings,label_file)
message("Saved data for topic_labels.py script to ",label_file)

