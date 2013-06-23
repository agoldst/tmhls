
tym_coha <- function(coha_dir) {

    coha_freqs <- read.table("1_pos_n_cs_n.txt",header=F,skip=3,
                             col.names=c("freq","word","decade"),
                             colClasses=c(integer(),factor(),factor()),
                             quote="",comment.char="",fill=T)

    coha_decades <- read.table("COHAdecadeSize.txt",header=F,skip=3,
                               col.names=c("code","decade","total"),
                               quote="",comment.char="",as.is=T)
    decade_map <- with(coha_decades,
                       as.Date(paste(substr(decade,1,4),
                                     "-01-01",
                                     sep="")))
    names(decade_map) <- coha_decades$code

    result <- acast(coha_freqs,word ~ decade,value.var="freq",fill=0)
    dec_code <- colnames(result)
    colnames(result) <- as.character(decade_map[dec_code])
    result
}

coha_word_plot <- function(word,m,...) {
    mallet_word_plot(word,m,year_seq=colnames(m),vocab=rownames(m),...)
}
