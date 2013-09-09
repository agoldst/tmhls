# USAGE
#
# All the plotting functions look for data in a global variable m.
# To set this up:
#
# source("make_figures.R")
# m <- make_figures_setup(workingdir="underwood",
#                         dfr_analysis="/location/of/dfr-analysis/",
#                         citations_files=c("/location/of/1/citations.CSV",
#                                           "/location/of/2/citations.CSV",...))
# render_all(fig_dir="location/of/essay/figure/")
# # or
# fig_criticism(fig_dir="location/of/essay/figure/")
# # etc 

library(ggplot2)
library(plyr)
library(Matrix)
library(zoo,warn.conflicts=F)

# global variables: plot_theme, m (initialized below)
# ----------------

# ggplot theming 
plot_theme <- theme_bw(base_size=10,base_family="sans") 

add_year_proportion_axes <- function(p,
        xlabel="article publication year",
        ylabel="proportion of words in corpus",
        yscale=scale_y_continuous(labels=percent_format())) {
    p + xlim(as.Date("1895-01-01"),as.Date("2005-01-01")) +
        yscale +
        xlab(xlabel) +
        ylab(ylabel)
}

our_geom_smooth <- geom_smooth(method="loess",span=0.5,
                               fill="grey60",
                               color="black",se=T)
time_series_geom <-
        geom_bar(stat="identity",fill="grey80",width=90)

# functions
# ---------

# topic_name_fig
#
# generate names for labeling topics on a figure
#
# topic_names (dfr-analysis/topics_vis.R) goes by weight (naive ordering)
#
# topic: a vector of topic numbers (from 1)

topic_name_fig <- function(topics,n=4) {
    topic_names(m$wkf,n,topics,
                "%d %s")
}

# render_plot
#
# wrapper for setting up graphics device

render_plot <- function(p,filename,fig_dir="essay/figure",
                        w=5,h=3, # pdf: inches
                        render_function=print) {

    pdf(file.path(fig_dir,filename),width=w,height=h)

    render_function(p)

    dev.off()
    message("Saved ",filename," in ",fig_dir)
}

single_topic_plot <- function(topic,filename,fig_dir,w=5,h=3) {
    to_plot <- topic_proportions_series_frame(yearly=m$topic_year,
                                              topics=topic,
                                              denominator=NULL,
                                              rolling_window=1)
    p <- ggplot(to_plot,aes(year,weight))
    p <- p +
        time_series_geom +
        our_geom_smooth

    p <- add_year_proportion_axes(p)    
    p <- p + plot_theme + ggtitle("")
    render_plot(p,filename,fig_dir)
    
    p
}

# specific figures...


fig_criticism <- function(filename="criticism.pdf",fig_dir="essay/figure") {
    message("[fig:criticism]")

    to_plot <- list()
    to_plot[[1]] <- topic_proportions_series_frame(
        yearly=m$topic_year,
        topics=16,
        denominator=NULL,
        rolling_window=1)

    # TODO show frequency in unstopped corpus?
    to_plot[[2]] <- term_year_series_frame("criticism",
        term_year=m$term_year,
        year_seq=m$term_year_yseq,
        vocab=m$vocab,
        raw_counts=F) # take yearly proportions

    render_plot(to_plot,filename,fig_dir,
                w=5,h=3, # TODO figure dimensions
                render_function=function(to_plot) {
        grid.newpage()
        pushViewport(viewport(layout=grid.layout(2,1,heights=c(2,1))))


        plot_labels <- c("topic 16","the word \"criticism\"")
        plot_rows <- list(c(1,2),3)
        for(i in 1:2) {

            p <- ggplot(to_plot[[i]],aes(year,weight)) +
                time_series_geom +
                our_geom_smooth

            p <- p +
                annotate(geom="text",
                         size=rel(3),   # nasty suspicion this is relative to
                                        # the size of the bars
                         hjust=0,
                         label=plot_labels[i],
                         x=as.Date("1895-01-01"),
                         y=0.95 * max(to_plot[[i]]$weight))

            # TODO better axis/label placement

            if(i == 1) {
                p <- add_year_proportion_axes(p,
                                              xlabel="")
            }
            if(i == 2) {
                p <- add_year_proportion_axes(p,ylabel="",
                    yscale=scale_y_continuous(limits=c(0,0.0015),
                                              labels=percent_format()))
            }


            p <- p + plot_theme

            print(p,
                  vp=viewport(layout.pos.row=i,
                              layout.pos.col=1))

        }
    })

}

fig_formalism_waves <- function(filename="formalism-waves.pdf",
                                fig_dir="essay/figure") {
    message("[fig:formalism-waves]")

    topics <- c(17,29,53)
    to.plot <- topic_proportions_series_frame(
        yearly=m$topic_year,
        topics=topics,
        denominator=NULL,
        rolling_window=3)

    # TODO rolling average smoothing okay?
    # TU's original manual smoothing looks like:
    # ntf = array(data= 0, dim = dim(NormalizedTopicFreqs))
    # for (i in 1:150) {
    #   ntf[i,] = smooth(NormalizedTopicFreqs[i, ] * 100, twiceit = TRUE)
    # }
    

    to.plot$topic <- factor(to.plot$topic)


    chromatic <- c("gray20", "gray40", "gray65")
    
    # TODO order=rev(topic) is voodoo that only happens to work in this case
    p <- ggplot(to.plot,aes(year,weight,fill=topic,group=topic,
                            order=rev(topic))) +
         geom_area()
    p <- p +
        scale_fill_manual(values=chromatic) +
        scale_colour_manual(values=chromatic)
    p <- add_year_proportion_axes(p)

    # TODO topic labels on plot body

    p <- p + plot_theme + ggtitle("")
    render_plot(p,filename,fig_dir)

    p
}

fig_recent <- function(filename="recent.pdf",fig_dir="essay/figure") {
    message("[fig:recent]")

    # on fixing up layout and strip.background=element_blank()
    # http://stackoverflow.com/questions/17144182/ggplot2-and-gridextra-completely-remove-strip-in-facet-grid-not-just-invisibl
    # http://stackoverflow.com/questions/14185754/remove-strip-background-keep-panel-border?rq=1

    roll <- 3
    # 010 would be good to show, but it's confusing
    recent_theory <- c(015,143,138,058)

    recent_themes <- c(019,025,048,069,036,
                       004,108,077,102)             

    p <- list()
    tlist <- list(recent_theory,recent_themes)
    for(i in 1:2) {
        topics <- tlist[[i]]
        to_plot <- topic_proportions_series_frame(
            yearly=m$topic_year,
            topics=topics,
            denominator=NULL,
            rolling_window=roll)

        to_plot$topic <- factor(to_plot$topic,levels=topics)
        levels(to_plot$topic) <- topic_name_fig(topics)

        # NB free scale on y axis
        p[[i]] <- ggplot(to_plot,aes(year,weight)) +
            time_series_geom +
            our_geom_smooth +
            facet_wrap(~ topic,ncol=1,scales="free_y")

        p[[i]] <- add_year_proportion_axes(p[[i]]) +
            theme(axis.text=element_text(size=9),
                  strip.text=element_text(size=9)) +
            plot_theme + ggtitle("")
    }


    render_plot(p,filename,fig_dir,
                w=5,h=7, # TODO figure dimensions
                render_function=function(p) {
        grid.newpage()
        pushViewport(viewport(layout=grid.layout(1,2)))

        print(p[[1]],
              vp=viewport(layout.pos.row=1,layout.pos.col=1))

        print(p[[2]],
              vp=viewport(layout.pos.row=1,layout.pos.col=2))
        }
    )

    p
}

fig_numbers <- function(filename="numbers.pdf",fig_dir="essay/figure") {
    message("[fig:numbers]")

    cardinals <- c("one", "two", "three", "four", "five", "six", "seven",
                   "eight", "nine", "ten", "eleven", "twelve", "thirteen",
                   "fourteen", "fifteen", "sixteen", "seventeen", "eighteen",
                   "nineteen", "twenty", "thirty", "forty", "fifty", "sixty",
                   "seventy", "eighty", "ninety", "hundred")
    ordinals <-  c("first", "second", "third", "fourth", "fifth", "sixth",
                   "seventh", "eighth", "ninth", "tenth")

    numbers <- c(cardinals,ordinals)

    # NB in stripped corpus, "one" is a stopword
    # TODO show frequency in unstopped corpus?
    to_plot <- term_year_series_frame(numbers,
                                      term_year=m$term_year,
                                      year_seq=m$term_year_yseq,
                                      vocab=m$vocab,
                                      raw_counts=F, # take yearly proportions
                                      total=T) # F?

    p <- ggplot(to_plot,aes(year,weight)) +
        time_series_geom +
        our_geom_smooth

    # TU's original plot looks like
    #
    # p <- qplot(yearsequence, numbertrajectory * 100, geom = c("point", "smooth"), span = 0.5, ylab = "percentage of corpus", xlab = "", main = "cardinal and ordinal number words, one through a hundred")
    #
    # I have swapped lines for points. Rolling averages would be less 
    # aggressively smooth than loess.


    p <- add_year_proportion_axes(p) +
        ggtitle("") +
        plot_theme

    render_plot(p,filename,fig_dir)

    p
}

fig_t080 <- function (filename="t080.pdf",fig_dir="essay/figure") {
    message("[fig:t080]")

    single_topic_plot(80,filename,fig_dir)
}


fig_theory <- function(filename="theory.pdf",fig_dir="essay/figure") {
    message("[fig:theory]")

    topics <- c(94,20,39,143)
    to_plot <- topic_proportions_series_frame(
        yearly=m$topic_year,
        topics=topics,
        denominator=NULL,
        rolling_window=1)

    to_plot$topic <- factor(to_plot$topic,levels=topics)
    levels(to_plot$topic) <- topic_name_fig(topics)

    # NB free scale on y axis
#    p <- ggplot(to_plot,aes(year,weight,fill=topic)) +
#        geom_area(position="stack") +
#        scale_fill_grey() +
#        theme(legend.position="bottom")
        
    p <- ggplot(to_plot,aes(year,weight)) +
         time_series_geom +
         our_geom_smooth +
         facet_wrap(~ topic,ncol=1,scales="free_y")
    #    facet_wrap(~ topic,ncol=2)

    p <- add_year_proportion_axes(p) +
            plot_theme +
            theme(axis.text=element_text(size=9),
                  strip.text=element_text(size=9),
                  strip.background=element_blank()
                  # strip.text.x=element_blank() 
                  ) +
             ggtitle("")

    render_plot(p,filename,fig_dir)
}


    

# setup and execution
# -------------------
make_figures_setup <- function(
        workingdir = "~/Documents/research/20c/hls/tmhls",
        model_dir = file.path(workingdir,"models/hls_k150_v100K"),
        dfr_analysis="~/Developer/dfr-analysis",
        dfr_data_root=file.path(workingdir,"dfr-data"),
        journal_dirs=c("elh_ci_all",
                       "mlr1905-1970",
                       "mlr1971-2013",
                       "modphil_all",
                       "nlh_all",
                       "pmla_all",
                       "res1925-1980",
                       "res1981-2012"),
        citations_files=file.path(dfr_data_root,journal_dirs,"citations.CSV"),
        keys_file=file.path(model_dir,"wkf_nosmooth.csv")) {

    # a special form for workingdir
    if (workingdir == "underwood")  {
        workingdir <- "/Users/tunderwood/Journals/new results/hls_k150_v100K"
        model_dir <- workingdir
        keys_file <- file.path(model_dir,"keys.csv") # NEEDED ?
    }

    # load functions from dfr-analysis
    setwd(dfr_analysis)
    message("wd now:",dfr_analysis)
    source("metadata.R")
    source("topics_rmallet.R")
    source("topics_vis.R")

    setwd(workingdir)
    message("wd now:",workingdir)


    # initialize result object
    m <- list()

    m$model_dir <- model_dir # store for later loading of tytm/*

    message("Loading metadata")
    # compute file paths
    m$metadata <- read_metadata(citations_files)

    message("Loading modeling results") 
    # compute file paths
    m$wkf <- read.csv(keys_file,as.is=T)
    m$doctops <- read.csv(file.path(model_dir,"doc_topics.csv"),as.is=T)
    m$vocab <- readLines(file.path(model_dir,"vocab.txt"))
    m$id_map <- readLines(file.path(model_dir,"id_map.txt"))

    m$dtw <- merge(m$doctops,m$metadata[,c("id","pubdate")],by="id")
    m$topic_year <- tm_yearly_totals(tm_wide=m$dtw)
    m$dtm <- doc_topics_matrix(m$doctops)
    m$n <- length(unique(m$wkf$topic))

    # tym_result:
    load(file.path(model_dir,"tym.rda"))
    m$term_year <- tym_result$tym
    m$term_year_yseq <- tym_result$yseq

    m
}

fig_power <- function(filename="power.pdf",fig_dir="essay/figure") {
  message("[fig:power]")
  
  # single word, but could tally up total for multiple words
  series_list <- vector("list",m$n)
  series_keep <- logical(m$n)
  cur <- 1
  topic_labels <- character()
  totals <- colSums(m$term_year)   # totals for *all* topics
  
  topics <- c(10,80,93) #??? 
  word <- "power"
  
  for(topic in topics) {
    load(file.path(m$model_dir,sprintf("tytm/%03d.rda",topic)))
    series <- term_year_series_frame(word,
                                     tytm_result$tym,tytm_result$yseq,
                                     m$vocab,
                                     raw_counts=F,
                                     denominator=totals)
    if(any(series$weight > 0)) {
      series$topic <- topic_name_fig(topic)
      series_list[[cur]] <- series
      cur <- cur + 1
    } # (if)
  } # (for)
  series_frame <- do.call(rbind,series_list)
  
  total_frame <- term_year_series_frame(word,
                                        term_year=m$term_year,
                                        year_seq=m$term_year_yseq,
                                        vocab=m$vocab,
                                        raw_counts=F)
  
  p <- ggplot(total_frame,aes(year,weight))
  p <- p + geom_line(linetype=2,color="grey50")
  p <- p + geom_area(data=series_frame,
                     aes(group=topic,fill=topic)) +
    scale_fill_grey()
  
  p <- p + plot_theme
  
  render_plot(p,filename,fig_dir,
              w=6,h=4)
  
}

render_all <- function (fig_dir="essay/figure") {
  stopifnot(exists("m"))
  fig_numbers(fig_dir=fig_dir)
  fig_criticism(fig_dir=fig_dir)
  fig_recent(fig_dir=fig_dir)
  fig_t080(fig_dir=fig_dir)
  fig_theory(fig_dir=fig_dir)
  alt_fig_power(fig_dir=fig_dir)
  alt_fig_form(fig_dir=fig_dir)
  
  return()
}

# main program
if(!exists("m")) {
  message("No 'm' found; do: m <- make_figures_setup()")
}

message("No plots rendered; call fig_* or render_all()")

## ------------------------------------------
## Alternate functions using paths that work on Underwood's computer.

moving_average <- function(avector, window) {
  vectorlen = length(avector)
  smoothedvector = numeric(vectorlen)
  for (i in seq(vectorlen)) {
    windowstart = i - window
    windowend = i + window
    if (windowstart < 1) windowstart = 1
    if (windowend > vectorlen) windowend = vectorlen
    smoothedvector[i] = mean(avector[windowstart: windowend])
  }
  smoothedvector
}

alt_fig_power <- function(filename="power.pdf",fig_dir="essay/figure", word = "power") {
  message("[fig:power]")
  
  AllWords <- m$vocab
  yseries = numeric()
  
  yearsequence = seq(1889, 2012)
  topics <- c(80, 10)
  topiclabel = c("80", "10", "total")
  wordidx = which(AllWords == word)
  
  library(Matrix)
  tym_m <- as.matrix(m$term_year)
  #   use this denominator for "percent of X word in topic Y"
  #   denominator = tym_m[wordidx, ]
  #   print(denominator)
  # this denominator gives "percent of total vocab that is X word in topic Y"
  denominator = integer(125)
  for (i in seq(125)) {
    denominator[i] = sum(tym_m[ , i])
  }
  theorder = numeric()
  count = 1
  for(topic in topics) {
    load(file.path(m$model_dir,sprintf("tytm/%03d.rda",topic)))
    tytm_m <- as.matrix(tytm_result$tym)
    termyearvector <- moving_average(((tytm_m[wordidx, ] / denominator) * 100), 2)
    termyearvector <- termyearvector[1:124]
    yseries = c(yseries, termyearvector)
    theorder = c(theorder, rep(count, 124))
    count = count + 1
  }
  
  allother <- rep(0, 124)
  for (topic in seq(150)) {
    if (!topic %in% topics) {
      load(file.path(m$model_dir,sprintf("tytm/%03d.rda",topic)))
      tytm_m <- as.matrix(tytm_result$tym)
      termyearvector <- ((tytm_m[wordidx, ] / denominator) * 100)
      allother <- allother + termyearvector[1:124]
    }
  }
  allother <- moving_average(allother, 2)
  yseries <- c(yseries, allother)
  theorder = c(theorder, rep(count, 124))
  
  df <- data.frame(x = rep(yearsequence, 3), y = yseries, topics = as.character(theorder), topic = c(rep(topiclabel[1],124), rep(topiclabel[2],124), rep(topiclabel[3], 124)))
  levels(df$topic) <- topiclabel
  df$topics <- factor(df$topics, levels = c(3,2,1))
  
  chromatic <- rev(c("gray10", "gray40", "gray75"))
  
  p <-ggplot(df, aes(x=x, y=y, group = topics, colour = topics, fill = topics, order = -as.integer(topics)))
  p <- p + geom_area(aes(colour= topics, fill = topics), position = 'stack') + scale_colour_manual(values=chromatic, legend = FALSE)  + scale_fill_manual(values = chromatic, labels = rev(topiclabel))
  p <- p + scale_x_continuous("") + scale_y_continuous("'power' as a % of words in the whole corpus")
  p <- p + plot_theme

  render_plot(p,filename, fig_dir,
              w=6,h=4)
  p
}

alt_fig_form <- function(filename="formalism-waves.pdf",fig_dir="essay/figure") {
  message("[fig:form]")
  
  AllWords <- m$vocab
  
  wordlists = rev(c("style manner", "verse meter", "pattern imagery symbol", "metaphor metaphors literal"))
  
  yseries = numeric()
  stackorder = numeric()
  yearsequence = seq(1889, 2012)
  
  tym_m <- as.matrix(m$term_year)
  
  denominator = integer(125)
  for (i in seq(125)) {
    denominator[i] = sum(tym_m[ , i])
  }
  ordercount = 1
  for (discourse in wordlists) {            # LOL
    thisdiscoursefrequency = rep(0, 124)
    words <- strsplit(discourse, " ")[[1]]
    for (word in words) {
      wordidx = which(AllWords == word)
      thisdiscoursefrequency = thisdiscoursefrequency + tym_m[wordidx, ]
    }
    thisdiscoursefrequency = moving_average((thisdiscoursefrequency / denominator), 2)
    yseries = c(yseries, thisdiscoursefrequency[1:124])
    stackorder = c(stackorder, rep(ordercount, 124))
    ordercount = ordercount + 1
  }
  
  
  df <- data.frame(x = rep(yearsequence, length(wordlists)), y = yseries, vocabulary = as.character(stackorder),  topic = c(rep(wordlists[1],124), rep(wordlists[2],124), rep(wordlists[3], 124), rep(wordlists[4], 124)))
  levels(df$topic) <- wordlists
  df$vocabulary <- factor(df$vocabulary, levels = c(1,2,3,4))
  chromatic <- c("gray10", "gray45", "gray80", "gray30")
  
  p <-ggplot(df, aes(x=x, y=y, group = vocabulary, colour = vocabulary, fill = vocabulary, order = -as.integer(vocabulary)))
  p <- p + geom_area(aes(colour= vocabulary, fill = vocabulary), position = 'stack') + scale_colour_manual(values=chromatic, legend = FALSE)  + scale_fill_manual(values = chromatic, labels = wordlists)
  p <- p + scale_x_continuous("") + scale_y_continuous("percentage of words in the whole corpus")
  p <- p + plot_theme

  render_plot(p,filename, fig_dir,
              w=6,h=4)
  
  p
}


