library(ggplot2)

# global variables: plot_theme, m (initialized below)
# ----------------

# ggplot theming 
plot_theme <- theme_bw(base_size=10,base_family="sans") 

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

# specific figures...


fig_criticism <- function(filename="criticism.pdf",fig_dir="essay/figure") {
    message("[fig:criticism]")

    p <- tm_yearly_line_plot(.yearly_totals=m$yrly,topics=16,raw_counts=T)
    p <- p + 
        scale_y_continuous(labels=percent_format()) +
        xlab("article publication year")
    
    p <- p + plot_theme + ggtitle("")
    render_plot(p,filename,fig_dir)
    
    p
}

fig_formalism_waves <- function(filename="formalism-waves.pdf",
                                fig_dir="essay/figure") {
    message("[fig:formalism-waves]")

    topics <- c(17,29,53)
    to.plot <- topic_proportions_series_frame(
        yearly=m$yrly,
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
    p <- p +
        scale_y_continuous(labels=percent_format()) +
        xlab("article publication year")

    # TODO topic labels on plot body

    p <- p + plot_theme + ggtitle("")
    render_plot(p,filename,fig_dir)

    p
}

fig_recent <- function(filename="recent.pdf",fig_dir="essay/figure") {
    message("[fig:recent]")

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
            yearly=m$yrly,
            topics=topics,
            denominator=NULL,
            rolling_window=roll)

        to_plot$topic <- factor(to_plot$topic,levels=topics)
        levels(to_plot$topic) <- topic_name_fig(topics)

        # NB free scale on y axis
        p[[i]] <- ggplot(to_plot) +
            geom_line(aes(year,weight)) +
            facet_wrap(~ topic,ncol=1,scales="free_y")

        p[[i]] <- p[[i]] +
            scale_y_continuous(labels=percent_format()) +
            xlab("article year") +
            ylab(ifelse(i==1,"proportion of words in topic","")) +
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
    to_plot <- term_year_series_frame(numbers,
                                      term_year=m$term_year,
                                      year_seq=m$term_year_yseq,
                                      vocab=m$vocab,
                                      raw_counts=F, # take yearly proportions
                                      total=T) # F?

    p <- ggplot(to_plot,aes(year,weight)) +
        geom_line(alpha=I(0.3)) +
        geom_smooth(se=F,method="loess",span=0.5,color="black")

    # TU's original plot looks like
    #
    # p <- qplot(yearsequence, numbertrajectory * 100, geom = c("point", "smooth"), span = 0.5, ylab = "percentage of corpus", xlab = "", main = "cardinal and ordinal number words, one through a hundred")
    #
    # I have swapped lines for points. Rolling averages would be less 
    # aggressively smooth than loess.


    p <- p +
        scale_y_continuous(labels=percent_format()) +
        xlab("year") +
        ylab("total proportion of corpus") +
        ggtitle("") +
        plot_theme

    render_plot(p,filename,fig_dir)

    p
}






    

# setup and execution
# -------------------
make_figures_setup <- function() {
    # load data
    setwd("~/Documents/research/20c/hls/tmhls")
    library(Matrix)
    source("analyze_model.R")
    m <- do.call(analyze_model,model_files("hls_k150_v100K"))
    m$dtw <- merge(m$doctops,m$metadata[,c("id","pubdate")],by="id")
    m$yrly <- tm_yearly_totals(tm_wide=m$dtw)
    m$dtm <- doc_topics_matrix(m$doctops)
    m$n <- length(unique(m$wkf$topic))

    # tym_result:
    load("models/hls_k150_v100K/tym.rda")
    m$term_year <- tym_result$tym
    m$term_year_yseq <- tym_result$yseq

    m
}

render_all <- function () {
    fig_criticism()
    fig_formalism_waves()
    fig_recent()
}

# main program
# model object (initialized with make_figures()) 
if(!exists("m")) {
    message("No 'm' found; initializing...")
    m <- make_figures_setup()
}

# only save all plots if this flag is set
# I liked R better when it was C

if(exists("MAKE_FIGURES_RENDER_ALL") && MAKE_FIGURES_RENDER_ALL) {
    render_all()
} else {
    message("No plots rendered; call fig_* or render_all()")
}
