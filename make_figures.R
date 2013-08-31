library(ggplot2)

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

our_geom_smooth <- geom_smooth(method="loess",span=0.5,color="black",se=F)
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




    

# setup and execution
# -------------------
make_figures_setup <- function() {
    # load data
    setwd("~/Documents/research/20c/hls/tmhls")
    library(Matrix)
    source("analyze_model.R")
    m <- do.call(analyze_model,model_files("hls_k150_v100K"))
    m$dtw <- merge(m$doctops,m$metadata[,c("id","pubdate")],by="id")
    m$topic_year <- tm_yearly_totals(tm_wide=m$dtw)
    m$dtm <- doc_topics_matrix(m$doctops)
    m$n <- length(unique(m$wkf$topic))

    # tym_result:
    load("models/hls_k150_v100K/tym.rda")
    m$term_year <- tym_result$tym
    m$term_year_yseq <- tym_result$yseq

    m
}

render_all <- function () {
    fig_numbers()
    fig_criticism()
    fig_formalism_waves()
    fig_recent()
    fig_t080()

    return()
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
