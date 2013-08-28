
# global variables

# ggplot theming

plot_theme <- theme_bw(base_size=10,base_family="serif")

# model object (initialized with make_figures())

m <- list()

# p: plot object

render_plot <- function(p,filename,fig_dir="essay/figure",
                        w=5,h=3, # pdf: inches
                        render_function=print) {

    pdf(file.path(fig_dir,filename),width=w,height=h)

    render_function(p)

    dev.off()
    message("Saved ",filename," in ",fig_dir)
}

fig_criticism <- function(filename="criticism.pdf",fig_dir="essay/figure") {
    message("[fig:criticism]")

    p <- tm_yearly_line_plot(.yearly_totals=m$yrly,topics=16,raw_counts=T)
    p <- p + 
        scale_y_continuous(labels=percent_format()) +
        theme(axis.title.x=element_text("article publication year"))
    
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
        theme(axis.title.x=element_text("article publication year"))

    # TODO topic labels on plot body

    p <- p + plot_theme + ggtitle("")
    render_plot(p,filename,fig_dir)

    p
}

fig_recent <- function(filename="recent.pdf",fig_dir="essay/figure") {
    recents <- c(015,143,138,058)


    to.plot <- topic_proportions_series_frame(
        yearly=m$yrly,
        topics=recents,
        denominator=NULL,
        rolling_window=3)

    to.plot$topic <- factor(to.plot$topic,
                            levels=recents)
    levels(to.plot$topic) <- topic_names(m$wkf,n=4,topics=recents)

    p <- ggplot(to.plot,aes(year,weight)) +
        geom_line() +
        facet_wrap(~ topic,nrow=1)

    # TODO factor labels on plot body, not in tiny title bars

    p <- p +
        scale_y_continuous(labels=percent_format()) +
        theme(axis.title.x=element_text("article publication year"))
    p <- p + plot_theme + ggtitle("")

    # TODO rest of recents all together on another grid panel?
                #010)
                 #019,025,048,069,036,
                 #004,108,077,102)             

    render_plot(p,filename,fig_dir)

    p
}

# load data
setwd("~/Documents/research/20c/hls/tmhls")
library(Matrix)
source("analyze_model.R")
m <- do.call(analyze_model,model_files("hls_k150_v100K"))
# tym_result:
load("models/hls_k150_v100K/tym.rda")
m$dtw <- merge(m$doctops,m$metadata[,c("id","pubdate")],by="id")
m$yrly <- tm_yearly_totals(tm_wide=m$dtw)
m$dtm <- doc_topics_matrix(m$doctops)
m$n <- length(unique(m$wkf$topic))

fig_criticism()
fig_formalism_waves()
fig_recent()
