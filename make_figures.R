
# global variables

# ggplot theme

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
        ggtitle("") +
        theme(axis.title.x=element_text("article publication year"))
    
    p <- p + plot_theme
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
        ggtitle("") +
        theme(axis.title.x=element_text("article publication year"))

    # TODO topic labels on plot body

    p <- p + plot_theme
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

