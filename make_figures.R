
# ggplot theming: global variables

plot_theme <- theme_bw(base_size=10,base_family="serif")

# p: plot object

render_plot <- function(p,filename,fig_dir="essay/figure",
                        w=5,h=3, # pdf: inches
                        render_function=print) {

    pdf(file.path(fig_dir,filename),width=w,height=h)

    render_function(p)

    dev.off()
    message("Saved ",filename," in ",fig_dir)
}

fig_criticism <- function(filename) {
    message("[fig:criticism]")

    p <- tm_yearly_line_plot(.yearly_totals=m$yrly,topics=16,raw_counts=T)
    p <- p +
        scale_y_continuous(labels=percent_format()) +
        ggtitle("") +
        theme(axis.title.x=element_text("article publication year"))
    
    p <- p + plot_theme
    render_plot(p,"criticism.pdf")
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

