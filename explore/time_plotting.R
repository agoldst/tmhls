
library(ggplot2)

# global variables: plot_theme, m (initialized below)
# ----------------

# ggplot theming 
plot_theme <- theme_bw(base_size=10,base_family="sans") 

setwd("~/Documents/research/20c/hls/tmhls")
if(!exists("m")) {
    source("analyze_model.R")
    m <- do.call(analyze_model,model_files("hls_k150_v100K"))
    m$dtw <- merge(m$doctops,m$metadata[,c("id","pubdate")],by="id")
    m$topic_year <- tm_yearly_totals(tm_wide=m$dtw)
    m$n <- length(unique(m$wkf$topic))
}

topic <- 80 # try others

to_plot <- topic_proportions_series_frame(yearly=m$topic_year,
                                          topics=topic,
                                          denominator=NULL,
                                          rolling_window=1)
p <- ggplot(to_plot,aes(year,weight))
p <- p + plot_theme

our_point_geom <- geom_point(alpha=I(0.25),size=1)

# specific figures...

pdf("explore/time_plotting.pdf",w=6,h=4)

print(p + geom_bar(stat="identity",fill="grey80") +
    geom_smooth(method="loess",span=0.5,color="black",se=F) +
    ggtitle("bars")
)

print(p + geom_bar(stat="identity",
                   fill="white",color="grey80",size=0.25) +
    geom_smooth(method="loess",span=0.5,color="black",se=F) +
    ggtitle("hollow bars")
)

# width in days (x-axis scale)
print(p + geom_bar(stat="identity",fill="grey80",width=90) +
    geom_smooth(method="loess",span=0.5,color="black",se=F) +
    ggtitle("thinner bars")
)

print(p + geom_bar(stat="identity",fill="grey80",width=90) +
    geom_smooth(method="loess",span=0.5,color="black",se=T) +
    ggtitle("thinner bars + se")
)

print(p + geom_bar(stat="identity",fill="grey80",width=90) +
    stat_smooth(method="loess",span=0.5,color="black",
                geom="ribbon",fill="NA",linetype=3) +
    stat_smooth(method="loess",span=0.5,color="black",se=F) +
    ggtitle("thinner bars + loess line + se edges only")
)

print(p + geom_bar(stat="identity",fill="grey80",width=90) +
    stat_smooth(method="loess",span=0.5,color="black",
                geom="ribbon",fill="NA",linetype=3) +
    ggtitle("thinner bars + loess se edges only")
)

print(p + geom_bar(stat="identity",fill="grey50",width=90) +
    stat_smooth(method="loess",span=0.5,color="black",
                geom="ribbon",fill="NA",linetype=3) +
    ggtitle("darker thin bars + loess se edges only")
)

print(p + our_point_geom +
    geom_smooth(method="loess",span=0.1,se=F,alpha=I(0.5)) + 
    ggtitle("span=0.1")
)

print(p + our_point_geom +
    geom_smooth(method="loess",span=0.25,se=F,alpha=I(0.5)) +
    ggtitle("span=0.25")
)

print(p + our_point_geom +
    geom_smooth(method="loess",span=0.5,se=F,alpha=I(0.5)) +
    ggtitle("span=0.5")
)

print(p + our_point_geom +
    geom_smooth(method="loess",span=0.75,se=F,alpha=I(0.5)) +
    ggtitle("span=0.75")
)

print(p + our_point_geom +
    geom_smooth(method="loess",span=0.5,degree=1,se=F) +
    ggtitle("degree=1")
)

print(p + our_point_geom +
    geom_smooth(method="loess",span=0.5,degree=2,se=F) +
    ggtitle("degree=2")
)

print(p + geom_line() +
    ggtitle("line plot")
)

print(p + geom_line(alpha=I(0.1)) +
    geom_smooth(method="loess",span=0.5,color="black",se=F) +
    ggtitle("line + smoother")
)

print(p + geom_line(alpha=I(0.1)) +
    geom_smooth(method="loess",span=0.5,color="black",se=T) +
    ggtitle("line + smoother + se band")
)

print(p + our_point_geom +
    ggtitle("point plot (size 1)")
)

print(p + geom_point(alpha=I(0.25),size=2) +
    ggtitle("point plot (size 2)")
)


print(p + our_point_geom +
    geom_smooth(method="loess",span=0.5,color="black",se=F) +
    ggtitle("point + smoother")
)

print(p + our_point_geom +
    geom_smooth(method="loess",span=0.5,color="black",se=T) +
    ggtitle("point + smoother + se band")
)

for(i in 1:5) {
    rolled <- topic_proportions_series_frame(yearly=m$topic_year,
                                              topics=topic,
                                              denominator=NULL,
                                              rolling_window=i)

    print(p + our_point_geom +
          geom_line(data=rolled,aes(year,weight)) +
          ggtitle(paste("rolling average:",i))
    )
}


dev.off()
