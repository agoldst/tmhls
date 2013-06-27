# make_stack_graph

userinput <- readline('Three comma-separated numbers for topics:')
commandvector <- strsplit(userinput, ',')[[1]]
a = as.integer(commandvector[1])
b = as.integer(commandvector[2])
c = as.integer(commandvector[3])

ntf = array(data= 0, dim = dim(NormalizedTopicFreqs))
for (i in 1:150) {
  ntf[i,] = smooth(NormalizedTopicFreqs[i, ] * 100, twiceit = TRUE)
}
df <- data.frame(x = rep(yearsequence, 3), y = c(ntf[a, ], ntf[b, ], ntf[c, ]), topic = c(rep(commandvector[1],125), rep(commandvector[2],125), rep(commandvector[3],125)))

chromatic <- c("gray20", "gray40", "gray65")

p <-ggplot(df, aes(x=x, y=y, group = topic, colour = topic)) + scale_colour_manual(values = chromatic)
p <- p + geom_area(aes(colour=topic, fill = topic), position = 'stack') + scale_fill_manual(values = chromatic)
thetitle <- readline("The title: ")

p <- p + scale_x_continuous("") + scale_y_continuous("percent of words in topic") + ggtitle(thetitle)
print(p)