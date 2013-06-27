# This script assumes that topic_brower.R has already been run, and the data structures it creates
# are still resident in memory. It creates a list of words, graphs the aggregate trajectory, and mines
# the vocabulary for correlations with that trajectory.
#
# Ultimately the word-year matrix used here is coming from a Python script. See /python/make_word_year_matrix.py

numbertrajectory <- numeric(125)
numbers <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
numbers <- c(numbers, "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen", "twenty")
numbers <- c(numbers, "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety", "hundred")
numbers <- c(numbers,"first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth")

for (word in numbers) {
  if (word %in% wordbyyearwords) {
    idx <- which(wordbyyearwords == word) 
    numbertrajectory <- numbertrajectory + wordbyyear[idx, ]
  }
}
par(mar = c(4,4,4,4))  
p <- qplot(yearsequence, numbertrajectory * 100, geom = c("point", "smooth"), span = 0.5, ylab = "percentage of corpus", xlab = "", main = "cardinal and ordinal number words, one through a hundred")
suppressMessages(print(p))

correlations <- numeric(100000)
correlates <- character(100000)

for (i in 1:100000) {
  word <- wordbyyearwords[i]
  correlation <- cor(wordbyyear[i, ], numbertrajectory)
  correlations[i] <- correlation
  correlates[i] <- word
}

names(correlations) <- correlates
correlations <- sort(correlations, decreasing = TRUE)