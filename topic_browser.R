# Topic browser based on mallet output format.

topic_browser <- function(
        metadata_directory="~/Journals/metadata",
        wordyear_path=file.path(metadata_directory, "wordbyyear.rda"),
        collection=NULL,
        doctopics_file=NULL) {

last_wd <- getwd()
metadata_directory <- normalizePath(metadata_directory)
wordyear_path <- normalizePath(wordyear_path)

library(ggplot2)

if(is.null(collection)) {
    userinput <- readline('Are you modeling a) AHR or b) the collection of literary journals? ')
    collection <- ifelse(userinput=="a","ahr","hls")
}

if(collection=="ahr") {
  meta_path <- file.path(metadata_directory, 'AHR_metadata.tsv')
} else if(collection=="hls") {
  meta_path <- file.path(metadata_directory, 'merged_metadata.tsv')
} else {
    stop("Unknown collection: ",collection)
}

message("Loading metadata ... ")
Metadata <- read.table(meta_path, header = TRUE, stringsAsFactors=FALSE, sep = '\t', fill = TRUE, nrows = 100000, quote = '')

# We're going to process metadata after getting a list of the documents actually in this model.

if(is.null(doctopics_file)) {
    cat('This script expects that all files for a model will be contained in the same folder.\n')
    cat('It will ask you for the location of a doc_topics.csv file, and then\n')
    cat('expect to find other files -- vocab.txt and keys.csv -- in the same folder.\n')

    DummyVar <- readline("Hit return when you're ready to browse for doc_topics.csv: ")
    doctopics_file <- file.choose()
} else {
    doctopics_file <- normalizePath(doctopics_file)
}

data_directory <- dirname(doctopics_file)
setwd(data_directory)

doc_frame <- read.csv(doctopics_file, header = TRUE, quote="", as.is = TRUE)

DocIndices <- doc_frame$id

message("Document-topic data frame loaded. Now selecting relevant metadata.")

# Now that we know the documents actually in the model, we can select the relevant subset
# of Metadata. Important that it be keyed to the sequence of docids in doc_frame.

Documents <- as.character(Metadata$id)

# Confirm that all the docs in our doc-topic table are also in the metadata.
matched = sum(DocIndices %in% Documents)
if (length(DocIndices) > matched) {
  print("We have unmatched documents:")
  print(DocIndices[!DocIndices %in% Documents])
}

DocDates <- as.numeric(Metadata$date)
authors <- Metadata$author
titles <- Metadata$title
journals <- as.factor(Metadata$journaltitle)
names(authors) <- Documents
names(titles) <- Documents
names(DocDates) <- Documents
names(journals) <- Documents

journalnames <- levels(journals)
if (journalnames[1] == "") {
  journalnames = journalnames[2:length(journalnames)]
}
JournalCount <- length(journalnames)

remove(Metadata)

# This selects a subset and also reorders it to correspond to doc-topic table.
DocDates <- DocDates[DocIndices]
authors <- authors[DocIndices]
titles <- titles[DocIndices]
journals <- journals[DocIndices]

doccount <- length(DocIndices)
Documents <- DocIndices

# Much easier to manipulate doc-topic distributions as a matrix than as a dataframe.
# We call it "Theta" just to be opaque.

message("Converting the doc-topic frame into a matrix.")

Theta <- t(as.matrix(subset(doc_frame, select = -id)))
TopicCount <- dim(Theta)[1]
DocCount <- dim(Theta)[2]

correlation_path <- file.path(data_directory, "top_correlations.rda")

if (file.exists(correlation_path)) {
  #load(correlation_path, verbose = FALSE)
  load(correlation_path) # no verbose on my system
  message("Loading previously calculated correlations, which I assume have not changed.")
} else {
  message("Finding correlations of topics with each other. Can easily take 10-15 min...")
  top_correlations <- vector("list", TopicCount)
  for (i in 1: TopicCount) {
    Correlations <- numeric(TopicCount)
    for (j in 1: TopicCount) {
      Correlations[j] = cor(Theta[i, ], Theta[j, ], method = "pearson")
      if (i == j) Correlations[j] = -1
    }
    names(Correlations) <- 1:TopicCount
    Correlations <- sort(Correlations, decreasing = TRUE)
    top_correlations[[i]] <- as.integer(names(Correlations[1:5]))
  }
  save(top_correlations, file = correlation_path)
}

aggregate_path <- file.path(data_directory, "aggregate_sizes.rda")

if (file.exists(aggregate_path)) {
  load(aggregate_path) # no verbose on my system
  #load(aggregate_path, verbose = FALSE)
} else {
  
  message("Measuring the aggregate sizes of topics and documents.")
  # Create topic sizes.
  TopicSize <- integer(TopicCount)
  TopicsByJournal <- array(data = 0, dim = c(TopicCount, JournalCount))
  JournalSums <- numeric(JournalCount)
  
  for (i in 1: TopicCount) {
    TopicSize[i] <- sum(Theta[i, ])
    j = 0
    for (name in journalnames) {
      j = j + 1
      TopicsByJournal[i, j] = TopicsByJournal[i, j] + sum(Theta[i, journals == name])
      JournalSums[j] = JournalSums[j] + sum(Theta[i, journals == name])
    }
    # normalize
    TopicsByJournal[i, ] = TopicsByJournal[i, ] / TopicSize[i]
  }
  
  JournalSums = JournalSums / sum(JournalSums)
  
  # Create document sizes.
  DocSize <- integer(doccount)
  for (i in 1: doccount) {
    DocSize[i] <- sum(Theta[ , i])
  }
  # Rank topics
  TopicRanks <- integer(TopicCount)
  names(TopicSize) <- 1:TopicCount
  TopicSize <- sort(TopicSize, decreasing = TRUE)
  for (i in 1: TopicCount) {
    TopicRanks[i] <- which(names(TopicSize) == as.character(i))
  }
  
  NumDocs <- length(Documents)
  
  MinDate = min(DocDates)
  MaxDate = max(DocDates)
  Timespan = (MaxDate - MinDate) + 1
  TotalsPerYear <- integer(Timespan)
  
  # Summing number of words per year, by topic.
  ThetaSum <- array(data=0, dim = c(TopicCount, Timespan))
  for (i in 1: NumDocs) {
    DateIndex = (DocDates[i] - MinDate) + 1
    ThetaSum[ , DateIndex] = ThetaSum[ , DateIndex] + Theta[ , i]
  }
  
  # Total number of words per year.
  for (i in 1: Timespan) {
    TotalsPerYear[i] = sum(ThetaSum[ , i])
  }
  
  # Now we can normalize ThetaSum.
  for (i in 1: TopicCount) {
    HoldVector = ThetaSum[i ,] / TotalsPerYear
    ThetaSum[i ,] <- HoldVector
  }
  save(ThetaSum, TotalsPerYear, Timespan, MinDate, MaxDate, NumDocs, TopicRanks, TopicSize, TopicsByJournal, DocSize, JournalSums, file = aggregate_path)
}

# message("Getting the vocabulary and top keys for each topic.")
# # Get vocabulary.
# vocab_path <- paste(data_directory, "/vocab.txt", sep = "")
# AllWords <- scan(vocab_path, what = character())

keys_path <- file.path(data_directory, "keys.csv")
keys <- read.csv(keys_path, as.is = TRUE, encoding = "UTF-8")

frame_length = length(keys$word)
Phi <- vector("list", TopicCount)
AllWords <- character(0)
for (i in 1: frame_length) {
	Topic = keys$topic[i]
	Phi[[Topic]] <- c(Phi[[Topic]], keys$word[i])
  if (!keys$word[i] %in% AllWords) {
    AllWords = c(AllWords, keys$word[i])
  }
}


if (file.exists(wordyear_path)) {
  message('Loading word by year data for this corpus, which I assume has not changed.')
  load(wordyear_path) # verbose?
  # load(wordyear_path, verbose = FALSE)
} else {
  yearlycountsfile = file.path(metadata_directory, 'hls_yearlycounts.tsv')
  yearlycounts <- read.table(yearlycountsfile, header = TRUE, stringsAsFactors=FALSE, sep = '\t', fill = TRUE, nrows = 100000, quote = '')
  yearsequence <- as.integer(yearlycounts$year)
  yearsums <- as.integer(yearlycounts$sumtotal)
  
  wordbyyearfile = file.path(metadata_directory, 'hls_wordcounts.tsv')
  wordcounts <-  read.table(wordbyyearfile, header = TRUE, stringsAsFactors=FALSE, sep = '\t', fill = TRUE, nrows = 100000, quote = '')
  wordbyyearwords <- wordcounts$word
  wordsums <- as.integer(wordcounts$total)
  columns <- dim(wordcounts)[2]
  rows <- dim(wordcounts)[1]
  wordbyyear <- as.matrix(wordcounts[ , 3: columns])
  if (dim(wordbyyear)[2] != length(yearsums)) {
    print('Discrepancy in timespans of word-by-year files.')
  }
  message('Normalizing word by year data ...')
  for (i in 1: rows) {
    wordbyyear[i, ] = wordbyyear[i, ] / yearsums
  }
  remove(wordcounts)
  save(wordbyyear, wordsums, wordbyyearwords, yearsums, yearsequence, file = wordyear_path)
}

print_words <- function(commandvector, wordbyyear, wordbyyearwords, yearsequence, Phi) {
  TopNum <- as.integer(commandvector[2])
  cat('TOPIC', TopNum,':', '\n')
  for (wordnum in 1:50) {
    cat(Phi[[TopNum]][wordnum], "  ")
    if (wordnum %% 10 == 0) {
      cat('\n')
    }
  }
  cat('Plotting the aggregate normalized frequency of the 50 most salient words in topic', TopNum, '\n')
  cat('including occurrences that may not actually be in the topic.\n')
  top50words <- Phi[[TopNum]]
  yearlyvalues <- numeric(length(yearsequence))
  for (word in top50words) {
    idx <- which(wordbyyearwords == word) 
    yearlyvalues <- yearlyvalues + wordbyyear[idx, ]
  }
  par(mar = c(4,4,4,4))	
  p <- qplot(yearsequence, yearlyvalues, geom = c("point", "smooth"), span = 0.5, ylab = "sum of all occurrences (in and out of topic) for the top 50 words", xlab = "", main = paste('Top 50 words in topic', TopNum, ':', Phi[[TopNum]][1], Phi[[TopNum]][2], Phi[[TopNum]][3], Phi[[TopNum]][4], Phi[[TopNum]][5], Phi[[TopNum]][6], Phi[[TopNum]][7], Phi[[TopNum]][8], Phi[[TopNum]][9], Phi[[TopNum]][10]))
  suppressMessages(print(p))
  p
}

compare_words <- function(commandvector, wordbyyear, wordbyyearwords, yearsequence, Phi, TopicFreqs) {
  TopNum <- as.integer(commandvector[2])
  cat('TOPIC', TopNum,':', '\n')
  for (wordnum in 1:50) {
    cat(Phi[[TopNum]][wordnum], "  ")
    if (wordnum %% 10 == 0) {
      cat('\n')
    }
  }
  cat('\nPlotting the aggregate normalized frequency of the 50 most salient words in topic', TopNum, '\n')
  cat('to the normalized frequency of the topic.\n\n')
  timespan <- length(yearsequence)
  top50words <- Phi[[TopNum]]
  yearlyvalues <- numeric(length(yearsequence))
  for (word in top50words) {
    idx <- which(wordbyyearwords == word) 
    yearlyvalues <- yearlyvalues + wordbyyear[idx, ]
  }
  PlotFrame <- data.frame(year = yearsequence, frequency = c(yearlyvalues, TopicFreqs[TopNum, ]), trend = as.factor(c(rep("Top 50 words", timespan), rep("Topic itself", timespan))))
  par(mar = c(4,4,4,4))  
  chromatic <- c('deeppink4', 'lightsteelblue4')
  p <- ggplot(PlotFrame, aes(x = year, y = frequency, group = trend, shape = trend, colour = trend, size = trend))
                         
  p <- p + geom_point() + geom_smooth() + scale_colour_manual(values = chromatic) + scale_size_manual(values = c(0.8, 0.8)) + scale_shape_manual(values = c(1, 2)) + scale_x_continuous(" ") + scale_y_continuous(" ")
  p <- p + ggtitle(paste('Topic', TopNum, ':', Phi[[TopNum]][1], Phi[[TopNum]][2], Phi[[TopNum]][3], Phi[[TopNum]][4], Phi[[TopNum]][5], Phi[[TopNum]][6], Phi[[TopNum]][7], Phi[[TopNum]][8], Phi[[TopNum]][9], Phi[[TopNum]][10]))
  suppressMessages(print(p))
  
  cat("Now measuring the Pearson correlation between individual words and topic.\n")
  correlations <- numeric()
  for (word in top50words) {
    idx <- which(wordbyyearwords == word) 
    yearlyvalues <- wordbyyear[idx, ]
    acorrelation <- cor(yearlyvalues, TopicFreqs[TopNum, ], method="pearson")
    correlations <- c(correlations, acorrelation)
  }
  names(correlations) <- top50words
  correlations <- sort(correlations)
  idx = 0
  for (i in 1:50) {
    acorrelation = correlations[i]
    cat(names(acorrelation), "=", acorrelation, " | ", sep = " ")
    idx = idx + 1
    if (idx %% 5 == 0) cat('\n') 
  }
  p
}

add_words <- function(commandvector, wordbyyear, wordbyyearwords, yearsequence, Phi) {
  cat('Plotting the aggregate normalized frequency of the words you specified.\n')
  yearlyvalues <- numeric(length(yearsequence))
  for (word in commandvector) {
    if (word %in% wordbyyearwords) {
      idx <- which(wordbyyearwords == word) 
      yearlyvalues <- yearlyvalues + wordbyyear[idx, ]
    }
  }
  par(mar = c(4,4,4,4))  
  p <- qplot(yearsequence, yearlyvalues, geom = c("point", "smooth"), span = 0.5, ylab = "normalized sum of occurrences", xlab = "", main = paste(commandvector, collapse = " "))
  suppressMessages(print(p))
  p
}

look_up_word <- function(TopicCount, Phi, Word) {
  cat('These are the topics most closely related to', Word, '.\n')
  Hits <- numeric(0)
  NumHits <- 0
  Indices <- numeric(0)
  for (i in 1: TopicCount) {
    if (Word %in% Phi[[i]]) {
      NumHits <- NumHits + 1
      Hits <- c(Hits, which(Phi[[i]] == Word))
      Indices <- c(Indices, i)
    }
  }
  names(Hits) <- Indices
  Hits <- sort(Hits, decreasing = FALSE)
  cat('\n')
  if (NumHits > 5) NumHits <- 5
  for (i in 1: NumHits) {
    Top <- as.integer(names(Hits[i]))
    cat("Topic", Top, ":", Phi[[Top]][1], Phi[[Top]][2], Phi[[Top]][3], Phi[[Top]][4], Phi[[Top]][5], Phi[[Top]][6], Phi[[Top]][7], Phi[[Top]][8], '\n')
  }
}


par(mar = c(4,4,4,4))
options(width = 70)
options(warn = -1)

# Fill NAs
NormalizedTopicFreqs <- array(data=0, dim = c(TopicCount, Timespan))
for (i in 1: TopicCount) {
  NormalizedTopicFreqs[i, ] <- ThetaSum[i, ]
  NormalizedTopicFreqs[i, is.na(NormalizedTopicFreqs[i, ])] <- 0
}

instructions <- function() {
  cat("You may enter a word to see topics where that word is salient, or a number\n")
  cat("to graph that topic. You can also enter two special commands. 'word,80' will\n")
  cat("graph the aggregate frequency of the top 50 words in topic #80. 'compare,80'\n")
  cat("will compare that aggregate frequency to the frequency of the topic itself.\n")
  cat("You can enter a list of comma-separated words to graph their aggregate frequency,\n")
  cat("or a word followed by a single comma to graph just one.\n")
}

par(adj = 0)
instructions()
repeat {
    
  Word <- readline('Enter a word, a topic number, "#help" or "#quit": ')
  Word <- gsub(" ", "", Word)
  # remove spaces which people may insert by accident after a comma
  
  if (Word == "#help") {
    instructions()
    next
  }
  if (Word == "#quit") {
      break
  }
    
  containscomma <- sum(grep(",", Word))
    
  # All entries containing commas are instructions to plot aggregate word frequencies
  # rather than a topic as such
  # You can say "words,80" to plot the top 50 words in topic 80, or "compare,80" to
  # compare that to the topic frequency. Or you can provide a comma-separated list of
  # words to be summed and graphed.
  # In every case we're normalizing these frequencies relative to the whole corpus of
  # 'fla' files.
  
  if (containscomma > 0) {
    commandvector <- strsplit(Word, ',')[[1]]
    if (length(commandvector) < 2) {
      p <- add_words(commandvector, wordbyyear, wordbyyearwords, yearsequence, Phi)
      next
    }
    isinteger <- suppressWarnings(!is.na(as.integer(commandvector)))
    # That's a good example of something Python does a lot better!
    if (isinteger[2] & commandvector[1] == "words") {
      p <- print_words(commandvector, wordbyyear, wordbyyearwords, yearsequence, Phi)
    } else if (isinteger[2]) {
      p <- compare_words(commandvector, wordbyyear, wordbyyearwords, yearsequence, Phi, NormalizedTopicFreqs)
    } else {
      p <- add_words(commandvector, wordbyyear, wordbyyearwords, yearsequence, Phi)
    }
    next
    # back to top of loop
  }
  
	TopNum <- suppressWarnings(as.integer(Word))
	if (!is.na(TopNum) | Word %in% AllWords | Word %in% Documents) Proceed = TRUE
	else {
    cat("That wasn't a valid entry, perhaps because we don't have that word.", '\n')
    next
	}
	
	# The following section deals with the case where the user has
	# entered a word to look up.
	
	if (Word %in% AllWords) {
		look_up_word(TopicCount, Phi, Word)
    next
	}
				
	if (TopNum < 1) TopNum <- 1
	if (TopNum > TopicCount) TopNum <- TopicCount	
	# By this point we presumably have a valid TopNum.
	
	cat('\n')
	
  Smoothed <- NormalizedTopicFreqs[TopNum, ]
	par(mar = c(4,4,4,4))	
	p <- qplot(seq(MinDate, MaxDate), Smoothed*100, geom = c("point", "smooth"), span = 0.5, ylab = "% of words in the topic", xlab = "", main = paste('Topic', TopNum, ':', Phi[[TopNum]][1], Phi[[TopNum]][2], Phi[[TopNum]][3], Phi[[TopNum]][4], Phi[[TopNum]][5], Phi[[TopNum]][6], Phi[[TopNum]][7], Phi[[TopNum]][8], Phi[[TopNum]][9], Phi[[TopNum]][10]))
	suppressMessages(print(p + theme_bw()))
  
	cat('TOPIC', TopNum,':', '\n')
  for (wordnum in 1:50) {
    cat(Phi[[TopNum]][wordnum], "  ")
    if (wordnum %% 10 == 0) {
      cat('\n')
    }
  }
	cat('OF', TopicCount, 'TOPICS this is #',TopicRanks[TopNum], 'in desc order, with', TopicSize[TopNum], 'words.\n')
  cat('\nRelated topics: \n')
	the_top5 <- top_correlations[[TopNum]]
  for (topic in the_top5){
    cat(topic, Phi[[topic]][1:7], '\n')
  }
  cat('\n')
	docsalience <- Theta[TopNum, ]/DocSize
	mostsalient <- order(docsalience, decreasing = TRUE)
	TopTen <- mostsalient[1:10]
	for (ordinal in TopTen) {
		cat(paste(authors[ordinal], titles[ordinal], as.character(DocDates[ordinal]), sep = ", "))
		cat('\n')
	}
	cat('\n')
  for (j in 1: JournalCount) {
    thisproportion = TopicsByJournal[TopNum, j]
    overallproportion = JournalSums[j]
    difference = thisproportion - overallproportion
    if (difference > 0) {
      cat(journalnames[j], ": \t", round(thisproportion * 100, 2), '% which is ', round(difference * 100, 2), '% greater than its share of the corpus.', '\n', sep="")
    }
    if (difference < 0) {
      cat(journalnames[j], ": \t", round(thisproportion * 100, 2), '% which is ', round(-difference * 100, 2), '% less than its share of the corpus.', '\n', sep = "")
    }
  }
  cat('\n')
} # repeat { }
		
setwd(last_wd)
	
} # topic_browser()
