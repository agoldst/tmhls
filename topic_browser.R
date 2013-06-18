# Topic browser based on mallet output format.

userinput <- readline('Are you modeling a) AHR or b) the collection of literary journals? ')
if (userinput == 'a') {
  meta_path = '~/Journals/tmhls/AHR_metadata.tsv'
} else {
  meta_path <- '~/Journals/tmhls/merged_metadata.tsv'
}

message("Loading metadata ... ")
Metadata <- read.table(meta_path, header = TRUE, stringsAsFactors=FALSE, sep = '\t', fill = TRUE, nrows = 100000, quote = '')

# We're going to process metadata after getting a list of the documents actually in this model.

# extract_directory
# given the path to a file, extracts only the directory part

extract_directory <- function(astring) {
  part_vector <- strsplit(astring, '/')[[1]]
  paste(part_vector[1:(length(part_vector) - 1)], collapse = '/')
}

cat('This script expects that all files for a model will be contained in the same folder.\n')
cat('It will ask you for the location of a doc_topics.csv file, and then\n')
cat('expect to find other files -- vocab.txt and keys.csv -- in the same folder.\n')

DummyVar <- readline("Hit return when you're ready to browse for doc_topics.csv: ")
file <- file.choose()

data_directory <- extract_directory(file)
setwd(data_directory)

doc_frame <- read.csv(file, header = TRUE, quote="", as.is = TRUE)

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

correlation_path <- paste(data_directory, "/top_correlations.rda", sep = "")

if (file.exists(correlation_path)) {
  load(correlation_path, verbose = FALSE)
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

message("Measuring the aggregate sizes of topics and documents.")
# Create topic sizes.
TopicSize <- integer(TopicCount)
TopicsByJournal <- array(data = 0, dim = c(TopicCount, JournalCount))
for (i in 1: TopicCount) {
  TopicSize[i] <- sum(Theta[i, ])
  j = 0
  for (name in journalnames) {
    j = j + 1
    TopicsByJournal[i, j] = TopicsByJournal[i, j] + sum(Theta[i, journals == name])
  }
  # normalize
  TopicsByJournal[i, ] = TopicsByJournal[i, ] / TopicSize[i]
}


# Create document sizes.
DocSize <- integer(doccount)
for (i in 1: doccount) {
  DocSize[i] <- sum(Theta[ , i])
}

# Rank topics
TopicBulk <- TopicSize
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

message("Getting the vocabulary and top keys for each topic.")
# Get vocabulary.
vocab_path <- paste(data_directory, "/vocab.txt", sep = "")
AllWords <- scan(vocab_path, what = character())

keys_path <- paste(data_directory, "/keys.csv", sep = "")
keys <- read.csv(keys_path, as.is = TRUE, encoding = "UTF-8")

frame_length = length(keys$word)
Phi <- vector("list", TopicCount)

for (i in 1: frame_length) {
	Topic = keys$topic[i]
	Phi[[Topic]] <- c(Phi[[Topic]], keys$word[i])
	}

par(mar = c(4,4,4,4))
options(width = 70)

par(adj = 0)
repeat {
	Proceed = FALSE
	while (!Proceed) {
		Word <- readline('Enter a word or a topic#: ')
		TopNum <- suppressWarnings(as.integer(Word))
		if (!is.na(TopNum) | Word %in% AllWords | Word %in% Documents) Proceed = TRUE
		else cat("That wasn't a valid entry, perhaps because we don't have that word.", '\n')
		}
	
	# The following section deals with the case where the user has
	# entered a word to look up.
	
	if (Word %in% AllWords) {
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
		User <- readline('Which of these topics do you select? ')
		TopNum <- as.integer(User)
		if (is.na(TopNum)) TopNum <- 1
		}
				
	if (TopNum < 1) TopNum <- 1
	if (TopNum > TopicCount) TopNum <- TopicCount	
	# By this point we presumably have a valid TopNum.
	
	cat('\n')
	
	# Generate smoothed curve.
	Smoothed <- numeric(Timespan)
	for (i in 1: Timespan) {
		Smoothed[i] = ThetaSum[TopNum, i]
		Smoothed[is.na(Smoothed)] <- 0
		}
	par(mar = c(4,4,4,4))	
	scatter.smooth(seq(MinDate, MaxDate), Smoothed*100, span = 0.33, col = "slateblue3", ylab = "% of words in the topic", xlab = "", main = paste('Topic', TopNum, ':', Phi[[TopNum]][1], Phi[[TopNum]][2], Phi[[TopNum]][3], Phi[[TopNum]][4], Phi[[TopNum]][5], Phi[[TopNum]][6], Phi[[TopNum]][7], Phi[[TopNum]][8]))
	
	cat('TOPIC', TopNum,':', '\n')
  for (wordnum in 1:50) {
    cat(Phi[[TopNum]][wordnum], "  ")
    if (wordnum %% 10 == 0) {
      cat('\n')
    }
  }
	cat('OF', TopicCount, 'TOPICS this is #',TopicRanks[TopNum], 'in desc order, with', TopicBulk[TopNum], 'words.\n')
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
    cat(journalnames[j], ": ", TopicsByJournal[TopNum, j], '\n')
  }
  cat('\n')
	}
		
	