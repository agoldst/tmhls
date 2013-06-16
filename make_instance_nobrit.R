# Code based on A. Goldstone's make_instance.R script.
# T. Underwood changed paths and date ranges and added 
# a function to translate British spellings

# translate_britticisms
#
# Accepts a long-form dataframe as returned by read_dfr with
# three columns, id, WORDCOUNTS, and WEIGHT.
#
# Also accepts a named vector used as a translation table,
# where the contents of the vector are American spellings
# and the names are British spellings. Using this, translates
# British items in WORDCOUNTS into corresponding American
# spellings. Does not attempt to merge duplicate rows of the
# data frame which may be created (e.g. when colour -> color,
# you may end up with two rows for "color.") Assumes that those
# rows will effectively be merged when wordcounts are inflated
# into actual text for Mallet by docs_frame or a similar function.
#
# Probably not very fast because named vectors in R are not
# implemented as hash tables like Python dictionaries. But it's
# not a bottleneck in practice.

translate_britticisms <- function(counts, american_translations) {
  british_spellings <- names(american_translations)
  indices_to_change <- which(counts$WORDCOUNTS %in% british_spellings) 
  counts$WORDCOUNTS[indices_to_change] <- american_translations[counts$WORDCOUNTS[indices_to_change]]
  counts
}

read_britticisms <- function(filepath) {
  aframe <- read.csv(filepath, stringsAsFactors = FALSE)
  trans_table <- aframe$AMERICAN
  names(trans_table) <- aframe$BRITISH
  trans_table
}

read_mlr_exclusions <- function(filepath) {
  # code creates a dummy column for reasons that appear related to an
  # 'infamous trailing comma' -- probably moot here, but you never know
  
  cols <- scan(f,nlines=1,what=character(),sep=",",quiet=T)
  cols <- c(cols,"unused")
  toskip <- subset(read.csv(f,skip=1,header=F,col.names=cols,quote="",as.is=T), select=-unused)
  
  # just return a vector of ids to skip
  toskip$id
}

# I edited this function to permit exclusions of MLR reviews misclassified as "fla."

prune_filelist <- function(files, metadata, aquo, adquem, types = "fla\t", toskip = NULL) {
  # apply cutoff dates
  keep_ids <- metadata$id[metadata$date >= as.Date(aquo) &
                            metadata$date <= as.Date(adquem) &
                            metadata$type %in% types &
                            !metadata$id %in% toskip]
  files[as.id(files) %in% keep_ids]
}


get_counts <- function(dirs,
                       aquo,adquem,
                       itemtypes) {
  
  message("Loading metadata...")
  
  metadata <- read_metadata(file.path(dirs,"citations.CSV"))
  metadata$date <- pubdate_Date(metadata$pubdate)
  
  message("Read ",nrow(metadata)," metadata entries")
  
  # added this to get the errors in MLR 2011-12
  ids_toskip <- read_mlr_exclusions('~/Journals/tmhls/MLR_mislabeled_fla.CSV')
  message("Excluding ", length(ids_toskip), " mislabeled book reviews from MLR.")
  
  globs <- file.path(dirs,"wordcounts","wordcounts*.CSV")
  files <- prune_filelist(files=Sys.glob(globs),
                          metadata=metadata,
                          aquo=aquo,adquem=adquem,
                          types=itemtypes,
                          toskip=ids_toskip)
  
  message("Importing ",length(files)," wordcount.CSV files")
  
  read_dfr(files=files)
}

rare_token_report <- function(overall,
                              freq_threshold=NULL,rank_threshold=NULL,
                              plotsfile="freqplots.png") {
  
  total <- sum(overall)
  ovf <- as.data.frame(overall)
  if(!is.null(freq_threshold)) { 
    count_threshold <- freq_threshold * total
  }
  else {
    if(is.null(rank_threshold)) {
      stop("No threshold supplied")
    }
    
    count_threshold <- sort(overall,decreasing=T)[rank_threshold]
  }
  
  ovf$keep <- ovf$Freq >= count_threshold
  
  message("Constructing plots...")
  
  png(plotsfile,width=800,height=600)
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(1,2)))
  
  freqdist <- qplot(Freq / total,data=ovf,geom="bar",log="x",fill=keep,
                    xlab="frequency",
                    ylab="number of word types",
                    main="Types") + theme(legend.position="none")
  
  tokdist <- freqdist + geom_bar(aes(weight=Freq)) +
    ylab("token count") + ggtitle("Tokens")
  
  print(freqdist,vp=viewport(layout.pos.row=1,layout.pos.col=1))
  print(tokdist,vp=viewport(layout.pos.row=1,layout.pos.col=2))
  dev.off()
  
  message("Plots saved to ",plotsfile)
  
  types_frac <- 1 - ecdf(overall)(count_threshold)
  types_total <- length(overall)
  types_msg <- sprintf("%.0f of %.0f types (%.3f)",
                       types_frac * types_total,types_total,types_frac)
  
  tokens_count <- sum(overall[overall >= count_threshold])
  tokens_msg <- sprintf("%.0f of %.0f tokens (%.3f)",
                        tokens_count,total,tokens_count / total)
  message("A frequency threshold of ",count_threshold / total,
          " or > ",floor(count_threshold)," tokens\n",
          "leaves ",types_msg," and ",tokens_msg)
  
  freqdist
}

stopword_report <- function(overall,stoplist_file) {
  stopwords <- scan(stoplist_file,what=character(),sep="\n",quiet=T)
  stopwords <- unique(stopwords)
  
  total <- sum(overall)
  stopcount <- sum(overall[stopwords],na.rm=T)
  
  message("The ",length(stopwords)," unique stopwords from ",
          stoplist_file,"\n",
          "correspond to ",stopcount, " of ",total," tokens (",
          sprintf("%.3f",stopcount / total),") in the corpus")
}

# main()

make_instance_main <- function() {
  pwd <- getwd()
  setwd("~/Journals/Rscripts")
  source("topics_rmallet.R")
  library(ggplot2)
  library(grid)
  dfr_dirs <- c("~/Journals/ELH-CI", "~/Journals/MLR/mlr1905-1970", "~/Journals/MLR/mlr1971-2013", "~/Journals/ModPhil", "~/Journals/NLH", "~/Journals/PMLA", "~/Journals/RES/RESto1980", "~/Journals/RES/RESfrom1981")
  stoplist_file <- "/Users/tunderwood/Journals/tmhls/stoplist_final.txt"
  outdir <- "/Users/tunderwood/Journals/Uresults/HLS"
  
  # corpus-trimming parameters
  aquo <- as.Date("1880-01-01")
  adquem <- as.Date("2013-12-31")
  itemtypes <- "fla\t"
  freq_threshold <- NULL
  rank_threshold <- 30000
  
  plotfile <- file.path(outdir,"freqplots.png")
  outfile <- file.path(outdir,"journals.mallet")
  
  # main script: commands
  
  if(!file.exists(outdir)) {
    dir.create(outdir)
  }
  
  counts <- get_counts(dfr_dirs,
                       aquo,
                       adquem,
                       itemtypes)
  
  message("Read ",nrow(counts)," rows")
  
  message("Translating British spellings ...")
  trans_table <- read_britticisms("~/Journals/tmhls/UK2UStransrules.csv")
  counts <- translate_britticisms(counts, trans_table)
  
  message("Aggregating token counts into overall counts")
  
  overall <- overall_counts(counts) 
  rare_token_report(overall,freq_threshold,rank_threshold,plotfile) 
  stopword_report(overall,stoplist_file)
  
  message("Removing infrequent word types...")
  
  counts <- remove_rare(counts,freq_threshold,rank_threshold,
                        .overall=overall)
  
  message(nrow(counts)," rows remain.") 
  message("Making MALLET instance...") 
  inst <- make_instances(docs_frame(counts),stoplist_file)
  
  write_instances(inst,outfile) 
  message("Instance saved to ",outfile)
  
  setwd(pwd)
}


