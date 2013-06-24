
create_tdm <- function(filename) {
    instances <- read_instances(file.path(model_dir,"journals.mallet")) 
    tdm <- instances_tdm(instances,big=T,parallel=F,verbose=T)
    message("Saving term-document matrix to ",tdm_file)
    save(tdm,file=tdm_file)
    tdm
}

# setup

setwd("~/Developer/dfr-analysis")
source("source_all.R")
topics_rmallet_setup()
setwd("~/Documents/research/20c/hls/tmhls")
source("analyze_model.R")

model_dir <- "models/hls_k150_v100K"
dfr_data_root <- "~/Documents/research/20c/hls/tmhls/dfr-data"
journal_dirs <- c("elh_ci_all",
                  "mlr1905-1970",
                  "mlr1971-2013",
                  "modphil_all",
                  "nlh_all",
                  "pmla_all",
                  "res1925-1980",
                  "res1981-2012")
dfr_dirs <- file.path(dfr_data_root,journal_dirs)

tdm_file <- file.path(model_dir,"tdm.rda")

# Load or create the term-document matrix
if (!file.exists(tdm_file)) { 
    message(tdm_file," is missing. Regenerating tdm (~10 mins)...")
    tdm <- create_tdm(model_dir,tdm_file)
} else {
    load(tdm_file)
}

# load metadata  
metadata <- read_metadata(file.path(dfr_dirs,"citations.CSV"))
journals <- unique(metadata$journaltitle)
info <- read_corpus_info_files(model_dir)

# the calculation happens here
jym <- journal_year_matrix(tdm,metadata,info$id_map)

# convert it to something ggplot-able
series <- melt(jym)
names(series) <- c("journal","year","count")
series$journal <- gsub("\t","",series$journal)
p <- ggplot(series,aes(as.Date(year),fill=journal))

# area plot of raw counts
area_plot <- p + geom_area(aes(y=count))
ggsave(file.path(model_dir,"journal_words.png"),plot=area_plot)

# filled plot showing proportions by year
fill_plot <- p + geom_histogram(aes(weight=count),
                                position="fill",
                                binwidth=366) # days
ggsave(file.path(model_dir,"journal_proportion.png"),plot=fill_plot)

