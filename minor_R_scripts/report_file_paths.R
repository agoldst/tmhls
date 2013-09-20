dfr_analysis <- "~/Developer/dfr-analysis"

workingdir <- "~/Documents/research/20c/hls/tmhls"
model_dir <- file.path(workingdir,"models/hls_k150_v100K")
dfr_data_root <- file.path(workingdir,"dfr-data")
journal_dirs <- c("elh_ci_all",
               "mlr1905-1970",
               "mlr1971-2013",
               "modphil_all",
               "nlh_all",
               "pmla_all",
               "res1925-1980",
               "res1981-2012")
citations_files <- file.path(dfr_data_root,journal_dirs,"citations.CSV")
keys_file <- file.path(model_dir,"wkf_nosmooth.csv")

# a special form for workingdir
if (workingdir == "underwood")  {
    workingdir <- "/Users/tunderwood/Journals/new results/hls_k150_v100K"
    model_dir <- workingdir
    keys_file <- file.path(model_dir,"keys.csv") # NEEDED ?
}

doc_topics_file <- file.path(model_dir,"doc_topics.csv")
vocab_file <- file.path(model_dir,"vocab.txt")
id_map_file <- file.path(model_dir,"id_map.txt")

