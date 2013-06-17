

write_ltab <- function(dirs,output_file="document_lengths.csv",
                       pythonscript="document_lengths.py") {
    cmd <- paste("python",pythonscript,paste(dirs,collapse=" "))
    cmd <- paste(cmd,">",output_file)
    message("Running ",cmd)
    system(cmd)
}

# main()
analyze_corpus_main <- function(report_only=T) {
    source("~/Developer/dfr-analysis/source_all.R")
    library(knitr)

    wd <- getwd()

    # parameters
    tmhls_root <- "/Users/agoldst/Documents/research/20c/hls/tmhls"

    dfr_data_root <- file.path(tmhls_root,"dfr-data")
    journal_dirs <- c("elh_ci_all",
                  "mlr1905-1970",
                  "mlr1971-2013",
                  "modphil_all",
                  "nlh_all",
                  "pmla_all",
                  "res1925-1980",
                  "res1981-2012")
    dfr_dirs <- file.path(dfr_data_root,journal_dirs)
    ltab_name <- file.path(tmhls_root,"document_lengths.csv")
    report_dir <- file.path(tmhls_root,"corpus_report")
    report_source <- "corpus.Rmd"
    report_md <- "corpus.md"
    report_output <- "corpus.html"

    # analysis

    if(!report_only) { 
        write_ltab(file.path(dfr_dirs,"wordcounts"),ltab_name) 
    }

    # report generation

    # switch to report_dir to knit
    if(!file.exists(report_dir)) {
        dir.create(report_dir)
    }
    setwd(report_dir)

    knit(report_source,output=report_md)

    pandoc_error <- system(paste("pandoc -o",report_output,report_md))

    message("pandoc generation of ",report_output," returned ",pandoc_error)

    setwd(wd)

    pandoc_error
}

# execution
# choose which model (file paths specified in main() subroutine)

results <- analyze_corpus_main(report_only=T)

