
logfile_name <- function(namestem="log",directory=".") {
    j <- 0
    base <- file.path(directory,
                      paste(namestem,as.character(Sys.Date()),sep="-"))
    result <- base
    while(file.exists(result)) {
        j <- j + 1
        result <- paste(base,j,sep="-")
    }
    result
}

