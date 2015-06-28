complete <- function(directory,id = 1:332)
{
        dir <- "/Users/mrunals/Desktop/Coursera/R/specdata"
        file_list <- list.files(dir)
        selected_files <- file_list[match(id,as.numeric(sub('\\.csv','',file_list)))]
        read_files <- lapply(file.path(dir,selected_files),read.csv)
        nobs <- sapply( read_files, function(f) sum(complete.cases(f)))
        i <- id
        data.frame(id = i,nobs)
}       
