corr <- function(directory, threshold = 0)
{
        dir <- "/Users/mrunals/Desktop/Coursera/R/specdata"
        file_list <- list.files(dir)
        read_files <- lapply(file.path(dir,file_list),read.csv)
        filtered_files <- list()
        final_vector <- c()
        for(i in 1:length(read_files)) 
        {
                if(sum(complete.cases(read_files[[i]])) > threshold)
                 {
                        
                        filtered_files <- append(filtered_files,read_files[i])
                        
                 }
        }
        filtered_files
        sapply(filtered_files, function(x){cor(x$nitrate,x$sulfate,use = "complete.obs")}) 
}     

