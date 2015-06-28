rankall <- function(outcome,num = "best")
{
        #Reading the outcome-of-care-measures file   
        r <- read.csv("/Users/mrunals/Desktop/Coursera/R/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",colClasses = "character")
        
        #Validating if the outcome input is a valid outcome(disease)
        valid_outcomes <- c("heart attack","heart failure","pneumonia")
        if(!(outcome %in% valid_outcomes))
        {
                stop('invalid outcome')
        }
        
        #Setting a variable based on the "outcome" parameter 
        col_index <- ifelse(outcome == "heart attack", 11 , ifelse(outcome == "heart failure",17,23))
        
        #Splitting the data frame based on the State column in the data frame
        split_df <- split(r,r$State)
        
        
        if(num == "best")
        {
                #Using the lapply function to get the best hospital for each state (list)
                final <- lapply(  split_df
                                  ,function(x)
                                   {
                                        y <- data.frame(x)
                                        z <- y[order(suppressWarnings(as.numeric(y[,col_index])),y[,2]),c("hospital" = 2,"state" = 7)]
                                        #Return the first row in the data frame
                                        z[1,]
                                   }        
                                )
        }
        else if(num == "worst")
        {
                #Using the lapply function to get the worst hospital for each state (list)
                final <- lapply(  split_df
                                  ,function(x)
                                  {
                                          y <- data.frame(x)
                                          z <- y[order(suppressWarnings(as.numeric(y[,col_index])),y[,2],decreasing = TRUE),c(2,7)]
                                          #Return the first row in the data frame 
                                          z[1,]
                                  }        
                               )
        }
        else 
        {
                #Using the lapply function to get the hospital based on the rank entered for each state (list)
                final <- lapply(  split_df
                                  ,function(x)
                                  {
                                          y <- data.frame(x)
                                          z <- y[order(suppressWarnings(as.numeric(y[,col_index])),y[,2]),c(2,7)]
                                          #Return the nth row in the data frame based on the num parameter
                                          z[as.numeric(num),]
                                  }        
                                )
        }
                
        
        a <- do.call(rbind.data.frame,final)
        
        #Changing the column names of the data frame
        colnames(a) <- c("hospital","state")
        
        a
}
