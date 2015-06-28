rankhospital <- function(state,outcome,num = "worst")
{
        #Reading the outcome-of-care-measures file        
        care_measures <- read.csv("/Users/mrunals/Desktop/Coursera/R/rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
        
        # Validating if the state input is a valid U.S state          
        valid_states <- state.abb        
        if(!(state %in% valid_states))
        {
                stop('invalid state')
        }
        
        #Validating if the outcome input is a valid outcome(disease)
        valid_outcomes <- c("heart attack","heart failure","pneumonia")
        if(!(outcome %in% valid_outcomes))
        {
                stop('invalid outcome')
        }
        
        col_index <- ifelse(outcome == "heart attack", 11 , ifelse(outcome == "heart failure",17,23))
        
        #rank_num <- ifelse(num == "best",1, ifelse(num == "worst",2,as.numeric(num)))
        
        #Filter the data frame based on the State entered
        filtered_data <- subset(care_measures,State == state)          
        
        #Sorted the filtered data based on metric and hospital name and get the second column
        if(num == "best")
                {
                        sorted_data <- filtered_data[order(suppressWarnings(as.numeric(filtered_data[,col_index]))),2]
                        final_answer <- as.character(sorted_data[1])
                }
        
        else if(num == "worst")
                {
                       sorted_data <- filtered_data[order(suppressWarnings(as.numeric(filtered_data[,col_index])),decreasing = TRUE),2]
                       final_answer <- as.character(sorted_data[1])  
                }
        else 
                {
                        sorted_data <- filtered_data[order(suppressWarnings(as.numeric(filtered_data[,col_index])),filtered_data[,2]),2]   
                        final_answer <- sorted_data[as.numeric(num)]
                }

        #Get the first row i.e the hospital name
        final_answer
        #data.frame(sorted_data,order(sorted_data[,c(3,2)]))
}