best <- function(state,outcome)
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
 
       #Filter the data frame based on the State entered
       filtered_data <- subset(care_measures,State == state)          
               
       #Sorted the filtered data based on metric and hospital name and get the second column
       sorted_data <- filtered_data[order(suppressWarnings(as.numeric(filtered_data[,col_index]))),2]
       
       #Get the first row i.e the hospital name
       as.character(sorted_data[1])
}