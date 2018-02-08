rankall <- function(outcome, num = "best") {
        
        ## Create index num for type of outcome desired
        index <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        data[,index] <- suppressWarnings(as.numeric(data[,index]))
        data <- data[!is.na(data[,index]),]
        
        ## Check that state and outcome are valid
        if(outcome %in% c("heart attack", "heart failure", "pneumonia")==FALSE)
                stop("Invalid Outcome")

        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        s <- data[order(data[,index], data[,2],na.last = TRUE),]
        s <- s[!is.na(s[,index]),]
        
        if (num == "worst"){num <- length(s)}
        
        if (num == "best"){num <- 1}
        
        states <- sort(unique(s[,7]))
        
        state_hospital_data <- function(state){
                tempState <- subset(s, State == state)
                tempState <- tempState[num, c(2,7)]
                tempState$State <- state
                return(tempState)
        }
        state_data <- lapply(states, state_hospital_data)
        dframe <- as.data.frame(do.call(rbind, lapply(states, state_hospital_data)), row.names=states)
        colnames(dframe) <- c("hospital", "state")
        return (dframe)
}
