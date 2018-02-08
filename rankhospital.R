rankhospital <- function(state, outcome, num = "best") {
        ## Check that state and outcome are valid
        if(outcome %in% c("heart attack", "heart failure", "pneumonia")==FALSE)
                stop("Invalid Outcome")
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        index <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
        
        data[,index] <- suppressWarnings(as.numeric(data[,index]))
        data <- na.omit(data)
        
        states <- table(data$State)
        if (!state %in% names(states)) { 
                stop("Invalid State")
        }
        
        ## Return hospital name in that state with lowest 30-day death
        s <- subset(data, State==state)
        s <- s[order(s[,index], s[,2],na.last = TRUE), 2]
        
        if (num == "worst"){num <- length(s)}
                
        if (num == "best"){num <- 1}
                
        s[num]
}

