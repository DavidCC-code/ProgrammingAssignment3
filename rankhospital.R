simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
}

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
        df.outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  
  ## Check that state and outcome are valid
        ## first get a list of valid states 
        valid.states <- factor( df.outcome$State)
        valid.states <- levels(valid.states)
        
        ## validation against valid states
        if( !state %in% valid.states){
          stop("Invalid state") 
        }
        
        
        # building de full column name from parameter outcome
        
        colnameStart <- "Hospital.30.Day.Death..Mortality..Rates.from."
        Fullcolname <- paste(colnameStart, sep="", gsub(" ",".", simpleCap(outcome)))
        
        ## getting valid column names to validate against
        Valid.colnames <- names(df.outcome)   
        
        if( !Fullcolname %in% Valid.colnames){
          stop("Invalid outcome") 
        }
        
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
        
        ## Frist it builds the ranking
        HNameCol <- "Hospital.Name"
        vars <- c("State",HNameCol ,Fullcolname)
        Ranking <- subset(df.outcome[vars], State == state)
        
        ## To avoid warning message of coercion
        suppressWarnings(Ranking[,Fullcolname] <- as.numeric(Ranking[,Fullcolname]))
        
        Ranking <- Ranking[order(Ranking[,Fullcolname],Ranking[,HNameCol]),]
        Ranking <- Ranking[complete.cases(Ranking),]
        
        ## Then it gets the hospital ranked as parameter num
        
       
        if (num == "best") {
                Ranking[1,HNameCol]
        } else if ( num == "worst") {
                
                Ranking[nrow(Ranking),HNameCol]
        } else if ( num > nrow(Ranking)) {
                return(NA)
        } else {
                Ranking[num,HNameCol]
        }
        
}