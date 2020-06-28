simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
}


best <- function(state, outcome) {
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
  
  ## building de full column name from parameter outcome
  
  colnameStart <- "Hospital.30.Day.Death..Mortality..Rates.from."
  Fullcolname <- paste(colnameStart, sep="", gsub(" ",".", simpleCap(outcome)))
  
  ## getting valid column names to validate against
  Valid.colnames <- names(df.outcome)   
  
  if( !Fullcolname %in% Valid.colnames){
    stop("Invalid outcome") 
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  HNameCol <- "Hospital.Name"
  vars <- c("State",HNameCol ,Fullcolname)
  Ranking <- subset(df.outcome[vars], State == state)
  
  ## To avoid warning message of coercion
  
  suppressWarnings(Ranking[,Fullcolname] <- as.numeric(Ranking[,Fullcolname]))
  
  Ranking <- Ranking[order(Ranking[,Fullcolname],Ranking[,HNameCol]),]
  
  Ranking[1,HNameCol]
}

