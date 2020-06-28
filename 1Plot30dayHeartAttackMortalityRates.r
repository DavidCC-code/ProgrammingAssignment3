setwd("G:/R/R projects/ProgrammingAssignment3")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
names(outcome)

#Because we originally read the data in as character (by specifying colClasses = "character" we need to
#coerce the column to be numeric. You may get a warning about NAs being introduced but that is okay.
outcome[, 11] <- as.numeric(outcome[, 11])

hist(outcome[, 11])
