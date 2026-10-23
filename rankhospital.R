rankhospital <- function (state, outcome, num = "best") {
  ## Read outcome data
  permittedOutcomes <- c("heart attack", "heart failure", "pneumonia")
  allOutcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  names (allOutcomes) [c(11,17,23)] <- permittedOutcomes
  
}