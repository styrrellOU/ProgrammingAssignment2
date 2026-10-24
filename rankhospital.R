library(tidyverse)

rankhospital <- function (state, outcome, num = "best") {
  ## Read outcome data
  permittedOutcomes <- c("heart attack", "heart failure", "pneumonia")
  allOutcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  names (allOutcomes) [c(11,17,23)] <- permittedOutcomes
  
  if (!state %in% allOutcomes$State) {
    stop ("invalid state")
  }
  
  if (!outcome %in% names(allOutcomes)) {
    stop("invalid outcome")
  }
  
  allOutcomes <- allOutcomes |>  
    mutate(rate = as.numeric(na_if(.data[[outcome]], "Not Available"))) |>
    filter (State == state, !is.na(rate)) |> 
    select(name = Hospital.Name, State, rate)
    #order_by (rate, name)
  
  n <- nrow(allOutcomes)
  
  if (num == "best") {
    num <- 1
  } else if (num == "worst") {
    num <- n
  } else {
    num <- as.numeric(num)   # in case a string like "3" is passed
  }
  
  if (is.na(num) || num < 1 || num > n) return(NA)
  
  
  rankedOutcome <- allOutcomes |> 
    arrange(rate, name) |> 
    slice(num)
  
  rankedOutcome$name
  
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
