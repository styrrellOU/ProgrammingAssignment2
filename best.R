library(tidyverse)


best <- function(state, outcome) {
  ## Read outcome data
  permittedOutcomes <- c("heart attack", "heart failure", "pneumonia")
  allOutcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  names (allOutcomes) [c(11,17,23)] <- permittedOutcomes
  ## Check that state and outcome are valid
  
  if (!state %in% allOutcomes$State) {
    stop ("invalid state")
  }
  
  if (!outcome %in% names(allOutcomes)) {
    stop("invalid outcome")
  }
  
  #allOutcomes <- allOutcomes |> 
  #  mutate(rate = as.numeric(.data[[outcome]])) |>
  #  filter (State == state)
  
  allOutcomes <- allOutcomes |>  
    mutate(rate = as.numeric(na_if(.data[[outcome]], "Not Available"))) |>
    filter (State == state, !is.na(rate)) |> 
    select(name = Hospital.Name, State, rate) 
  
  bestOutcome <- allOutcomes |> 
    arrange(rate, name) |> 
    slice(1)
  
  bestOutcome$name
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
