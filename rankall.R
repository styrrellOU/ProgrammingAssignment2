rankall <- function(outcome, num = "best") {
  
  
  ## Read outcome data
  
  permittedOutcomes <- c("heart attack", "heart failure", "pneumonia")
  allOutcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  names (allOutcomes) [c(11,17,23)] <- permittedOutcomes
  
  ## Check that  outcome is valid
  if (!outcome %in% names(allOutcomes)) {
    stop("invalid outcome")
  }
  
  stateList <- allOutcomes |> distinct(State) |> pull(State)
  
  #for (state %in% stateList) {
    
  #}
  
  # Rank the data once, for all states
  rankedOutcomes <- allOutcomes |> 
    mutate(rate = as.numeric(na_if(.data[[outcome]], "Not Available"))) |>
    filter(!is.na(rate)) |>
    select(hospital = Hospital.Name, state = State, rate) |>
    arrange(state, rate, hospital)
  
  rankedOutcomes |> group_by(state) |> slice(
    if (num == "best") 1
    else if (num == "worst") n()
    else as.numeric(num)
  ) |> ungroup() |> select(hospital, state) |> arrange(state)
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
}

head(rankall("heart attack", 20), 10)

rankall("heart attack")
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)


r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
