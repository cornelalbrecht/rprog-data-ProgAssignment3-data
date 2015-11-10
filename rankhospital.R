rankhospital <- function(state, outcome, num = "best") {
  # Read Data
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  # Subset Data
  filt <- subset(dat, dat$State == state)
  # Check for valid state (here: if it returns no entries)
  if(nrow(filt) == 0) stop("invalid state")
  
  # Check for valid condition
  if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia") stop("invalid outcome")
  
  # Convert to Numeric
  suppressWarnings(filt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(filt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
  suppressWarnings(filt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(filt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
  suppressWarnings(filt$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(filt$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
  
  
  # What condition and
  # Sort the subset by rating
  if (outcome == "heart attack") ord <- order(filt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, filt$Hospital.Name)
  if (outcome == "heart failure") ord <- order(filt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, filt$Hospital.Name)
  if (outcome == "pneumonia") ord <- order(filt$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, filt$Hospital.Name)
  
  
  # best worst -> numbers
  if (num == "best") num <- 1
  # Remove the NAs
  if (num == "worst") num <- nrow(na.omit(filt))
   
  # Debug Output
  #out <- cbind(filt$Hospital.Name[ord], filt$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[ord])
  #out <- data.frame(filt$Hospital.Name[ord], filt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[ord])
  
  #Final Output
  out <- filt$Hospital.Name[ord[num]]
  
  return(out)
}
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death
