rankall <- function(outcome, pnum = "best") {

  # Read Data
  dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Get levels
  lvs <- levels(as.factor(dat$State))
  
  # Initialize Data Frame
  df <- data.frame()

  
  # For loop over every state
  for (state in lvs){
    
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
    if (outcome == "heart attack") {
      ord <- order(filt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, filt$Hospital.Name) 
      wnum <- length(na.omit(filt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    }
    if (outcome == "heart failure"){
      ord <- order(filt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, filt$Hospital.Name)
      wnum <- length(na.omit(filt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    }
    if (outcome == "pneumonia"){
      ord <- order(filt$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, filt$Hospital.Name)
      wnum <- length(na.omit(filt$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    }
    
    ######## best worst -> numbers
    ### BUG FIXED - also implement in RANK HOSPITAL!!!!!
    
    if (pnum == "best") {num <- 1L}
    # Remove the NAs
    else if (pnum == "worst") {
      num <- wnum
    } else {
      num <- pnum
    }
    
    ##DEBUG
    #print(filt$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[ord])
    #print(filt$Hospital.Name[ord])
    #print("row wo")
    #print(state)
    #print(nrow(na.omit(filt)))
    
    
    # Debug Output
    #out <- cbind(filt$Hospital.Name[ord], filt$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[ord])
    #out <- data.frame(filt$Hospital.Name[ord], filt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[ord])
    
    #Final Output
    tmp <- cbind(filt$Hospital.Name[ord[num]],state)

    df <- rbind(df, tmp)
  }
  
  colnames(df) <- c("hospital","state")
  rownames(df) <- lvs
  return(df)
}
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death
