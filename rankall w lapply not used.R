rankall <- function(outcome, num = "best") {
  
  
  
  # Run Apply Loop with rankhospital over this
  out <- sapply(teslev, rankhospital, "heart attack",num)
  
  df = data.frame(out,teslev)
 
  
  return(df)
}
## Check that state and outcome are valid
## Return hospital name in that state with lowest 30-day death
