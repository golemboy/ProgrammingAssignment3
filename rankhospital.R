library(dplyr)

#data <- (read.csv("outcome-of-care-measures.csv", colClasses = "character"))

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- (read.csv("outcome-of-care-measures.csv", colClasses = "character"))
  
  ## Check that state and outcome are valid  
  tmp <- filter(data, State == state)          
  outcome <- sub(pattern = " ", replacement = ".", x = outcome)
  if (nrow(tmp) == 0) stop("invalid state")
  tmp2 <- tmp %>%
    select(Hospital.Name, State, starts_with("Hospital.30.Day.Death")) %>%        
    select(Hospital.Name, State, rates = contains(outcome, ignore.case = TRUE))
  
  if(ncol(tmp2) < 3) stop("invalid outcome")  
  #print(tmp2)  
  tmp3 <- tmp2 %>%
          filter(State == state, rates != "Not Available") %>%   
          mutate(rates = as.numeric(rates))  %>% 
          mutate(Rank = rank(rates, ties.method= "random"))  %>%
          arrange(Rank, Hospital.Name)
#   
#   print(tmp3)
#     
  if(num == "best") {
    tmp4 <- filter(tmp3, Rank == 1)
  }
  if(num == "worst") {
    tmp4 <- filter(tmp3, Rank == max(Rank))
  }
  if(is.numeric(num)) {
    tmp4 <- filter(tmp3, Rank == num)
  }         
  
  if(nrow(tmp4) == 0) c(NA)    
  else tmp4$Hospital.Name
      
  ## Return hospital name in that state with the given rank
  
  ## 30-day death rate
}
