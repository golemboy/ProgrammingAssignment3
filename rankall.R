library(dplyr)

ranker <- function(x, num) {      
  t<-x
  if(num == "best") {
    t <- filter(t, Rank == 1)
  }
  if(num == "worst") {
    t <- filter(t, Rank == max(Rank))
  }
  if(is.numeric(num)) {
    t <- filter(t, Rank == num)    
  }  
  
  if(nrow(t) == 0){
    data.frame(hospital = as.character(NA), state=x$State)
  }
  else
  {
    select(t, hospital = Hospital.Name, state = State)
  }    
}

rankall <- function(outcome, num = "best") { 
  ## Read outcome data  
  data <- (read.csv("outcome-of-care-measures.csv", colClasses = "character"))
  
  ## Check that outcome are valid    
  outcome <- sub(pattern = " ", replacement = ".", x = outcome)
  
  tmp <- data %>%          
          select(Hospital.Name, State, starts_with("Hospital.30.Day.Death")) %>%        
          select(Hospital.Name, State, rates = contains(outcome, ignore.case = TRUE)) %>%
          filter(rates != "Not Available")
  
  if(ncol(tmp) < 3) stop("invalid outcome")  
    
  tmp2 <- tmp %>%
          mutate(rates = as.numeric(rates))  %>% 
          group_by(State) %>%          
          #mutate(Rank = rank(rates, ties.method= "random")) %>%                    
          #mutate(Rank = row_number(rates)) %>%
          mutate(Rank = min_rank(rates)) %>%
    
          #arrange(State, Rank) %>%
          do(ranker(.,num)) %>%
          group_by(hospital, state) %>%          
          summarize() %>%
          arrange(state, hospital)
  
  tmp2
  
    


#   if(num == "best") {
#     tmp3 <- filter(tmp2, Rank == 1)
#   }
#   if(num == "worst") {
#     tmp3 <- filter(tmp2, Rank == max(Rank))
#   }
#   if(is.numeric(num)) {
#     tmp3 <- filter(tmp2, Rank == num)
#   }
#   
#   ## Return a data frame with the hospital names and the ## (abbreviated) state name
#   if(nrow(tmp3) == 0) c(NA)    
#   else select(tmp3, hospital = Hospital.Name, state = State)    
  
}
