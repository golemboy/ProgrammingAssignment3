library(dplyr)

# data <- (read.csv("outcome-of-care-measures.csv", colClasses = "character"))
# head(data)
# names(data)[11]

# o <- "heart attack"
# o <- sub(pattern = " ", replacement = ".", x = o)
# d <- data %>%  
#   select(Hospital.Name, State, starts_with("Hospital.30.Day.Death")) %>%
#   select(Hospital.Name, State, rates = contains(o, ignore.case = TRUE)) %>%  
#   #mutate(rates = as.numeric(rates))  %>% 
#   #filter(State == "TX", !is.na(rates)) %>%   
#   arrange(Hospital.Name)

# head(d)

best <- function(state, outcome) { ## Read outcome data  
  data <- (read.csv("outcome-of-care-measures.csv", colClasses = "character"))
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
              arrange(rates, Hospital.Name)
  #print(tmp3)
  print(head(tmp3$Hospital.Name, 1))
  
    
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death ## rate
}
