#
## Function estimate proportion hospital beds occupies
#__________________________________________

# Arguments
#   modelOutput = output from simulating from the covid model: a dataframe describing the number of 
#                 individuals in each disease state through time
#   beds = number of isolation beds available for covid patients

# Output
#   matrix of with ncol = ncol(modelOutput) and nrow = 2. Columns represent days, row 1 is hospital
#   beds occupied, row 2 is number of those beds that need to be ICU


hospital_beds <- function(modelOutput, beds = 10947){ 
  beds <- data.frame(
    beds_all = (modelOutput$Hosp+modelOutput$ICU), 
    beds_critical = modelOutput$ICU)
  
  return(beds)
  
}

