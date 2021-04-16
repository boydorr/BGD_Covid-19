## Extract day of first of two negative tests from Fig.1 of Hu et al.
# if final available test is positive, take the day after
# if two negs followed by positive, continue counting
days <- c(8,18,1,6,1,1,6,17,1,11,14,4,11,14,12,6,3,11,1)

# Get median
median(days) # matches quoted by byrne

# Get mean
mean(days)

# Get CI
confint(lm(days~1))
