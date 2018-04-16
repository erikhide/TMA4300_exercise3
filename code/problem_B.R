
## ----permTest

# --------------------------------------------------
# Creates a permutation of the bilirubin data set,
# fits a linear model to the permutated data,
# computes and returns the F-statistic
# ---------------------------------------------------
permutation <- function(data){
  
  data$meas <- sample(data$meas, size = length(data$meas))
  
  lm <- lm(log(meas) ~ pers, data = data)
  
  s <- summary(lm)
  fval <- s$fstatistic[1]
  
  return(fval)
}

# ---------------------------------------------
# Does a permutation test of the bilirubin data 
# using n permutations
# ---------------------------------------------
permTest <- function(data, n){
  
  f_vec <- vector("numeric", n)
  
  for(i in 1:n){
    f_vec[i] <- permutation(bilirubin)
  }
  
  return(f_vec)
}