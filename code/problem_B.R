
## ----permTest
permTest <- function(data){
  
  data$meas <- sample(data$meas, size = length(data$meas))
  
  lm <- lm(log(meas) ~ pers, data = data)
  
  s <- summary(lm)
  fval <- s$fstatistic[1]
  
  return(fval)
}
