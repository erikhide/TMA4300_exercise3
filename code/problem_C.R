## ---- problemC ----

z <- as.matrix(read.table("files/z.txt", header=FALSE))
u <- as.matrix(read.table("files/u.txt", header=FALSE))

EM <- function(z, u, lambda0_init, lambda1_init, iterations) {
  lambda0 <- vector("numeric")
  lambda1 <- vector("numeric")
  n <- length(z)
  
  lambda0 <- c(lambda0, lambda0_init)
  lambda1 <- c(lambda1, lambda1_init)
  
  for (i in 1:iterations) {
    lambda0 <- c(lambda0, n * (1 / sum(u*z + (1 - u) * (1/lambda0[i] - z/(exp(lambda0[i] * z) - 1)))))
    lambda1 <- c(lambda1, n * (1 / sum((1 - u) * z + u * (1/lambda1[i] - z/(exp(lambda1[i] * z) - 1)))))
  }
  
  return(data.frame(lambda0, lambda1))
}

bootstrap <- function(z, u, lambda0_init, lambda1_init, iterations, numSamples) {
  n <- length(z)
  bootstrapSample <- array(0, c(numSamples, n, 2))
  bootstrapVec <- c(1:200)
  
  lambda0 <- vector("numeric")
  lambda1 <- vector("numeric")
  
  for (i in 1:numSamples) {
    temp <- sample(bootstrapVec, replace=TRUE)
    for (j in 1:n) {
      bootstrapSample[i,j,1] <- z[temp[j]]
      bootstrapSample[i,j,2] <- u[temp[j]]
    }
    temp <- EM(bootstrapSample[i,,1], bootstrapSample[i,,2], lambda0_init, lambda1_init, iterations)
    lambda0 <- c(lambda0, temp$lambda0[iterations+1])
    lambda1 <- c(lambda1, temp$lambda1[iterations+1])
  }
  
  se_lambda0 <- sqrt((1/(numSamples - 1)) * sum((lambda0 - mean(lambda0))^2))
  se_lambda1 <- sqrt((1/(numSamples - 1)) * sum((lambda1 - mean(lambda1))^2))
  bias_lambda0 <- (1/numSamples) * sum(lambda0) - 
    EM(z, u, lambda0_init, lambda1_init, iterations)$lambda0[iterations+1]
  bias_lambda1 <- (1/numSamples) * sum(lambda1) - 
    EM(z, u, lambda0_init, lambda1_init, iterations)$lambda1[iterations+1]
  cov <- (1/(numSamples - 1)) * sum((lambda0 - mean(lambda0)) * (lambda1 - mean(lambda1)))
  corr <- cov / (se_lambda0 * se_lambda1)
  
  return(data.frame(se_lambda0, se_lambda1, bias_lambda0, bias_lambda1, corr))
}

calculateLikelihood <- function(lambda) {
  
  z <- as.matrix(read.table("files/z.txt", header=FALSE))
  u <- as.matrix(read.table("files/u.txt", header=FALSE))
  
  likelihood <- 0
  n <- length(z)
  
  for (i in 1:n) {
    likelihood <- likelihood + 2*u[i]*log(lambda[1]) + 2*(1-u[i])*log(lambda[2]) + log(lambda[1]+lambda[2]) - lambda[1]*z[i]*u[i] + u[i]*log(1-exp(-lambda[2]*z[i])) - lambda[2]*z[i]*(1-u[i]) + (1-u[i])*log(1-exp(-lambda[1]*z[i])) - log(2) - log(lambda[1]) - log(lambda[2])
  }

  return(likelihood)
}

library(optimization)
opt <- optim_nm(fun=calculateLikelihood, k=2, start=c(3.4657,9.3532), maximum=TRUE)

x <- seq(3,4,0.01)
y <- seq(9,10,0.01)
res <- c()
for (i in 1:length(x)) {
  res <- c(res, calculateLikelihood(c(x[i], y[i])))
}
plot(x, res)
plot(y, res)
