source("code/problem_C.R")
library(optimization)

calculateLikelihood <- function(lambda) {
  
  # Must choose which data set to use:
  z <- as.matrix(read.table("files/z.txt", header=FALSE))
  u <- as.matrix(read.table("files/u.txt", header=FALSE))
  #z <- as.matrix(read.table("files/z_gen.txt", header=FALSE))
  #u <- as.matrix(read.table("files/u_gen.txt", header=FALSE))
  
  likelihood <- 0
  #likelihood2 <- 1
  n <- length(z)
  
  for (i in 1:n) {
    likelihood <- likelihood + 2*u[i]*log(lambda[1]) + 2*u[i]*log(lambda[2]) - u[i]*log(lambda[1]^2-lambda[2]^2) + u[i]*log(exp(-lambda[2]*z[i])-exp(-lambda[1]*z[i])) + (1-u[i])*log(lambda[1]) + 2*(1-u[i])*log(lambda[2]) - (1-u[i])*log(lambda[2]^2-lambda[1]^2) + (1-u[i])*log(exp(-lambda[1]*z[i])-exp(-lambda[2]*z[i]))
    #likelihood2 <- likelihood2 * ((lambda[1]^2*lambda[2]/(lambda[1]^2-lambda[2]^2)) * (exp(-lambda[2]*z[i]) - exp(-lambda[1]*z[i])))^u[i]
    #likelihood2 <- likelihood2 * ((lambda[1]*lambda[2]^2/(lambda[2]^2-lambda[1]^2)) * (exp(-lambda[1]*z[i]) - exp(-lambda[2]*z[i])))^(1-u[i])
  }
  
  return(likelihood)
}

#opt <- optim_nm(fun=calculateLikelihood, k=2, start=c(3,9), maximum=TRUE)

x <- seq(3,4,0.01)
y <- seq(9,10,0.01)
res <- c()
for (i in 1:length(x)) {
  res <- c(res, calculateLikelihood(c(x[i], y[i])))
}
plot(x, res)
plot(y, res)

################################################################################

# Generate new (larger) set to test new solution vs. EM algorithm:

# lambda1_real <- 3
# lambda2_real <- 4
# n <- 100
# 
# x_gen <- rexp(n, lambda1_real)
# y_gen <- rexp(n, lambda2_real)
# u_gen <- as.integer(c(x_gen > y_gen))
# z_gen <- x_gen*u_gen + y_gen*(1-u_gen)
# write.table(z_gen, "files/z_gen.txt", row.names=FALSE, col.names=FALSE)
# write.table(u_gen, "files/u_gen.txt", row.names=FALSE, col.names=FALSE)
# 
# em_gen <- EM(z_gen, u_gen, 5, 5, 20)
# opt_gen <- optim_nm(fun=calculateLikelihood, start=c(3.2636,3.6876), maximum=TRUE)

