## ---- problemC ----

z <- as.matrix(read.table("files/z.txt", header=FALSE))
u <- as.matrix(read.table("files/u.txt", header=FALSE))

lambda0 <- vector("numeric")
lambda1 <- vector("numeric")
n <- length(z)

lambda0 <- c(lambda0, 1000)
lambda1 <- c(lambda1, 1000)

for (i in 1:10) {
  lambda0 <- c(lambda0, n * (1 / sum(u*z + (1 - u) * (1/lambda0[i] - z/(exp(lambda0[i] * z) - 1)))))
  lambda1 <- c(lambda1, n * (1 / sum((1 - u) * z + u * (1/lambda1[i] - z/(exp(lambda1[i] * z) - 1)))))
}