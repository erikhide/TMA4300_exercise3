
# 
# calcResiduals <- function(x, beta){
#   t <- length(x)
#   res <- x[-c(1, 2)] - 
#     beta[1] * x[-c(1, t)] -
#     beta[2] * x[-c(t - 1, t)]
#   
#   return(res)
# }


bootstrapA <- function(x, beta, epsilon, boot_iter, type){
  
  if(type != "LS" && type != "LA"){
    return("error! Not acceptable type.")
  }
  
  t <- length(x)
  p <- 2
  
  beta_mat <- matrix(0, boot_iter, p)
  colnames(beta_mat) <- c("beta[1]", "beta[2]")
  
  for(b in 1:boot_iter){
    
    e <- sample(epsilon, size = t, replace = TRUE)
    x0_start <- ceiling(runif(1) * (t - p + 1))
    
    x_star <- ARp.filter(x0 = x[c(x0_start, x0_start + 1)], beta = beta, e = e)
    
    betas_star <- ARp.beta.est(x_star, p)
    beta_mat[b, ] <- betas_star[type][[1]]
  }
  
  return(beta_mat)
}



displayBeta <- function(samples, title = NULL, nbin = 50){
  
  data <- as_tibble(samples)
  data <- gather(data)
  
  gg <- ggplot(data = data) +
    geom_histogram(aes(x = value), bins = nbin) +
    facet_wrap(~key, scale = "free", labeller = label_parsed)
  
  if(!is.null(title)){
    gg <- gg + labs(title = title)
  }
    
  return(gg)
}
