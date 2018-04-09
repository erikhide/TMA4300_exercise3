
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
  
  # x_pred <- vector("numeric", boot_iter)
  
  for(b in 1:boot_iter){
    
    e <- sample(epsilon, size = t, replace = TRUE)
    x0_start <- ceiling(runif(1) * (t - p + 1))
    
    x_star <- ARp.filter(x0 = x[c(x0_start, x0_start + 1)], beta = beta, e = e)
    
    betas_star <- ARp.beta.est(x_star, p)
    beta_mat[b, ] <- betas_star[type][[1]]
    
    # x_101 <- beta_mat[b, 1] * x_star[t] + beta_mat[b, 2] * x_star[t - 1]
    # x_pred[b] <- x_101
  }
  
  # return(list(beta_mat = beta_mat,
  #             x_pred = x_pred))
  
  return(beta_mat)
}



displayBeta <- function(samples, samples_2 = NULL, title = NULL, nbin = 50, names = NULL){
  
  if(!is.null(samples_2)){
    data_1 <- as.tibble(samples) %>% mutate(name = names[1])
    data_2 <- as.tibble(samples_2) %>% mutate(name = names[2])
    data <- rbind(data_1, data_2)
    # data <- gather(data, key = key, value = value, "beta[1]", "beta[2]")
  } else{
    data <- as_tibble(samples)
    data <- gather(data)
  }
  
  

  
  if(!is.null(samples_2)){
    gg_1 <- ggplot(data) + 
      geom_histogram(aes(x = data$'beta[1]'), bins = nbin) +
      facet_wrap(~name, labeller = label_parsed, scales = "free_y", ncol = 1) +
      labs(title = expression(beta[1]), x = expression(beta))
    
    gg_2 <- ggplot(data) + 
      geom_histogram(aes(x = data$'beta[2]'), bins = nbin) +
      facet_wrap(~name, labeller = label_parsed, scales = "free_y", ncol = 1) +
      labs(title = expression(beta[2]), x = expression(beta))
    
    gg <- ggarrange(gg_1, gg_2, ncol = 2)
    
  } else{
    gg <- ggplot(data) +
      geom_histogram(aes(x = value), bins = nbin) +
      facet_wrap(~key, scales = "free", labeller = label_parsed)
  }
  
  if(!is.null(title)){
    gg <- gg + labs(title = title)
  }
    
  return(gg)
}
