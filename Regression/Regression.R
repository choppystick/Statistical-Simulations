regression_simulation <- function(normal=T, conditioned=T){
  # Generate 15 random samples with distribution Unif(2,5)
  x = runif(15, 2, 5)
  if(normal == T){
    # Response values with normal errors
    y = 3 + 1.5*x + rnorm(15, 0, 0.25)
  }
  
  else{
    # Response values with non-normal errors 
    y = 3 + 1.5*x + rexp(15, 1)
  }

  ## Values of the explanatory variable ##
  plot(x, y, xlab = "Explanatory Variable", ylab = "Response Variable", xlim
       = c(2, 5.5), ylim = c(6, 11), main = "Scatterplot of y vs. x", pch = 19,
       cex = 0.5)
  
  if(normal==T){abline(3, 1.5, lwd = 2)}
  
  mod1 = lm(y~x)
  abline(mod1$coe[1], mod1$coe[2], col = 2, lwd = 2)
  for(i in 1:15)
  {
    abline(v = x[i])
  }
  
  plot(0, 0, xlab = "Explanatory Variable", ylab = "Response Variable", xlim
       = c(2, 5.5), ylim = c(6, 11), main = "Scatterplot of y vs. x", pch = 19,
       cex = 0.5)
  I = 1000
  beta_0 = numeric(I)
  beta_1 = numeric(I)
  sigma2 = numeric(I)
  if(normal==T){  
    for(i in 1:I){
      if(conditioned==F){x = runif(15, 2, 5) }
      y = 3 + 1.5*x + rnorm(15, 0, 0.25)
      mod1 = lm(y~x)
      summ1 = summary(mod1)
      abline(mod1$coe[1], mod1$coe[2], col = 2)
      beta_0[i] = mod1$coe[1]
      beta_1[i] = mod1$coe[2]
      sigma2[i] = summ1$sigma^2
    }
      points(x, y)
      abline(3, 1.5, lwd = 1)
  }
  
  else{
    for(i in 1:I){
      if(conditioned==F){x = runif(15, 2, 5) }
      y = 3 + 1.5*x + rexp(15, 1)
      mod1 = lm(y~x)
      summ1 = summary(mod1)
      abline(mod1$coe[1], mod1$coe[2], col = 2)
      beta_0[i] = mod1$coe[1]
      beta_1[i] = mod1$coe[2]
      sigma2[i] = summ1$sigma^2
    }
    points(x, y)
    abline(3, 1.5, lwd = 1)
  }
  
  if(normal==T){
    hist(beta_0, xlab = "Estimate", main = "Histogram of the Intercept
  Estimates")
    mean(beta_0)
    var_beta_0 = ((0.25^2)*sum(x^2))/(15*14*var(x))
    var(beta_0)
    
    hist(beta_1, xlab = "Estimate", main = "Histogram of the Slope Estimates")
    mean(beta_1)
    var_beta_1 = ((0.25^2))/(14*var(x))
    var(beta_1)
    
    hist(sigma2, main = "Histogram of the Error Variance Estimate")
    mean(sigma2)
    
    
    plot(beta_0, beta_1, xlab = "Intercept Estimate", ylab = "Slope Estimate",
         pch = 19, cex = 0.05)
    cov_beta = -(0.25^2)*(mean(x)/(14*var(x)))
    cov(beta_0, beta_1)
  }
  
  
  ### We use bootstrap to calculate the covariance
  else{
    B <- 1000
    beta_0_boot <- numeric(B)
    beta_1_boot <- numeric(B)
    
    for(i in 1:B) {
      indices <- sample(1:15, 15, replace = TRUE)
      x_boot <- x[indices]
      y_boot <- y[indices]
      
      mod_boot <- lm(y_boot ~ x_boot)
      
      beta_0_boot[i] <- coef(mod_boot)[1]
      beta_1_boot[i] <- coef(mod_boot)[2]
    }
    
    var_beta_0_boot <- var(beta_0_boot)
    var_beta_1_boot <- var(beta_1_boot)
    cov_beta_boot <- cov(beta_0_boot, beta_1_boot)
    
    hist(beta_0_boot, xlab = "Estimate", main = "Histogram of the Intercept
  Estimates")
    mean(beta_0_boot)
    var_beta_0_boot
    
    hist(beta_1_boot, xlab = "Estimate", main = "Histogram of the Slope Estimates")
    mean(beta_1_boot)
    var(var_beta_1_boot)
    
    hist(sigma2, main = "Histogram of the Error Variance Estimate")
    mean(sigma2)
    
    plot(beta_0_boot, beta_1_boot, xlab = "Intercept Estimate", ylab = "Slope Estimate",
         pch = 19, cex = 0.05)
    cov(beta_0_boot, beta_1_boot)
  }
}



