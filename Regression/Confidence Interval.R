###Initialization 
store <- rep(NA,100)
lower <- rep(NA, 100)
upper <- rep(NA, 100)
pop.mean <- 30
pop.sd <- 5
n <- 100


###Simulation of random samples
for(i in 1:100){
  sample <- rnorm(n, mean=pop.mean, sd=pop.sd)
  mean <- mean(sample)
  
  #Margin of error calculation
  standard_error <- pop.sd / sqrt(n)
  alpha = 0.05
  degrees_of_freedom = n - 1
  t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)
  margin_error <- t_score * standard_error
  
  #Confidence Interval
  lower[i] <- mean - margin_error
  upper[i] <- mean + margin_error
  
  if(lower[i] < pop.mean & upper[i] > pop.mean){
    store[i] <- T
  }
  
  else{
    store[i] <- F
  }
}

cis <- cbind(lower, upper)
print(store)
summary(store)

### 95% of the intervals should contain the population mean ###

# Plot of the intervals
line.width <- ifelse(store==F, 2, 1)
cis <- cbind(cis, line.width)
x <- 0:100
y <- seq(25, 35, by=1/10)
plot(x, y, type="n", xlab="i-th repeated sample", ylab="Scores", main="95% CI Sample")
abline(30, 0, lwd=2)
arrows(x, cis[,1], x, cis[,2], length=0, lwd=cis[,3])
