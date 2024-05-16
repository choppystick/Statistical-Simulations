#Cochran's Theorem states that given i.i.d. standard normal distributed random variables X1...Xn, a set of quadratic forms Q1...Qk and
#symmetric matrices A1...Ak each with rank ri where each Qi = (X^T)*Ai*X , and X = (X1...Xn)^T. Assuming that sum(Qi) = (X^T)*X = sum(Xi^2):
# r1 + ... + rk = n if and only if Qi are independent chi square variable with ri degrees of freedom

#Simulation for the above statements in single-variable normal distribution:
n <- 1000

data <- matrix(rnorm(n), ncol=1)

group <- gl(3, n/3)
X <- model.matrix(~ group)

# projection vector
H <- X %*% solve(t(X) %*% X) %*% t(X)
I <- diag(n)
J <- matrix(1, 1000, 1000)

# proof for the symmetric matrices: https://www.robots.ox.ac.uk/~fwood/teaching/W4315_Fall2011/Lectures/lecture_12/lecture_12.pdf
A_total <- I - (1/n)*J # Total sum of squares
A_between <- H - (1/n)*J  # Between-groups sum of squares
A_within <- I - H  # Within-groups sum of squares

#Quadratic forms
Q_total <- as.numeric(t(data) %*% A_total %*% data)
Q_between <- as.numeric(t(data) %*% A_between %*% data)
Q_within <- as.numeric(t(data) %*% A_within %*% data)

# Verify the sum of quadratic forms
Q_total_sum <- Q_total
Q_parts_sum <- Q_between + Q_within
cat("Sum of parts:", Q_parts_sum, "\n")
cat("Total sum:", Q_total_sum, "\n")
norm_sum <- t(data) %*% data
cat("Is sum of squared normal equal to sum of all quadratic forms", round(norm_sum) == round(Q_total_sum), "\n") 

#Verify distributions
#We first simulate quadratic forms using bootstrap
sim_size <- 500
simulated_QB <- c(0, 1:sim_size-1)
simulated_QW <- c(0, 1:sim_size-1)
for (i in 1:sim_size) {
  data <- matrix(rnorm(1000), ncol=1)
  group <- gl(3, n/3)
  X <- model.matrix(~ group)
  
  # projection vector
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  I <- diag(n)
  J <- matrix(1, 1000, 1000)
  
  A_between <- H - (1/n)*J  # Between-groups sum of squares
  A_within <- I - H  # Within-groups sum of squares
  
  simulated_QB[i] <- as.numeric(t(data) %*% A_between %*% data)
  simulated_QW[i] <- as.numeric(t(data) %*% A_within %*% data)
  }

hist(simulated_QB, main="Between-Groups Sum of Squares", xlab="Value", freq=FALSE)
curve(dchisq(x, df=2), add=TRUE, col="red")

hist(simulated_QW, main="Within-Groups Sum of Squares", xlab="Value", freq=FALSE)
curve(dchisq(x, df=n-3), add=TRUE, col="red")

# Test for independence
cor.test(simulated_QB, simulated_QW)

# Chi-squared goodness-of-fit test
between_chi2 <- sum((simulated_QB - mean(simulated_QB))^2 / mean(simulated_QB))
within_chi2 <- sum((simulated_QW - mean(simulated_QW))^2 / mean(simulated_QW))

cat("Chi-squared test statistic for between-groups:", between_chi2, "\n")
cat("Chi-squared test statistic for within-groups:", within_chi2, "\n")

pchisq(between_chi2, 1)
pchisq(within_chi2, 1)