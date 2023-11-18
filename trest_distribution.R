set.seed(515) # Set the seed for the random number generator




bootstrap <- function(B=499,alpha=0.05, df, sd)
  
  B <- 499         # Number of bootstrap replications
alpha <- 0.05    # Nominal level of the test



# load or simulate your data and store as X
X <- df_type2$Duration

n <- length(X)                                          # Sample size
X.bar <- mean(X)                                        # Sample mean of X
St.Dev <- sd(X)                                         # Standard deviation of X
Q.n <- sqrt(n)*X.bar/St.Dev                            # Test statistic


P=30
test <- rep(NA, times = P)
for (p in 0:P) {
  
k= 1+(0.1*p)
beta= k 
# We use the bootstrap to find the critical value
Q.star <- rep(NA, times = B)     # Initialise vector for bootstrap statistics
X.bar.star.mean <- rep(NA, times = B)
reject.t <- rep(0, times = B)
cv.t <-qt(0.975,n-1)   

for (b in 1:B) {
  #J <- sample.int(n, size = n, replace = TRUE)        # Draw the indices J
  # X.star <- X[J].  # Draw the bootstrap sample
  X.star <- rgamma(n, shape = k, scale = beta)
  #X.star <- rnorm(n, mean = X.bar, sd = St.Dev)
  X.bar.star <- mean(X.star)  #Bootstrap sample mean
  X.bar.star.mean[b] <- X.bar.star
  St.Dev.star <- sd(X.star)                           # Bootstrap standard deviation
  Q.star[b] <- sqrt(n)*(X.bar.star-X.bar)/St.Dev.star # Bootstrap statistic
  if (abs(Q.star[b]) > cv.t) {reject.t[p] <- 1}
}
test[p]<-mean(reject.t)

}
print(test)

print(qt(0.975,n-1))



cv <- quantile(Q.star, probs = c(0.025,0.975) )              # Calculate the bootstrap critical value
p.val <- mean(absolute > Q.n