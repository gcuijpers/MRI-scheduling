
set.seed(1000) # Set the seed for the random number generator




bootstrap <- function(B=499,alpha=0.05, df, sd)
   
B <- 500         # Number of bootstrap replications
alpha <- 0.05    # Nominal level of the test



# load or simulate your data and store as X
X <- df_type2$Duration

n <- length(X)                                          # Sample size
X.bar <- mean(X)                                        # Sample mean of X
St.Dev <- sd(X)                                         # Standard deviation of X
Q.n <- sqrt(n)*X.bar/St.Dev                            # Test statistic

# We use the bootstrap to find the critical value
Q.star <- rep(NA, times = B)             # Initialise vector for bootstrap statistics
X.bar.star.sd <- rep(NA, times = B)
X.bar.star.mean <- rep(NA, times = B)
for (b in 1:B) {
    #J <- sample.int(n, size = n, replace = TRUE)        # Draw the indices J
    #X.star <- X[J]                                      # Draw the bootstrap sample
     X.star <- rnorm(n, mean = X.bar, sd = St.Dev)
    X.bar.star <- mean(X.star)  #Bootstrap sample mean
    St.Dev.star <- sd(X.star) ## Bootstrap standard deviation
    X.bar.star.mean[b] <- X.bar.star
    X.bar.star.sd[b] <- St.Dev.star
                               
    Q.star[b] <- sqrt(n)*(X.bar.star-X.bar)/St.Dev.star # Bootstrap statistic
    
}
cv <- quantile(X.bar.star.sd, probs = c(0.025,0.975) )              # Calculate the confidence interval
p.val <- mean(absolute > Q.n)                             # Calculate the bootstrap p-value

print(mean(X.bar.star.sd))
print(cv)
hist(X.bar.star.mean)
print(X.bar.star.mean) 
absolute <- abs(Q.star)
hist(X.bar.star.mean)

upperbound <- X.bar-(-1.996781*St.Dev/sqrt(n)) ## critical values are calculated above at cv
lowerbound <- X.bar-(1.753916*St.Dev/sqrt(n))
print(upperbound)
print(lowerbound)


