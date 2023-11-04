
set.seed(515) # Set the seed for the random number generator




bootstrap <- function(B=499,alpha=0.05, df, mean=0, sd)
   
B <- 499         # Number of bootstrap replications
alpha <- 0.05    # Nominal level of the test

# load or simulate your data and store as X
X <- df

n <- length(X)                                          # Sample size
X.bar <- mean(X)                                        # Sample mean of X
St.Dev <- sd(X)                                         # Standard deviation of X
Q.n <- sqrt(n)*(X.bar-mean)/St.Dev                      # Test statistic

# We use the bootstrap to find the critical value
Q.star <- rep(NA, times = B)                            # Initialise vector for bootstrap statistics
for (b in 1:B) {
    J <- sample.int(n, size = n, replace = TRUE)        # Draw the indices J
    X.star <- X[J]                                      # Draw the bootstrap sample
    X.bar.star <- mean(X.star)                          # Bootstrap sample mean
    St.Dev.star <- sd(X.star)                           # Bootstrap standard deviation
    Q.star[b] <- sqrt(n)*(X.bar.star-X.bar)/St.Dev.star # Bootstrap statistic
}
cv <- quantile(Q.star, probs = 1-alpha )              # Calculate the bootstrap critical value
p.val <- mean(Q.star > Q.n)                             # Calculate the bootstrap p-value





