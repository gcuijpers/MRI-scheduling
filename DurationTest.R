set.seed(600) # Set the seed for the random number generator
library(MASS)
library(survival)
library(fitdistrplus)
library(logspline)
library(carData)
library(car)





## Bootstrap stuf ---------------------------------------------------------------------------------------------------

B <- 500        # Number of bootstrap replications
X <- df_type2$Duration

n <- length(X)                                          # Sample size
X.bar <- mean(X)                                        # Sample mean of X
St.Dev <- sd(X)                                         # Standard deviation of X
Q.n <- sqrt(n)*X.bar/St.Dev                            # Test statistic
  

beta= X.bar/k
# We use the bootstrap to find the critical value
Q.star <- rep(NA, times = B)  # Initialise vector for bootstrap statistics
stat_norm <- rep(NA, times = B)
stat_gamma <- rep(NA, times = B)
X.bar.star.mean <- rep(NA, times = B)
reject.t <- rep(0, times = B)
cv.t <-qt(0.975,n-1)   

for (b in 1:B) {
  J <- sample.int(n, size = n, replace = TRUE)        # Draw the indices J
   X.star <- X[J]  # Draw the bootstrap sample
  #X.star <- rgamma(n, shape = k, scale = beta)
  #X.star <- rnorm(n, mean = X.bar, sd = St.Dev)
  X.bar.star <- mean(X.star)  #Bootstrap sample mean
  X.bar.star.mean[b] <- X.bar.star
  St.Dev.star <- sd(X.star)                           # Bootstrap standard deviation
  Q.star[b] <- sqrt(n)*(X.bar.star-X.bar)/St.Dev.star # Bootstrap statistic
  stat_gamma[b] <- ks.test(X.star, "pgamma", shape = 12.58373, rate = 18.80033)$statistic. ##Kolmogorov-Smirnov Test
  stat_norm[b] <- ks.test(X.star, "pnorm", mean = 0.6693389, sd = 0.1868936)$statistic
  
}

print(mean(stat_norm))
print(mean(stat_gamma))
print(mean(X.star))
print(mean(X.bar.star.mean))


## Fitting distribution-------------------------------------------------------------------------------------

print(descdist(df_type2$Duration, discrete = FALSE,boot = 500)) ## checks skewness and kurtosis 

## some code to test the fit of distribution and find parameters 
fit.gamma <- fitdist(df_type2$Duration, "gamma")
fit.norm <- fitdist(df_type2$Duration, "norm")

print(summary(fit.gamma))
print(summary(fit.norm))


## Code to graph the fit of the distributions-----------------------------------------------------------------------------
              
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1)) ## some code to make nice graphs 
fg <- fitdist(df_type2$Duration, "gamma")
fln <- fitdist(df_type2$Duration, "norm")
plot.legend <- c( "norm", "gamma")
denscomp(list(fln, fg), legendtext = plot.legend)
qqcomp(list( fln, fg), legendtext = plot.legend)
cdfcomp(list( fln, fg), legendtext = plot.legend)
ppcomp(list(fln, fg), legendtext = plot.legend)
plotdist(df_type2$Duration, histo = TRUE, demp = TRUE)              
            

              
## Some more bootsstrap code to check quantiles------------------------------------------------------------------------------
k=1000
simsamples <- replicate(k, X[sample.int(n, size = n, replace = TRUE)])
print(simsamples)
Q1 <- function(x){quantile(x,0.25)}
Q3 <- function(x){quantile(x, 0.75)}
simmean <- apply(simsamples, 2, mean)
simQ1 <- apply(simsamples,2, Q1)
simQ3 <- apply(simsamples, 2, Q3)
print(mean(simQ1))
quantile(simmean, c(0.025, 0.975))
quantile(simQ3, c(0.025, 0.975))
print(mean(simQ1))

              