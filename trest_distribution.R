set.seed(600) # Set the seed for the random number generator
library(MASS)
library(survival)
library(fitdistrplus)
library(logspline)
library(carData)
library(car)



bootstrap <- function(B=499,alpha=0.05, df, sd)
  
  B <-         # Number of bootstrap replications
alpha <- 0.05    # Nominal level of the test



# load or simulate your data and store as X
x <- df_type2$Duration

n <- length(X)                                          # Sample size
X.bar <- mean(X)                                        # Sample mean of X
St.Dev <- sd(X)                                         # Standard deviation of X
Q.n <- sqrt(n)*X.bar/St.Dev                            # Test statistic


P=30
test <- rep(NA, times = P)
for (p in 0:P) {
  
k= 0.1+(0.1*p)
beta= X.bar/k
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
  if (abs(Q.star[b]) > cv.t){
    reject.t[b] <- 1
    }
}
test[p]<-mean(reject.t)

}
print(test)

print(qt(0.975,n-1))
print(Q.star)
hist(X.star)
hist(df_type2$Duration)


## some code to test the fit of distribution 
print(descdist(df_type2$Duration, discrete = FALSE,boot = 500)) ## graph for skewness, kurtiosis
fit.gamma <- fitdist(df_type2$Duration, "gamma")
fit.norm <- fitdist(df_type2$Duration, "norm")
fit.lnorm <- fitdist(df_type2$Duration, "c")

print(mean(df_type2$Duration))
print(summary(fit.gamma))


cv <- quantile(Q.star, probs = c(0.025,0.975) )              # Calculate the bootstrap critical value
p.val <- mean(absolute > Q.n)
              
              par(mfrow = c(2, 2), mar = c(4, 4, 2, 1)) ## some code to make nice graphs 
              fg <- fitdist(df_type2$Duration, "gamma")
              fln <- fitdist(df_type2$Duration, "norm")
              plot.legend <- c( "norm", "gamma")
              denscomp(list(fln, fg), legendtext = plot.legend)
              qqcomp(list( fln, fg), legendtext = plot.legend)
              cdfcomp(list( fln, fg), legendtext = plot.legend)
              ppcomp(list(fln, fg), legendtext = plot.legend)
              plotdist(df_type2$Duration, histo = TRUE, demp = TRUE)              
            
              