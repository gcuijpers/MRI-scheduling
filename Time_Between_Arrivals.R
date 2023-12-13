library(readr)
library(moments)
library(MASS)
library(survival)
library(fitdistrplus)
library(logspline)
library(carData)
library(car)

set.seed(515)													# Set the seed



# ----------------------------------------------------------------------------------------------------------------------------------------------------

#Format date variable
df_type1$Date <- as.Date(df_type1$Date, format= "%Y-%m-%d")

#Add an empty column to the data frame
df_type1$time_diff <- 0 

#Make a data frame with only Date, Time, and Time_diff
Time_Type1 <- data.frame(Date= df_type1$Date, 
                         Time = df_type1$Time, 
                         Time_diff = df_type1$time_diff)

# Iterate over each row in the data frame
for (i in 2:nrow(Time_Type1)) {
  # Check if dates are different
  if (Time_Type1$Date[i] != Time_Type1$Date[i - 1]) {
    # Subtract 9 hours from the last row of each date group
    Time_Type1$Time[i-1] <- Time_Type1$Time[i-1] - 9
  }
  
  # Calculate the time difference
  Time_Type1$Time_diff[i] <- as.numeric(difftime(Time_Type1$Time[i], 
                                                 Time_Type1$Time[i - 1]))
}

# Remove the first row, as the first difference is 0
Time_Type1 <- Time_Type1[-1, ]

#Check if the difference between dates problem is solved
print(Time_Type1) 

##  YES!!

# ----------------------------------------------------------------------------------------------------------------------------------------------------

## Arrival times of Type 1 patients is poisson distributed
## Consequently, we have a exponential distribution in the difference between two arrival times


# Calculate the sample mean
T1_mean <- mean(Time_Type1$Time_diff)
print(T1_mean)

#Calculate sample lambda
T1_Lambda <- 1/T1_mean
print(T1_Lambda)

# Calculate the range of the data
data_range_T1 <- range(Time_Type1$Time_diff)

# Calculate the minimum and maximum values for breaks
breaks_min_T1 <- floor(data_range_T1[1] / 0.05) * 0.05
breaks_max_T1 <- ceiling(data_range_T1[2] / 0.05) * 0.05

# Create a histogram with bins of width 0.05
h1 <- hist(Time_Type1$Time_diff, 
           breaks = seq(breaks_min_T1, breaks_max_T1, by = 0.05), 
           col = "lightblue", main = "Histogram", 
           xlab = "Time Difference", 
           ylab = "Frequency")

## The histogram looks exponential - what we expected 

##------------------------------------------------------------------------------------------------------

B <- 499         # Number of bootstrap replications
alpha <- 0.05    # Nominal level of the test

# load data and store as X
X <- Time_Type1$Time_diff

n <- length(X)                                          # Sample size

# We use the bootstrap to find the critical value
Lambda <- rep(NA, times = B)                            # Initialise vector for bootstrap statistics
for (b in 1:B) {
  J <- sample.int(n, size = n, replace = TRUE)        # Draw the indices J
  X.star <- X[J]                                      # Draw the bootstrap sample
  X.bar.star <- mean(X.star)                        # Bootstrap sample mean
  Lambda[b]  <- 1/X.bar.star                        #Bootstrap lambda
}

# Calculate the bootstrap confidence interval
conf_interval <- quantile(Lambda, probs = c(0.025,0.975))                 # Calculate the bootstrap critical value

# Display the results
cat("True Rate Parameter:", T1_Lambda, "\n")
cat("Bootstrap Confidence Interval:", conf_interval, "\n")


# ------------------------------------------------------------------------------------------------------------------------------------------------------

## We perform the same formatting steps to obtain the time difference for Type 2

#Format date variable
df_type2$Date <- as.Date(df_type1$Date, format= "%Y-%m-%d")

#Add an empty column to the data frame
df_type2$time_diff <- 0 

#Make a data frame with only Date, Time, and Time_diff
Time_Type2 <- data.frame(Date= df_type2$Date, 
                         Time = df_type2$Time, 
                         Time_diff = df_type2$time_diff)

# Iterate over each row in the data frame
for (i in 2:nrow(Time_Type2)) {
  # Check if dates are different
  if (Time_Type2$Date[i] != Time_Type2$Date[i - 1]) {
    # Subtract 9 hours from the last row of each date group
    Time_Type2$Time[i-1] <- Time_Type2$Time[i-1] - 9
  }
  
  # Calculate the time difference
  Time_Type2$Time_diff[i] <- as.numeric(difftime(Time_Type2$Time[i], 
                                                 Time_Type2$Time[i - 1]))
}

# Remove the first row, as the first difference is 0
Time_Type2 <- Time_Type2[-1, ]

#Check if the difference between dates problem is solved
print(Time_Type2) 

##  YES!!

# ------------------------------------------------------------------------------------------------------------------------------------------------------

## We do not know the distribution of the difference between arrival times for Type 2
## Hence we will first plot and try to find a fit for the distribution

# Calculate the mean
T2_mean <- mean(Time_Type2$Time_diff)
T2_sd <- sd(Time_Type2$Time_diff)
print(T2_mean)
print(T2_sd)

# Calculate the range of the data
data_range_T2 <- range(Time_Type2$Time_diff)

# Calculate the minimum and maximum values for breaks
breaks_min_T2 <- floor(data_range_T2[1] / 0.05) * 0.05
breaks_max_T2 <- ceiling(data_range_T2[2] / 0.05) * 0.05

# Create a histogram with bins of width 0.05
h2 <- hist(Time_Type2$Time_diff, 
           breaks = seq(breaks_min_T2, breaks_max_T2, by = 0.05), 
           col = "orange", 
           main = "Histogram", 
           xlab = "Time Difference", 
           ylab = "Frequency")

## The histogram looks normal or gamma - different from the type 1
# ------------------------------------------------------------------------------------------------------------------------------------------------------

skewness_dur2 <- skewness(Time_Type2$Time_diff)
kurtosis_dur2 <- kurtosis(Time_Type2$Time_diff)
jarque.test(Time_Type2$Time_diff)

##----------------------------------------------------------------------------------------------------------------------


print(descdist(Time_Type2$Time_diff, discrete = FALSE,boot = 499))

## We observe that our sample + 499 bootstrapped samples are 
## either normal or gamma in terms of skewness and kurtosis

##Graphs to compare Normal and Gamma
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1)) 
fg <- fitdist(Time_Type2$Time_diff, "gamma")
fln <- fitdist(Time_Type2$Time_diff, "norm")
plot.legend <- c( "norm", "gamma")
denscomp(list(fln, fg), legendtext = plot.legend)
qqcomp(list( fln, fg), legendtext = plot.legend)
cdfcomp(list( fln, fg), legendtext = plot.legend)
ppcomp(list(fln, fg), legendtext = plot.legend)
plotdist(Time_Type2$Time_diff, histo = TRUE, demp = TRUE) 

##------------------------------------------------------------------------------------------------------------------

## Test the fit of distribution and find parameters 
fit.gamma_T <- fitdist(Time_Type2$Time_diff, "gamma")
fit.norm_T <- fitdist(Time_Type2$Time_diff, "norm")

print(summary(fit.gamma_T))
print(summary(fit.norm_T))

##---------------------------------------------------------------------------------------------------------------

B <- 499       # Number of bootstrap replications
X <- Time_Type2$Time_diff

n <- length(X)                                  # Sample size
X.bar <- mean(X)                                # Sample mean of X
St.Dev <- sd(X)                                 # Standard deviation of X


# Initialize empty vectors 

stat_norm <- rep(NA, times = B)
stat_gamma <- rep(NA, times = B)
X.bar.star.sd <- rep(NA, times = B)
X.bar.star.mean <- rep(NA, times = B)
   

for (b in 1:B) {
  J <- sample.int(n, size = n, replace = TRUE)        # Draw the indices J
  X.star <- X[J]  # Draw the bootstrap sample
  X.bar.star <- mean(X.star)  #Bootstrap sample mean
  St.Dev.star <- sd(X.star)   # Bootstrap standard deviation
  X.bar.star.mean[b] <- X.bar.star
  X.bar.star.sd[b] <- St.Dev.star
  stat_gamma[b] <- ks.test(X.star, 
                           "pgamma", 
                           shape = 6.365265, 
                           rate = 7.344667)$statistic ##Kolmogorov-Smirnov Test
  
  stat_norm[b] <- ks.test(X.star, 
                          "pnorm", 
                          mean =  0.8666387, 
                          sd =  0.3101336)$statistic
  
}

print(mean(stat_norm))
print(mean(stat_gamma))

# We observe a very low KS statistic for both distributions - high p value
# Meaning there is no significant evidence 
# to reject the null that our sample fits these distributions
# Given that normal has the lower test statistic we will take it 

##----------------------------------------------------------------------------------------------------

cv_mean <- quantile(X.bar.star.mean, probs = c(0.025,0.975) )  # Calculate the confidence interval
cv_sd <- quantile(X.bar.star.sd, probs = c(0.025,0.975) )  # Calculate the confidence interval

print(mean(X.bar.star.mean))
print(cv_mean)

print(mean(X.bar.star.sd))
print(cv_sd)
hist(X.bar.star.mean)
hist(X.bar.star.mean)


##------------------------------------------------------------------------------------------------------

nr.sim <- 5000                           # Number of simulations
n <- length(Time_Type2$Time_diff)
ks_statistics <- numeric(nr.sim)         # Vector to store KS statistics

for (i in 1:nr.sim) {                    # Start the simulations
  ## Step 1: Simulate ##
  X <- rnorm(n)  # Draw a single simulated sample
  
  ## Step 2: Apply ##
  ks_statistics[i] <- ks.test(X, "pnorm")$statistic
}

## Step 3: Evaluate and Summarize
print(mean(ks_statistics))



