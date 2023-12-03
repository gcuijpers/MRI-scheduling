library(readr)
library(moments)

## importing data 
df <- read_csv("C:/Maastircht University/Master/Year 1/Computational Research Skills/ScanRecords.csv")

## splitting type 1 and type 2 patiens 
df_type1 <- df[df$PatientType == 'Type 1',]
df_type2 <- df[df$PatientType == 'Type 2',]
# ---------------------------------------------------------------------------------------------------------------------------------------
  
## Time of Type 1 patients is normally distributed (known information)
## Mean and sd can easily be calculated the following 
Time1_mean <- mean(df_type1$Time)
Time1_sd <- sd(df_type1$Time)
h1 <- hist(df_type1$Time)
##two sided confidence interval for Type 1 time with a=5% is [0.4233055, 0.4427022]  C ∗(X ) =[ X n − c∗ α/2,B Sn/√n, X n − c∗ 1−α/2,B Sn/√n]


# ----------------------------------------------------------------------------------------------------------------------------------------------------
##Arrival times of Type 1 patients is poisson distributed
## We expect exponential distribution in the difference between two arrival times
  
df_type1$Date <- as.Date(df_type1$Date, format= "%Y-%m-%d")
time_diff <- c(0, diff(df_type1$Time))
Time_Type1 <- data.frame(Date= df_type1$Date, time = time_diff) ## constructing time diff of all dates
Time_Type1 <- Time_Type1[Time_Type1$time != 0,] ## delete weekends.
Time_Type1 <- Time_Type1[Time_Type1$time > 0,] ## Drop the difference between two dates
T1_mean <- mean(Time_Type1$time)
T1_sd<- sd(Time_Type1$time)
print(T1_mean)
print(T1_sd)

# Calculate the range of the data
data_range <- range(Time_Type1$time)

# Calculate the minimum and maximum values for breaks
breaks_min <- floor(data_range[1] / 0.05) * 0.05
breaks_max <- ceiling(data_range[2] / 0.05) * 0.05

# Create a histogram with bins of width 0.05
h2 <- hist(Time_Type1$time, breaks = seq(breaks_min, breaks_max, by = 0.05), col = "lightblue", main = "Histogram", xlab = "Time Difference", ylab = "Frequency")

## The histogram looks exponential - what we expected 

# ------------------------------------------------------------------------------------------------------------------------------------------------------
##Arrival times of Type 1 patients is poisson distributed
## We expect exponential distribution in the difference between two arrival times
h3 <- hist(df_type2$Time)

##Arrival times of Type 2 patiens is poisson distributed. Labda is the average patients arrival time

df_type2$Date <- as.Date(df_type2$Date, format= "%Y-%m-%d")
time_diff <- c(0, diff(df_type2$Time))
Time_Type2 <- data.frame(Date= df_type2$Date, time = time_diff) ## constructing time diff of all dates
Time_Type2 <- Time_Type2[Time_Type2$time != 0,] ## delete weekends.
Time_Type2 <- Time_Type2[Time_Type2$time > 0,] ## Drop the difference between two dates
T2_mean <- mean(Time_Type2$time)
T2_sd<- sd(Time_Type2$time)
print(T2_mean)
print(T2_sd)

# Calculate the range of the data
data_range <- range(Time_Type2$time)

# Calculate the minimum and maximum values for breaks
breaks_min <- floor(data_range[1] / 0.05) * 0.05
breaks_max <- ceiling(data_range[2] / 0.05) * 0.05

# Create a histogram with bins of width 0.05
h4 <- hist(Time_Type2$time, breaks = seq(breaks_min, breaks_max, by = 0.05), col = "orange", main = "Histogram", xlab = "Time Difference", ylab = "Frequency")

## The histogram looks normal with a slight skew to the left or gamma? - different from the type 1
# ------------------------------------------------------------------------------------------------------------------------------------------------------

skewness_dur2 <- skewness(Time_Type2$time)
kurtosis_dur2 <- kurtosis(Time_Type2$time)
jarque.test(Time_Type2$time)
ks.test(Time_Type2$time, "pnorm")
ks.test(Time_Type2$time, df_type2$Time)


  