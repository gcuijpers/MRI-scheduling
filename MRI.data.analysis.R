library(readr)
library(moments)

## importing data 
df <- read_csv("~/Desktop/computational research skills/MRI-scheduling/ScanRecords.csv")

## splitting type 1 and type 2 patiens 
df_type1 <- df[df$PatientType == 'Type 1',]
df_type2 <- df[df$PatientType == 'Type 2',]
---------------------------------------------------------------------------------------------------------------------------------------
## Duration op Type 1 patients is normally distributed (known information)
## Mean and sd can easily be calculated the following 
Dur1_mean <- mean(df_type1$Duration)
Dur1_sd <- sd(df_type1$Duration)
h1 <- hist(df_type2$Duration)
##two sided confidence interval for Type 1 duration with a=5% is [0.4233055, 0.4427022]  C ∗(X ) =[ X n − c∗ α/2,B Sn/√n, X n − c∗ 1−α/2,B Sn/√n]
----------------------------------------------------------------------------------------------------------------------------------------------------
##Number of Type 1 patiens per day is poisson distributed. Labda is the average patients arraving each day
  
df_type1$Date <- as.Date(df_type1$Date, format= "%Y-%m-%d")
tab <- table(cut(df_type1$Date, 'day'))
Frequencies_Type1 <- data.frame(Date=format(as.Date(names(tab)), '%Y-%m-%d'), frequency = as.vector(tab)) ## constructing frequency of all dates
Frequencies_Type1 <- Frequencies_Type1[Frequencies_Type1$frequency != 0,] ## delete weekends.
labda <- sum(Frequencies_Type1$frequency)/length(Frequencies_Type1$frequency)
print(labda)





------------------------------------------------------------------------------------------------------------------------------------------------------
## Distribution of duration of Type 2 is unknown
## Try normal distribution 
h2 <- hist(df_type2$Time)
skewness_dur2 <- skewness(df_type2$Duration)
kurtosis_dur2 <- kurtosis(df_type2$Duration)
jarque.test(df_type2$Duration)
ks.test(df_type1$Duration, "pnorm")
ks.test(df_type1$Duration, df_type2$Duration)


  