library(readr)
library(moments)

## importing data 
df <- read.csv("~/Desktop/computational research skills/MRI-scheduling/ScanRecords.csv")

## splitting type 1 and type 2 patiens 
df_type1 <- df[df$PatientType == 'Type 1',]
df_type2 <- df[df$PatientType == 'Type 2',]
#---------------------------------------------------------------------------------------------------------------------------------------
  
## Duration op Type 1 patients is normally distributed (known information)
## Mean and sd can easily be calculated the following 
  
Dur1_mean <- mean(df_type1$Duration)
Dur1_sd <- sd(df_type1$Duration)
h1 <- hist(df_type2$Duration)
##two sided confidence interval for Type 1 duration with a=5% is [0.4233055, 0.4427022]  C ∗(X ) =[ X n − c∗ α/2,B Sn/√n, X n − c∗ 1−α/2,B Sn/√n]


#----------------------------------------------------------------------------------------------------------------------------------------------------
##Number of Type 1 patiens per day is poisson distributed. Labda is the average patients arraving each day
  
df_type1$Date <- as.Date(df_type1$Date, format= "%Y-%m-%d")
tab1 <- table(cut(df_type1$Date, 'day'))
Frequencies_Type1 <- data.frame(Date=format(as.Date(names(tab1)), '%Y-%m-%d'), frequency = as.vector(tab1)) ## constructing frequency of all dates
Frequencies_Type1 <- Frequencies_Type1[Frequencies_Type1$frequency != 0,] ## delete weekends.
labda <- mean(Frequencies_Type1$frequency)
F1_sd<- sd(Frequencies_Type1$frequency)
print(labda)





------------------------------------------------------------------------------------------------------------------------------------------------------
## Distribution of duration of Type 2 is unknown
## Try normal distribution or gamma  
h2 <- hist(df_type2$Time)
skewness_dur2 <- skewness(df_type2$Duration)
kurtosis_dur2 <- kurtosis(df_type2$Duration)
jarque.test(df_type2$Duration)
ks.test(df_type1$Duration, "pnorm")
ks.test(df_type1$Duration, df_type2$Duration)

## we use the non-parametric bootstrap to find the meand and sd. For the mean we have condidence interval [0.64696,0.69284] and for the sd is 0.1862451. 
#looking at the Cullen and frey graph, we try out a normal and gamma distribution. 
shape = 0.6702939^2/0.1862451^2
scale = 0.1862451^2/0.6702939

##Chi-square uitproberen!!!

#-----------------------------------------------------------------------------------------------------------------------------------------------------
## Distribution of number of Type 2 patients every day is also unknown. 
## First maybe try poisson (since Type 1 is also poisson)

df_type2$Date <- as.Date(df_type2$Date, format= "%Y-%m-%d")
tab2 <- table(cut(df_type2$Date, 'day'))
Frequencies_Type2 <- data.frame(Date=format(as.Date(names(tab2)), '%Y-%m-%d'), frequency = as.vector(tab2)) ## constructing frequency of all dates
Frequencies_Type2 <- Frequencies_Type2[Frequencies_Type2$frequency != 0,] ## delete weekends.
F2_mean <- mean(Frequencies_Type2$frequency)
F2_sd <- sd(Frequencies_Type2$frequency)



## Since the variance (1.5217) is a lot lower than the mean (10.39) it is probably not poisson distributed
hist(Frequencies_Type2$frequency)
## looking at the histogram it is probably geometric distributed. 
  
  