library(readr)
library(moments)

## importing data 
df <- read_csv("Desktop/computational research skills/MRI-scheduling/ScanRecords.csv")

## splitting type 1 and type 2 patiens 
df_type1 <- df[df$PatientType == 'Type 1',]
df_type2 <- df[df$PatientType == 'Type 2',]

## Duration op Type 1 patients is normally distributed (known information)
## Mean and sd can easily be calculated the following 
Dur1_mean <- mean(df_type1$Duration)
Dur1_sd <- sd(df_type1$Duration)
h1 <- hist(df_type1$Duration)

## Distribution of duration of Type 2 is unknown
## Try normal distribution 
h2 <- hist(df_type2$Duration)
skewness_dur2 <- skewness(df_type2$Duration)
kurtosis_dur2 <- kurtosis(df_type2$Duration)
jarque.test(df_type2$Duration)
ks.test(df_type1$Duration, "pnorm")
ks.test(df_type1$Duration, df_type2$Duration)


