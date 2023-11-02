library(readr)
## importing data 
df <- read_csv("Desktop/computational research skills/MRI-scheduling/ScanRecords.csv")

## splitting type 1 and type 2 patiens 
df_type1 <- df[df$PatientType == 'Type 1',]
df_type2 <- df[df$PatientType == 'Type 2',]

