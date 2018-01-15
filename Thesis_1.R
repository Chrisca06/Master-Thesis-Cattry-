# Extract dta dta files and convert into csv
library(foreign)
getwd()
setwd("/Users/Christophe/Desktop/Thesis")
getwd()
Y <- write.table(read.dta(file.choose()), file="output.csv", quote = FALSE, sep = ",")

# extract unique gvkey to get wharton data
library(foreign)
getwd()
setwd("/Users/Christophe/Desktop/Thesis")
Ccodes <- read.csv(file="Company_Codes_copy.csv", header=TRUE, sep=",")
Ccodes$viol<-NULL
Ccodes$datadate<-NULL
Ccodes_unique<-unique(Ccodes, incomparables = FALSE)
write.csv(Ccodes_unique, file = "Company_Codes_Unique.csv")

# -------------

# Prepare Wharton Data / CSTATVIolation Data / CSTATSEC data
getwd()
setwd("/Users/Christophe/Desktop/Thesis")
FinancialData <- read.csv(file="Compustat_Financial_Data_Wharton.csv", header=TRUE, sep=",") # extracted from Wharton database
FinancialData_2 <- read.csv(file="Compustat_Financial_Data_Wharton_2.csv", header=TRUE, sep=",")
CSTATViolation<- read.csv(file="CSTATVIOLATION_copy.csv", header=TRUE, sep=",")
CSTATSEC <- read.csv(file="CSTATSEC.csv", header=TRUE, sep=",")

# Focus data from 2004 - 2008 / remove the rest

CSTATSEC<-CSTATSEC[!(CSTATSEC$quarter<20039),] 
CSTATSEC<-CSTATSEC[!(CSTATSEC$quarter>20080),] 
CSTATViolation<-CSTATViolation[!(CSTATViolation$Year<2004),]
CSTATViolation<-CSTATViolation[!(CSTATViolation$Year>=2008),]
FinancialData<-FinancialData[!(FinancialData$Year<2004),]
FinancialData<-FinancialData[!(FinancialData$Year>=2008),]
FinancialData_2<-FinancialData_2[!(FinancialData_2$Year<2004),]
FinancialData_2<-FinancialData_2[!(FinancialData_2$Year>=2008),]


# Prepare new data frame which will combine all the data together
CSTATSEC["Index"] <- NA # new column for the index
CSTATSEC$Index <- seq.int(nrow(CSTATSEC)) # add the indices to the column
FinancialData["Index"] <- NA # new column for the index
FinancialData$Index <- seq.int(nrow(FinancialData)) # add the indices to the column
FinancialData_2["Index"] <- NA # new column for the index
FinancialData_2$Index <- seq.int(nrow(FinancialData_2)) # add the indices to the column
CSTATViolation["Index"] <- NA # new column for the index
CSTATViolation$Index <- seq.int(nrow(CSTATViolation)) # add the indices to the column

# Create the new data frame combining all data frames
Z<-CSTATViolation[1,]
Y<-FinancialData_2[1,]
X <- cbind(Z,Y)
remove(Z)
remove(Y)

# Drop the levels
CSTATViolation[,8]<-droplevels(CSTATViolation[,8])
levels(CSTATViolation[,8])
CSTATViolation[1,8]
FinancialData_2[,2]<-droplevels(FinancialData_2[,2])
levels(FinancialData_2[,2])
FinancialData_2[1,2]

# Set Same levels as CSTATViolation
# Levels are extracted from levels(CSTATViolation)
FinancialData_2[,2] <- factor(FinancialData_2[,2], levels=c(
  "28.02.2005", "28.02.2006", "28.02.2007", "29.02.2004", "30.04.2004", "30.04.2005", "30.04.2006", "30.04.2007",
   "30.06.2004", "30.06.2005", "30.06.2006", "30.06.2007", "30.09.2004", "30.09.2005", "30.09.2006", "30.09.2007",
  "30.11.2004", "30.11.2005", "30.11.2006", "30.11.2007", "31.01.2004", "31.01.2005", "31.01.2006", "31.01.2007",
   "31.03.2004", "31.03.2005", "31.03.2006", "31.03.2007", "31.05.2004", "31.05.2005", "31.05.2006", "31.05.2007",
  "31.07.2004", "31.07.2005", "31.07.2006", "31.07.2007", "31.08.2004", "31.08.2005", "31.08.2006", "31.08.2007",
  "31.10.2004", "31.10.2005", "31.10.2006", "31.10.2007", "31.12.2004", "31.12.2005", "31.12.2006", "31.12.2007"
))

# Test 
CSTATViolation[1,3] == FinancialData_2[5,1] & CSTATViolation[1,8] == FinancialData_2[5,2]

#Loop to combine data frames
# Y<-data.frame(matrix(0,1)) # to track all missing values

# Remove all NA in Compustat Data
Final <- apply(FinancialData, 1, function(x){any(is.na(x))})
FinancialData<- FinancialData[!Final,]

for (i in 1:nrow(CSTATViolation)) {
  for (j in 1:nrow(FinancialData)) {
    if (CSTATViolation[i,3] == FinancialData[j,1] & CSTATViolation[i,8] == FinancialData[j,2]) {
      X[i,] <- cbind(CSTATViolation[i,], FinancialData[j,]) 
      break}
        else if (j==78300) 
        {next}
  } 
}



# Structure 
X <- data.frame(matrix(NA, ncol = (ncol(CSTATSEC)+ncol(FinancialData)), nrow = nrow(CSTATSEC)))
namesCSTATSEC <- names(CSTATSEC)
namesFinancialData <- names(FinancialData)
names(X) <- c(namesCSTATSEC, namesFinancialData )




