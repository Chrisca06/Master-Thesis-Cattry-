# ------- Prepare data files --------

# Extract dta dta files and convert into csv
library(foreign)
getwd()
setwd("/Users/Christophe/Desktop/Thesis")
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

# ------- Prepare dataset from CSTATViolatio, Financial Data (WRDS / Compustat) and CSTATSEC data --------

# Prepare Wharton Data / CSTATVIolation Data / CSTATSEC data
getwd()
setwd("/Users/Christophe/Desktop/Thesis")
FinancialData <- read.csv(file="Compustat_Financial_Data_Wharton_2.csv", header=TRUE, sep=",")
CSTATViolation<- read.csv(file="CSTATVIOLATION_copy.csv", header=TRUE, sep=",")
CSTATSEC <- read.csv(file="CSTATSEC.csv", header=TRUE, sep=",")

# Focus data from 2004 - 2008 (2004 included and 2008 excluded due to distress period) / remove the rest

CSTATSEC<-CSTATSEC[!(CSTATSEC$quarter<20039),] 
CSTATSEC<-CSTATSEC[!(CSTATSEC$quarter>20080),] 
CSTATViolation<-CSTATViolation[!(CSTATViolation$Year<2004),]
CSTATViolation<-CSTATViolation[!(CSTATViolation$Year>=2008),]
FinancialData<-FinancialData[!(FinancialData$Year<2004),]
FinancialData<-FinancialData[!(FinancialData$Year>=2008),]

# Prepare new data frame which will combine all the data together
CSTATSEC["Index"] <- NA # new column for the index
CSTATSEC$Index <- seq.int(nrow(CSTATSEC)) # add the indices to the column
FinancialData["Index"] <- NA # new column for the index
FinancialData$Index <- seq.int(nrow(FinancialData)) # add the indices to the column
CSTATViolation["Index"] <- NA # new column for the index
CSTATViolation$Index <- seq.int(nrow(CSTATViolation)) # add the indices to the column

# Names of Variables and Column
NamesColumnFinancialDate<-colnames(FinancialData) # Current Names

# actq = Current Assets
# cheq = Cash and Short Term Investments
# dlcq = Debt in Current Liabilities (Short Term Debt)
# dlttq =  Long Term Debt
# lctq = Current Liabilities
# ltq= Total Liabilities
# oibdpq = Operating Income Before Depreciation
# ppentq = PP&E (Net)
# seqq = Stockholders Equity
# txditcq = Deferred Tax and Investment Tax Credit
# xintq = Interest and Related Expenses
# aqcy = Cash acquisitions
# capxy = Capital Expenditures
# prstkcy = Purchase of Common and Preffered Stocks 
# costat = ??
# dvpspq = Dividends per Share - Pay Date - Quarter 
# dvpsxq = Div per Share - Exdate - Quarter
# mkvaltq = Market Value 

# Filter the data - Remove rows with missing information on the following: 

# atq = Total Assets
# cshoq = Common Shares Outstanding
# saleq = Total Sales
# prccq = Closing Share Price
# datacqtr = Calendar Quarter Date

# To Remove Data where missing information on atq, cshoq, saleq, prccq, datacqtr
FinancialData <- FinancialData[complete.cases(FinancialData$atq, FinancialData$cshoq, FinancialData$saleq, FinancialData$prccq, FinancialData$datacqtr), ]

# -----
# To remove data where missing information (all Variables)
# FinancialData <- FinancialData[complete.cases(FinancialData[,1:35]),] that would remove almost all data

# > sum(is.na(FinancialData$actq))
# [1] 1435
# > sum(is.na(FinancialData$cheq))
# [1] 28
# > sum(is.na(FinancialData$dlcq))
# [1] 1633
# > sum(is.na(FinancialData$dlttq))
# [1] 622
# > sum(is.na(FinancialData$lctq))
# [1] 1391
# > sum(is.na(FinancialData$ltq))
# [1] 69
# > sum(is.na(FinancialData$oibdpq))
# [1] 4436
# > sum(is.na(FinancialData$ppentq))
# [1] 114
# > sum(is.na(FinancialData$seqq))
# [1] 58
# > sum(is.na(FinancialData$txditcq))
# [1] 4600
# > sum(is.na(FinancialData$xintq))
# [1] 12064
# > sum(is.na(FinancialData$aqcy))
# [1] 1996
# > sum(is.na(FinancialData$capxy))
# [1] 493
# > sum(is.na(FinancialData$prstkcy))
# [1] 6903
# > sum(is.na(FinancialData$costat))
# [1] 0
# > sum(is.na(FinancialData$dvpspq))
# [1] 0
# > sum(is.na(FinancialData$dvpsxq))
# [1] 0
# > sum(is.na(FinancialData$mkvaltq))
# [1] 40436

# We need to remove the mkvaltq  (can be computed by multiplying prrcq * cshoq)
# New Financial Data file will not include the mkvaltq and xintq 

FinancialData$mkvaltq <- NULL
FinancialData$xintq <- NULL
FinancialData <- FinancialData[complete.cases(FinancialData[,1:33]),] 
# 60915 obs. of 33 variables 

# remove data with no violation 
CSTATViolation <- CSTATViolation[CSTATViolation$viol == 1,]

#------

# FinancialData doesn't have any missing information on atq, cshoq, saleq, prccq, datacqtr

# Create the new data frame combining all data frames
Z<-CSTATViolation[1,]
Y<-FinancialData[1,]
X <- cbind(Z,Y)
remove(Z)
remove(Y)

# Drop the levels
CSTATViolation[,8]<-droplevels(CSTATViolation[,8])
levels(CSTATViolation[,8])
CSTATViolation[1,8]
FinancialData[,2]<-droplevels(FinancialData[,2])
levels(FinancialData[,2])
FinancialData[1,2]

# Set Same levels as CSTATViolation
# Levels are extracted from levels(CSTATViolation)
FinancialData[,2] <- factor(FinancialData[,2], levels=c(
  "28.02.2005", "28.02.2006", "28.02.2007", "29.02.2004", "30.04.2004", "30.04.2005", "30.04.2006", "30.04.2007",
  "30.06.2004", "30.06.2005", "30.06.2006", "30.06.2007", "30.09.2004", "30.09.2005", "30.09.2006", "30.09.2007",
  "30.11.2004", "30.11.2005", "30.11.2006", "30.11.2007", "31.01.2004", "31.01.2005", "31.01.2006", "31.01.2007",
  "31.03.2004", "31.03.2005", "31.03.2006", "31.03.2007", "31.05.2004", "31.05.2005", "31.05.2006", "31.05.2007",
  "31.07.2004", "31.07.2005", "31.07.2006", "31.07.2007", "31.08.2004", "31.08.2005", "31.08.2006", "31.08.2007",
  "31.10.2004", "31.10.2005", "31.10.2006", "31.10.2007", "31.12.2004", "31.12.2005", "31.12.2006", "31.12.2007"
))

# Test 
CSTATViolation[1,3] == FinancialData[43,1] & CSTATViolation[1,8] == FinancialData[43,2]

# ---------- Loop to match Violation and Financial Data ---------- 

for (i in 1:nrow(CSTATViolation)) {
  for (j in 1:nrow(FinancialData)) {
    if (CSTATViolation[i,3] == FinancialData[j,1] & CSTATViolation[i,8] == FinancialData[j,2]) {
      X[i,] <- cbind(CSTATViolation[i,], FinancialData[j,]) 
      break}
    else if (j==78952) 
    {next}
  } 
}

# ---------- Download all Log Files ---------- 

setwd("/Users/Christophe/Desktop/Thesis/Log")
LogFile <- read.csv(file="LogFile.csv", header=FALSE, sep=",")

# loop to download all Log files
for (i in 1:nrow(LogFile)) {
  CSV <- toString(LogFile[i,5])
  browseURL(CSV)
  Sys.sleep(10)
}

# ---------- CIK and GVKEY ---------- 

UNQ <- data.frame(CSTATSEC)
UNQ <- UNQ[!duplicated(UNQ[,c('gvkey', 'cikSEC')]),]
write.csv(UNQ, file = "GVKEYSEC.csv")


