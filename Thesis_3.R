# -------------------- Open DTA data from Nicolas  ---------------------------

# period investigated 2004 until 2007 (both included), 2008 removed and afterwards as well due to distress period

install.packages("readstata13")
library(readstata13)
setwd("/Users/Christophe/Desktop/Thesis")
dat <- read.dta13("Cattry.dta")
write.csv(dat, file = "Final_Viol.csv")

# -------------------- Cleaning Data  ---------------------------

setwd("/Users/Christophe/Desktop/Thesis")
Violation <- read.csv(file="Final_Viol.csv", header=TRUE, sep=",")
# 80'079 observations / 6141 gvkeys
# U <- as.data.frame(unique(Violation$gvkey, incomparables = FALSE))
# remove(U)

# Filter the data - Remove rows with missing information on the following: 
# atq = Total Assets - OK 
# cshoq = Common Shares Outstanding - OK
# saleq = Total Sales - OK
# prccq = Closing Share Price - OK
# datacqtr = Calendar Quarter Date - OK

# To Remove Data where missing information on atq, cshoq, saleq, prccq, datacqtr
Violation <- Violation[complete.cases(Violation$atq, Violation$cshoq, Violation$saleq, Violation$prccq, Violation$datacqtr), ]
# 80'030 observations / 6141 gvkeys
# U <- as.data.frame(unique(Violation$gvkey, incomparables = FALSE))
# remove(U) 

# Should have the below data (TO BE CHECKED)

# actq = Current Assets - OK
# cheq = Cash and Short Term Investments - OK
# dlcq = Debt in Current Liabilities (Short Term Debt) - OK
# dlttq =  Long Term Debt - OK
# lctq = Current Liabilities - OK
# ltq= Total Liabilities - OK
# oibdpq = Operating Income Before Depreciation - OK
# ppentq = PP&E (Net) - OK 
# seqq = Stockholders Equity - OK 
# txditcq = Deferred Tax and Investment Tax Credit - OK
# xintq = Interest and Related Expenses - OK
# ibq = Income before extraordinary Items - OK

Violnames<-colnames(Violation) # Current Names
Violnames

# aqcy = Cash acquisitions - NO
# capxy = Capital Expenditures - NO
# prstkcy = Purchase of Common and Preffered Stocks - NO
# dvpspq = Dividends per Share - Pay Date - Quarter 
# mkvaltq = Market Value  - TO BE CALCULATED
# splticrm = OK / ?? (Guess it's credit ratings)
# sich = OK / ??
# sic =  Standard Industry Classification - OK 
# cuur0000aa0 = OK / ??$

#----
# remove data with no violation 
# Violation <- Violation[Violation$viol == 1,]
#---

# 4114 observations with 1383 gvkeys for which there is a violation during the period
# U <- as.data.frame(unique(Violation$gvkey, incomparables = FALSE))
# remove(U) 

# -------------------- Add Missing Variables  ---------------------------

# Extract first gvkeys
# Gvkey_unique <- as.data.frame(unique(Violation$gvkey, incomparables = FALSE))
# write.csv(Gvkey_unique, file = "Gvkey_Unique.csv")

# Read Csv of missing variables
setwd("/Users/Christophe/Desktop/Thesis")
Missing_Var <- read.csv(file="Missing_Variables.csv", header=TRUE, sep=",")

# create variable to store the merged file
Dm <- cbind(Violation[1,],Missing_Var[1,])
Dm <- Dm[0,]

# set the same level of factors
Missing_Var[,2] <- factor(Missing_Var[,2], levels = levels(Violation[,3]))

# Test condition before running the loop
which(Missing_Var$gvkey == 1021 & Missing_Var$datadate == "30.09.2006", arr.ind=T) 
Violation[41,2] == Missing_Var[12,1] & Violation[41,3] == Missing_Var[12,2]
# True

# datadate and gvkey to be matched
for (i in 1:nrow(Violation)) {
  pol <- which(Missing_Var$gvkey == Violation[i,2] & Missing_Var$datadate == Violation[i,3], arr.ind=T)   # gives position of matched data
  if(length(pol)==1){ # use length of the position as a condition because if the Merging value is not found in the UNQ data, then it will return 0
    Dm[i,] <- cbind(Violation[i,], Missing_Var[pol,])
  }
  else {next}
}

# Added the following missing Variables: 

# aqcy  = Acquisition
# CAPXY = Capex
# prcraq = Repurchase Price (Average per share)
# prstkcy = Purchase of common and prefered shares
# dvpspq = dividends per share - pay date

Dm <- Dm[complete.cases(Dm$gvkey),]
# Data with the gvkey for which there is at least one violation between 2004 and 2007
# with missing variables

Dm$prstkccy <- NULL
write.csv(Dm, file = "Final_Viol_2.csv")

# remove prstkccy (cash flow related to purchases)

# ------------------------------- Organize Data to Filter for New Violations ONLY ------------------------------- 

setwd("/Users/Christophe/Desktop/Thesis")
Violation <- read.csv(file="Final_Viol_2.csv", header=TRUE, sep=",")
CSTATViolation<- read.csv(file="CSTATVIOLATION_copy.csv", header=TRUE, sep=",")
CSTATViolation<-CSTATViolation[!(CSTATViolation$Year<2003),]
CSTATViolation<-CSTATViolation[!(CSTATViolation$Year>=2004),]

# Remove NAs (since we merged Violation Data with Missing Variables for which the gvkey had at least one viol between
# 2004 and 2007 included.)
Violation <- Violation[complete.cases(Violation$gvkey),]

# remove duplicate
Violation<-Violation[!(Violation$Year==2003),]

# merge two data sets of violations data only with date
CSTATViolation <- CSTATViolation[,c(3,4,8)]
Violation <- Violation[,c(3:5)] # Main File

NewViol <- CSTATViolation
NewViol <- NewViol[0,]

Violation_gvkey <- as.data.frame(Violation[,1])
Violation_gvkey <- unique(Violation_gvkey, incomparables = FALSE)
colnames(Violation_gvkey) <- c("Gvkey")

for (i in 1:nrow(Violation_gvkey)) {
  pol <- which(CSTATViolation$gvkey == Violation_gvkey[i,1], arr.ind=T)    # gives position of matched data
  if(length(pol) != 0){ # use length of the position as a condition because if the Merging value is not found in the UNQ data, then it will return 0
    NewViol <- rbind(NewViol, CSTATViolation[c(pol),])
  }
  else {next}
}

# combine violations data from 2003 until 2007 included both
remove(Violation_gvkey)
colnames(NewViol) <- c("gvkey", "viol","datadate")
CheckViol <- rbind(NewViol, Violation)

# check
which(CheckViol$gvkey == 1021, arr.ind=T) 
length(which(CheckViol$gvkey == 1021, arr.ind=T) )
CheckViol[which(CheckViol$gvkey == 1034, arr.ind=T), ]

CheckViol$datadate <- as.Date(CheckViol$datadate, format="%d.%m.%Y")
class(CheckViol$datadate)

CheckViol <- CheckViol[order(CheckViol$gvkey,as.Date(CheckViol$datadate, format="%d/%m/%Y")),]

write.csv(CheckViol, file = "Check_Viol.csv")

# ----------------------- FILTER FOR NEW VIOLATIONS ONLY ----------------------- 

setwd("/Users/Christophe/Desktop/Thesis")
CheckViol <- read.csv(file="Check_Viol.csv", header=TRUE, sep=",")
Datacqtr <- read.csv(file="Datacqtr.csv", header=TRUE, sep=",")

# extract gvkey from Check_Viol Data and use it to get appropriate Datacqtr for each one of them)
# U <- as.data.frame(unique(CheckViol$gvkey, incomparables = FALSE))
# write.csv(U, file = "GG.csv")

# Keep only when there is a violation

Dm <- cbind(CheckViol[1,],Datacqtr[1,])
Dm <- Dm[0,]

Datacqtr[,2] <- factor(Datacqtr[,2], levels = levels(CheckViol$datadate))
CheckViol<- CheckViol[CheckViol$viol == 1,]
Datacqtr <- Datacqtr[complete.cases(Datacqtr$datadate),]
which(Datacqtr$gvkey == CheckViol[1,1] & Datacqtr$datadate == CheckViol[1,3], arr.ind=T)

for (i in 1:nrow(CheckViol)) {
  pol <- which(Datacqtr$gvkey == CheckViol[i,1] & Datacqtr$datadate == CheckViol[i,3], arr.ind=T)   # gives position of matched data
  if(length(pol)==1){ # use length of the position as a condition because if the Merging value is not found in the UNQ data, then it will return 0
    Dm[i,] <- cbind(CheckViol[i,], Datacqtr[pol,])
  }
  else {next}
}
sum(is.na(Dm$gvkey))
# 1 NA - gvkey: 230796
which(is.na(Dm$gvkey))
# located at 4782 row
which(CheckViol$gvkey == 230796)
Dm[4782,1] <- CheckViol[4782,1]
Dm[4782,2] <- CheckViol[4782,2]
Dm[4782,3] <- CheckViol[4782,3]
Dm[4782,4] <- CheckViol[4782,1]
Dm[4782,5] <- Dm[4782,3]
Dm[4782,6] <- c("2003Q4")
Dm[4782,7] <- c("2003Q4")
# Fixed NA
# X <- Datacqtr[which(Datacqtr$gvkey==230796),] (proof that 2003Q4 for the dates on the missing gvkey)
# write.csv(Dm, file = "Check_Viol_2.csv") write hte file 

CheckViol<- read.csv(file="Check_Viol_2.csv", header=TRUE, sep=",")

# -------------------- Check Violation on previous four quarters --------------------

setwd("/Users/Christophe/Desktop/Thesis")
CheckViol<- read.csv(file="Check_Viol_2.csv", header=TRUE, sep=",")
# create index
CheckViol["Index"] <- NA # new column for the index
CheckViol$Index <- seq.int(nrow(CheckViol)) # add the indices to the column

Function_Quarters <- function(Datacqtr){
  if(Datacqtr=="2004Q1"){
    return(c("2003Q1","2003Q2","2003Q3", "2003Q4"))
  }
  else if (Datacqtr=="2004Q2"){
    return(c("2003Q2","2003Q3","2003Q4", "2004Q1"))
  }
  else if (Datacqtr=="2004Q3"){
    return(c("2003Q3","2003Q4","2004Q1", "2004Q2"))
  }
  else if (Datacqtr=="2004Q4"){
    return(c("2003Q4","2004Q1","2004Q2", "2004Q3"))
  }
  else if (Datacqtr=="2005Q1"){
    return(c("2004Q1","2004Q2","2004Q3", "2004Q4"))
  }
  else if (Datacqtr=="2005Q2"){
    return(c("2004Q2","2004Q3","2004Q4", "2005Q1"))
  }
  else if (Datacqtr=="2005Q3"){
    return(c("2004Q3","2004Q4","2005Q1", "2005Q2"))
  }
  else if (Datacqtr=="2005Q4"){
    return(c("2004Q4","2005Q1","2005Q2", "2005Q3"))
  }
  else if (Datacqtr=="2006Q1"){
    return(c("2005Q1","2005Q2","2005Q3", "2005Q4"))
  }
  else if (Datacqtr=="2006Q2"){
    return(c("2005Q2","2005Q3","2005Q4", "2006Q1"))
  }
  else if (Datacqtr=="2006Q3"){
    return(c("2005Q3","2005Q4","2006Q1", "2006Q2")) 
  }
  else if (Datacqtr=="2006Q4"){
    return(c("2005Q4","2006Q1","2006Q2", "2006Q3")) 
  }
  else if (Datacqtr=="2007Q1"){
    return(c("2006Q1","2006Q2","2006Q3", "2006Q4"))
  }
  else if (Datacqtr=="2007Q2"){
    return(c("2006Q2","2006Q3","2006Q4", "2007Q1"))
  }
  else if (Datacqtr=="2007Q3"){
    return(c("2006Q3","2006Q4","2007Q1", "2007Q2"))
  }
  else if (Datacqtr=="2007Q4"){
    return(c("2006Q4","2007Q1","2007Q2", "2007Q3"))
  }
}

# get gvkey to be checked
Gvkey <- as.data.frame(unique(CheckViol$gvkey, incomparables = FALSE))
colnames(Gvkey) <- c("Gvkey")
z <- 1 # Variable for the loop
Stock <- CheckViol[1,] # variable to stock only gvkey for which there is no violations in previous four quarters
Stock <- Stock[0,]

# correct

for (i in 1:nrow(Gvkey)) {
  Checking <- CheckViol[which(CheckViol$gvkey==Gvkey[i,]),]
  for (j in 1:nrow(Checking)) {
    if(length(Function_Quarters(toString(Checking[j,7])))==0) 
    {next}
    else {
      Quarters <- Function_Quarters(Checking[j,7])
      if (length(which(Checking$datacqtr==Quarters[1])) == 0 & length(which(Checking$datacqtr==Quarters[2])) == 0 &  length(which(Checking$datacqtr==Quarters[3])) == 0 & length(which(Checking$datacqtr==Quarters[4])) == 0) {
        Stock[z,] <- Checking[j,]
        z = z+1
        remove(Quarters)}
      else 
      {remove(Quarters)}
    }  
  }  
  remove(Checking) 
}

# number of violations with no prior 4 quarters violations
write.csv(Stock, file = "Only_Viol.csv")

# -------------------- Next Step  ---------------------------

setwd("/Users/Christophe/Desktop/Thesis")
Viol_Only <- read.csv(file="Only_Viol.csv", header=TRUE, sep=",")
# 1116 observations (1116 new violations) and 1025 companies (gvkey)
# U <- as.data.frame(unique(Viol_Only$gvkey, incomparables = FALSE))

# Merge New Viol data with
Violation <- read.csv(file="Final_Viol.csv", header=TRUE, sep=",")
Violation <- Violation[Violation$viol == 1,]

New_Violation <- Violation
New_Violation <- New_Violation[0,]

# give the factors levels
Violation$datadate <- factor(Violation$datadate, levels = levels(Viol_Only[,5]))

for (i in 1:nrow(Viol_Only)){
  New_Violation[i,] <- Violation[which(Violation$gvkey==Viol_Only[i,3] & Violation$datadate==Viol_Only[i,5] ),]
}

sum(is.na(New_Violation$gvkey))
# 0 NA 

# New Data that has the following characteristics: 
# "New Violations" for firms with no violations in the previous quarters
# Financial Data
# CSTATSEC & CSTATViolation

write.csv(New_Violation, file="Violation_Data.csv")

# -------------------- Calculate the NAs  ---------------------------

NAs <- Violation
NAs <- NAs[0,] 
for (i in 1:ncol(Violation)) {
  NAs[1,i] <- sum(is.na(Violation[,i]))
}
NAs <- NAs[, colSums(is.na(NAs)) != nrow(NAs)]



