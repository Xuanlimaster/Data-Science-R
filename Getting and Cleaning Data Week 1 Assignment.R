#Q1. 
# The American Community Survey distributes downloadable data about United States communities. Download the 2006
# microdata survey about housing for the state of Idaho using download.file() from here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv
# and load the data into R. The code book, describing the variable names is here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf
# How many properties are worth $1,000,000 or more?

data <- read.csv("getdata_data_ss06hid.csv")
sum(data[,"VAL"]>=24,na.rm = TRUE)

#Q2. 
# Use the data you loaded from Question 1. Consider the variable FES in the code book. 
# Which of the "tidy data" principles does this variable violate?

# Answer: Tidy data has one variable per column.

#Q3.
# Download the Excel spreadsheet on Natural Gas Aquisition Program here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx
# Read rows 18-23 and columns 7-15 into R and assign the result to a variable called: dat
# What is the value of: sum(dat$Zip*dat$Ext,na.rm=T)?
# (Original data source: http://catalog.data.gov/dataset/natural-gas-acquisition-program)

library(xlsx)
dat <- read.xlsx("getdata_data_DATA.gov_NGAP.xlsx", sheetIndex = 1, rowIndex = 18:23, colIndex = 7:15)
sum(dat$Zip*dat$Ext,na.rm=T)

#Q4. 
# Read the XML data on Baltimore restaurants from here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml
# How many restaurants have zipcode 21231?

library(XML)
datXML <- xmlTreeParse("getdata_data_restaurants.xml", useInternalNodes = TRUE)
rootNode <- xmlRoot(datXML)
sum(xpathSApply(rootNode, "//zipcode",xmlValue)==21231)

#Q5.
# The American Community Survey distributes downloadable data about United States communities. Download the 2006
# microdata survey about housing for the state of Idaho using download.file() from here:
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv
# using the fread() command load the data into an R object: DT. The following are ways to calculate the average 
# value of the variable: pwgtp15 broken down by sex. Using the data.table package, which will deliver the fastest
# user time?

library(data.table)
DT <- fread("getdata_data_ss06pid.csv")
system.time(DT[,mean(pwgtp15),by=SEX])
