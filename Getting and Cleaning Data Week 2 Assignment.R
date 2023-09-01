library(httr)
library(RSQLite)
library(sqldf)

#Q1. 
# Register an application with the Github API here https://github.com/settings/applications. Access the API to 
# get information on your instructors repositories (hint: this is the url you want "https://api.github.com/users/jtleek/repos").
# Use this data to find the time that the datasharing repo was created. What time was it created?

#1. Find OAuth settings for github: http://developer.github.com/v3/oauth/
oauth_endpoints("github")
#2. Make your own application
myapp <- oauth_app("github",
                   key = "89ff4b27b292f06eeb87",
                   secret = "e9db617c3cf1bba0f059d22b4d220e3e95eb9c89",
                   redirect_uri = "http://localhost:1410")
#3. Get OAuth credentials
github_token <- oauth1.0_token(oauth_endpoints("github"), myapp)
#4. Use API
req <- GET("https://api.github.com/users/jtleek/repos", config(token = github_token))
stop_for_status(req)
output <- content(req)

#5. Find the place of the "datasharing"
datashare <- which(sapply(output, FUN=function(X) "datasharing" %in% X))

#6. Get the result
output[[datashare]]$created_at

#Q2.
# The sqldf package allows for execution of SQL commands on R data frames. We will use the sqldf package to practice the queries
# we might send with the dbSendQuery command in RMySQL.
# Download the American Community Survey data and load it into an R project called: acs.
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv
# Which of the following commands will select only the data for the probability weights pwgtp1 with ages less than 50?
acs <- read.csv("getdata_data_ss06pid.csv")
sqldf("select pwgtp1 from acs where AGEP < 50")

#Q3.
# Using the same data frame you created in the previous problem, what is the equivalent function to unique(acs$AGEP)?
unique(acs$AGEP)
sqldf("select distinct AGEP from acs")

#Q4.
# How many characters are in the 10th, 20th, 30th, 100th lines of HTML from this page:
# http://biostat.jhsph.edu/~jleek/contact.html
# (Hint: the nchar() function in R may be helpful)
url <- "http://biostat.jhsph.edu/~jleek/contact.html"
biocontact <- readLines(url)
c(nchar(biocontact[10]), nchar(biocontact[20]), nchar(biocontact[30]), nchar(biocontact[100]))

#Q5.
# Read this data set into R and report the sum of the numbers in the fourth of the nine columns.
# https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fwksst8110.for
# Original source of the data: http://ww.cpc.ncep.noaa.gov/data/indices/wksst8110.for
# (Hint this is a fixed width file format)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for"
file <- read.fwf(fileUrl, skip = 4, widths=c(10, 9, 4, 9, 4, 9, 4, 9, 4))
sum(file[,4])
