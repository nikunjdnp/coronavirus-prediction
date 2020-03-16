##################################################
### PROG 01                                    ### 
##################################################

##################################################
# Written by Niikunj Prajapati                   #
##################################################
### Basic Set Up                               ###
##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Set work directory
setwd("E:/R_prog/R_Workdir")

options(scipen=9)

##################################################
### Install Libraries                           ##
##################################################
#If the library is not already downloaded, download it

if(!require(dplyr)){install.packages("dplyr")}
library("dplyr")

if(!require(lattice)){install.packages("lattice")}
library("lattice")

if(!require(prophet)){install.packages("prophet")}
library("prophet")

##################################################
### Read in Data                                ##
##################################################
##################################################
### Read data and do preliminary data checks    ##
##################################################

# Read "comma separated value" files (".csv")
# Coronavirus data sheet
coronaData <- read.csv("covid_19_data.csv", header = TRUE, sep = ",")

coronaData[0:15,]     #Prints data 0 to 15 to make sure it looks correct

ls(coronaData)      #Lists all objects for Coronavirus Data
names(coronaData)   #List the variables for Coronavirus Data
str(coronaData)     #List structure of Coronavirus Data

#Rename columns
colnames(coronaData)[4] <- "State" 
colnames(coronaData)[5] <- "Country"
#names(coronaData) <- c("ObservationDate","State","Country", "Confirmed", "Deaths", "Recovered")

#Select only obsetvationDate, country, confirmed, deaths, recovered columns
coronaData <- coronaData[,c(2,4,6:8)]

# display column names
colnames(coronaData)

#Year - Convert to a proper date format
coronaData$ObservationDate <- as.Date(coronaData$ObservationDate,format="%m/%d/%Y")
head(coronaData,50)

##################################################
### Data transformation                        ###
##################################################

# group by date
coronaData <- coronaData %>%
  select(ObservationDate, State, Confirmed, Deaths, Recovered )  %>% 
  group_by(ObservationDate) %>%
  summarise(Confirmed = sum(Confirmed,na.rm=TRUE), Deaths = sum(Deaths,na.rm=TRUE), 
            Recovered = sum(Recovered,na.rm=TRUE))

#barchart of current worldwide coronavirus data.
barchart(ObservationDate ~ Confirmed + Deaths + Recovered,
         data=coronaData, beside=TRUE, main="Coronavirus spread worldwide breakdown by dates",
         xlab="Total#", ylab="Date", 
         auto.key=list(space='bottom'))

# get confirmed coronavirus cases data
coronaData_confirmed <- coronaData %>%
  select(ObservationDate, Confirmed ) 

# get recover coronavirus cases data 
coronaData_recover <- coronaData %>%
  select(ObservationDate, Recovered )

# get death coronavirus cases data 
coronaData_death <- coronaData %>%
  select(ObservationDate, Deaths )

##################################################
### Transforming data for forecasting          ###
##################################################

names(coronaData_confirmed) <- c("ds", "y")
names(coronaData_death) <- c("ds", "y")
names(coronaData_recover) <- c("ds", "y")

##################################################
### Forecasting Confirmed Cases Worldwide      ###  
### with Prophet (Baseline)                    ###
##################################################

# for coronaData_confirmed
m = prophet(coronaData_confirmed)
future = make_future_dataframe(m,periods=14)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m,forecast, xlabel =  "Date", ylabel = "Count #", )

# for coronaData_death
m = prophet(coronaData_death)
future = make_future_dataframe(m,periods=14)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m,forecast, xlabel =  "Date", ylabel = "Count #", )

# for coronaData_recover
m = prophet(coronaData_recover)
future = make_future_dataframe(m,periods=14)
forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m,forecast, xlabel =  "Date", ylabel = "Count #", )
