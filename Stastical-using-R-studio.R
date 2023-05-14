# import the data set
library(readr)
PODS_CSV_DATASET <- read.csv("PODST2.csv")
View(PODS_CSV_DATASET)
#review the first and last six rows of each column.
head(PODS_CSV_DATASET)
tail(PODS_CSV_DATASET)
#we examine the number of rows and columns, and the columns name
nrow(PODS_CSV_DATASET)
ncol(PODS_CSV_DATASET)
colnames(PODS_CSV_DATASET)

# rename the columns name
library(plyr)
new_data <- plyr:: rename(PODS_CSV_DATASET,c("CountryName" = "Country_Name","Country.Code"="Country_Code","Time"="Year",
                                             "Time.Code"="Time_Code","CO2.emissions..kt." = "CO2_Emis",
                                             "Access.to.clean.fuels.and.technologies.for.cooking....of.population." = "ACFAT",
                                             "Forest.area..sq..km" = "For_Area","International.tourism..number.of.arrivals." = "Inter_Tour",
                                             "Agriculture..forestry..and.fishing..value.added..annual...growth." = "Agri_Fish",
                                             "Total.greenhouse.gas.emissions..kt.of.CO2.equivalent." = "Green_Gas",
                                             "Agricultural.methane.emissions..thousand.metric.tons.of.CO2.equivalent." = "Agri_methane",
                                             "Agricultural.nitrous.oxide.emissions..thousand.metric.tons.of.CO2.equivalent." = "Agri_nit_oxide",
                                             "Total.natural.resources.rents....of.GDP." = "Natur_reso"))
colnames(new_data)
#check any missing values.
sum(is.na(new_data))
sapply(new_data, function(x) sum(is.na(x)))
which(is.na(new_data$For_Area))
# check for outliers in any columns.
boxplot(new_data$CO2_Emis,new_data$ACFAT,new_data$For_Area,new_data$Inter_Tour,
        new_data$Agri_Fish,new_data$Green_Gas,new_data$Agri_methane,new_data$Agri_nit_oxide,new_data$Natur_reso,
        main = 'Outlier in data',col="red",border = "black",outpch = 25,outbg = "green",whiskcol = "blue",whisklty = 10)
# Determine the number of outliers.
boxplot.stats(new_data$CO2_Emis)$out 
boxplot.stats(new_data$ACFAT)$out
boxplot.stats(new_data$Inter_Tour)$out
boxplot.stats(new_data$Agri_Fish)$out
boxplot.stats(new_data$Natur_reso)$out

#4.1
# take a look at the columns 'Forest_Area' has missing values
boxplot.stats(new_data$For_Area)$out
# We impute our missing values with the mean because this column does not have an outlier.
# On the other hand, we could not drop them because it would affect the data.
mean(new_data$For_Area)
mean_PODs<- mean(new_data$For_Area,na.rm = TRUE)
mean_PODs
new_data[is.na(new_data$For_Area),"For_Area"]<- mean_PODs
new_data$For_Area
sum(is.na(new_data))
# we examine the data using the descriptive statistics data analysis
summary(new_data)
# we divide our country into western and eastern Europe
library('dplyr')
Eastern_Europe<-new_data[new_data$`Country_Name`== 'Romania'|
                           new_data$`Country_Name`== 'Serbia'|
                           new_data$`Country_Name`== 'Bosnia and Herzegovina'|
                           new_data$`Country_Name`== 'Georgia'|
                           new_data$`Country_Name`== 'Montenegro'|
                           new_data$`Country_Name`== 'Moldova'|
                           new_data$`Country_Name`== 'Belarus'|
                           new_data$`Country_Name`== 'Finland'|
                           new_data$`Country_Name`== 'Ukraine',]
Western_Europe<-new_data[new_data$`Country_Name`== 'United Kingdom'|
                           new_data$`Country_Name`== 'France'|
                           new_data$`Country_Name`== 'Italy'|
                           new_data$`Country_Name`== 'Switzerland'|
                           new_data$`Country_Name`== 'Ireland'|
                           new_data$`Country_Name`== 'Germany'|
                           new_data$`Country_Name`== 'Spain',]

# we delete categorical columns such as ( country_name, country_code,Time_code and Year)
col_remove <- c('Country_Name','Country_Code','Time_Code','Year')
Eastern_Europe <- Eastern_Europe%>%
  select(- one_of(col_remove))
# determine Descriptive statistics for Eastern Europe
library(dplyr)
library(moments)
sapply(Eastern_Europe, mean, na.rm = TRUE)
sapply(Eastern_Europe, median)
sapply(Eastern_Europe, sd, na.rm = TRUE)
mode(Eastern_Europe)
mode(Eastern_Europe$CO2_Emis)
skewness(Eastern_Europe)
kurtosis(Eastern_Europe)
#we delete categorical columns such as ( country_name, country_code,Time_code,Year)
col_remove1 <- c('Country_Name','Country_Code','Time_Code','Year')
Western_Europe <- Western_Europe%>%
  select(- one_of(col_remove1))
# determine Descriptive statistics for Western Europe
sapply(Western_Europe, mean, na.rm = TRUE)
sapply(Western_Europe, median)
sapply(Western_Europe, sd, na.rm = TRUE)
mode(Western_Europe)
mode(Western_Europe$CO2_Emis)
skewness(Western_Europe)
kurtosis(Western_Europe)

#4.2
#we round the sectors to two decimals.
round(cor(Eastern_Europe), digits = 2)
# correlation to eastern Europe
library('corrplot')
col1 <- colorRampPalette(c("red","pink","purple","cyan","green","orange","yellow","blue"))
corrplot(cor(Eastern_Europe), addCoef.col = "white",number.cex = 0.7,
         number.digits = 2, diag = TRUE,bg= "grey", outline = "black",col = col1(9),
         addgrid.col = "white", mar= c(1,1,1,1))
# we round the sectors to two decimals
round(cor(Western_Europe), digits = 2)
#  correlation to western Europe
corrplot(cor(Western_Europe), addCoef.col = "white",number.cex = 0.7,
         number.digits = 2, diag = TRUE,bg= "grey", outline = "black",col = col1(9),
         addgrid.col = "white", mar= c(1,1,1,1))
# delete ACFAT columns from western Europe
col_delete <- c('ACFAT')
new_western_Europe <- Western_Europe%>%
  select(- one_of(col_delete))
# correlation to western Europe
corrplot(cor(new_western_Europe), addCoef.col = "white",number.cex = 0.7,
         number.digits = 2, diag = TRUE,bg= "grey", outline = "black",col = col1(9),
         addgrid.col = "white", mar= c(1,1,1,1))

#4.3
# we divide our country into western and eastern Europe
library('dplyr')
Eastern_Europe<-new_data[new_data$`Country_Name`== 'Romania'|
                           new_data$`Country_Name`== 'Serbia'|
                           new_data$`Country_Name`== 'Bosnia and Herzegovina'|
                           new_data$`Country_Name`== 'Georgia'|
                           new_data$`Country_Name`== 'Montenegro'|
                           new_data$`Country_Name`== 'Moldova'|
                           new_data$`Country_Name`== 'Belarus'|
                           new_data$`Country_Name`== 'Finland'|
                           new_data$`Country_Name`== 'Ukraine',]
Western_Europe<-new_data[new_data$`Country_Name`== 'United Kingdom'|
                           new_data$`Country_Name`== 'France'|
                           new_data$`Country_Name`== 'Italy'|
                           new_data$`Country_Name`== 'Switzerland'|
                           new_data$`Country_Name`== 'Ireland'|
                           new_data$`Country_Name`== 'Germany'|
                           new_data$`Country_Name`== 'Spain',]

library(ggplot2)  
Eastern_Europe %>% ggplot(aes(x= Country_Name,y= CO2_Emis))+
  geom_point(color ='red')+ xlab('Country Name')+
  ylab('CO2 emissions')+theme_light()


# We perform regression to eastern Europe for one country, Belarus.
library(ggplot2)
new_data
Eastern_regression <- new_data [new_data$Country_Name == 'Belarus',]

# We remove any unnecessary columns. 
col_remove_1 <- c('Country_Name','Country_Code','Year','Time_Code')
Eastern_regression <- Eastern_regression%>%
  select(- one_of(col_remove_1))
# we visualization our new data frame bu using box plot
boxplot(Eastern_regression,main = 'Outlier in data',col="red",border = "black",
        outpch = 25,outbg = "green",whiskcol = "blue",whisklty = 10)
# we show the correlation between our dependent variables and others independent variables
corrplot(cor(Eastern_regression), addCoef.col = "white",number.cex = 0.8,
         number.digits = 2, diag = TRUE,bg= "grey", outline = "black",
         addgrid.col = "white", mar= c(1,1,1,1))
# simple linear regression
model <- lm (CO2_Emis ~ Green_Gas ,Eastern_regression)
summary(model)
plot(CO2_Emis ~ Green_Gas ,Eastern_regression,
     col = "blue",
     main = "Regression: CO2_Emissions & Total Green Gas Emissions ",
     xlab = "Total Green Gas Emissions",
     ylab = "CO2_Emissions")
abline(model, col="red")
plot(model, 1)
plot(model, 2)
plot(model, 3)

# multiple linear regression
model_2 <- lm(CO2_Emis~  Green_Gas+Agri_nit_oxide,Eastern_regression)
summary(model_2)
model_3 <- lm(CO2_Emis~  Green_Gas+Agri_nit_oxide+Natur_reso,Eastern_regression)                  
summary(model_3)
model_4 <- lm(CO2_Emis~  Green_Gas+Agri_nit_oxide+Natur_reso+Agri_Fish,Eastern_regression)                  
summary(model_4)
model_5 <- lm(CO2_Emis~ Green_Gas+Agri_nit_oxide+Natur_reso+Agri_Fish + ACFAT ,Eastern_regression)                  
summary(model_5)
model_6 <- lm(CO2_Emis~ Green_Gas+Agri_nit_oxide+Natur_reso+Agri_Fish + ACFAT + For_Area ,Eastern_regression)                 
summary(model_6)
model_7 <- lm(CO2_Emis~ Green_Gas+Agri_nit_oxide+Natur_reso+Agri_Fish + ACFAT + For_Area + Agri_methane ,Eastern_regression)
summary(model_7)
model_8 <- lm(CO2_Emis~ Green_Gas+Agri_nit_oxide+Natur_reso+Agri_Fish + ACFAT + For_Area + Agri_methane + Inter_Tour ,Eastern_regression)
summary(model_8)

# doing our assumption
data.frame(colnames(Eastern_regression))
pairs((Eastern_regression)[,c(1,8,9,5,2,3,7,4)], lower.panel = NULL, pch = 19,cex = 1)
plot(model_8, 1)
plot(model_8, 2)
plot(model_8, 3)

#western Europe
Western_Europe %>% ggplot(aes(x= Country_Name,y= CO2_Emis))+
  geom_point(color ='red')+ xlab('Country Name')+
  ylab('CO2 emissions')+theme_light()
# We perform regression to western Europe for one country,Switzerland .
Western_regression <- new_data [new_data$Country_Name == 'Switzerland',]
# We remove any unnecessary columns.
col_remove2 <- c('Country_Name','Country_Code','Year','Time_Code','ACFAT')
Western_regression <- Western_regression%>%
  select(- one_of(col_remove2))
# we visualization our new data frame bu using box plot
boxplot(Western_regression,main = 'Outlier in data',col="red",border = "black",
        outpch = 25,outbg = "green",whiskcol = "blue",whisklty = 10)
# we show the correlation between our dependent variables and others independent variables
corrplot(cor(Western_regression), addCoef.col = "white",number.cex = 0.8,
         number.digits = 2, diag = TRUE,bg= "grey", outline = "black",
         addgrid.col = "white", mar= c(1,1,1,1))
# simple linear regression
model_9 <- lm (CO2_Emis ~ Green_Gas , Western_regression)
summary(model_9)
plot(CO2_Emis ~ Green_Gas , Western_regression,
     col = "blue",
     main = "Regression: CO2_Emis & Total Green Gas Emissions ",
     xlab = "Total Green Gas Emissions",
     ylab = "CO2_Emis")
abline(model_9, col="red")
plot(model_9, 1)
plot(model_9, 2)
plot(model_9, 3)
# multiple linear regression
model_9 <- lm(CO2_Emis ~  Agri_methane + Agri_nit_oxide ,Western_regression )                  
summary(model_9)
model_10 <- lm(CO2_Emis~ Agri_methane + Agri_nit_oxide + Natur_reso ,Western_regression)
summary(model_10)
model_11 <- lm(CO2_Emis~ Agri_methane + Agri_nit_oxide + Natur_reso + For_Area , Western_regression)
summary(model_11)
model_12 <- lm(CO2_Emis~ Agri_methane + Agri_nit_oxide + Natur_reso + For_Area + Inter_Tour , Western_regression)
summary(model_12)
model_13 <- lm(CO2_Emis~ Agri_methane + Agri_nit_oxide + Natur_reso + For_Area + Inter_Tour + Agri_Fish , Western_regression)
summary(model_13)
data.frame(colnames(Western_regression))
pairs((Western_regression)[,c(1,6,7,8,2,3,4)], lower.panel = NULL, pch = 19,cex = 1)
par(mfrow=c(2,2))
plot(model_13,1)
plot(model_13,2)
plot(model_13,3)

#4.4
#Eastern_ Europe(Holt-winter)
# We perform time series to Eastern Europe for one country,Ukraine 
library(TTR)
library(forecast)
Eastern_ts <- subset(new_data,select = c('Country_Name','Year','CO2_Emis','Agri_methane'))
Eastern_timeseries <- Eastern_ts [Eastern_ts$Country_Name == 'Ukraine',]
Eastern_timeseries
# check the type of data frame
class(Eastern_timeseries)
#change the type of data frame to time series
Eastern_CO2_timeseries <- ts(Eastern_timeseries$CO2_Emis, frequency = 2, start = c(2005))
class(Eastern_CO2_timeseries)
# plot the time series
plot(Eastern_CO2_timeseries)
#smooth time series data
Eastern_CO2_timeseriesSMA3 <- SMA(Eastern_CO2_timeseries,n=1)
plot.ts(Eastern_CO2_timeseriesSMA3)
# forcast using holtwinter model
forecast_eastern_CO2 <- HoltWinters(Eastern_CO2_timeseries, gamma=FALSE)
forecast_eastern_CO2
#we have stored the output of the HoltWinters() function in the list variable
forecast_eastern_CO2$fitted
# calculate the sum of squared errors for the in-sample forecast 
forecast_eastern_CO2$SSE
plot(forecast_eastern_CO2)
# specify the initial values of the level and the slope b of the trend component
HoltWinters(Eastern_timeseries, gamma=FALSE, l.start=312570.0, b.start=8579.9865)
#forcast for future time
forecast_eastern_CO2_2 <- forecast(forecast_eastern_CO2, h=19)
plot(forecast_eastern_CO2_2)
acf(forecast_eastern_CO2_2$residuals, lag.max=12,na.action = na.pass)
Box.test(forecast_eastern_CO2_2$residuals, lag=12, type="Ljung-Box")
# check the histogram
plot.ts(forecast_eastern_CO2_2$residuals)
plotForecastCO2Errors <- function(forecasterrors)
{
  mybinsize <- IQR(forecasterrors)/4 
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5 
  mymax <- max(forecasterrors) + mysd*3
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 <- mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
forecast_eastern_CO2_2$residuals <-forecast_eastern_CO2_2$residuals[!is.na(forecast_eastern_CO2_2$residuals)]
plotForecastCO2Errors(forecast_eastern_CO2_2$residuals)

forecast_eastern_CO2_2$residuals <-forecast_eastern_CO2_2$residuals[!is.na(forecast_eastern_CO2_2$residuals)]
plotForecastco2Errors(forecast_eastern_CO2_2$residuals)
# Airma model
# use another series Agriculture methane emissions
# change to time series
Eastern_methane_timeseries <- ts(Eastern_timeseries$Agri_methane, frequency = 1, start = c(2005))
#check the type of data
class(Eastern_methane_timeseries)
Eastern_methane_timeseries
plot(Eastern_methane_timeseries)
# check for stationary time series.
acf(Eastern_methane_timeseries)
# by using difference change time series to stationary
Eastern_methane_timeseries1 <- diff(Eastern_methane_timeseries, differences=2)
plot.ts(Eastern_methane_timeseries1)
# selecting a Candidate ARIMA Model
acf(Eastern_methane_timeseries1, lag.max=20)
acf(Eastern_methane_timeseries1, lag.max=20, plot=FALSE)
pacf(Eastern_methane_timeseries1, lag.max=20) 
pacf(Eastern_methane_timeseries1, lag.max=20, plot=FALSE)
# select ARIMA model
forecast_eastern_methanel <- arima(Eastern_methane_timeseries1, order=c(0,1,2))
forecast_eastern_methanel
forecast_eastern_methanel <- forecast(Eastern_methane_timeseries, h=15)
forecast_eastern_methanel
plot(forecast_eastern_methanel)
acf(forecast_eastern_methanel$residuals, lag.max=14)
Box.test(forecast_eastern_methanel$residuals, lag=14, type="Ljung-Box")
plot.ts(forecast_eastern_methanel$residuals)
plotForecastmethaneErrors <- function(forecasterrors)
{
  mybinsize <- IQR(forecasterrors)/4 
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5 
  mymax <- max(forecasterrors) + mysd*3
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
forecast_eastern_methanel$residuals <-forecast_eastern_methanel$residuals[!is.na(forecast_eastern_methanel$residuals)]
plotForecastmethaneErrors(forecast_eastern_methanel$residuals)

# western Europe
western_ts <- subset(new_data,select = c('Country_Name','Year','CO2_Emis','Agri_methane'))
Western_timeseries <- western_ts [western_ts$Country_Name == 'United Kingdom',]
Western_timeseries
plot.ts(Western_timeseries$CO2_Emis)
# check the type of data frame
class(Western_timeseries)
#change the type of data frame to time series
western_CO2_timeseries <- ts(Western_timeseries$CO2_Emis, frequency = 1, start = c(2005))
class(western_CO2_timeseries)
western_CO2_timeseries
# plot the time series
plot(western_CO2_timeseries)
#smooth time series data
western_CO2_timeseriesSMA3 <- SMA(western_CO2_timeseries,n=2)
plot.ts(western_CO2_timeseriesSMA3)
# forcast using holtwinter model
forecast_western_CO2 <- HoltWinters(western_CO2_timeseries, gamma=FALSE)
forecast_western_CO2
#we have stored the output of the HoltWinters() function in the list variable
forecast_western_CO2$fitted
# calculate the sum of squared errors for the in-sample forecast
forecast_western_CO2$SSE
plot(forecast_western_CO2)
# specify the initial values of the level and the slope b of the trend component
HoltWinters(Eastern_timeseries, gamma=FALSE, l.start=543200, b.start=1140.015)
#forcast for future time
forecast_western_CO2_2 <- forecast(forecast_western_CO2, h=19)
plot(forecast_western_CO2_2)
# check the histogram
acf(forecast_western_CO2_2$residuals, lag.max=10,na.action = na.pass)
Box.test(forecast_western_CO2_2$residuals, lag=10, type="Ljung-Box")
plot.ts(forecast_western_CO2_2$residuals)
plotForecastwesternco2Errors <- function(forecasterrors)
{
  mybinsize <- IQR(forecasterrors)/4 
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5 
  mymax <- max(forecasterrors) + mysd*3
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
forecast_western_CO2_2$residuals <-forecast_western_CO2_2$residuals[!is.na(forecast_western_CO2_2$residuals)]
plotForecastwesternco2Errors(forecast_western_CO2_2$residuals)

# Airma model
# use another series Agriculture methane emissions
# change to time series
western_methane <- ts(Western_timeseries$Agri_methane, frequency = 1, start =c(2005))
#check the type of data
class(western_methane)
plot(western_methane)
# check for stationary time series.
acf(western_methane)
western_methane_1 <- diff(western_methane, differences=2)
plot.ts(western_methane_1)
# selecting a Candidate ARIMA Model
acf(western_methane_1, lag.max=20)
acf(western_methane_1, lag.max=20, plot=FALSE)
pacf(western_methane_1, lag.max=20) 
pacf(western_methane_1, lag.max=20, plot=FALSE)
# select ARIMA model
Western_methane_mode_1 <- arima(western_methane_1, order=c(0,2,0))
forecast_western_methane <- forecast(Western_methane_mode_1, h=15)
plot(forecast_western_methane)
acf(forecast_western_methane$residuals, lag.max=11)
Box.test(forecast_western_methane$residuals, lag=11, type="Ljung-Box")
plot.ts(forecast_western_methane$residuals)
plotForecastwesternmethaneErrors <- function(forecasterrors)
{
  mybinsize <- IQR(forecasterrors)/4 
  mysd <- sd(forecasterrors)
  mymin <- min(forecasterrors) - mysd*5 
  mymax <- max(forecasterrors) + mysd*3
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
forecast_western_methane$residuals <-forecast_western_methane$residuals[!is.na(forecast_western_methane$residuals)]
plotForecastwesternmethaneErrors(forecast_western_methane$residuals)

#4.5
#t-test two tail
# divide our country into two regions.
library(ggplot2)
library(datarium)
library(qqplotr)
Western_test <-new_data[new_data$`Country_Name`== 'United Kingdom'|
                           new_data$`Country_Name`== 'France'|
                           new_data$`Country_Name`== 'Italy'|
                           new_data$`Country_Name`== 'Switzerland'|
                           new_data$`Country_Name`== 'Ireland'|
                           new_data$`Country_Name`== 'Germany'|
                           new_data$`Country_Name`== 'Spain',]
Eastern_test<-new_data[new_data$`Country_Name`== 'Romania'|
                           new_data$`Country_Name`== 'Serbia'|
                           new_data$`Country_Name`== 'Bosnia and Herzegovina'|
                           new_data$`Country_Name`== 'Georgia'|
                           new_data$`Country_Name`== 'Montenegro'|
                           new_data$`Country_Name`== 'Moldova'|
                           new_data$`Country_Name`== 'Belarus'|
                           new_data$`Country_Name`== 'Finland'|
                           new_data$`Country_Name`== 'Ukraine',]
# remove unnecessary columns
col_remove_3 <- c('Country_Code','Time_Code',"ACFAT","For_Area","Agri_Fish","Green_Gas","Agri_methane",
                  "Agri_nit_oxide","Natur_reso")
Eastern_test <- Eastern_test%>%
  select(- one_of(col_remove_3))
# check normality 
log10(Eastern_test$CO2_Emis)
shapiro.test(Eastern_test$CO2_Emis)
# check the unique
n_distinct(Eastern_test)
#check if we have extreme outliers
boxplot(Eastern_test_sample$CO2_Emis, plot = TRUE,col="darkmagenta")$out
boxplot(Eastern_test_sample$Inter_Tour , plot = TRUE,col="darkmagenta")$out
set.seed(10)
Eastern_test_sample <- sample_n(Eastern_test,70,replace = TRUE)
# with sample n > 25 the normality in t-test is not important assumption
library(dplyr)
# Examine the dimension 
dim(Eastern_test_sample)
summary(Eastern_test_sample$Inter_Tour)
summary(Eastern_test_sample$CO2_Emis)
sd(Eastern_test_sample$CO2_Emis)
hist(Eastern_test_sample$CO2_Emis,col="darkmagenta")
#welch two sample t-test
# changing the International toursim (Non_Tourist_area,Tourist_area)
Eastern_Europe_ttail <- within (Eastern_test_sample, {
  Inter_Tour.new <- NA
  Inter_Tour.new[Inter_Tour < 4600000] <- "Non_Tourist_area"
  Inter_Tour.new[Inter_Tour >= 4600000] <- "Tourist_area"
} )
Eastern_Europe_ttail$Inter_Tour.new[Eastern_Europe_ttail$Inter_Tour.new == 'Non_Tourist_area'] <- 0
Eastern_Europe_ttail$Inter_Tour.new[Eastern_Europe_ttail$Inter_Tour.new == 'Tourist_area'] <- 1

# change CO2_Emis.new to vector
Eastern_Europe_ttail$Inter_Tour.new<-as.factor(Eastern_Europe_ttail$Inter_Tour.new)
summary(Eastern_Europe_ttail)
boxplot(CO2_Emis ~ Inter_Tour.new, data=Eastern_Europe_ttail, names=c("Non_Tourist_area", "Tourist_area"),
        xlab="Non_Tourist_area Or Tourist_area", ylab="CO2 emissions (kt)",
        main="CO2 Emiisions in Eastern Europe for Non_Tourist_area and Tourist_area",col="darkmagenta")

# 𝐻0 :𝜇Non_Tourist_area =𝜇Tourist_area
# 𝐻1 :𝜇Non_Tourist_area ≠𝜇Tourist_area
t.test(CO2_Emis ~ Inter_Tour.new, Eastern_Europe_ttail)

####### WESTERN EUROPE ##########
col_remove_4 <- c('Country_Code','Time_Code',"ACFAT","For_Area","Agri_Fish","Green_Gas","Agri_methane",
                  "Agri_nit_oxide","Natur_reso")
Western_test <- Western_test%>%
  select(- one_of(col_remove_3))
# check normality 
log10(Western_test$CO2_Emis)
shapiro.test(Western_test$CO2_Emis)
# check the unique
n_distinct(Eastern_test)
#check if we have extreme outliers
boxplot(Eastern_test_sample$CO2_Emis , plot = TRUE,col="darkmagenta")$out
boxplot(Eastern_test_sample$Inter_Tour , plot = TRUE,col="darkmagenta")$out
set.seed(10)
Western_test_sample <- sample_n(Western_test,70,replace = TRUE)
# with sample n > 25 the normality in t-test is not important assumption
library(dplyr)
# Examine the dimension 
dim(Western_test_sample)
summary(Western_test)
sd(Western_test$CO2_Emis)
hist(Western_test_sample$CO2_Emis,col="darkmagenta")
Western_Europe_ttail <- within (Western_test_sample, {
  Inter_Tour.new <- NA
  Inter_Tour.new[Inter_Tour < 66000000] <- "Non_Tourist_area"
  Inter_Tour.new[Inter_Tour >= 66000000] <- "Tourist_area"
} )
Western_Europe_ttail$Inter_Tour.new[Western_Europe_ttail$Inter_Tour.new == 'Non_Tourist_area'] <- 0
Western_Europe_ttail$Inter_Tour.new[Western_Europe_ttail$Inter_Tour.new == 'Tourist_area'] <-1
Western_Europe_ttail$Inter_Tour.new<-as.factor(Western_Europe_ttail$Inter_Tour.new)
summary(Western_Europe_ttail)
boxplot(CO2_Emis~ Inter_Tour.new ,data = Western_Europe_ttail, names=c('Non_Tourist_area','Tourist_area'),
        xlab='Tourist_area or Non_Tourist_area', ylab='CO2 emissions',
        main='CO2 emissions in Western Europe for Tourist_area and Non_Tourist_area',col="darkmagenta")
# 𝐻0 :𝜇Non_Tourist_area =𝜇Tourist_area
# 𝐻1 :𝜇Non_Tourist_area ≠𝜇Tourist_area
t.test(CO2_Emis ~ Inter_Tour.new, Western_Europe_ttail)

###################### paired########################
# with sample n > 25 the normality in t-test is not important assumption
library(dplyr)
# Examine the dimension 
dim(Eastern_test_sample)
summary(Eastern_test_sample$CO2_Emis)
sd(Eastern_test_sample$CO2_Emis)
hist(Eastern_test_sample$CO2_Emis,col="darkmagenta")
# classificatopn the data before and after 2012

BEFORE_2012 <- c(16230,295410,4240,59120,51460,8140,2100,2100,57650,57650,56660,8150,95420,60530,60530,301200,56660,4840,20590,46870,
                 7640,5620,5620,46870,5320,77620,77620,2580,51230,58550,283340,49410,277110,7190,2330)
AFTER_2012 <- c(21850,59990,46470,270270,71540,38960,59500,46160,19490,38960,54820,54820,191070,22340,8140,9650,46620,22340,2150,45700,
                55140,9730,43020,43020,22680,47340,75190,2500,8540,44580,9570,46130,73950,8860,8860)
# compine them in one dataframe
Eastern_paired <- data.frame(CO2_Emis = rep(c("BEFORE","AFTER")),Eastern_test_sample= c(BEFORE_2012,AFTER_2012))
length(BEFORE_2012) <- length(AFTER_2012)
Eastern_test_sample <- data.frame(BEFORE_2012=BEFORE_2012, AFTER_2012=AFTER_2012)
boxplot(Eastern_test_sample$BEFORE_2012, Eastern_test_sample$AFTER_2012, names=c("BEFORE_2012", "AFTER_2012"),
        xlab=" CO2 emissions Before & After 2012", ylab="CO2 emissions",
        main="CO2 Emissions in Eastern Europe",col="darkmagenta")
# H0
# 𝜇before 2012 = 𝜇after 2012
# 𝐻1
# 𝜇before 2012 ≠ 𝜇after 2012
t.test(Eastern_test_sample$BEFORE_2012,Eastern_test_sample$AFTER_2012, paired=TRUE)

##### western Europe #######
dim(Western_test_sample)
summary(Western_test_sample$CO2_Emis)
sd(Western_test_sample$CO2_Emis)
hist(Western_test_sample$CO2_Emis,col="darkmagenta")
# classification the data before and after 2012
BEFORE_2012_w <- c(540920,46650,540920,45850,473830,45480,45480,341780,814410,466650,46360,354680,362830,354680,783800,43540,43540,43540,
                 362830,459370,789690,45780,44960,357990,466490,466490,343730,734810,287490,405270,40190,405270,445590,36680,445590,
                 274400,760130,467780,42520)
AFTER_2012_w <- c(453760,338560,776150,776150,240960,415600,306100,39790,327500,35530,35530,742310,39670,37780,263450,367000,329190,
                 707700,707700,324880,324880,300520,348920,37380,657400,37380,37380,300520,348920,239980,37380)

length(BEFORE_2012_w) <- length(AFTER_2012_w)
Western_test_sample <- data.frame(BEFORE_2012_w=BEFORE_2012_w, AFTER_2012_w=AFTER_2012_w)
boxplot(Western_test_sample$BEFORE_2012_w, Western_test_sample$AFTER_2012_w, names=c("Before", "After"),
        xlab=" CO2 emissions Before & After 2012", ylab="CO2 emissions",
        main="CO2 Emissions in Western Europe",col="darkmagenta")
t.test(Western_test_sample$BEFORE_2012_w,Western_test_sample$AFTER_2012_w, paired=TRUE)

















































