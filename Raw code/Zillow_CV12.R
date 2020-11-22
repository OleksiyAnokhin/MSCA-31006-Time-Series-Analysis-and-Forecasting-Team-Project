library(fpp)
library(zoo)
library(tidyverse)
library(xts)


sales <- read_csv("./Zillow monthly raw data (full).csv") #Import Data

Chicago_median<-sales[4,6:ncol(sales)]
Chicago_sales <- Chicago_median %>% gather(key = "Date", value = "Price") %>%select(Date, Price) 
Chicago_sales$Date<-as.POSIXct(Chicago_sales$Date, format="%m/%d/%y")
Chicago_TS<-xts(Chicago_sales$Price, order.by = Chicago_sales$Date, frequency=12)
attr(Chicago_TS,'frequency') <- 12

chi_train<-window(Chicago_TS, end = "2019-08-24")
chi_test<-window(Chicago_TS,start = "2019-08-31")

#check stationarity
tsdisplay(diff(BoxCox(chi_train, lambda=0),1))
adf.test(diff(BoxCox(chi_train, lambda=0),1)[-1])
kpss.test(diff(BoxCox(chi_train, lambda=0),1)[-1])



fixed_start <- length(index(Chicago_TS)[index(Chicago_TS) <= '2013-12-31 CST'])
idx <- length(index(Chicago_TS)[index(Chicago_TS) <= '2016-12-31 CST'])
aic <- c()
residuals <- c()
preds <- c()
actuals <-c()

for (i in 0:32) {
  #fixed window
  #chi_train<-window(Chicago_TS, start = index(Chicago_TS)[fixed_start], end = index(Chicago_TS)[idx+i])
  
  #rolling window - 5 years
  rolling_width <- 12 * 5 - 1
  chi_train<-window(Chicago_TS, start = index(Chicago_TS)[idx+i-rolling_width], end = index(Chicago_TS)[idx+i])
  model <- Arima(chi_train, lambda=0, order=c(0,1,3), seasonal=list(order=c(0,1,1), period=12))
  aic[i+1] <- model$aicc  
  actual <- as.numeric(Chicago_TS[idx+i+12])
  pred <- forecast(model,h=12)$mean[12]
  preds[i+1] <- pred
  actuals[i+1] <- actual
  residuals[i+1] <- actual - pred
}
dates <- index(Chicago_TS[(idx+12):(idx+12+32)])
results <- accuracy(preds, actuals)
print(results)
plot(dates,aic)
