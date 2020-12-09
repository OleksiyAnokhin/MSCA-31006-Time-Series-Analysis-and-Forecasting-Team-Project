library(fpp)
library(zoo)
library(tidyverse)
library(xts)
library(TSA)
library(lubridate)

sales_month <- read_csv("Zillow monthly raw data (full).csv") #import raw data file

chicago_sales_m <- sales_month %>% filter(RegionName == "Chicago, IL") #import Chicago data only 

chicago_sales_m <- chicago_sales_m %>% #select median price and data only 
  gather(key = "Date", value = "Price", -RegionID:-StateName) %>%
  select(Date, Price) %>%
  rename(Median_price = Price)

chicago_sales_m$Date <- mdy(chicago_sales_m$Date)


chi_median <- ts(chicago_sales_m$Median_price, start=c(2008,2), frequency=12)


d <- diff(diff(chi_median), lag=12)


undo <- c(chi_median[1:13])

for (i in (14:length(chi_median))) {
  undo[i] <- undo[i-1] + undo[i-12] - undo[i-13] + d[i-13]
}

chi_median - undo

