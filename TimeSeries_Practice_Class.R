## Time series forecasting
setwd("D:/Srimatha_BABI/Time Series Forecasting")

## install package

install.packages("timeSeries")
library("timeSeries")

## Load airpassengers data

str(AirPassengers)
frequency(AirPassengers)
d<- stl(AirPassengers, s.window = 12)
plot(d)

## Read the petrol price data

petrol <- read.csv("BengaluruPetrolPrices.csv", header = TRUE)
class(petrol)
View(petrol)

## Read rainfall data
rainfall <- read.csv("CleanSKINTRainfall.csv", header = TRUE)
View(rainfall)
class(rainfall)
train <- seq(from = as.Date("1871-01-01"), by = "month", length.out = 1692)
class(train)
tsrain <- as.timeSeries(rainfall$Rainfall, train)
class(tsrain)
plot(tsrain)
View(tsrain)

stlrain <- stl(tsrain,s.window = 12)
plot(stlrain)
             

## Using Holt Winters method
h1 <- HoltWinters(AirPassengers, alpha = 0.5, beta = 0.5, gamma = 0.5, 
                  seasonal = "multiplicative")
plot(h1)


h2 <- HoltWinters(AirPassengers, alpha = 0.8, beta = 0.5, gamma = 0.5, 
                  seasonal = "multiplicative")
plot(h2)

h3 <- HoltWinters(AirPassengers, alpha = 0.8, beta = 0.5, gamma = 0.5, 
                                    seasonal = "additive")
plot(h3)

h4 <- HoltWinters(AirPassengers, alpha = 0.8, beta = 0.5, gamma = NULL, 
                  seasonal = "additive")
plot(h4)

h5 <- HoltWinters(AirPassengers, alpha = 0.8, beta = NULL, gamma = NULL, 
                  seasonal = "additive")
plot(h5)

predict(h2, n.ahead = 60)

## ACF funtion - Day 2
acf(AirPassengers)
acf(CO2)
acf(co2)

## ARIMA model

ma <- auto.arima(AirPassengers)
install.packages("forecast")
library("forecast")

setwd("D:/Srimatha_BAB/Time Series Forecasting")
getwd()
rain <- read.csv("D:/Srimatha_BAB/Time Series Forecasting/CleanSKINTRainfall.csv")
