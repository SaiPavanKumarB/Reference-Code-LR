## Timeseries Sensex data

## install package

install.packages("timeSeries")
library("timeSeries")

setwd("D:/Srimatha_BABI/Time Series Forecasting")
getwd()
## Load the data set
crest <- read.csv("D:/Srimatha_BABI/Time Series Forecasting/weekly-market-share-data-of-cres.csv", header = TRUE)

# Get the structure of Sensex data frame
str(crest)

# class of sensex
class(crest)

View(crest)

# Create a timeseries object
t <- seq(from=as.Date("1958-01-01"), by = "week", length.out = 276)
str(t)
class(t)

# Create timeseries object
tscrest <- as.timeSeries(crest$Weekly.market.share.data.of.Crest.toothpaste.from.Junuary.1958.to.April.1963,t)
View(tscrest)

# Create stl object
stlcrest <- stl(tscrest, s.window = 1)
plot(stlcrest)


## Using Holt Winters method
h1 <- HoltWinters(crest, alpha = 0.5, beta = 0.5, gamma = 0.5, 
                  seasonal = "multiplicative")
plot(h1)

frequency(crest)
cycle(crest)
plot(crest)
