require(quantmod)
require(pastecs)
require(ggplot2)

## Retrieving Data & Refining
file1 <- "./data/GTAA_data.csv"
GTAA_data <- read.csv(file1)
GTAA_data <- as.xts(GTAA_data[,-1], order.by=as.POSIXct(GTAA_data[,1]))
tmp <- GTAA_data[,c(9,11)]

# Change Asset data into return data
# filter1. 3M return
for (i in 1:7) {
  GTAA_data <- cbind(GTAA_data,ROC(GTAA_data[,i], n=3, type="discrete"))
}
GTAA_data[,14:20] <- lag(GTAA_data[,14:20])

for (i in 1:13) {
  GTAA_data[,i] <- ROC(GTAA_data[,i], type="discrete")
} 
GTAA_data[,c(9,11)] <- tmp
GTAA_data[,9] <- GTAA_data[,9] - lag(GTAA_data[,9])
GTAA_data[,8:13] <- lag(GTAA_data[,8:13], k=2) # Filter2 lagging
## Others are all rate of change except cpi yoy which apply net change

#1.Summary of Statistics
stat.desc(GTAA_data)
for (i in 1:20) {
  hist(GTAA_data[,i])
} 
#2.Correlation Table
corstarsl(GTAA_data[,1:7])
corstarsl(GTAA_data[,8:13])
corstarsl(GTAA_data['2005/'])
#3.Scatter Plot
plot(coredata(GTAA_data[,1]), coredata(lag(GTAA_data[,14],k=0)), pch=19)
abline(lm(coredata(GTAA_data[,2])~coredata(lag(GTAA_data[,1],k=1))))
#4.Regression