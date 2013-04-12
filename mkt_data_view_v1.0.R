require(quantmod)
require(pastecs)
require(ggplot2)
require(car)
sapply(list.files(pattern="[.]R$", path='functions/', full.names=TRUE), source)

## Retrieving Data & Refining
file1 <- "./data/GTAA_data.csv"
GTAA_data <- read.csv(file1)
GTAA_data <- as.xts(GTAA_data[,-1], order.by=as.POSIXct(GTAA_data[,1]))
tmp <- GTAA_data[,c(9,11)]

# Change Asset data into return data
# filter1. 3M return
for (i in 1:7) {
  GTAA_data <- cbind(GTAA_data,ROC(GTAA_data[,i], n=3, type="discrete"))
  GTAA_data[,i+13] <- GTAA_data[,20+i] / GTAA_data[,i+13]*100 # Filter3 Sharpe Ratio
}
GTAA_data[,21:27] <- lag(GTAA_data[,21:27])
GTAA_data[,14:20] <- lag(GTAA_data[,14:20])

for (i in 1:13) {
  GTAA_data[,i] <- ROC(GTAA_data[,i], type="discrete")
} 
GTAA_data[,c(9,11)] <- tmp
GTAA_data[,9] <- GTAA_data[,9] - lag(GTAA_data[,9])
GTAA_data[,8:11] <- lag(GTAA_data[,8:11], k=2) 
GTAA_data[,12:13] <- lag(GTAA_data[,12:13], k=1) # Filter2 lagging. Difference applied
## Others are all rate of change except cpi yoy which apply net change

Macro_sig <- ifelse(GTAA_data[,8] > 0, 1, -1)
for (i in 9:12) {
  Macro_sig <- Macro_sig + ifelse(GTAA_data[,i] > 0, 1, -1)
}
Macro_sig <- Macro_sig + ifelse(GTAA_data[,13] < 0, 1, -1)

#1.Summary of Statistics
stat.desc(GTAA_data)
for (i in 1:27) {
  hist(GTAA_data[,i])
} 
#2.Correlation Table
corstarsl(GTAA_data[,1:7])
corstarsl(GTAA_data[,8:13])
corstarsl(GTAA_data['2005/'])
corrgram(coredata(GTAA_data['2005/']), order=FALSE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt)
#3.Scatter Plot
plot(coredata(GTAA_data[,5]), coredata(lag(GTAA_data[,18],k=0)), pch=19)
abline(v=0, h=1)
abline(h=-1)
scatterplotMatrix(cbind(coredata(GTAA_data['2005/',6]),coredata(GTAA_data['2005/',8:13])))
scatterplotMatrix(cbind(coredata(GTAA_data['2005/',1]),coredata(Macro_sig['2005/'])))
#4.Regression