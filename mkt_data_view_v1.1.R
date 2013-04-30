require(quantmod)
require(pastecs)
require(ggplot2)
require(corrgram)
require(car)
sapply(list.files(pattern="[.]R$", path='functions/', full.names=TRUE), source)

## Retrieving Data & Refining
file1 <- "./data/Stock_Indice.csv"
Asset_data <- read.csv(file1)
Asset_data <- as.xts(Asset_data[,-1], order.by=as.POSIXct(Asset_data[,1]))

file2 <- "./data/US_Econ_Indicators.csv"
Indicators <- read.csv(file2)
Indicators <- as.xts(Indicators[,-1], order.by=as.POSIXct(Indicators[,1]))

# Change Asset data into return data
for (i in 1:NROW(Asset_data)) {
  Asset_data[,i] <- ROC(Asset_data[,i], n=1, type="continuous")
}

# Change Indicators
## Apply change
for (i in c(1,2,4,6,8)) {
  Indicators[,i] <- ROC(Indicators[,i], type="continuous")
}

## Lagging


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