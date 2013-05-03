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

file2 <- "./data/SK_Econ_Indicators.csv"
Indicators <- read.csv(file2)
Indicators <- as.xts(Indicators[,-1], order.by=as.POSIXct(Indicators[,1]))

# Change Asset data into return data
for (i in 1:NCOL(Asset_data)) {
  Asset_data[,i] <- ROC(Asset_data[,i], n=1, type="continuous")
}

# Change Indicators
## Apply Net Chg
for (i in c(1,2,3,5)) {
  Indicators[,i] <- Indicators[,i] - lag(Indicators[,i])
}

## Apply change
for (i in 4:4) {
  Indicators[,i] <- ROC(Indicators[,i], type="continuous")
}

## Lagging
for (i in c(1,2,3,5,6,7)) {
  Indicators[,i] <- lag(Indicators[,i], k=2)
}

#1.Summary of Statistics
stat.desc(Indicators)
for (i in 1:14) {
  hist(Indicators[,i])
} 
#2.Correlation Table
corstarsl(Indicators)
corstarsl(cbind(Asset_data, Indicators)[-3:-1])[6:12,1:5] #Pearson Corr
corstarsl(cbind(Asset_data, Indicators)[-3:-1],type="spearman")[6:12,1:5] #Spearman Corr

corstarsl(cbind(Asset_data, Indicators)['2008-03/'])[6:12,1:5] #Pearson Corr
corstarsl(cbind(Asset_data, Indicators)['2008-03/'],type="spearman")[6:12,1:5] #Spearman Corr

#3.Scatter Plot
plot(coredata(GTAA_data[,5]), coredata(lag(GTAA_data[,18],k=0)), pch=19)
abline(v=0, h=1)
abline(h=-1)
scatterplotMatrix(cbind(coredata(GTAA_data['2005/',6]),coredata(GTAA_data['2005/',8:13])))
scatterplotMatrix(cbind(coredata(GTAA_data['2005/',1]),coredata(Macro_sig['2005/'])))
#4.Regression