require(quantmod)
require(pastecs)
require(ggplot2)
require(corrgram)
require(car)
sapply(list.files(pattern="[.]R$", path='functions/', full.names=TRUE), source)

## Retrieving Data & Refining
file1 <- "./data/SX5E_Vol.csv"
Asset_data <- read.csv(file1)
Asset_data <- as.xts(Asset_data[,-1], order.by=as.POSIXct(Asset_data[,1]))

# Change Asset data into return data and add Vol Index return data
Asset_data[,1] <- ROC(Asset_data[,1], n=1, type="continuous")
for(i in 2:5) {
  Asset_data <- cbind(Asset_data,ROC(Asset_data[,i], n=1, type="continuous")) 
  names(Asset_data)[NCOL(Asset_data)] <- paste(names(Asset_data)[i],"return",sep="_")
}

# Add difference
Asset_data <- cbind(Asset_data, Asset_data[,2] - Asset_data[,3]) #Imp-HIst
names(Asset_data)[NCOL(Asset_data)] <- "Imp_Hist"
Asset_data <- cbind(Asset_data, Asset_data[,3] - Asset_data[,4]) #Short-Long
names(Asset_data)[NCOL(Asset_data)] <- "Short_Long"
Asset_data <- cbind(Asset_data, Asset_data[,3] - Asset_data[,5]) #HIst-PHIst
names(Asset_data)[NCOL(Asset_data)] <- "Hist_Phist"
Asset_data <- cbind(Asset_data, Asset_data[,2] - Asset_data[,5]) #Imp-Phist
names(Asset_data)[NCOL(Asset_data)] <- "Imp-Phist"
Asset_data <- cbind(Asset_data, MACD(Asset_data[,2])$macd) #macd
Asset_data <- cbind(Asset_data, MACD(Asset_data[,2])$macd - MACD(Asset_data[,2])$signal) #macd signal gap
names(Asset_data)[NCOL(Asset_data)] <- "macd_sig"

## Lagging
for(i in 2:NCOL(Asset_data)) {
  Asset_data[,i] <- lag(Asset_data[,i], k=2)
}

##1.Summary of Statistics
#stat.desc(Indicators)
#for (i in 1:14) {
#  hist(Indicators[,i])
#} 
#2.Correlation Table
cor_out <- {}
cor_out <- corstarsl(Asset_data)[1]
names(cor_out)[NCOL(cor_out)] <- "All"
cor_out <- cbind(cor_out, corstarsl(Asset_data['2008-03/'])[1])
names(cor_out)[NCOL(cor_out)] <- "5yr"
cor_out <- cbind(cor_out, corstarsl(Asset_data['2012-03/'])[1])
names(cor_out)[NCOL(cor_out)] <- "1yr"

#3.Scatter Plot
plot(coredata(GTAA_data[,5]), coredata(lag(GTAA_data[,18],k=0)), pch=19)
abline(v=0, h=1)
abline(h=-1)
scatterplotMatrix(cbind(coredata(GTAA_data['2005/',6]),coredata(GTAA_data['2005/',8:13])))
scatterplotMatrix(cbind(coredata(GTAA_data['2005/',1]),coredata(Macro_sig['2005/'])))
#4.Regression