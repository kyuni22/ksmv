require(quantmod)

## MOnthly
## Retrieving Data & Refining
file1 <- "./data/Stock_Indice.csv"
Asset_data <- read.csv(file1)
Asset_data <- as.xts(Asset_data[,-1], order.by=as.POSIXct(Asset_data[,1]))
index(Asset_data) <- as.Date(as.yearmon(index(Asset_data))+1/12)-1

file2 <- "./data/Econ_Indicators.csv"
Indicators <- read.csv(file2)
Indicators <- as.xts(Indicators[,-1], order.by=as.POSIXct(Indicators[,1]))
index(Indicators) <- as.Date(as.yearmon(index(Indicators))+1/12)-1

# Change Asset data into return data
for (i in 1:NCOL(Asset_data)) {
  Asset_data[,i] <- ROC(Asset_data[,i], n=1, type="continuous")
}

for (i in 1:NCOL(Indicators)) {
  Indicators[,i] <- ifelse(Indicators[,i] > lag(Indicators[,i], k=1), 1, 0)
}

for (i in 1:1) {
  Indicators[,i] <- lag(Indicators[,i], k=1)
}


for (i in 2:3) {
  Indicators[,i] <- lag(Indicators[,i], k=2)
}

results <- {}

for(i in 1:NCOL(Asset_data)) {
  results <- cbind(results,Asset_data[,i])
  for(j in 1:NCOL(Indicators)) {
    results <- cbind(results, Asset_data[,i] * Indicators[,j])
  }
}

results[is.na(results)] <- 0

write.zoo(results, file="./data/results.csv", sep=",")

## Daily
## Retrieving Data & Refining
file1 <- "./data/Stock_Indice_d.csv"
Asset_data <- read.csv(file1)
Asset_data <- as.xts(Asset_data[,-1], order.by=as.POSIXct(Asset_data[,1]))

file2 <- "./data/signal_d.csv"
Indicators <- read.csv(file2)
Indicators <- as.xts(Indicators[,-1], order.by=as.POSIXct(Indicators[,1]))

# Change Asset data into return data
for (i in 1:NCOL(Asset_data)) {
  Asset_data[,i] <- ROC(Asset_data[,i], n=1, type="discrete")
}

for (i in 1:5) {
  Indicators[,i] <- ifelse(Indicators[,i] > lag(Indicators[,i], k=5), 1, 0)
}

for (i in 1:5) {
  Indicators[,i] <- lag(Indicators[,i], k=1) #Enter based on today value
}

results <- {}

for(i in 1:NCOL(Asset_data)) {
  results <- cbind(results, Asset_data[,i])
  results <- cbind(results, Asset_data[,i] * Indicators[,i])
}

results[is.na(results)] <- 0

write.zoo(results, file="./data/results_d.csv", sep=",")