require(quantmod)
require(tseries)
sapply(list.files(pattern="[.]R$", path='functions/', full.names=TRUE), source)

## Retrieving Data & Refining
  file1 <- "./data/pf_opt_data.csv"
  pf_opt_data <- read.csv(file1)
  pf_opt_data <- as.xts(pf_opt_data[,-1], order.by=as.POSIXct(pf_opt_data[,1]))
  prc_pf_opt_data <- pf_opt_data
  
  # Change to return data
  for (i in 1:7) {
    pf_opt_data[,i] <- ROC(pf_opt_data[,i], type="discrete")
  }
  monthly_data <- to.period(prc_pf_opt_data[,1], period = 'months', indexAt='endof')[,4]
  
  # Change to return data
  for (i in 2:7) {
    monthly_data <- cbind(monthly_data, to.period(prc_pf_opt_data[,i], period = 'months', indexAt='endof')[,4])
  }
  names(monthly_data) <- names(prc_pf_opt_data)
## end of data refining

## backtesting part
  # 1: SPX, 2:NKY, 3: SX5E, 4:Copper, 5:WTI, 6:Gold, 7:UST10Y Fut
  # settings: 1.Selecting assets to optimize, 2.Target Vol level, 3.lookback period
  assetsToTest <- c(1,2,3,4,5,6)
  cash_asset <- 7
  targetVolLv <- 0.1
  lookback <- 3
  vollookback <- 12
  return_series <- xts(x=matrix(c(0,0), nrow=1), order.by=index(monthly_data[lookback]))
  
  #loop
  for(i in (vollookback+2):(nrow(monthly_data)-1)) {
    #get today and lookback date
    date_today <- index(monthly_data[i])
    date_lookback <- index(monthly_data[i-lookback])
    #mu.vec
    mu.vec <- matrix(coredata(monthly_data[date_today,assetsToTest])/coredata(monthly_data[date_lookback,assetsToTest])-1, nrow=1)*12/lookback
    #cov.mat
    cov.mat <- cov(pf_opt_data[paste(index(monthly_data[i-vollookback]), index(monthly_data[i]), sep="/"),assetsToTest])*252
    #calculate max sharpe ratio weight
    ef <- effFrontier_k(mu.vec, cov.mat)
    ef2 <- effFrontier_k2(as.vector(mu.vec), cov.mat, reslow=rep(-1,6), reshigh=rep(1,6))
    ef3 <- effFrontier_k3(mu.vec, cov.mat, reslow=rep(-1,6), reshigh=rep(1,6))    
    #adjust vol to certain level
    adj_w <- ef3$w #targetVolLv/ef2$mx_vol*ef2$w  #ef$w_tgt_vol
    #calculate realized return
    return_i <- matrix(coredata(monthly_data[i+1,assetsToTest])/coredata(monthly_data[i,assetsToTest])-1,nrow=1) %*% adj_w
    return_c <- as.numeric((1-targetVolLv/ef$mx_vol)*(coredata(monthly_data[i+1,cash_asset])/coredata(monthly_data[i,cash_asset])-1))
    #add to return index
    return_series <- rbind(return_series,xts(x=matrix(c(return_i, return_c),nrow=1), order.by=index(monthly_data[i])))
  } #end of loop

plot(ef$vol, ef$ret)
plot(ef2$vol, ef2$ret)