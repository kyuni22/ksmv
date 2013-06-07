require(quantmod)
require(tseries)
require(quadprog)
require(corpcor)
sapply(list.files(pattern="[.]R$", path='functions/', full.names=TRUE), source)

## Retrieving Data & Refining
  file1 <- "./data/pf_opt_data.csv"
  pf_opt_data <- read.csv(file1)
  pf_opt_data <- as.xts(pf_opt_data[,-1], order.by=as.POSIXct(pf_opt_data[,1], tz="UTC"))
  prc_pf_opt_data <- pf_opt_data
  
  getSymbols("SPY", src = 'yahoo', from = '2003-01-01', auto.assign = T)
  pf_opt_data <- Cl(adjustOHLC(get("SPY"), use.Adjusted=T))
  tickers <- c("QQQ", "EEM", "IWM", "EFA", "TLT", "IYR", "GLD")
  for(ticker in tickers) {
    getSymbols(ticker, src = 'yahoo', from = '2003-01-01', auto.assign = T)
    pf_opt_data <- cbind(pf_opt_data, Cl(adjustOHLC(get(ticker), use.Adjusted=T)))
  }

  w_pf_opt_data <- {}
  w_pf_opt_data <- to.period(pf_opt_data[,1], period='weeks')[,4]

  # To Weekly
  for (i in 2:NCOL(pf_opt_data)) {
    w_pf_opt_data <- cbind(w_pf_opt_data, to.period(pf_opt_data[,i], period='weeks')[,4])
  }
  names(w_pf_opt_data) <- names(pf_opt_data)
  prc_pf_opt_data <- w_pf_opt_data

  # Change to return data
  for (i in 1:NCOL(pf_opt_data)) {
    w_pf_opt_data[,i] <- ROC(w_pf_opt_data[,i], type="discrete")
  }
  
## end of data refining

## backtesting part
  # 1: SPX, 2:NKY, 3: SX5E, 4:Copper, 5:WTI, 6:Gold, 7:UST10Y Fut
  # settings: 1.Selecting assets to optimize, 2.Target Vol level, 3.lookback period
  start_i <- 2
  assetsToTest <- 1:NCOL(w_pf_opt_data)
  cash_asset <- 7
  targetVolLv <- 0.1
  lookback <- 13
  return_series <- xts(x=matrix(c(0,0), nrow=1), order.by=index(w_pf_opt_data[lookback+start_i]))
  weight <- xts(x=matrix(c(rep(0,NCOL(w_pf_opt_data))), nrow=1), order.by=index(w_pf_opt_data[lookback+start_i]))
  
  #loop
  for(i in (lookback+start_i):(nrow(w_pf_opt_data)-1)) {
    #Checking Return
    uplimit <- as.vector(ifelse(coredata(prc_pf_opt_data[i]) /coredata(prc_pf_opt_data[i-lookback+1])-1>0, 1,0))
    downlimit <- as.vector(ifelse(coredata(prc_pf_opt_data[i]) /coredata(prc_pf_opt_data[i-lookback+1])-1<0, -1,0))    
    mu.vec <- as.vector(coredata(prc_pf_opt_data[i]) /coredata(prc_pf_opt_data[i-lookback+1])-1)
    
    #cov.mat
    cov.mat <- cov(w_pf_opt_data[(i-lookback+1):i,assetsToTest])
    cov.mat <- make.positive.definite(cov.mat, 0.000000001)
    ### Optimizing part
    D.mat <- 2*cov.mat
    d.vec <- rep(0,NCOL(w_pf_opt_data))
#    A.mat <- cbind(rep(1,NCOL(w_pf_opt_data)))
#    b.vec <- c(1) #b.vec <- c(1, rep(-1,6), -rep(1,6)) 
    A.mat <- cbind(mu.vec, diag(NCOL(w_pf_opt_data)), -diag(NCOL(w_pf_opt_data)))   
    b.vec <- c(1,rep(-1,NCOL(w_pf_opt_data)),-rep(1,NCOL(w_pf_opt_data))) #, downlimit, -uplimit)
    
    optimized <- solve.QP(Dmat=D.mat, dvec=d.vec, Amat=A.mat, bvec=b.vec, meq=1)
    adj_w <- optimized$solution
    weight <- rbind(weight,xts(x=matrix(adj_w,nrow=1), order.by=index(w_pf_opt_data[i+1])))    
    
#    #calculate max sharpe ratio weight
#    ef <- effFrontier_k(mu.vec, cov.mat)
#    #adjust vol to certain level
#    adj_w <- targetVolLv/ef$mx_vol*ef$w  #ef$w_tgt_vol
    
    #calculate realized return
    return_i <-  matrix(w_pf_opt_data[i+1], nrow=1) %*% adj_w
    return_c <- return_i#as.numeric((1-targetVolLv/ef$mx_vol)*(coredata(monthly_data[i+1,cash_asset])/coredata(monthly_data[i,cash_asset])-1))
    #add to return index
    return_series <- rbind(return_series,xts(x=matrix(c(return_i, return_c),nrow=1), order.by=index(w_pf_opt_data[i+1])))
  } #end of loop

plot(ef$vol, ef$ret)