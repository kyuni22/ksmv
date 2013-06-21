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
  
  getSymbols("SPY", src = 'yahoo', from = '2004-01-01', auto.assign = T)
  pf_opt_data <- Cl(adjustOHLC(get("SPY"), use.Adjusted=T))
  tickers <- c("EFA", "EWJ", "EEM", "IYR", "RWX", "IEF", "TLT", "DBC", "GLD")
  for(ticker in tickers) {
    getSymbols(ticker, src = 'yahoo', from = '2004-01-01', auto.assign = T)
    pf_opt_data <- cbind(pf_opt_data, Cl(adjustOHLC(get(ticker), use.Adjusted=T)))
  }

  w_pf_opt_data <- {}
  w_pf_opt_data <- to.period(pf_opt_data[,1], period='weeks')[,4]

  # To Weekly
  for (i in 2:NCOL(pf_opt_data)) {
    w_pf_opt_data <- cbind(w_pf_opt_data, to.period(pf_opt_data[,i], period='weeks')[,4])
  }
  names(w_pf_opt_data) <- names(pf_opt_data)

  ## Add Cash
  w_pf_opt_data <- cbind(w_pf_opt_data, rep(100,NROW(w_pf_opt_data)))
  names(w_pf_opt_data)[NCOL(w_pf_opt_data)] <- "Cash"

  prc_pf_opt_data <- w_pf_opt_data

  # Change to return data
  for (i in 1:NCOL(w_pf_opt_data)) {
    w_pf_opt_data[,i] <- ROC(w_pf_opt_data[,i], type="discrete")
  }  
  
## end of data refining

## backtesting part
  # 1: SPX, 2:NKY, 3: SX5E, 4:Copper, 5:WTI, 6:Gold, 7:UST10Y Fut
  # settings: 1.Selecting assets to optimize, 2.Target Vol level, 3.lookback period
  topn <- 4
  start_i <- 2#167#2#60
  assetsToTest <- 1:NCOL(w_pf_opt_data)
  cash_asset <- 7
  targetVolLv <- 0.1
  lookback <- 13
  vollookback <- 52
  return_series <- xts(x=matrix(rep(0,4), nrow=1), order.by=index(w_pf_opt_data[lookback+start_i]))
  names(return_series) <- c("All_Min_Var","Equal_W","Momentum","Momentum_Min_Var")
  weight <- xts(x=matrix(c(rep(0,NCOL(w_pf_opt_data))), nrow=1), order.by=index(w_pf_opt_data[lookback+start_i]))
  weight2 <- xts(x=matrix(c(rep(0,topn)), nrow=1), order.by=index(w_pf_opt_data[lookback+start_i]))
  volhist <- xts(x=matrix(c(rep(0,1)), nrow=1), order.by=index(w_pf_opt_data[lookback+start_i]))
  
  #loop
  for(i in (vollookback+start_i):(nrow(w_pf_opt_data)-1)) {
    #Checking Return
    uplimit <- as.vector(ifelse(coredata(prc_pf_opt_data[i]) /coredata(prc_pf_opt_data[i-lookback])-1>0, 1,0))
    downlimit <- as.vector(ifelse(coredata(prc_pf_opt_data[i]) /coredata(prc_pf_opt_data[i-lookback])-1<0, -1,0))
    uplimit[7] <- 1
    downlimit[7] <- -1
    uplimit[8] <- 1
    downlimit[8] <- 0    
    mu.vec <- as.vector(coredata(prc_pf_opt_data[i]) /coredata(prc_pf_opt_data[i-lookback])-1)*52/lookback
    
    #Momentum Part
    topindex <- ntop(mu.vec,topn) #nbottom(mu.vec, topn)
    
    #cov.mat
    cov.mat <- cov(w_pf_opt_data[(i-vollookback):i,assetsToTest])*52
    ef2 <- effFrontier_k2(mu.vec, cov.mat, nports=20, reslow=rep(-1,NCOL(w_pf_opt_data)), reshigh=rep(1,NCOL(w_pf_opt_data)), tgt_vol=0.1) #effFrontier_k2(mu.vec, cov.mat, nports=20, reslow=downlimit, reshigh=uplimit, tgt_vol=0.1)
    ef3 <- effFrontier_k2(mu.vec, cov.mat, nports=20, reslow=downlimit, reshigh=uplimit, psum=1, tgt_vol=0.1)
    
    adj_w <- ef3$w_tgt_vol
    if(is.null(adj_w)) {
      print(paste("warning no weight for",index(w_pf_opt_data[i]),sep=" "))
      adj_w <- rep(0,NCOL(w_pf_opt_data))
    }
    adj_w <- round(adj_w, digit=2)
    
    #Storing data
    volhist <- rbind(volhist,xts(x=matrix(sqrt(adj_w%*%cov.mat%*%adj_w),nrow=1), order.by=index(w_pf_opt_data[i])))
    
    mu.vec <- as.vector(coredata(prc_pf_opt_data[i,topindex]) /coredata(prc_pf_opt_data[i-lookback,topindex])-1)*52/lookback
    cov.mat <- cov(w_pf_opt_data[(i-vollookback):i,topindex])*52    
    ef4 <- effFrontier_k2(mu.vec, cov.mat, nports=20, reslow=rep(0,topn), reshigh=rep(1,topn), psum=1, tgt_vol=0.1)
    
    adj_w2 <- ef4$w
    if(is.null(adj_w2)) {
      print(paste("warning2 no weight for",index(w_pf_opt_data[i]),sep=" "))
      adj_w2 <- rep(0,topn)
    }
    adj_w2 <- round(adj_w2, digit=2)    
    
    # Storing Value
    weight <- rbind(weight,xts(x=matrix(adj_w,nrow=1), order.by=index(w_pf_opt_data[i+1])))
    if (!is.null(adj_w2)) {
      weight2 <- rbind(weight2,xts(x=matrix(adj_w2,nrow=1), order.by=index(w_pf_opt_data[i+1])))    
    } else {
      weight2 <- rbind(weight2,xts(x=matrix(c(rep(NA,topn)),nrow=1), order.by=index(w_pf_opt_data[i+1])))
    }
    
    #calculate realized return
    return_w <- matrix(w_pf_opt_data[i+1], nrow=1) %*% adj_w
    return_e <- matrix(w_pf_opt_data[i+1], nrow=1) %*% rep(1/NCOL(w_pf_opt_data),NCOL(w_pf_opt_data))
    return_me <- matrix(w_pf_opt_data[i+1, topindex], nrow=1) %*% rep(1/topn,topn)
    return_mw <- matrix(w_pf_opt_data[i+1, topindex], nrow=1) %*% adj_w2
    #add to return index
    return_series <- rbind(return_series,xts(x=matrix(c(return_w, return_e, return_me, return_mw),nrow=1), order.by=index(w_pf_opt_data[i+1])))
  } #end of loop

plot(ef3$vol, ef3$ret)