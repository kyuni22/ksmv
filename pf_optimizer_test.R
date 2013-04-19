require(quantmod)
require(tseries)

## Retrieving Data & Refining
file1 <- "./data/pf_opt_data.csv"
pf_opt_data <- read.csv(file1)
pf_opt_data <- as.xts(pf_opt_data[,-1], order.by=as.POSIXct(pf_opt_data[,1]))
prc_pf_opt_data <- pf_opt_data

# Change to return data
for (i in 1:7) {
  pf_opt_data[,i] <- ROC(pf_opt_data[,i], type="discrete")
}

mu.vec <- matrix(coredata(prc_pf_opt_data['2012-12-31',1:7])/coredata(prc_pf_opt_data['2012-09-28',1:7])-1, nrow=1)*4
cov.mat <- cov(pf_opt_data['2012-09-28/2012-12-31',1:7])*252

w_step = 0.01
reshigh <- c(0.1,0.1,0.1,0,0,0,1)
reslow <- c(0,0,0,0,0,-0.1,0)
each_w <- array(data=rep(0,7),dim=7)

ret <- {}
vol <- {}
max_sr <- 0
max_w <- {}

for(i in seq(reslow[1],reshigh[1],by=w_step)) {
  for(j in seq(reslow[2],reshigh[2],by=w_step)) {
    for(k in seq(reslow[3],reshigh[3],by=w_step)) {
      for(l in seq(reslow[4],reshigh[4],by=w_step)) {
        for(m in seq(reslow[5],reshigh[5],by=w_step)) {
          for(n in seq(reslow[6],reshigh[6],by=w_step)) {
#            for(o in seq(reslow[7],reshigh[7],by=0.05)) {
              
              w_pf <- c(i,j,k,l,m,n,o)
              mu_pf <- mu.vec%*%w_pf
              vol_pf <- sqrt(252*t(w_pf)%*%cov.mat%*%w_pf)
              
              if(!is.nan(mu_pf/vol_pf) & (mu_pf/vol_pf > max_sr)) {
                max_w <- w_pf
                max_sr <- mu_pf/vol_pf
              }
              
              ret <- append(ret, mu_pf)
              vol <- append(vol, vol_pf)
#            }
          }
        }
      }
    }
  }
}



mu.vec%*%max_w
sqrt(252*t(max_w)%*%cov.mat%*%max_w)


for(arr.test in 1:3) {
  print(arr.test[1])
}

## Weight
w_pf <- rep(1,6)/6
## Mean, cov
mu.vec <- matrix(colMeans(pf_opt_data['2012-09-30/2012-12-31',1:6]), nrow=1)
cov.mat <- cov(pf_opt_data['2012-09-30/2012-12-31',1:6])

reshigh <- c(1,1,1,1,1,1)
reslow <- c(0,0,0,0,0,0)/6


mu.vec <- matrix(coredata(prc_pf_opt_data['2012-12-31',1:6])/coredata(prc_pf_opt_data['2012-09-28',1:6])-1, nrow=1)*4
cov.mat <- cov(pf_opt_data['2012-09-28/2012-12-31',1:6])*252

ef <- effFrontier(mu.vec, cov.mat, shorts=T)
ef <- effFrontier2(mu.vec, cov.mat, nports = 100, rlow=reslow, rhigh=reshigh)
ef
plot(ef$vol, ef$ret)
ef$ret[which.max(ef$ret/ef$vol)]

## pf mean, sig
mu_pf <- crossprod(w_pf, mu.vec)
sqrt(252*t(w_pf)%*%cov(pf_opt_data[2101:2321,1:2])%*%w_pf)

portfolio.optim(x=mu.vec, covmat=cov.mat, shorts = T)
portfolio.optim(x=mu.vec, covmat=cov.mat, shorts = T, reslow = reslow, reshigh=reshigh)



effFrontier2 = function (averet, rcov, nports = 20, shorts=T, rlow, rhigh) 
{
  mxret = max(abs(averet))/3
  mnret = -mxret
  n.assets = ncol(averet)
  reshigh = rhigh
  reslow = rlow
  
  min.rets = seq(mnret, mxret, len = nports)
  vol = rep(NA, nports)
  ret = rep(NA, nports)
  for (k in 1:nports)
  {
    port.sol = NULL
    try(port.sol <- portfolio.optim(x=averet, pm=min.rets[k], covmat=rcov,
                                    reshigh=reshigh, reslow=reslow,shorts=shorts),silent=T)
    #print(min.rets[k])
    if ( !is.null(port.sol) )
    {
      vol[k] = sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
      ret[k] = averet %*% port.sol$pw
    }
  }
  return(list(vol = vol, ret = ret))
}

## using this functions..
effFrontier_k = function (averet, rcov, nports = 20, shorts=T, wmax=1) 
{
  mxret = max(abs(averet))
  mnret = -mxret
  n.assets = ncol(averet)
  reshigh = rep(wmax,n.assets)
  if( shorts )
  {
    reslow = rep(-wmax,n.assets) 
  } else {
    reslow = rep(0,n.assets) 
  }
  min.rets = seq(mnret, mxret, len = nports)
  vol = rep(NA, nports)
  ret = rep(NA, nports)
  mx_sr <- 0
  w <- {} 
  for (k in 1:nports)
  {
    port.sol = NULL
    try(port.sol <- portfolio.optim(x=averet, pm=min.rets[k], covmat=rcov,
                                    reshigh=reshigh, reslow=reslow,shorts=shorts),silent=T)
    if ( !is.null(port.sol) )
    {
      vol[k] = sqrt(as.vector(port.sol$pw %*% rcov %*% port.sol$pw))
      ret[k] = averet %*% port.sol$pw
      if(mx_sr < ret[k]/vol[k]) {
        mx_sr <- ret[k]/vol[k]
        w <- port.sol$pw
      }
    }
  }
  return(list(vol = vol, ret = ret, mx_sr=mx_sr, w=w))
}


if(require(PerformanceAnalytics)){
  charts.PerformanceSummary(return_series[,1]+return_series[,2],main="Performance", geometric=FALSE)
}