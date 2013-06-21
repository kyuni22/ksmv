effFrontier_k3 = function (averet, rcov, nports = 20, reslow, reshigh, tgt_vol=0.1) 
{
  # Return Setting
  mxret = max(abs(averet))
  mnret = -mxret
  min.rets = seq(mnret, mxret, len = nports)
  # Setting Output Variables
  vol <- rep(NA, nports)
  ret<- rep(NA, nports)
  mx_sr <- 0
  mx_vol <- 0
  mx_ret <- 0
  w <- {} 
  w_tgt_vol <- {}
  # Loading libarary for solve.QP
  require(quadprog)
  require(corpcor)
  
  for (k in 2:nports)
  {
    port.sol <- NULL

    try(port.sol <- portfolio.optim(x=averet, pm=min.rets[k], covmat=rcov,
                                    reshigh=reshigh, reslow=reslow, shorts=TRUE),silent=T)
        
    if ( !is.null(port.sol) )
    {
      vol[k] = sqrt((port.sol$pw %*% rcov %*% port.sol$pw))
      ret[k] = averet %*% port.sol$pw
      if(mx_sr < ret[k]/vol[k]) {
        mx_sr <- ret[k]/vol[k]
        w <- port.sol$pw
        mx_vol <- vol[k]
        mx_ret <- ret[k]
      }
      if(tgt_vol == round(vol[k],1)) {
        w_tgt_vol <- port.sol$pw
      }
    }
  }
  return(list(vol = vol, ret = ret, mx_sr=mx_sr, mx_vol = mx_vol, mx_ret = mx_ret, w=w, w_tgt_vol=w_tgt_vol))
}