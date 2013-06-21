effFrontier_k2 = function (averet, rcov, nports = 20, reslow, reshigh, psum=1, tgt_vol=0.1) 
{
  # Return Setting
  if(!is.null(reslow) && !is.null(reshigh)) {
    mxret = max(abs(c(averet*reslow, averet*reshigh)))
  } else {
    mxret = max(abs(averet))
  }
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
  w_min_vol <- {}
  # Loading libarary for solve.QP
  require(quadprog)
  require(corpcor)
  ### Optimizing part1
  cov.mat <- make.positive.definite(rcov, 0.000000001)  
  D.mat <- 2*cov.mat
  d.vec <- rep(0,length(averet))  
  
  for (k in 1:nports)
  {
    port.sol <- NULL
    ### Optimizing part2
    if(!is.null(psum)) {
      A.mat <- cbind(averet, rep(1,length(averet)), diag(length(averet)), -diag(length(averet)))
      b.vec <- c(min.rets[k], psum, reslow, -reshigh)
    } else {
      A.mat <- cbind(averet, diag(length(averet)), -diag(length(averet)))
      b.vec <- c(min.rets[k], reslow, -reshigh)      
    }

    try(port.sol <- solve.QP(Dmat=D.mat, dvec=d.vec, Amat=A.mat, bvec=b.vec, meq=2),silent=T)
        
    if ( !is.null(port.sol) )
    {
      vol[k] = sqrt((port.sol$solution %*% rcov %*% port.sol$solution))
      ret[k] = averet %*% port.sol$solution
      if(mx_sr < ret[k]/vol[k]) {
        mx_sr <- ret[k]/vol[k]
        w <- port.sol$solution
        mx_vol <- vol[k]
        mx_ret <- ret[k]
      }
      if(tgt_vol == round(vol[k],1)) {
        w_tgt_vol <- port.sol$solution
      }
    }
  }

  ## Calculate Min Var
  A.mat <- cbind(rep(1,length(averet)), diag(length(averet)), -diag(length(averet)))
  b.vec <- c(psum, reslow, -reshigh)
  try(port.sol <- solve.QP(Dmat=D.mat, dvec=d.vec, Amat=A.mat, bvec=b.vec, meq=2),silent=T)  
  w_min_vol <- port.sol$solution
  
  return(list(vol = vol, ret = ret, mx_sr=mx_sr, mx_vol = mx_vol, mx_ret = mx_ret, w=w, w_tgt_vol=w_tgt_vol, w_min_vol=w_min_vol))
}