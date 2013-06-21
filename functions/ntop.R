ntop = function (
    data,
    topn=1
  )
{
  temp = coredata(data)
  out <- {}
  for(i in 1:topn) {
    out <- rbind(out,which(temp == sort(temp, decreasing=TRUE)[i]))
  }
  return(as.vector(out))
}