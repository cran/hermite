qhermite <- function(p, a, b, m=2, lower.tail=TRUE)
{
  if (a < 0 | b < 0 | floor(m)!=m | m < 2)
  {
    warning("improper parameter specification")
    return(NaN)
  }
  q <- vector()
  j <- 1
  if (lower.tail == FALSE) p <- 1-p
  if (a < 20 & b < 20)
  {
    while(!is.na(p[j]))
    {
      f <- 0
      q[j] <- -1
      if ((p[j] > 1 | p[j] < 0) & !is.na(a) & !is.na(b))
      {
        warning("NaNs produced")
        return(NaN)
      }
      if (p[j] == 1 & !is.na(p[j]) & !is.na(a) & !is.na(b)) return(Inf)
      while(f<p[j])
      {
        q[j] <- q[j] + 1
        f <- dhermite(q[j],a,b,m) + f
      }
      j <- j + 1
    }
  }
  if (a > 20 | b > 20)
  {
    while(!is.na(p[j]))
    {
      if ((p[j] > 1 | p[j] < 0) & !is.na(a) & !is.na(b))
      {
        warning("NaNs produced")
        return(NaN)
      }
      if (p[j] == 1 & !is.na(p[j]) & !is.na(a) & !is.na(b)) return(Inf)
      q[j] <- floor(cofi(p[j], a, b, m)) + 1
      j <- j + 1
    }
  }
    return(q)
}