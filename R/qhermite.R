qhermite <- function(p, a, b, m=2, lower.tail=TRUE)
{
  if (p > 1 & !is.na(p) & !is.na(a) & !is.na(b))
  {
    warning("NaNs produced")
    return(NaN)
  }
  if (p == 1 & !is.na(p) & !is.na(a) & !is.na(b)) return(Inf)
  q <- vector()
  j <- 1
  f <- 0
  q[j] <- -1
  if (lower.tail == FALSE) p <- 1-p
  while(!is.na(p[j]))
  {
    while(f<p[j])
    {
      q[j] <- q[j] + 1
      f <- dhermite(q[j],a,b,m) + f
    }
    j <- j + 1
  }
  return(q)
}