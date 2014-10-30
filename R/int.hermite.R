int.hermite <-
  function(x, a, b, m)
  {
    if(a < 0 || b < 0 || m < 0) stop ("parameters should be positive")
    if (floor(m) != m || m < 2) stop ("improper m parameter specification")
    if (x < 0) return(0)
    p <- vector()
    mu <- a + m*b
    d  <- (a+(m^2)*b)/(a+m*b)
    p0 <- exp(mu*(-1+(d-1)/m))
    p[1:(m-1)] <- vapply(1:(m-1), function(k) p0*((mu^k)/(factorial(k)))*(((m-d)/(m-1))^k),1)
    if (x >= m)
    {
      for(k in m:x)
      {
        p[k] <- ifelse(k>m, (mu/(k*(m-1)))*(p[k-m]*(d-1)+p[k-1]*(m-d)), (mu/(k*(m-1)))*(p0*(d-1)+p[k-1]*(m-d)))
      }
    }
    res <- ifelse(x>0, p[x], p0)
    return(res)
  }


