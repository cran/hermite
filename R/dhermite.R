dhermite <-
  function(x, a, b, m=2)
  {
    pr <- function(x)
    {
      if (x%%1 != 0)
      {
        warning("non-integer x=", x)
        return(0)
      }else{
        if (a > 20 | b > 20) return(dnorm(x, a+m*b, sqrt(a+(m^2)*b)))
        if (a <= 20 & b <= 20) return(int.hermite(x, a, b, m))
      }
    }
    res <- vapply(x, pr, 1)
    return(res)
  }
