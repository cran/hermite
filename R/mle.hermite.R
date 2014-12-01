mle.hermite <- function(x, a=NA, b=NA, m=NA)
{
  p0 <- length(x[x==0])/length(x)
  mu <- mean(x)
  d  <- var(x)/mean(x)
  if (is.na(m)) m  <- ifelse(p0!=0, round((d-1)/(1+log(p0)/mu)), m)
  if (is.na(m) | m < 2) m <- 2
  
  llike <- function(init.values)
  {
    return(-1*sum(log(dhermite(x, init.values[1], init.values[2], m))))
  }
  
  fm <- 0
  for (i in 1:length(x))
  {
    mult <- 1
    for (j in 0:(m-1))
    {
      mult <- mult*(x[i]-j)
    }
    fm <- fm + mult
  }
  fm <- fm/length(x)
  if (fm <= (mean(x))^m)
  {
    pars    <- c(mean(x), 0, 2)
    value   <- llike(c(pars[1], pars[2]))
    hessian <- length(x)/mean(x)
    w    <- 2*(llike(c(pars[1], 0))-llike(c(pars[1], pars[2])))
    pval <- pchisq(w, 1, lower.tail=F)
    warning("MLE equations have no solution")
    return(list(pars, value, hessian, w, pval))
  }
  if (is.na(b)) b <- (var(x)-mean(x))/(m^2-m)        
  if (is.na(a)) a <- mean(x)-m*b 
  init.values <- c(a, b)
  op  <- optim(par=init.values, fn=llike, method="L-BFGS-B", lower=c(0.01, 0.01), hessian=TRUE)
  pars    <- c(op$par[1:2], m)
  value   <- op$value
  hessian <- op$hessian
  w    <- 2*(llike(c(pars[1], 0))-llike(c(pars[1], pars[2])))
  pval <- pchisq(w, 1, lower.tail=F)/2
  res     <- list(pars, value, hessian, w, pval)
  return(res)
}