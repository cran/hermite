phermite <- function(q, a, b, m=2, lower.tail=TRUE)
{
  res <- vapply(q, function(x) sum(dhermite(0:x, a, b, m)), 1)
  if (lower.tail == FALSE) res <- 1 - res
  return(res)
}
