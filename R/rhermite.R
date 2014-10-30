rhermite <-
function(n, a, b, m=2)
{
  res <- rpois(n, a) + m*rpois(n, b)
  return(res)
}
