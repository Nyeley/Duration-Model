durationLiabilities <-
function(x, y){
  u <- length(x)
  p <- x[1:u]/sum(x[1:u])
  return (sum(p[1:u]*y[1:u]))
}
