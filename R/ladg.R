ladg <-
function(x, y, z, w){
  u <- length(x)
  p <- x[1:u]/sum(x[1:u])
  Da <- (sum(p[1:u]*y[1:u]))
  n <- length(z)
  o <- z[1:n]/sum(z[1:n])
  Dl <- sum(o[1:n]*w[1:n])
  k <- sum(z)/sum(x)
  return(Da - (Dl*k))
}
