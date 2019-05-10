changeLiabilities <-
function(x, y, z, r){
  u <- length(x)
  r_shock <- r/(1 + z[1:u])
  individualDelta_L <- -y[1:u] * x[1:u] * r_shock
  print(sum(individualDelta_L))
}
