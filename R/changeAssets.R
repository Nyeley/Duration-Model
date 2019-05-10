changeAssets <-
function(x, y, z, r){
  u <- length(x)
  r_shock <- r/(1 + z[1:u])
  individualDelta_A <- -y[1:u] * x[1:u] * r_shock
  return(sum(individualDelta_A))
}
