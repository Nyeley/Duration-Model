aggregateDeltaE <-
function(x, y, z, r1, h, i, j, r2){
  u <- length(x)
  r_shock_1 <- r1/(1 + z[1:u])
  individualDelta_A <- -y[1:u] * x[1:u] * r_shock_a
  d <- sum(individualDelta_A)
  n <- length(h)
  r_shock_2 <- r2/(1 + j[1:n])
  individualDelta_L <- -i[1:n] * h[1:n] * r_shock_l
  e <- sum(individualDelta_L)
  return(d - e)
}
