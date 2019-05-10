# durationGapFunctions
# Author: Tetteng Gaduel, Phoebe Staenz

durationAssets <- function(x, y){
  u <- length(x)
  p <- x[1:u]/sum(x[1:u])
  return(sum(p[1:u]*y[1:u]))
}

durationLiabilities <- function(x, y){
  u <- length(x)
  p <- x[1:u]/sum(x[1:u])
  return (sum(p[1:u]*y[1:u]))
}

changeAssets <- function(x, y, z, r){
  u <- length(x)
  r_shock <- r/(1 + z[1:u])
  individualDelta_A <- -y[1:u] * x[1:u] * r_shock
  return(sum(individualDelta_A))
}

changeLiabilities <- function(x, y, z, r){
  u <- length(x)
  r_shock <- r/(1 + z[1:u])
  individualDelta_L <- -y[1:u] * x[1:u] * r_shock
  print(sum(individualDelta_L))
}

aggregateDeltaE <- function(x, y, z, r1, h, i, j, r2){
  u <- length(x)
  r_shock_1 <- r1/(1 + z[1:u])
  individualDelta_A <- -y[1:u] * x[1:u] * r_shock_1
  d <- sum(individualDelta_A)
  n <- length(h)
  r_shock_2 <- r2/(1 + j[1:n])
  individualDelta_L <- -i[1:n] * h[1:n] * r_shock_2
  e <- sum(individualDelta_L)
  return(d - e) 
}

ladg <- function(x, y, z, w){
  u <- length(x)
  p <- x[1:u]/sum(x[1:u])
  Da <- (sum(p[1:u]*y[1:u]))
  n <- length(z)
  o <- z[1:n]/sum(z[1:n])
  Dl <- sum(o[1:n]*w[1:n])
  k <- sum(z)/sum(x)
  return(Da - (Dl*k))
}

deltaE <- function(x,y,z,w,r){
  u <- length(x)
  p <- x[1:u]/sum(x[1:u])
  Da <- (sum(p[1:u]*y[1:u]))
  n <- length(z)
  o <- z[1:n]/sum(z[1:n])
  Dl <- sum(o[1:n]*w[1:n])
  k <- sum(z)/sum(x)
  ladg <- Da - (Dl*k)
  A <- sum(x)
  return(-ladg * A * r)
}

