\name{aggregateDeltaE}
\alias{aggregateDeltaE}
\title{Function that computes the change in equity when interest spread >0}
\description{
Calculates the effect of the predicted interest rate changes on equity by subtracting the aggregate change in assets by the aggregate change in liabilities
}
\usage{
aggregateDeltaE(x, y, z, r1, h, i, j, r2)
}
\arguments{
  \item{x}{
The vector containg monetary values of assets
}
  \item{y}{
The vector containing duration of assets
}
  \item{z}{
The vector containing the yield of assets
}
  \item{r1}{
The potential change in interest rate for the assets
}
  \item{h}{
The vector containing the monetary value of liabilities
}
  \item{i}{
The vector containing the duration of liabilities
}
  \item{j}{
The vector containing the yield of liabilities
}
  \item{r2}{
The portential change in interest rate for liabilities
}
}
\details{No further detail}
\value{
The value returned is a monetary value, which depends on the predetermined magnitude and currency in the monetary value vectors.
}
\references{
A. Saunders and M. Cornett, Financial Institution Management, 8th International Edition, McGraw Hill
}
\author{
Tetteng Gaduel
Phoebe Staenz
}
\note{no further notes}
\seealso{no further notes}
\examples{
e <- c(750, 500, 250) ## asset values
f <- c(2.5, 0.5, 1) ## asset durations
g <- c(0.03, 0.5, 0.15) ## asset yields
a <- c(300, 175, 50) ## liability values
b <- c(0.75, 5, 3.25) ## liability durations
c <- c(0.001, 0.35, 0.25) ## liability yields

aggregateDeltaE <- function (x, y, z, r1, h, i, j, r2)
{
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
aggregateDeltaE(e, f, g, 0.0015, a, b, c, 0.0025)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
