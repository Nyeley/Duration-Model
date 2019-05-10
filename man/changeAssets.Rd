\name{changeAssets}
\alias{changeAssets}
\title{
Function to compute aggregate change in assets due to shock in level of interest rate
}
\description{
The shock in the level of the interest rate is determined by dividing the potential change in i.r. by one plus the existing yield
Shock is computed element by element, which implies that each asset in the asset vector has a different interest rate shock
Computes the affect the interest rate change has on the individual asset's value
Aggregates the individual changes to the total change in asset value
}
\usage{
changeAssets(x, y, z, r)
}
\arguments{
  \item{x}{
Vector containing the asset values
}
  \item{y}{
Vector containing the asset durations
}
  \item{z}{
Vector containing the asset yields
}
  \item{r}{
The change in interest rate of assets
}
}
\details{
no further details
}
\value{
The value returned is the monetary amount of change in asset value
}
\references{
A. Saunders and M. Cornett, Financial Institution Management, 8th International Edition, McGraw Hill
}
\author{
Tetteng Gaduel
Phoebe Staenz
}
\note{
no further notes
}
\seealso{
n/a
}
\examples{
e <- c(750, 500, 250) ## asset values
f <- c(2.5, 0.5, 1) ## asset durations
g <- c(0.03, 0.5, 0.15) ## asset yields
r <- 0.0015

changeAssets <- function (x, y, z, r)
{
    u <- length(x)
    r_shock <- r/(1 + z[1:u])
    individualDelta_A <- -y[1:u] * x[1:u] * r_shock
    return(sum(individualDelta_A))
  }
changeAssets(e, f, g, r)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
