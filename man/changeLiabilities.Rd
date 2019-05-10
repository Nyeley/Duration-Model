\name{changeLiabilities}
\alias{changeLiabilities}
\title{
Function to compute aggregate change in liabilities due to shock in level of interest rate
}
\description{
The shock in the level of the interest rate is determined by dividing the potential change in i.r. by one plus the existing yield
Shock is computed element by element, which implies that each liability in the asset vector has a different interest rate shock
Computes the affect the interest rate change has on the individual liability's value
Aggregates the individual changes to the total change in liabilities value
}
\usage{
changeLiabilities(x, y, z, r)
}
\arguments{
  \item{x}{
Vector containing the liability values
}
  \item{y}{
Vector containing the liability durations
}
  \item{z}{
Vector containing the liability yield
}
  \item{r}{
The change in interest rate of liabilities
}
}
\details{
no further details
}
\value{
The value returned is the monetary amount of change in liabilities value
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
a <- c(300, 175, 50) ## liability values
b <- c(0.75, 5, 3.25) ## liability durations
c <- c(0.001, 0.35, 0.25) ## liability yields

changeLiabilities <- function (x, y, z, r)
{
    u <- length(x)
    r_shock <- r/(1 + z[1:u])
    individualDelta_L <- -y[1:u] * x[1:u] * r_shock
    print(sum(individualDelta_L))
  }
changeLiabilities(a, b, c, 0.0025)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
