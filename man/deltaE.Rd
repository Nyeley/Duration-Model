\name{deltaE}
\alias{deltaE}
\title{
Function computes the change in equity due to a predicted change in interest rate
}
\description{
Computes the duration of assets and liabilites
Computes the market measure of risk and the leverage adjusted duration gap
Computes the total asset value
Combines all previous computations to calculate change in equity
}
\usage{
deltaE(x, y, z, w, r)
}
\arguments{
  \item{x}{
The vector containg monetary values of assets
}
  \item{y}{
The vector containing duration of assets
}
  \item{z}{
The vector containing the monetary value of liabilities
}
  \item{w}{
The vector containing the duration of liabilities
}
  \item{r}{
The portential change in interest rate for both liabilities and assets
}
}
\details{
Spread between interest rate of assets and interest rate of liabilities must be 0
}
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
\note{
no further notes
}
\seealso{
n/a
}
\examples{
e <- c(750, 500, 250) ## asset values
f <- c(2.5, 0.5, 1) ## asset durations
a <- c(300, 175, 50) ## liability values
b <- c(0.75, 5, 3.25) ## liability durations


deltaE <- function (x, y, z, w, r)
{
    u <- length(x)
    p <- x[1:u]/sum(x[1:u])
    Da <- (sum(p[1:u] * y[1:u]))
    n <- length(z)
    o <- z[1:n]/sum(z[1:n])
    Dl <- sum(o[1:n] * w[1:n])
    k <- sum(z)/sum(x)
    ladg <- Da - (Dl * k)
    A <- sum(x)
    return(-ladg * A * r)
  }
deltaE (e, f, a, b)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
