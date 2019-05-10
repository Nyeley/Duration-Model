\name{ladg}
\alias{ladg}
\title{
Function computes the leverage adjusted duration gap
}
\description{
Computes the proportion vectors for assets and liabilities
Computes the duration of assets and liabilities
Computes the market risk measure, dividing total liabilities by total assets
Computes the leverage adjusted duration gap by subtracting the scaled duration of liabilities from the duration of assets
}
\usage{
ladg(x, y, z, w)
}
\arguments{
  \item{x}{
The vector containing monetary value of assets
}
  \item{y}{
The vector containing duration of assets
}
  \item{z}{
The vector containing monetary values of liabilities
}
  \item{w}{
The vector containing the duration of liabilities
}
}
\details{
no further details
}
\value{
The value returned is the gap in number of years
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
e <- c(750, 500, 250) ## asset values
f <- c(2.5, 0.5, 1) ## asset durations

ladg <- function (x, y, z, w)
{
    u <- length(x)
    p <- x[1:u]/sum(x[1:u])
    Da <- (sum(p[1:u] * y[1:u]))
    n <- length(z)
    o <- z[1:n]/sum(z[1:n])
    Dl <- sum(o[1:n] * w[1:n])
    k <- sum(z)/sum(x)
    return(Da - (Dl * k))
  }
ladg(e, f, a, b)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
