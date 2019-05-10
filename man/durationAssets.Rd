\name{durationAssets}
\alias{durationAssets}
\title{
A function to compute the duration of the assets
}
\description{
Calculates the propotions, the weights, of each element in the asset vector
Divides the individual asset value by the total asset value, element by element
multiples duration by proportion element by element
}
\usage{
durationAssets(x, y)
}
\arguments{
  \item{x}{
The vector containg monetary values of assets
}
  \item{y}{
The vector containing duration of assets
}
}
\details{
no further details
}
\value{
The value returned is the weighted duration of assets
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

durationAssets <- function (x, y)
{
    u <- length(x)
    p <- x[1:u]/sum(x[1:u])
    return(sum(p[1:u] * y[1:u]))
  }
durationAssets(e, f)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
