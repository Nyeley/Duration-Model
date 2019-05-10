\name{durationLiabilities}
\alias{durationLiabilities}
\title{
A function to compute the duration of the liabilities
}
\description{
Calculates the propotions, the weights, of each element in the liability vector
Divides the individual liability value by the total liabilities value, element by element
Multiples duration by proportion, element by element
}
\usage{
durationLiabilities(x, y)
}
\arguments{
  \item{x}{
The vector containg monetary values of liabilities
}
  \item{y}{
The vector containing duration of liabilities
}
}
\details{
no further details
}
\value{
The value returned is the weighted duration of liabilities
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

durationLiabilities <- function (x, y)
{
    u <- length(x)
    p <- x[1:u]/sum(x[1:u])
    return(sum(p[1:u] * y[1:u]))
  }
durationLiabilities(a, b)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
