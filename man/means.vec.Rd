\name{means.vec}
\alias{means.vec}

\title{
Computing means vector for each population
}
\description{
This function computes the means of each population. It must be noted that the means of several TFNs is a TFN too.
}
\usage{
means.vec(Data)
}
\arguments{
  \item{Data}{
a matrix with \eqn{dim=c(n, 4)} and \code{FANOVA.Data} format.
}
}

\value{
A matrix with dim=c(length(n), 3), where each of its rows show the mean of TF observation in relevant population.
}

\examples{
data(Data)

means.vec(Data)

\donttest{ # Computing the waighted mean of mean populations: }
data.. = ( means.vec(Data)[1,]*n(Data)[1] + 
	   means.vec(Data)[2,]*n(Data)[2] +
	   means.vec(Data)[3,]*n(Data)[3] ) / sum(n(Data))
round(data..,3)
}

% \keyword{ means.vec }
\keyword{ plotTFNs }
% \keyword{ crisp.Data }
% \keyword{ sst }
% \keyword{ sstr }
% \keyword{ sse }
% \keyword{ mstr }
% \keyword{ mse }
% \keyword{ f }
\keyword{ sst.fuzzy }
\keyword{ sstr.fuzzy }
\keyword{ sse.fuzzy }
% \keyword{ mstr.fuzzy }
% \keyword{ mse.fuzzy }
% \keyword{ f.fuzzy }
\keyword{ FANOVA }
% \keyword{ ANOVA }

\seealso{
FuzzyNumbers
}
