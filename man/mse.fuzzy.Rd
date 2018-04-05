\name{mse.fuzzy}
\alias{mse.fuzzy}
\title{
Mean of Squares within treatments (Errors) for Triangular Fuzzy observation
}
\description{
This function calculate the Mean of Squares Errors (SSE) for Triangular Fuzzy observation on the basis of m=1,2,3,... (Parchami et al., 2017, 2018).
}
\usage{
mse.fuzzy(Data, m = 1)
}
\arguments{
  \item{Data}{
a matrix with \eqn{dim=c(n, 4)} and \code{FANOVA.Data} format.
}
  \item{m}{
a positive integer number which related to the weight of distance between two cuts of fuzzy numbers, and its default is m=1. For more details see (Parchami et al., 2017, 2018).
}
}

\examples{
data(Data)

mse.fuzzy( Data )  \donttest{# For m=1}
mse.fuzzy( Data, m=2)
}

% \keyword{ means.vec }
% \keyword{ plotTFNs }
% \keyword{ crisp.Data }
% \keyword{ sst }
% \keyword{ sstr }
\keyword{ sse }
% \keyword{ mstr }
\keyword{ mse }
% \keyword{ f }
\keyword{ sst.fuzzy }
\keyword{ sstr.fuzzy }
\keyword{ sse.fuzzy }
\keyword{ mstr.fuzzy }
\keyword{ mse.fuzzy }
\keyword{ f.fuzzy }
\keyword{ FANOVA }
% \keyword{ ANOVA }

\seealso{
FuzzyNumbers
}
