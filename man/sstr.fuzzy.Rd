\name{sstr.fuzzy}
\alias{sstr.fuzzy}
\title{
Sum of Squares between Treatments for Triangular Fuzzy observation
}
\description{
This function calculates Sum of Squares between Treatments (SSTr) for Triangular Fuzzy observation on the basis of m=1,2,3,... (Parchami et al., 2017, 2018).
}
\usage{
sstr.fuzzy(Data, m = 1)
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

sstr.fuzzy( Data )  \donttest{ # For m=1 }
sstr.fuzzy( Data, m=2)
}

% \keyword{ means.vec }
% \keyword{ plotTFNs }
% \keyword{ crisp.Data }
% \keyword{ sst }
\keyword{ sstr }
% \keyword{ sse }
% \keyword{ mstr }
% \keyword{ mse }
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
