\name{sse.fuzzy}
\alias{sse.fuzzy}
\title{
Sum of Squares within treatments (Errors) for Triangular Fuzzy observation
}
\description{
This function calculate Sum of Squares Errors (SSE) for Triangular Fuzzy observation on the basis of m=1,2,3,... (Parchami et al., 2017, 2018).
}
\usage{
sse.fuzzy(Data, m = 1)
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

# Example 1: 
data(Data)

sse.fuzzy( Data )  \donttest{ # For m=1 }
sse.fuzzy( Data, m=2)

# Example 2  ( Checking relation  sst = sstr + sse  for different m):
sst.fuzzy(Data) == sstr.fuzzy(Data) + sse.fuzzy(Data)
sst.fuzzy(Data, m=3) == sstr.fuzzy(Data, m=3) + sse.fuzzy(Data, m=3)

for(m in 1:10) 
  print( round(sst.fuzzy(Data, m), 10) == round(sstr.fuzzy(Data, m) + sse.fuzzy(Data, m), 10) )
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
