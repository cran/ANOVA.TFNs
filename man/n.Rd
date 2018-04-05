\name{n}
\alias{n}
\title{
The sample size of factor levels
}
\description{
This function calculate the sample size for each factor level based on fuzzy data.
}
\usage{
n(Data)
}
\arguments{
  \item{Data}{
a matrix with \eqn{dim=c(n, 4)} and \code{FANOVA.Data} format.
}
}

\examples{
data(Data)
n(Data)    \donttest{ # Number of TFNs in three populations }
n(Data)[2] \donttest{ # Number of TFNs in the second population }
}

% \keyword{ means.vec }
% \keyword{ plotTFNs }
%\keyword{ crisp.Data }
%\keyword{ sst }
%\keyword{ sstr }
%\keyword{ sse }
%\keyword{ mstr }
%\keyword{ mse }
% \keyword{ f }
% \keyword{ sst.fuzzy }
% \keyword{ sstr.fuzzy }
% \keyword{ sse.fuzzy }
% \keyword{ mstr.fuzzy }
% \keyword{ mse.fuzzy }
\keyword{ f.fuzzy }
\keyword{ FANOVA }
\keyword{ ANOVA }

\seealso{
FuzzyNumbers
}
