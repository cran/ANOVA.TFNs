\name{mstr}
\alias{mstr}
\title{
Mean of Squares between Treatments for crisp observation
}
\description{
This function calculates the Mean of Squares between Treatments (MSTr) for crisp observation.
}
\usage{
mstr(Data, par = "C")
}
\arguments{
  \item{Data}{
a matrix with \eqn{dim=c(n, 4)} and \code{FANOVA.Data} format.
}
  \item{par}{
a character argument with possible values "C", "L" and "R". If par="C", which is default, the cut part will be the cores of TFNs. If par="L" (par="R"), the cut part will be the Left (Right) spreads of TFNs.
}
}

\examples{
data(Data)

mstr( Data )
mstr( Data, "L" )
}

% \keyword{ means.vec }
% \keyword{ plotTFNs }
\keyword{ crisp.Data }
\keyword{ sst }
\keyword{ sstr }
\keyword{ sse }
\keyword{ mstr }
\keyword{ mse }
% \keyword{ f }
% \keyword{ sst.fuzzy }
\keyword{ sstr.fuzzy }
% \keyword{ sse.fuzzy }
\keyword{ mstr.fuzzy }
% \keyword{ mse.fuzzy }
% \keyword{ f.fuzzy }
\keyword{ FANOVA }
\keyword{ ANOVA }

\seealso{
FuzzyNumbers
}
