\name{f.fuzzy}
\alias{f.fuzzy}
\title{
Test statistics in ANOVA test for Triangular Fuzzy observation
}
\description{
This function calculate the observed test statistics (f) for Triangular Fuzzy observation on the basis of m=1,2,3,... (Parchami et al., 2017, 2018).
}
\usage{
f.fuzzy(Data, m = 1)
}
\arguments{
  \item{Data}{
a matrix with \eqn{dim=c(n, 4)} and \code{FANOVA.Data} format.
}
  \item{m}{
a positive integer number which related to the weight of distance between two cuts of fuzzy numbers, and its default is m=1. For more details see (Parchami et al., 2017, 2018).
}
}

\references{
A. Parchami, M.R. Nourbakhsh, M. Mashinchi (2017) \emph{Analysis of Variance in Uncertain Environments}, Complex & Intelligent Systems, Volume 3, Number 3, 189-196, DOI: 10.1007/s40747-017-0046-8

A. Parchami, M. Mashinchi, C. Kahraman (2018) \emph{An Implication of Fuzzy ANOVA in Vehicle Battery Manufacturing}, Journal of Enterprise Information Management, Preprint.
}

\examples{
data(Data)

f.fuzzy( Data )
f.fuzzy( Data, m=3 )
}

\keyword{ means.vec }
\keyword{ plotTFNs }
% \keyword{ crisp.Data }
% \keyword{ sst }
% \keyword{ sstr }
% \keyword{ sse }
% \keyword{ mstr }
% \keyword{ mse }
\keyword{ f }
\keyword{ sst.fuzzy }
\keyword{ sstr.fuzzy }
\keyword{ sse.fuzzy }
\keyword{ mstr.fuzzy }
\keyword{ mse.fuzzy }
% \keyword{ f.fuzzy }
\keyword{ FANOVA }
\keyword{ ANOVA }

\seealso{
FuzzyNumbers
}
