\name{plotTFNs}
\alias{plotTFNs}

\title{
Plotting Triangular Fuzzy Numbers
}
\description{
The exported figure of function \code{plotTFNs} is a \code{length(n)} figures which are putted top of each other, where i-th figure shows the membership functions of TFNs in i-th population, for i=1,2,...,length(n).
}
\usage{
plotTFNs(Data, means.in.plot = TRUE, col = "gray63", lwd = 2, lty = 1, xlim  )
}
\arguments{
  \item{Data}{
a matrix with \eqn{dim=c(n, 4)} and \code{FANOVA.Data} format.
}
  \item{means.in.plot}{
a logical argument with default TRUE. If means.in.plot=TRUE, the membership function of the means of TFNs depicted in output figure for each population. Elsewhere, the membership function of the means of TFNs not plotted.
}
  \item{col}{
the color of lines in plot
}
  \item{lwd}{
the width of lines in plot
}
  \item{lty}{
the type of lines in plot
}
  \item{xlim}{
the limits of x-axis for plots
}
}

\examples{
data(Data)

\donttest{
  plotTFNs(Data)
  plotTFNs(Data, means.in.plot=FALSE)
  plotTFNs(Data, col=2, xlim=c(0.8,4), lty=6)
}
}

\keyword{ means.vec }
% \keyword{ plotTFNs }
% \keyword{ crisp.Data }
% \keyword{ sst }
% \keyword{ sstr }
% \keyword{ sse }
% \keyword{ mstr }
% \keyword{ mse }
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
