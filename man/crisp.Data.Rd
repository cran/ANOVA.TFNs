\name{crisp.Data}
\alias{crisp.Data}
\title{
Cutter a part of Data
}
\description{
This function cut a part of Data (with format "FANOVA.Data") to obtain a vector of crisp data. The cutter part can be related part for a special factor level, or can be related part for core's (or e.g. left spreads) of TFNs.
}
\usage{
crisp.Data(Data, par = "C", factor = "all")
}
\arguments{
  \item{Data}{
a matrix with \eqn{dim=c(n, 4)} and \code{FANOVA.Data} format.
}
  \item{par}{
a character argument with possible values "C", "L" and "R". If par="C", which is default, the cut part will be the cores of TFNs. If par="L" (par="R"), the cut part will be the Left (Right) spreads of TFNs.
}
  \item{factor}{
its default is "all" which means all factor levels. If "factor=i", the cut part will be relevant to the i-th population where i=1,2,...,length(n).
}
}
\details{
One can consider both factor and par arguments to cut a part of "FANOVA.Data" format.
}
\value{
a part of Data matrix with format "FANOVA.Data"; see examples.
}

\examples{
data(Data)
crisp.Data(Data)		\donttest{ # Or crisp.Data(Data, par="C", factor="all") }
crisp.Data(Data, "L")
crisp.Data(Data, "L", factor=2)

}

% \keyword{ means.vec }
% \keyword{ plotTFNs }
% \keyword{ crisp.Data }
% \keyword{ sst }
% \keyword{ sstr }
% \keyword{ sse }
% \keyword{ mstr }
% \keyword{ mse }
 \keyword{ f }
% \keyword{ sst.fuzzy }
% \keyword{ sstr.fuzzy }
% \keyword{ sse.fuzzy }
% \keyword{ mstr.fuzzy }
% \keyword{ mse.fuzzy }
% \keyword{ f.fuzzy }
\keyword{ FANOVA }
\keyword{ ANOVA }

\seealso{
FuzzyNumbers
}
