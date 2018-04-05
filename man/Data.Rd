\name{Data}
\alias{Data}
\docType{data}

\title{Triangular fuzzy data for batteries lifetime}

\description{Triangular fuzzy data for the batteries lifetime from assembly lines 1, 2 and 3 in an Iranian factory. The sample sizes of the collected triangular fuzzy data are 10, 8 and 11, respectively.
}

\usage{ data(Data) }

\format{
  A matrix with \eqn{4 \times (10+8+11)} numbers on the following fuzzy variable.
 \describe{
    
   \item{the car battery length-life}{The first three columns of matrix are about TFNsnumber of nonconformities }
   \item{factor (assembly lines)}{number of computers inspected}
  
}

}


\details{
The inputted data for FANOVA test must be in a above mentioned special format which we named it \code{FANOVA.Data} format and some of inputted parameters in the most functions of this package must be in \code{FANOVA.Data} format.
In fact, the \code{FANOVA.Data} is a matrix with \eqn{dim=c(n,4)} which can be created/simulated by following two steps:

 Step 1 (saving/creating TFNs of i-th population in a matrix with \eqn{ dim=c(n_i,3))}: 
	Save Triangular Fuzzy Numbers (TFNs) for FANOVA test in length(n) matrix and input each TFN in one row of them such that:
	1st column is for Cores of TFNs (with \code{col.name="C"}),
	2nd column is for Left spreads of TFNs (with \code{col.name="L"}), and
	3nd column is for Right spreads of TFNs (with \code{col.name="R"}).

 Step 2 (Combine all population data in a matrix with \eqn{ dim=c(n,4))}: 
	Put The created matrixes in Step 1, top of each other to create a matrix with \eqn{dim=c(n,4)} such that:
	n=sum(n_i),
	its 1st column is the Core of TFNs (with \code{col.name="C"}),
	its 2nd and 3nd columns are the Left and Right spreads of TFNs (with \code{col.name="L"} and \code{col.name="R"}) respectively, and
	its 4nd column is the factor which shows the population number for each row (with \code{col.name="factor"}).
}


%\source{}

\references{
A. Parchami, M. Mashinchi, C. Kahraman (2018) \emph{An Implication of Fuzzy ANOVA in Vehicle Battery Manufacturing}, Journal of Enterprise Information Management, Preprint.
}

\examples{

data(Data)
attach(Data)
summary(Data)
plotTFNs(Data)
detach(Data)

}

\keyword{ means.vec }
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
\keyword{ mstr.fuzzy }
\keyword{ mse.fuzzy }
\keyword{ f.fuzzy }
\keyword{ FANOVA }
% \keyword{ ANOVA }
