\name{FANOVA}
\alias{FANOVA}
\title{
Fuzzy ANOVA Test
}
\description{
This function is the main function of package \code{ANOVA.TFNs} it can easily test one-way Analysis of Variance based on Triangular Fuzzy observation on the basis of m=1,2,3,... by (Parchami et al., 2017, 2018) method.
}
\usage{
FANOVA(Data, m = 1, sig.level = 0.05, fig = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
a matrix with \eqn{dim=c(n, 4)} and \code{FANOVA.Data} format.
}
  \item{m}{
a positive integer number which related to the weight of distance between two cuts of fuzzy numbers, and its default is m=1. For more details see (Parchami et al., 2017, 2018).
}
  \item{sig.level}{
the significance level of test with default sig.level=0.05.
}
  \item{fig}{
a logical argument with default TRUE. If fig=TRUE, the membership functions of inputted fuzzy data will be depicted in a figure as presented in \code{plotTFNs} function.
}
}
\details{
Function \code{FANOVA} compute all needed statistics for Fuzzy ANOVA test which introduced in (Parchami et al., 2018) and is able report the FANOVA table and the p-value of FANOVA test.
}
\value{
Function \code{FANOVA} returned the values:
  \item{table}{ Fuzzy ANOVA Table which contains details of FANOVA based on Triangular fuzzy data }
  \item{f_ob}{ The observed value of test statistic in FANOVA test, which sometimes denoted by F-statistic }
  \item{p_value}{ The p-value in FANOVA test which is equal to the above tail area of F-statistic }
  \item{FANOVA_test_result}{ Final report (Accept/Reject) H_0 at the considered significance level }
}

\references{
Gagolewski, M., Caha, J. (2015) \emph{FuzzyNumbers Package}: Tools to deal with fuzzy numbers in R. R package version 0.4-1, https://cran.r-project.org/web/packages=FuzzyNumbers

A. Parchami, M.R. Nourbakhsh, M. Mashinchi (2017) \emph{Analysis of Variance in Uncertain Environments}, Complex & Intelligent Systems, Volume 3, Number 3, 189-196, DOI: 10.1007/s40747-017-0046-8

A. Parchami, M. Mashinchi, C. Kahraman (2018) \emph{An Implication of Fuzzy ANOVA in Vehicle Battery Manufacturing}, Journal of Enterprise Information Management, Preprint.
}

\examples{
# Example 1: 

data(Data)
head(Data)

FANOVA( Data )
FANOVA( Data, fig=FALSE )
FANOVA( Data, m=2, sig.level=0.1 )
FANOVA( Data, fig=FALSE  )$p_value
FANOVA( Data, fig=FALSE  )$FANOVA_test_result
FANOVA( Data, fig=FALSE  )[[1]]  \donttest{ # Only FANOVA Table }


# Example 2: 
t <- 20
f <-p <- c()

for(i in 1:t){
		 f[i] <- FANOVA(Data, m=i, fig=FALSE)$f_ob
		 p[i] <- FANOVA(Data, m=i, fig=FALSE)$p_value
		 }

par(mfcol=c(2,1))
  plot(1:t, f, main="FANOVA test statistics (f) v.s m (1:20)")
  plot(1:t, p, main="p-value v.s m (1:20)")
par(mfcol=c(1,1))

}


% \keyword{ means.vec }
\keyword{ plotTFNs }
% \keyword{ crisp.Data }
\keyword{ sst }
\keyword{ sstr }
\keyword{ sse }
\keyword{ mstr }
\keyword{ mse }
\keyword{ f }
\keyword{ sst.fuzzy }
\keyword{ sstr.fuzzy }
\keyword{ sse.fuzzy }
\keyword{ mstr.fuzzy }
\keyword{ mse.fuzzy }
\keyword{ f.fuzzy }
\keyword{ FANOVA }
\keyword{ ANOVA }

\seealso{
FuzzyNumbers
}
