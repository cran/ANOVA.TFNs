\name{ANOVA.TFNs-package}
\alias{ANOVA.TFNs-package}
\alias{ANOVA.TFNs}
\docType{package}
\title{
\packageTitle{ANOVA.TFNs}
}
\description{
Long formulas and complex computations are two major disadvantages of the extended ANOVA approaches for fuzzy data. 
The applied users/practitioners of fuzzy ANOVA can overcome to these challenges/disadvantages, by using package \code{ANOVA.TFNs} when the observed data for ANOVA test are triangular fuzzy numbers.
The most important functions in package \code{ANOVA.TFNs} is \code{FANOVA}. Using \code{FANOVA} is easy since after import fuzzy observation (with format \code{FANOVA.Data}), the \code{FANOVA} function easily export:
(1) draw separately the membership functions of TFNs for each population in one figure,
(2) FANOVA Table (which include details of fuzzy ANOVA based on Triangular fuzzy data,
(3) the observed value of the FANOVA test statistics,
(4) the p-value of FANOVA test, and 
(5) FANOVA test result at the considered significance level.
}
\details{
The inputted data for FANOVA test must be in a special format which we named it \code{FANOVA.Data} format and some of inputted parameters in several functions of this package (such as \code{means.vec}, \code{FANOVA}, \code{plotTFNs}, \code{crisp.Data}, \code{f.fuzzy} and ...) must be in \code{FANOVA.Data} format.
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

See Example 1 in bellow.
}

\author{
Abbas Parchami
}

\references{
Gagolewski, M., Caha, J. (2015) \emph{FuzzyNumbers Package}: Tools to deal with fuzzy numbers in R. R package version 0.4-1, https://cran.r-project.org/web/packages=FuzzyNumbers

A. Parchami, M.R. Nourbakhsh, M. Mashinchi (2017) \emph{Analysis of Variance in Uncertain Environments}, Complex & Intelligent Systems, Volume 3, Number 3, 189-196, DOI: 10.1007/s40747-017-0046-8

A. Parchami, M. Mashinchi, C. Kahraman (2018) \emph{An Implication of Fuzzy ANOVA in Vehicle Battery Manufacturing}, Journal of Enterprise Information Management, Preprint.
}

\seealso{
FuzzyNumbers
}
\examples{
## Example 1 (input TFNs in \code{FANOVA.Data} format)
data(Data)
head(Data)
Data[,"R"]
Data[,"C"][Data[,"factor"]==2]  \donttest{ # Or Data[,"C"][Data[,4]==2] }


## Example 2:  
FANOVA( Data, m=2, sig.level=0.1 )


## Example 3: 
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
