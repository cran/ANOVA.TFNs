plotTFNs <-
function(Data, means.in.plot=TRUE, col='gray63', lwd=2, lty=1
    , xlim=c(min(Data[,"C"]-Data[,"L"]), max(Data[,"C"]+Data[,"R"]) )){

par(mfcol=c(length(n(Data)),1), mai=c(.3, .3, .1, .1) )    #length(n) graphs per page

for(k in 1:length(n(Data)))
{
sum = TriangularFuzzyNumber(0, 0, 0)

for(i in 1:sum(n(Data)))
   {
    if(Data[i,"factor"]==k)
{
  X = TriangularFuzzyNumber(  Data[i,"C"] - Data[i,"L"], 
Data[i,"C"], 
Data[i,"C"] + Data[i,"R"]  )
  FuzzyNumbers::plot( X, add = (Data[i,"factor"]==Data[ifelse(i==1,sum(n(Data)),i-1),"factor"]), xlim=xlim, col=col, lwd=lwd, lty=lty )
}
   }

if( means.in.plot==TRUE ){
raw <- means.vec(Data)[k,]
assign( paste("x.bar", k, sep = ""), TriangularFuzzyNumber(raw[1]-raw[2], raw[1], raw[1]+raw[3]) )
FuzzyNumbers::plot( get( paste("x.bar", k, sep="")), xlim=supp(get(paste("x.bar", k, sep = ""))), lw=6, col=1, lty=1, add=TRUE )
}

}

par(mfcol=c(1,1), mai=c(.5, .5, .2, .2) )    #Reset display
}
