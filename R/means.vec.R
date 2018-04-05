means.vec <-
function(Data){

means.vec <- matrix(, nrow=length(n(Data)), ncol=3)
dimnames(means.vec) = list( 1:length(n(Data)), c("C","L","R")) 

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
  sum <- sum + X
}
   }

assign( paste("x.bar", k, sep = ""), (1/n(Data)[k]) * sum )
#print( get( paste("x.bar", k, sep="") ) )
means.vec[k,] <- c( core(get(paste("x.bar", k, sep="")))[1], 
  core(get(paste("x.bar", k, sep="")))[1] -  supp(get(paste("x.bar", k, sep="")))[1], 
  supp(get(paste("x.bar", k, sep="")))[2] -  core(get(paste("x.bar", k, sep="")))[1]  )
}
return( means.vec )
}
