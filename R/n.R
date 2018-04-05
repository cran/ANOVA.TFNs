n <-
function(Data){
n = numeric( max(Data[,4]) )
for (i in 1:max(Data[,4]))  n[i] <- length( which(Data[,4]==i) )
return(n)
}

