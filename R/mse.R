mse <-
function(Data, par="C"){
n = n(Data)
mse = sse( Data, par ) / (sum(n)-length(n))
return( mse )
}
