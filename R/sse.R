sse <-
function(Data, par="C"){

for(k in 1:length(n(Data))){
      assign( paste("y_", k, ".", sep = ""), mean(crisp.Data(Data, par, factor=k)) )
}

y_i._vector = c()

for(k in 1:length(n(Data))){
      y_i._vector = c(y_i._vector, rep(get(paste("y_", k, ".", sep = "")), n(Data)[k]))
}

sse = sum( ( crisp.Data(Data, par) - y_i._vector )^2 )
return( sse )
}
