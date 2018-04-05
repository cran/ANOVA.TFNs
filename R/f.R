f <-
function(Data, par="C"){
f = mstr( Data, par ) / mse( Data, par )
return( f )
}
