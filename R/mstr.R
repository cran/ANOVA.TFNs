mstr <-
function(Data, par="C"){
n = n(Data)
mstr = sstr( Data, par ) / (length(n)-1)
return( mstr )
}
