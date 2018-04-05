f.fuzzy <-
function(Data, m=1){
f.fuzzy = mstr.fuzzy( Data, m ) / mse.fuzzy( Data, m )
return( f.fuzzy )
}
