mse.fuzzy <-
function(Data, m=1){
mse.fuzzy = mse(Data, "C") - (mse(Data, "L")+mse(Data, "R"))/(2*(m+1))
return( mse.fuzzy )
}
