mstr.fuzzy <-
function(Data, m=1){
mstr.fuzzy = mstr(Data, "C") - (mstr(Data, "L")+mstr(Data, "R"))/(2*(m+1))
return( mstr.fuzzy )
}
