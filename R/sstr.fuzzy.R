sstr.fuzzy <-
function(Data, m=1){
sstr.fuzzy = sstr(Data, "C") - (sstr(Data, "L")+sstr(Data, "R"))/(2*(m+1))
return( sstr.fuzzy )
}
