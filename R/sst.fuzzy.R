sst.fuzzy <-
function(Data, m=1){
sst.fuzzy = sst(Data, "C") - (sst(Data, "L")+sst(Data, "R"))/(2*(m+1))
return( sst.fuzzy )
}
