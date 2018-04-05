sse.fuzzy <-
function(Data, m=1){
sse.fuzzy = sse(Data, "C") - (sse(Data, "L")+sse(Data, "R"))/(2*(m+1))
return( sse.fuzzy )
}
