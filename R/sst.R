sst <-
function(Data, par="C"){
y.. = mean( crisp.Data(Data, par) )
sst = sum( (crisp.Data(Data, par) - y..)^2 )
return( sst )
}
