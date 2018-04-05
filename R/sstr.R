sstr <-
function(Data, par="C"){
y.. = mean( crisp.Data(Data, par) )
sstr = 0

for(k in 1:length(n(Data))){
sstr.in = n(Data)[k] * (( mean(crisp.Data(Data, par, factor=k)) - y.. )^2 )
sstr = sstr + sstr.in
}

return( sstr )
}
