crisp.Data <-
function(Data, par="C", factor='all'){
  if(factor=='all')
{
crisp.Data = as.vector( Data[,par] )
return( crisp.Data )
}
  else{
crisp.Data = as.vector( Data[,par][Data[,"factor"]==factor] )
return( crisp.Data )
}
}
