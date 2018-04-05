FANOVA <-
function(Data, m=1, sig.level=0.05, fig=TRUE){

f = f.fuzzy( Data, m )
q = qf(1-sig.level, df1=length(n(Data))-1, df2=sum(n(Data))-length(n(Data)) )
if(f>q){ result <- "Reject H0 at the considered significance level" }
   else{ result <- "Accept H0 at the considered significance level" }

p_val = pf(f, df1=length(n(Data))-1, df2=sum(n(Data))-length(n(Data)), lower.tail=FALSE)

if(fig==TRUE) plotTFNs(Data)

  table <- c(noquote( paste( " Table: Details of fuzzy ANOVA (FANOVA) based on Triangular fuzzy data ")),
noquote( paste( " ___________________________________________________________________________ ")),
noquote( paste( "|      Source of     |   Sum of    | Degrees of |    Mean     | F-statistic |")),
noquote( paste( "|      Variation     |   Squares   |   freedom  |   Squares   |             |")),
noquote( paste( "|--------------------|-------------|------------|-------------|-------------|")),
noquote( paste( "| Between Treatments | sstr =", round(sstr.fuzzy(Data, m), 2), "|  df =", length(n(Data))-1, "   | mstr =", round(mstr.fuzzy(Data, m), 2), "|             |")),
noquote( paste( "|                    |             |            |             |  f =", round(f.fuzzy(Data, m), 3),"|")),
noquote( paste( "| Within Treatments  |  sse =", round(sse.fuzzy(Data, m), 2), "|  df =", sum(n(Data))-length(n(Data)), "  |  mse =", round(mse.fuzzy(Data, m), 2), "|             |")),
noquote( paste( "|      (Errors)      |             |            |             |             |")),
noquote( paste( "|--------------------|-------------|------------|-------------|-------------|")),
noquote( paste( "|       Total        |  sst =", round(sst.fuzzy(Data, m), 2), "|  df =", sum(n(Data))-1, "  |             |             |")),
noquote( paste( "|____________________|_____________|____________|_____________|_____________|")),
noquote( paste( "   ")),
noquote( paste( "   ")) )

return( list( 
 table,
 noquote("The observed value of the FANOVA test statistics is eaual to:"), 
 "f_ob" = f, 
 noquote("The p-value (i.e., the above tail area of F-statistic) is eaual to:"), 
 p_value = p_val, 
 "FANOVA test result" = result
            ) 
 )

}
