# from https://stackoverflow.com/questions/31544726/how-to-create-frequency-tables-with-xtabs

print_prop_xtab<-function(xtab,fmt='%s (%1.2f%%)',big.mark=',',na.print="NA",...) {
  ## PURPOSE: print an xtab with percentages in
  ## parentheses in addition to counts at every value.
  ## TODO: alignment the percentages at the decimal point.
  xtab.am<-addmargins(xtab)
  xtab.pt.am<-addmargins(prop.table(xtab,...))
  res<-sprintf(fmt,format(xtab.am,big.mark=big.mark),100*xtab.pt.am)
  attributes(res)<-attributes( xtab.am)
  print(quote=FALSE
        ,na.print=na.print
        ,res)
}