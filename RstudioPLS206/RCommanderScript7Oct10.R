
setwd("/Users/ealaca/Documents/TeachServ/CLASSES/AGR206/PLS206F10/Examples")
filaree <- 
  read.table("/Users/ealaca/Documents/TeachServ/CLASSES/AGR206/PLS206F10/Examples/xmpl_filaree.csv",
   header=TRUE, 
  sep=",", 
  na.strings="NA", 
  dec=".", 
  strip.white=TRUE)
fix(filaree)
library(relimp, 
  pos=4)
showData(filaree, 
  placement='-20+200',
   
  font=getRcmdr('logFont'),
   maxwidth=80, 
  maxheight=30)
summary(filaree)

