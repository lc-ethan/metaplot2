## metplotfunction
metaplotfunction <- function(path){
  require(rmeta, meta)
  data(Olkin95)
  data(Fleiss93cont)
  source(paste(path,"/meta2DF.r", sep=""))
  source(paste(path,"/metaDF2MatrixFunctions.r", sep=""))
  source(paste(path,"/makeDesc.r", sep=""))
}