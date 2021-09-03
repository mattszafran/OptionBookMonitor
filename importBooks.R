optionBook <- read.csv("DerivativesBook.csv")
hedgesBook <- read.csv("HedgesBook.csv")

namesInBook <- unique(optionBook[,1])

dte = function(){
    nextFri = function(x){
      return(7 * ceiling(as.numeric(x-1)/7) + as.Date(1))
    }
  return(as.numeric(Sys.Date() - nextFri(Sys.Date())))
}

for(i in 1:nrow(optionBook)){
  if(dte() == 0){
    optionBook[i,5] = 0.5
  } else {
    optionBook[i,5] = dte()
  }
}

colnames(optionBook) = c(colnames(optionBook)[1:4], "DTE")