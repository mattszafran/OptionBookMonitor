globalOptions()

optionFinal = c("Hi", "Hi", 0, 0, 0, 0, 0)

for(i in 1:nrow(optionBook)){
  optionFinal = rbind(optionFinal, rowFxn(optionBook[i,1], optionBook[i,2], optionBook[i,3], optionBook[i,4], optionBook[i,5]))
}
optionFinal = as.data.frame(optionFinal[-1,])

optionFinal = cbind(optionFinal, optionBook[,3], optionBook[,4])

optionFinal$V3 = as.double(optionFinal$V3)
optionFinal$V4 = as.double(optionFinal$V4)
optionFinal$V5 = as.double(optionFinal$V5) * 100 * optionFinal[,8]
optionFinal$V6 = as.double(optionFinal$V6) * 100 * optionFinal[,8]
optionFinal$V7 = as.double(optionFinal$V7) / 100 * optionFinal[,8]


colnames(optionFinal) = c("Underlying", "Option", "CurrPx", "IV", "Delta", "Gamma", "Vega", "Position", "PaidPx")

tempHedges = cbind(hedgesBook[,1], "Eq", getQuote(hedgesBook[,1])[,2], 0, as.double(hedgesBook[,2]), 0, 0, as.double(hedgesBook[,2]), hedgesBook[,3])

colnames(tempHedges) = c("Underlying", "Option", "CurrPx", "IV", "Delta", "Gamma", "Vega", "Position", "PaidPx")

optionFinal = rbind(optionFinal, tempHedges)

for(i in 3:9){
  optionFinal[,i] = as.double(optionFinal[,i])
}

PnL = double(length = nrow(optionFinal))

for(i in 1:nrow(optionFinal)){
  if(optionFinal[i,6] != 0){
    PnL[i] = 100 * (optionFinal[i,3] - optionFinal[i,9]) * optionFinal[i,8]
  } else {
    PnL[i] = (optionFinal[i,3] - optionFinal[i,9]) * optionFinal[i,8]
  }
}

optionFinal = cbind(optionFinal, PnL)

consolShares = aggregate(cbind(optionFinal$Delta, optionFinal$Gamma, optionFinal$Vega, optionFinal$PnL), by=list(Underlying=optionFinal$Underlying), FUN = sum)
colnames(consolShares) = c("Underlying", "Delta", "Gamma", "Vega", "PnL")

currUnder = getQuote(consolShares$Underlying)[,2]

consolDollar = as.data.frame(cbind(consolShares$Underlying, consolShares$Delta*currUnder, consolShares$Gamma*currUnder, consolShares$Vega*currUnder, consolShares$PnL))

for(i in 2:5){
  consolDollar[,i] = format(round(as.double(consolDollar[,i]), digits = 2), big.mark = ",")
}

colnames(consolDollar) = c("Underlying", "Delta", "Gamma", "Vega", "PnL")

writeLines("Aggregate Dollar Greek Exposure and PnL by Name: \n")
print(consolDollar)
  
  
