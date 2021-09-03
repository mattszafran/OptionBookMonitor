# This doc contains a number of the helper functions that need only be loaded into the global environment once. 
# Once they have been loaded, book exposure can be refreshed without reloading them, so long as you remain in
# the same R session. Exiting R sessions on Terminal and reopening it will force this doc to be run again. 
# Has been tested on Mac OSX 11.2.3 on an Intel i3, as well as Windows 10 Spring 2021 Update on an Intel i5 7400
# NOT TESTED FOR M1 MACS

library(quantmod)
library(utils)

## QuantMod provides the all-in-one function for retrieving the options chain and underlying px that I am
## too lazy to write myself because JSON sucks. 
## Utils is needed for the optimization function to approximate IVM, since this cannot be done analytically. 

options(warn = -1) ## AWFUL practice FYI but importing CSVs can often lead to warnings if done cross-system.

# Sets some useful global variables, namely wd, risk free rate, days in a year
globalOptions <- function(){
  setwd("~/Desktop/MAIF Derivatives Book")
  assign("rf_rate", .0022, envir = .GlobalEnv) #May need to be updated, I converted the 1YR LIBOR to continuous. In the future use SOFR or something idk
  assign("days", 365.25, envir = .GlobalEnv)
}

# Function to match the info in DerivativesBook CSV to the identifying strings returned from QuantMod's chain table. 
stringMatch <- function(symbol, option, dte){
  finalString = ""
  dateString = gsub("-", "", substr(toString(Sys.Date()+dte), 3, 10))
  putCallFlag = substr(option, nchar(option), nchar(option))
  strikeStrDec = paste0(substr(option, 1, nchar(option)-1), "000")
  strikeStrDec = paste0(paste(rep("0",8-nchar(strikeStrDec)), collapse = ""), strikeStrDec)
  return(paste0(symbol, dateString, putCallFlag, strikeStrDec))
}



getIV <- function(price, underlying, strike, dyld, rf, time, putCall){
  ## Essentially works through a less-efficient (but similar) method to Newton-Raphson. 
  ## Load in generalized BSM as our target function for price (maybe later implementations w/binom trees?)
  ## then tell it to tweak IV around (subject to bounds of [0%, 1,000%]) until we match the price within
  ## an exponentially small tolerance. This must be our IV, since all other parameters are known. 
  
  gBSMMin <- function(price, S, K, sigma, r, q, ttm, type){
    #S = stock price
    #X = strike price
    #sigma = volatility
    #r = risk free interest rate
    #q = dividend yield
    #ttm = time to maturity in days
    #type = option type
    
    b <- r - q
    t <- ttm/365.25
    
    d1 <- (log(S / K) + (b + sigma^2 / 2) * t) / (sigma * sqrt(t))
    d2 <- d1 - sigma * sqrt(t)
    
    if(type == "call"){
      price1 <- S * exp((b - r) * t) * pnorm(d1) - K * exp(-r * t) * pnorm(d2)
    }else if (type == "put"){
      price1 <-  (K * exp(-r * t) * pnorm(-d2) - S * exp((b - r) * t) * pnorm(-d1))
    }
    
    return(abs(price-price1)) 
    
  }
  
  volMin <- optimize(gBSMMin, interval = c(0, 10), price = price, S = underlying, K = strike, q = dyld, r = rf, ttm = time, type = putCall)
  
  # Returning the variable value for IV that minimizes the difference between observed PX and our gBSM fxn
  return(volMin$minimum)
  
}

getDelta <- function(underlying, strike, rf, dyld, ttm, vol, putCall){
  if(putCall == "call"){
    callFlag = 1
  } else {
    callFlag = -1
  }
  
  b <- rf - dyld
  t <- ttm/365.25
  
  d1 <- (log(underlying / strike) + (b + vol^2 / 2) * t) / (vol * sqrt(t))
  
  delta = callFlag * exp(-dyld * t) * pnorm(callFlag*d1)
  
  return(delta)
  
}

getGamma <- function(underlying, strike, rf, dyld, ttm, vol){
  b <- rf - dyld
  t <- ttm/365.25
  
  d1 <- (log(underlying / strike) + (b + vol^2 / 2) * t) / (vol * sqrt(t))
  
  gamma = exp(-dyld * t) * dnorm(d1) / (underlying * vol * sqrt(t))
  
  return(gamma)
}

getVega <- function(underlying, strike, rf, dyld, ttm, vol){
  b <- rf - dyld
  t <- ttm/365.25
  
  d1 <- (log(underlying / strike) + (b + vol^2 / 2) * t) / (vol * sqrt(t))
  
  vega = underlying * exp(-dyld * t) * dnorm(d1) * sqrt(t)
  
  return(vega)
  
}


rowFxn <- function(symbol, option, position, cost, dte){
  desVec = numeric()
  currPx <- getQuote(symbol)[1,2]
  dyld <- getQuote(symbol, what = yahooQF("Dividend Yield"))$'Dividend Yield' #Yeah, it's lazy to only GET it this late in a separate request. I forgot about it. Fight me.
  matchStr <- stringMatch(symbol, option, dte) #Create the reference to match rowname in GET'd chain
  # From our book positions import, we can automatically detect if put/call by referencing the last char of the ID string
  if(substr(option,nchar(option),nchar(option)) == "C"){
    callFlag = "call"
  } else {
    callFlag = "put"
  }
  if(callFlag == "call"){
    chain = as.data.frame(getOptionChain(symbol)[1]) #If call, pull the call chain
  } else {
    chain = as.data.frame(getOptionChain(symbol)[2]) #Ditto for puts 
  }
  # Getting two other vars needed for calcs. optionPx obviously needs to be dynamic, but getting the strike like this is lazy code.
  # Always minimize and combine web GET requests. Not model code right here. 
  optionPx <- chain[matchStr,2]
  optionStrike <- chain[matchStr,1]
  # Argument names are just synomyns of extant fxn env vars, which is **at best** shitty code practice. This thing was a bitch
  # to write and debug and I don't care enough to clean it up as long as it's performant. 
  IV = getIV(optionPx, currPx, optionStrike, dyld, rf_rate, dte, callFlag)
  delta = getDelta(currPx, optionStrike, rf_rate, dyld, dte, IV, callFlag)
  gamma = getGamma(currPx, optionStrike, rf_rate, dyld, dte, IV)
  vega = getVega(currPx, optionStrike, rf_rate, dyld, dte, IV)
  
  return(c(symbol, option, optionPx, IV, delta, gamma, vega))
}



