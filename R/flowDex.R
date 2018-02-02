#' Calculate the flow and returns the 
#'
#' The function calculates the bullish strength ,bearish stength (on Average) and returns.
#' Basically it calulates the Average amount of bearish flow and average amount of bullish flow in a given 'n' terms.
#' @param m - OHLC
#' @param FUN - Which price is OHLC to be used for calculating. Default Cl. If 
#' If Hi to be used, use FUN - Hi
#' @return returns the bearishFlow,bullishFlow
#' @author Siva Sunku
#' @keywords flowDex
#' @note
#' @examples
#' elderRay(m,n=13)
#' @export
#' 
flow <- function(m){
  GREEN <- 1
  RED <- -1
  if (!is.xts(m) || !is.HLC(m)){
    stop("flowDex can be calculated on xts variables & OHLCV")
  }
  HLC <- HLC(m)
  m$closeLag <- lag.xts(HLC[, 3])
  m$trueHigh <- pmax(HLC[, 1], m$closeLag, na.rm = FALSE)
  m$trueLow  <- pmin(HLC[, 2], m$closeLag, na.rm = FALSE)
  #1- Green : -1 - Red
  m$color <- ifelse( (HLC[,3] > m$closeLag),GREEN,RED)
  
  #GreenMove - If green color, trueLow to close is considered as move
  #RedMove   - If Red color,  trueHigh minus Close is the move
  m$flow <- ifelse(m$color==GREEN,m$Close - m$trueLow,m$trueHigh - m$Close )
  return(m[,c("color","flow")])
}

#' This function returns the flowDex of bulls & bears on a window n
#'
#' The function calculates the bullish strength ,bearish stength (on Average) and returns.
#' Basically it calulates the Average amount of bearish flow and average amount of bullish flow in a given 'n' terms.
#' @export
flowDex <- function(m){
  GREEN <-  1
  RED   <- -1

  if (!is.xts(m)){
    stop("flowDex can be calculated on xts variables & OHLCV")
  }
  
  d <- m[,c("color","flow")]
  a <- as.numeric(ifelse(d$color == GREEN,d$flow,NA))
  bullFlow <- mean(a,na.rm = TRUE)
  b <- as.numeric(ifelse(d$color == RED,d$flow,NA))
  bearFlow <- mean(b,na.rm = TRUE)
  c("bullFlow" = bullFlow,"bearFlow" = bearFlow)
}
