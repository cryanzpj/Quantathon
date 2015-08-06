setwd("G:/Dropbox/Quantathon")
library(stockPortfolio)
library(TTR)
source("Markov Regime Switching.R")


strategy <- setClass("Strategy",
                     slots=c(strategyname="character",
                             strategy="ANY",
                             hist.spot="numeric",
                             hist.futureprices="data.frame",
                             hist.futurers="data.frame",
                             predictions="data.frame",
                             other="list"),
                     prototype=list(strategyname="nothing",
                                    strategy=function(object) return(list(object,"oiltransactions", "futurestransactions")),
                                    hist.spot=numeric(),
                                    hist.futureprices=data.frame(),
                                    hist.futurers=data.frame(),
                                    predictions=data.frame(spot=-1, futures=-1),
                                    other=list()))


setMethod("show", "Strategy",
          function(object){
            return(object@strategyname)
          })

setGeneric(name="initWithHistorical",
           function(object, ...){
             standardGeneric("initWithHistorical")
           })

setMethod(f="initWithHistorical",
          signature="Strategy",
          definition=function(object, name, strategy, hist.spot, hist.futureprices=data.frmae(), hist.futurers=data.frame(), other=list()){
            object@strategyname <- name
            object@strategy <- strategy
            object@hist.spot <- hist.spot
            object@hist.futureprices <- hist.futureprices
            object@hist.futurers <- hist.futurers
            object@predictions <- data.frame(spot=-1, futures=-1)
            object@other <- other
            return(object)
          })

setGeneric(name="baseline",
           def=function(object, ...){
             standardGeneric("baseline")
           })

setMethod(f="baseline",
          signature="Strategy",
          definition=function(object, curdate, curspot, curfutureprice, curfuturers,
                              curcash, curstorage, curposition){
            oiltrans <- - round(0.01 * curstorage)
            futuretrans <- - round(oiltrans/2)
            oiltrans <- oiltrans + 10000
            return(list(object, oiltrans, futuretrans))
          })



setGeneric(name="baseline2",
           def=function(object, ...){
             standardGeneric("baseline2")
           })

setMethod(f="baseline2",
          signature="Strategy",
          definition=function(object, curdate, curspot, curfutureprice, curfuturers,
                              curcash, curstorage, curposition){
            oiltrans <- - round(0.01 * curstorage)
            futuretrans <- - round(2*oiltrans)
            oiltrans <- oiltrans + 10000
            return(list(object, oiltrans, futuretrans))
          })


setGeneric(name="regimeSwitching",
           def=function(object, ...){
             standardGeneric("regimeSwitching")
           })


setMethod(f="regimeSwitching",
          signature="Strategy",
          definition=function(object, curdate, curspot, curfutureprice, curfuturers,
                              curcash, curstorage, curposition){
            #model fitting
            object@hist.spot <- c(object@hist.spot, curspot)
            n <- length(object@hist.spot)
            logreturnspot <- log(object@hist.spot[2:n]/object@hist.spot[1:(n-1)])
            parspot <- object@other[[1]]
            modelspot <- nlminb(parspot, negloglik, data=logreturnspot, lower=c(-Inf, -Inf, 0, 0, 0, 0), upper = c(Inf, Inf, Inf, Inf, 1, 1),control=list(rel.tol=1e-4, x.tol=1e-4))
            object@other[[1]] <- modelspot$par
            
            object@hist.futureprices <- rbind(object@hist.futureprices, curfutureprice[1])
            logreturnfutures <- log(object@hist.futureprices[2:n,1]/object@hist.futureprices[1:(n-1),1])
            parfutures <- object@other[[2]]
            modelfutures <- nlminb(parfutures, negloglik, data=logreturnfutures, lower=c(-Inf, -Inf, 0, 0, 0, 0), upper = c(Inf, Inf, Inf, Inf, 1, 1),control=list(rel.tol=1e-4, x.tol=1e-4))
            object@other[[2]] <- modelfutures$par
            
            
            
            #predict state & past states
            statespot <- currstate(modelspot$par, logreturnspot)
            statefutures <- currstate(modelfutures$par, logreturnfutures)
            object@other[[3]] <- rbind(object@other[[3]], data.frame(spot=statespot, futures=statefutures))
            
            
            #adj to oil
            #expr_duration <- (1-modelspot$par[c(5,6)][statespot])/modelspot$par[c(5,6)][statespot]
            if(modelspot$par[statespot]>0){
              if(curstorage > 50000 && curstorage+curposition > 60000){
                oiltrans <- max(-30000, 50000-curstorage, 60000-curstorage-curposition)
              }
              else{
                oiltrans <- max(10000, 50000-curstorage)
              }
            }else if(modelspot$par[statespot]<0){
              if(curstorage < 500000 && curstorage+curposition < 500000){
                oiltrans <- min(30000, 500000-curstorage, 500000-curstorage-curposition)
              }
              else{
                oiltrans <- 0
              }
            }else{
              oiltrans <- 0
            }
            
            #adj to futures
            
            if(modelfutures$par[statefutures]>0){
              if(curstorage+curposition < 500000){
                futuretrans <- min(50000, 500000-curstorage-curposition)
              }
              else{
                futuretrans <- 0
              }
            }else if(modelfutures$par[statefutures]<0){
              if(curstorage+curposition > 60000){
                futuretrans <- max(-50000, 60000-curstorage-curposition)
              }
              else{
                futuretrans <- 0
              }
            }else{
              futuretrans <- 0
            }
            #predict price
            spotpred <- as.numeric(curspot*(1+modelspot$par[statespot]))
            futurespred <- as.numeric(curfutureprice[1]*(1+modelfutures$par[statefutures]))
            object@predictions <- rbind(object@predictions, 
                                        data.frame("spot"=spotpred, "futures"=futurespred))
            return(list(object, oiltrans, futuretrans))
          })

