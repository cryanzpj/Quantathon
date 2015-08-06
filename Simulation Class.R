setwd("G:/Dropbox/Quantathon")
source("Strategy Class.R")




simulation <- setClass("Simulation", 
                       slots=c(simlength="numeric",
                               currtime="numeric",
                               dates="ANY",
                               spotprices="numeric",
                               futureprices="data.frame",
                               futurers="data.frame",
                               strategy="Strategy",
                               cashaccount="numeric",
                               storage="numeric",
                               position="numeric",
                               pnl="numeric"),
                       prototype=list(simlength=100,
                                      currtime=0,
                                      dates=as.Date("2010-01-01", format="%Y-%m-%d"),
                                      spotprices=numeric(100),
                                      futureprices=data.frame(matrix(0, nrow=100, ncol=11)),
                                      futurers=data.frame(matrix(0, nrow=100, ncol=11)),
                                      strategy=new("Strategy"),
                                      cashaccount=numeric(100),
                                      storage=numeric(100),
                                      position=numeric(11),
                                      pnl=numeric(100)))

setMethod("show", "Simulation",
          function(object){
            cat("At current time ", object@currtime-1, ", the date is ", as.character(object@dates[object@currtime-1]), "\n")
            cat("The oil spot price is: ", object@spotprices[object@currtime-1], "\n")
            cat("Storage level is: ", object@storage[object@currtime], "\n")
            cat("Cash Account: ", object@cashaccount[object@currtime], "\n")
            cat("Strategy: ", show(object@strategy), "\n\n")
            cat("Futures holding is as follows: \n")
            port <- rbind(object@position[object@currtime],
                          object@futureprices[object@currtime-1,1])
            rownames(port) <- c("Position", "Price")
            print(port)
            cat("\n")
            cat("Pnl at the end of the day: ", object@pnl[object@currtime], "\n")
          })

setGeneric(name="startNewSimulation",
           def=function(object, ...){
             standardGeneric("startNewSimulation")
           })

setMethod(f="startNewSimulation",
          signature="Simulation",
          definition=function(object, simlength, dates, spotprices, futureprices, futurers, strategy){
            if(max(dist(c(simlength, length(dates), length(spotprices), NROW(futureprices), NROW(futurers))))>0){
              return("ERROR: lengths differ")
            }else if(sum(dim(futureprices)==dim(futureprices))<2){
              return("ERROR: futures and futures_rs dimension not match")
            }
            object@simlength <- simlength
            object@currtime <- 1
            object@dates <- dates
            object@spotprices <- spotprices
            object@futureprices <- futureprices
            object@futurers <- futurers
            object@strategy <- strategy
            object@cashaccount <- rep(0, simlength+1)
            object@storage <- c(500000, rep(0, simlength))
            object@position <- numeric(simlength+1)
            object@pnl <- rep(0, simlength+1)
            return(object)
          })

setGeneric(name="oneDayForward",
           def=function(object, ...){
             standardGeneric("oneDayForward")
           })


setMethod(f="oneDayForward",
          signature="Simulation",
          definition=function(object){
            object@currtime <- object@currtime + 1
            temp <- object@strategy@strategy(object@strategy, object@dates[object@currtime-1], object@spotprices[object@currtime-1],
                                             object@futureprices[object@currtime-1,], object@futurers[object@currtime-1,],
                                             object@cashaccount[object@currtime-1], object@storage[object@currtime-1],
                                             object@position[object@currtime-1])
            object@strategy <- temp[[1]]
            oiltrans <- temp[[2]]
            if(object@storage[object@currtime-1]+oiltrans>1000000){
              oiltrans <- 1000000-object@storage[object@currtime-1]
            }else if(object@storage[object@currtime-1]+oiltrans<10000){
              oiltrans <- 10000 - object@storage[object@currtime-1]
            }
            if(oiltrans>=60000){
              oiltrans <- 60000
            }else if(oiltrans<=-30000){
              oiltrans <- -30000
            }
            oilcf <- 0
            if(oiltrans<0){
              oilcf <- - oiltrans*0.99*object@spotprices[object@currtime-1]
            }else{
              oilcf <- - (min(oiltrans, 30000)*object@spotprices[object@currtime-1] + max(0, oiltrans-30000)*1.01*object@spotprices[object@currtime-1])
            }
            futurestrans <- temp[[3]]
            interest <- max(object@cashaccount[object@currtime-1],0)*0.00008 + min(object@cashaccount[object@currtime-1],0)*0.0001
            if(object@currtime>2){
              pnl <- object@position[object@currtime-1]*(object@futureprices[object@currtime-1,1]-object@futureprices[object@currtime-2,1]-object@futurers[object@currtime-1,1])
            }else{pnl <- 0}
            #Overnight Procedure
            #1 Interest
            object@cashaccount[object@currtime] <- object@cashaccount[object@currtime-1] + interest
            #2 Futures pnl
            object@cashaccount[object@currtime] <- object@cashaccount[object@currtime] + pnl
            object@pnl[object@currtime] <- pnl
            #3 Oil transaction
            object@storage[object@currtime] <- object@storage[object@currtime-1] + oiltrans
            object@cashaccount[object@currtime] <- object@cashaccount[object@currtime] + oilcf
            #4 sell to refinery
            object@storage[object@currtime] <- object@storage[object@currtime] - 10000
            object@cashaccount[object@currtime] <- object@cashaccount[object@currtime]+10000*object@spotprices[object@currtime-1]
            #5 Futures transaction
            object@position[object@currtime] <- object@position[object@currtime-1] + futurestrans
            object@cashaccount[object@currtime] <- object@cashaccount[object@currtime] - abs(futurestrans)*0.0005*object@futureprices[object@currtime-1,1]
            #6 Check debt-to-equity ratio
#             print(list(debtcheck=(object@cashaccount[object@currtime]<=-0.5*object@storage[object@currtime]*object@spotprices[object@currtime])))
            if(object@cashaccount[object@currtime]<=-0.5*object@storage[object@currtime]*object@spotprices[object@currtime-1]){
              tosell <- min(round(-(object@cashaccount[object@currtime] + 0.5*object@storage[object@currtime]*object@spotprices[object@currtime-1])/object@spotprices[object@currtime-1]),30000)
              object@storage[object@currtime] <- object@storage[object@currtime-1] - tosell
              object@cashaccount[object@currtime] <- object@cashaccount[object@currtime] + tosell*0.99*object@spotprices[object@currtime]
            }
            #7
            
            #return
#             print(list(oiltrans=oiltrans, oilcf=oilcf, futurestrans=futurestrans, pnl=pnl, interest=interest))
            return(object)
          })


setGeneric(name="runAll",
           def=function(object, ...){
             standardGeneric("runAll")
           })

setMethod(f="runAll",
          signature="Simulation",
          definition=function(object){
            for(i in 1:(object@simlength - object@currtime+1)){
              object <- oneDayForward(object)
            }
            return(object)
         })
