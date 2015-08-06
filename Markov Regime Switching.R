#Markov Regime Switching
# setwd("G:/Dropbox/Quantathon")
oil <- read.csv("in_sample_data.csv", header=T, stringsAsFactors=F, strip.white=T)
oilspot <- oil[,2]
n <- length(oilspot)
logreturns <- log(oilspot[2:n]/oilspot[1:(n-1)])
negloglik <- function(par, data){
  #par={mu1, mu2, sd1, sd2, p12, p21}
  mu <- par[c(1,2)]
  sd <- par[c(3,4)]
  ps <- par[c(5,6)]
  #ps[1]=p12, ps[2]=p21
  n <- length(data)
  loglik <- 0
  P <- matrix(c(1-ps[1], ps[1], ps[2], 1-ps[2]), nrow=2, ncol=2, byrow=T)
  pi <- c(ps[2], ps[1])/sum(ps)
  fr <- c(pi[1]*dnorm(data[1], mu[1], sd[1]), 
          pi[2]*dnorm(data[1], mu[2], sd[2]))
  f <- sum(fr)
  loglik <- loglik + log(f)
  pr <- c(fr[1]/f, fr[2]/f)
  frr <- matrix(0, nrow=2, ncol=2)
  for(i in 1:2){
    for(j in 1:2){
      frr[i,j] <- pr[j]*P[j,i]*dnorm(data[2], mu[i], sd[i])
    }
  }
  f <- sum(frr)
  loglik <- loglik + log(f)
  for(t in 3:n){
    for(i in 1:2){
      pr[i] <- sum(frr[i,])/f
    }
    
    for(i in 1:2){
      for(j in 1:2){
        frr[i,j] <- pr[j]*P[j,i]*dnorm(data[t], mu[i], sd[i])
      }
    }
    f <- sum(frr)
    loglik <- loglik + log(f)
  }
  return(-loglik)
}



par <- c(0.01, -0.01, 0.1, 0.2, 0.15, 0.21)
negloglik(par, data=logreturns)
model <- nlminb(par, negloglik, data=logreturns[1:1500], lower=c(-Inf, -Inf, 0, 0, 0, 0), upper = c(Inf, Inf, Inf, Inf, 1, 1))

estimatestate <- function(par, data){
  #par={mu1, mu2, sd1, sd2, p12, p21}
  mu <- par[c(1,2)]
  sd <- par[c(3,4)]
  ps <- par[c(5,6)]
  #ps[1]=p12, ps[2]=p21
  n <- length(data)
  state <- numeric(n-1)
  prm <- matrix(0, nrow=n, ncol=2)
  P <- matrix(c(1-ps[1], ps[1], ps[2], 1-ps[2]), nrow=2, ncol=2, byrow=T)
  pi <- c(ps[2], ps[1])/sum(ps)
  fr <- c(pi[1]*dnorm(data[1], mu[1], sd[1]), 
          pi[2]*dnorm(data[1], mu[2], sd[2]))
  f <- sum(fr)
  pr <- c(fr[1]/f, fr[2]/f)
  prm[1,] <- pr
  state[1] <- which.max(pr)
  frr <- matrix(0, nrow=2, ncol=2)
  for(i in 1:2){
    for(j in 1:2){
      frr[i,j] <- pr[j]*P[j,i]*dnorm(data[2], mu[i], sd[i])
    }
  }
  f <- sum(frr)
  for(t in 3:n){
    for(i in 1:2){
      pr[i] <- sum(frr[i,])/f
    }
    prm[t-1,] <- pr
    state[t-1] <- which.max(pr)
    
    for(i in 1:2){
      for(j in 1:2){
        frr[i,j] <- pr[j]*P[j,i]*dnorm(data[t], mu[i], sd[i])
      }
    }
    f <- sum(frr)
  }
  return(state)
}

# states <- estimatestate(model$par, logreturns[1:1500])
# par(mfrow=c(3,1))
# plot(oilspot[2:1501])
# plot(logreturns[1:1500])
# plot(states[[1]][,1], type="l", col="green")
# lines(states[[1]][,2], type="l", col="red")
# 

currstate <- function(par, data){
  #par={mu1, mu2, sd1, sd2, p12, p21}
  mu <- par[c(1,2)]
  sd <- par[c(3,4)]
  ps <- par[c(5,6)]
  #ps[1]=p12, ps[2]=p21
  n <- length(data)
  state <- numeric(n)
  P <- matrix(c(1-ps[1], ps[1], ps[2], 1-ps[2]), nrow=2, ncol=2, byrow=T)
  pi <- c(ps[2], ps[1])/sum(ps)
  fr <- c(pi[1]*dnorm(data[1], mu[1], sd[1]), 
          pi[2]*dnorm(data[1], mu[2], sd[2]))
  f <- sum(fr)
  pr <- c(fr[1]/f, fr[2]/f)
  frr <- matrix(0, nrow=2, ncol=2)
  for(i in 1:2){
    for(j in 1:2){
      frr[i,j] <- pr[j]*P[j,i]*dnorm(data[2], mu[i], sd[i])
    }
  }
  f <- sum(frr)
  for(t in 3:n){
    for(i in 1:2){
      pr[i] <- sum(frr[i,])/f
    }
    
    for(i in 1:2){
      for(j in 1:2){
        frr[i,j] <- pr[j]*P[j,i]*dnorm(data[t], mu[i], sd[i])
      }
    }
    f <- sum(frr)
  }
  return(which.max(pr))
}