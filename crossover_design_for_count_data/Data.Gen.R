rm(list=ls(all=TRUE)) 
require(faraway)
require(numDeriv)
require(MASS)
require(extraDistr)
require(simstudy)
library(emplik)
library(copula)
#================================================================================
# Count Data Generator
# 1.use genCorGen generate Poisson Data
# 2.use coupula generate Count Data with Weibull interval time
#================================================================================
param=c(2.5,0,-0.5,-0.1)
xmat=matrix(c(1,1,1,1, 0,1,1,0, 0,1,0,1, 0,0,1,1), nrow = 4, ncol = 4,byrow = TRUE)
mean.true=exp(param%*%xmat)
Type='ABBA';seq=100;cor_par=0.4
#==================================
# Poisson Data Generator
# DT: crossover design type
# TM: mean.true
# NN: number of patients for each sequence
# Rho: correlation within patients
# output is dataframe for Yist
#==================================
Data.Po<-function(DT,TM,NN,Rho){
  if (nchar(DT) == 2){
    m1 <- c(TM[1], TM[2]); m2 <- c(TM[3], TM[4]) # lambda for each new variable
    y1 <- genCorGen(NN, nvars = 2, params1 = m1, dist = "poisson", rho = Rho, corstr = "cs", wide = TRUE,cnames='y11,y12')
    y1 <-as.matrix(y1[,c('y11','y12')])
    y2 <- genCorGen(NN, nvars = 2, params1 = m2, dist = "poisson", rho = Rho, corstr = "cs", wide = TRUE,cnames='y21,y22')
    y2 <-as.matrix(y2[,c('y21','y22')])
    y11<-y1[,1];y12<-y1[,2];y21<-y2[,1];y22<-y2[,2]
    # Y <- c(y11,y12,y21,y22)
    Y<-data.frame(y11,y12,y21,y22)
  }else if(nchar(DT) == 6){
    m1 <- c(TM[1], TM[2], TM[3]); m2 <- c(TM[4], TM[5], TM[6])
    y1 <- genCorGen(NN, nvars = 3, params1 = m1, dist = "poisson", rho = Rho, corstr = "cs", wide = TRUE,cnames='y11,y12,y13')
    y1 <-as.matrix(y1[,c('y11','y12','y13')])
    y2 <- genCorGen(NN, nvars = 3, params1 = m2, dist = "poisson", rho = Rho, corstr = "cs", wide = TRUE,cnames='y21,y22,y23')
    y2 <-as.matrix(y2[,c('y21','y22','y23')])
    
    y11<-y1[,1];y12<-y1[,2];y13<-y1[,3];y21<-y2[,1];y22<-y2[,2];y23<-y2[,3]
    # Y <- c(y11,y12,y13,y21,y22,y23)
    Y<-data.frame(y11,y12,y13,y21,y22,y23)
  }else{
    m1 <- c(TM[1], TM[2], TM[3]); m2 <- c(TM[4], TM[5], TM[6]); m3 <- c(TM[7], TM[8], TM[9]) # lambda for each new variable
    y1 <- genCorGen(NN, nvars = 3, params1 = m1, dist = "poisson", rho = Rho, corstr = "cs", wide = TRUE,cnames='y11,y12,y13')
    y1 <-as.matrix(y1[,c('y11','y12','y13')])
    y2 <- genCorGen(NN, nvars = 3, params1 = m2, dist = "poisson", rho = Rho, corstr = "cs", wide = TRUE,cnames='y21,y22,y23')
    y2 <-as.matrix(y2[,c('y21','y22','y23')])
    y3 <- genCorGen(NN, nvars = 3, params1 = m3, dist = "poisson", rho = Rho, corstr = "cs", wide = TRUE,cnames='y31,y32,y33')
    y3 <-as.matrix(y3[,c('y31','y32','y33')])
    y11<-y1[,1];y12<-y1[,2];y13<-y1[,3];y21<-y2[,1];y22<-y2[,2];y23<-y2[,3];y31<-y3[,1];y32<-y3[,2];y33<-y3[,3]
    # Y <- c(y11,y12,y13,y21,y22,y23,y31,y32,y33)
    Y<-data.frame(y11,y12,y13,y21,y22,y23,y31,y32,y33)
  }
  return(Y)
}


yy<-Data.Po(DT=Type,TM=mean.true,NN=seq,Rho=cor_par)
Y<-yy
dim(Y) <- c(seq*9,1 )

#==================================
# Weibull Data Generator
# DT: crossover design type
# TM: mean.true
# NN: number of patients for each sequence
# Rho: correlation within patients
# output is dataframe for Yist
#==================================
# w.m<- function(w.shape,w.scale) w.scale*factorial(1/w.shape)
# w.v<- function(w.shape,w.mean) (w.mean^2)*((factorial(1/w.shape)^(-2))*factorial(2/w.shape)-1)
#-------------------------------------------
# Weibull Count Data ABBA
#-------------------------------------------
w.scale<-function(w.shape,w.mean) round(w.mean/factorial(1/w.shape),2)
Gen.ind.yy<-function(Wi.shape,Wi.scale,Wi.time){
  wi<-yi<-0
  while (wi <= Wi.time) { 
    yi<-yi+1
    wi<-wi+rweibull(1,shape = Wi.shape,scale = Wi.scale)}
  return(yi)
}
Data.Wei.ABBA<-function(Rho,NN,W.shape,W.scale,W.time){
  
  if (Rho == 0){
    
    YY<-t(replicate(n = NN, mapply(Gen.ind.yy, Wi.shape=W.shape , Wi.scale = W.scale , Wi.time= W.time)))
    
  }else{
    YY = matrix(0, nrow = NN, ncol = 4)
    myCop=gumbelCopula(param=round(1/(1-Rho),2), dim = 2)
    
    for (jj in 1:NN){
      
      xij=rep(0,4);kij=rep(1,4)
      myMvd1=mvdc(copula=myCop, margins=c("weibull", "weibull"), 
                  paramMargins=list(list(shape = W.shape[1],scale = W.scale[1]),
                                    list(shape = W.shape[2],scale = W.scale[2])))
      w1=rMvdc(W.time,myMvd1)
      myMvd2=mvdc(copula=myCop, margins=c("weibull", "weibull"), 
                  paramMargins=list(list(shape = W.shape[3],scale = W.scale[3]),
                                    list(shape = W.shape[4],scale = W.scale[4])))
      w2=rMvdc(W.time,myMvd2)
      w<-cbind(w1,w2)
      
      for (kk in 1:4){ 
        while (xij[kk] <= W.time) {
          YY[jj,kk]<-YY[jj,kk]+1
          xij[kk]<-xij[kk]+w[kij[kk],kk]
          kij[kk]=kij[kk]+1
        }
      }  
      
    } 
    
  }
  
  return(YY)
  
  
}

param=c(2.5,0,-0.5,-0.1)
xmat=matrix(c(1,1,1,1, 0,1,1,0, 0,1,0,1, 0,0,1,1), nrow = 4, ncol = 4,byrow = TRUE)
mean.true=exp(param%*%xmat)

Type='ABBA';cor_par=0.6;seq=10000;W.t=50;d.ABBA<-c(0.61,0.62,0.65,0.65);
W.aa<-rep(2,nchar(Type));W.bb<-mapply(w.scale,w.shape=W.aa,w.mean=1/(mean.true-d.ABBA) * W.t)
Y<-Data.Wei.ABBA(Rho=cor_par,NN=seq,W.shape=W.aa,W.scale=W.bb,W.time=W.t)

#-------------------------------------------
# Weibull Count Data ABBBAA
#-------------------------------------------
w.scale<-function(w.shape,w.mean) round(w.mean/factorial(1/w.shape),2)
Gen.ind.yy<-function(Wi.shape,Wi.scale,Wi.time){
  wi<-yi<-0
  while (wi <= Wi.time) { 
    yi<-yi+1
    wi<-wi+rweibull(1,shape = Wi.shape,scale = Wi.scale)}
  return(yi)
}
Data.Wei.ABBBAA<-function(Rho,NN,W.shape,W.scale,W.time){
  
  if (Rho == 0){
    
    YY<-t(replicate(n = NN, mapply(Gen.ind.yy, Wi.shape=W.shape , Wi.scale = W.scale , Wi.time= W.time)))
    
  }else{
    YY = matrix(0, nrow = NN, ncol = nchar(Type))
    myCop=gumbelCopula(param=round(1/(1-Rho),2), dim = 3)
    
    for (jj in 1:NN){
      
      xij=rep(0,nchar(Type));kij=rep(1,nchar(Type))
      myMvd1=mvdc(copula=myCop, margins=c("weibull", "weibull", "weibull"), 
                  paramMargins=list(list(shape = W.shape[1],scale = W.scale[1]),
                                    list(shape = W.shape[2],scale = W.scale[2]),
                                    list(shape = W.shape[3],scale = W.scale[3])))
      w1=rMvdc(W.time,myMvd1)
      myMvd2=mvdc(copula=myCop, margins=c("weibull", "weibull", "weibull"), 
                  paramMargins=list(list(shape = W.shape[4],scale = W.scale[4]),
                                    list(shape = W.shape[5],scale = W.scale[5]),
                                    list(shape = W.shape[6],scale = W.scale[6])))
      w2=rMvdc(W.time,myMvd2)
      w<-cbind(w1,w2)
      
      for (kk in 1:nchar(Type)){ 
        while (xij[kk] <= W.time) {
          YY[jj,kk]<-YY[jj,kk]+1
          xij[kk]<-xij[kk]+w[kij[kk],kk]
          kij[kk]=kij[kk]+1
        }
      }  
      
    } 
    
  }
  
  return(YY)
  
  
}

param=c(2.5,0,-0.5,-0.5,-0.1)
xmat=matrix(c(rep(1,6), 0,rep(1,3),rep(0,2),rep(c(0,1,0),2),rep(c(0,0,1),2) ,rep(0,3) ,rep(1,3)), nrow = 5, ncol = 6,byrow = TRUE)
mean.true=exp(param%*%xmat)

Type='ABBBAA';cor_par=0.6;seq=10000;W.t=50;d.ABBBAA<-c(0.61,0.63,0.63,0.63,0.63,0.63)
W.aa<-rep(2,nchar(Type));W.bb<-mapply(w.scale,w.shape=W.aa,w.mean=1/(mean.true-d.ABBBAA) * W.t)
Y<-Data.Wei.ABBBAA(Rho=cor_par,NN=seq,W.shape=W.aa,W.scale=W.bb,W.time=W.t)

#-------------------------------------------
# Weibull Count Data AABABABAA
#-------------------------------------------
w.scale<-function(w.shape,w.mean) round(w.mean/factorial(1/w.shape),2)
Gen.ind.yy<-function(Wi.shape,Wi.scale,Wi.time){
  wi<-yi<-0
  while (wi <= Wi.time) { 
    yi<-yi+1
    wi<-wi+rweibull(1,shape = Wi.shape,scale = Wi.scale)}
  return(yi)
}
Data.Wei.AABABABAA<-function(Rho,NN,W.shape,W.scale,W.time){
  
  if (Rho == 0){
    
    YY<-t(replicate(n = NN, mapply(Gen.ind.yy, Wi.shape=W.shape , Wi.scale = W.scale , Wi.time= W.time)))
    
  }else{
    YY = matrix(0, nrow = NN, ncol = nchar(Type))
    myCop=gumbelCopula(param=round(1/(1-Rho),2), dim = 3)
    
    for (jj in 1:NN){
      
      xij=rep(0,nchar(Type));kij=rep(1,nchar(Type))
      myMvd1=mvdc(copula=myCop, margins=c("weibull", "weibull", "weibull"), 
                  paramMargins=list(list(shape = W.shape[1],scale = W.scale[1]),
                                    list(shape = W.shape[2],scale = W.scale[2]),
                                    list(shape = W.shape[3],scale = W.scale[3])))
      w1=rMvdc(W.time,myMvd1)
      myMvd2=mvdc(copula=myCop, margins=c("weibull", "weibull", "weibull"), 
                  paramMargins=list(list(shape = W.shape[4],scale = W.scale[4]),
                                    list(shape = W.shape[5],scale = W.scale[5]),
                                    list(shape = W.shape[6],scale = W.scale[6])))
      w2=rMvdc(W.time,myMvd2)
      myMvd3=mvdc(copula=myCop, margins=c("weibull", "weibull", "weibull"), 
                  paramMargins=list(list(shape = W.shape[7],scale = W.scale[7]),
                                    list(shape = W.shape[8],scale = W.scale[8]),
                                    list(shape = W.shape[9],scale = W.scale[9])))
      w3=rMvdc(W.time,myMvd3)
      w<-cbind(w1,w2,w3)
      
      for (kk in 1:nchar(Type)){ 
        while (xij[kk] <= W.time) {
          YY[jj,kk]<-YY[jj,kk]+1
          xij[kk]<-xij[kk]+w[kij[kk],kk]
          kij[kk]=kij[kk]+1
        }
      }  
      
    } 
    
  }
  
  return(YY)
  
  
}

param=c(2.5,0,-0.5,-0.5,-0.1,-0.1)
xmat=matrix(c(rep(1,9), 0,rep(0:1,3),rep(0,2),rep(c(0,1,0),3),rep(c(0,0,1),3),rep(0,3),rep(1,3),rep(0,3), rep(0,6),rep(1,3)), nrow = 6, ncol = 9,byrow = TRUE)
mean.true=exp(param%*%xmat)

Type='AABABABAA';cor_par=0.6;seq=10000;W.t=50;d.AABABABAA<-c(0.59,0.62,0.63,0.63,0.65,0.65,0.65,0.65,0.63)
W.aa<-rep(2,nchar(Type));W.bb<-mapply(w.scale,w.shape=W.aa,w.mean=1/(mean.true-d.AABABABAA) * W.t)
Y<-Data.Wei.AABABABAA(Rho=cor_par,NN=seq,W.shape=W.aa,W.scale=W.bb,W.time=W.t)

#-------------------------------------------
# Weibull Count Data ABCBCACAB
#-------------------------------------------
w.scale<-function(w.shape,w.mean) round(w.mean/factorial(1/w.shape),2)
Gen.ind.yy<-function(Wi.shape,Wi.scale,Wi.time){
  wi<-yi<-0
  while (wi <= Wi.time) { 
    yi<-yi+1
    wi<-wi+rweibull(1,shape = Wi.shape,scale = Wi.scale)}
  return(yi)
}
Data.Wei.ABCBCACAB<-function(Rho,NN,W.shape,W.scale,W.time){
  
  if (Rho == 0){
    
    YY<-t(replicate(n = NN, mapply(Gen.ind.yy, Wi.shape=W.shape , Wi.scale = W.scale , Wi.time= W.time)))
    
  }else{
    YY = matrix(0, nrow = NN, ncol = nchar(Type))
    myCop=gumbelCopula(param=round(1/(1-Rho),2), dim = 3)
    
    for (jj in 1:NN){
      
      xij=rep(0,nchar(Type));kij=rep(1,nchar(Type))
      myMvd1=mvdc(copula=myCop, margins=c("weibull", "weibull", "weibull"), 
                  paramMargins=list(list(shape = W.shape[1],scale = W.scale[1]),
                                    list(shape = W.shape[2],scale = W.scale[2]),
                                    list(shape = W.shape[3],scale = W.scale[3])))
      w1=rMvdc(W.time,myMvd1)
      myMvd2=mvdc(copula=myCop, margins=c("weibull", "weibull", "weibull"), 
                  paramMargins=list(list(shape = W.shape[4],scale = W.scale[4]),
                                    list(shape = W.shape[5],scale = W.scale[5]),
                                    list(shape = W.shape[6],scale = W.scale[6])))
      w2=rMvdc(W.time,myMvd2)
      myMvd3=mvdc(copula=myCop, margins=c("weibull", "weibull", "weibull"), 
                  paramMargins=list(list(shape = W.shape[7],scale = W.scale[7]),
                                    list(shape = W.shape[8],scale = W.scale[8]),
                                    list(shape = W.shape[9],scale = W.scale[9])))
      w3=rMvdc(W.time,myMvd3)
      w<-cbind(w1,w2,w3)
      
      for (kk in 1:nchar(Type)){ 
        while (xij[kk] <= W.time) {
          YY[jj,kk]<-YY[jj,kk]+1
          xij[kk]<-xij[kk]+w[kij[kk],kk]
          kij[kk]=kij[kk]+1
        }
      }  
      
    } 
    
  }
  
  return(YY)
  
  
}


param=c(2.5,0,0,-0.5,-0.5,-0.1,-0.1)
xmat=matrix(c(rep(1,9), rep(0:1,2),rep(0,4),1, 0,rep(c(0:1),3),rep(0,2), rep(c(0,1,0),3),rep(c(0,0,1),3),rep(0,3),rep(1,3),rep(0,3), rep(0,6),rep(1,3)), nrow = 7, ncol = 9,byrow = TRUE)
mean.true=exp(param%*%xmat)

Type='ABCBCACAB';cor_par=0.6;seq=10000;W.t=50;d.ABCBCACAB<-c(0.59,0.61,0.62,0.63,0.65,0.65,0.65,0.66,0.64)
W.aa<-rep(2,nchar(Type));W.bb<-mapply(w.scale,w.shape=W.aa,w.mean=1/(mean.true-d.ABCBCACAB) * W.t)
Y<-Data.Wei.ABCBCACAB(Rho=cor_par,NN=seq,W.shape=W.aa,W.scale=W.bb,W.time=W.t)





#-------------------------------------------
# Weibull Count Data BACACBBCA
#-------------------------------------------
w.scale<-function(w.shape,w.mean) round(w.mean/factorial(1/w.shape),2)
Gen.ind.yy<-function(Wi.shape,Wi.scale,Wi.time){
  wi<-yi<-0
  while (wi <= Wi.time) { 
    yi<-yi+1
    wi<-wi+rweibull(1,shape = Wi.shape,scale = Wi.scale)}
  return(yi)
}
Data.Wei.BACACBBCA<-function(Rho,NN,W.shape,W.scale,W.time){
  
  if (Rho == 0){
    
    YY<-t(replicate(n = NN, mapply(Gen.ind.yy, Wi.shape=W.shape , Wi.scale = W.scale , Wi.time= W.time)))
    
  }else{
    YY = matrix(0, nrow = NN, ncol = nchar(Type))
    myCop=gumbelCopula(param=round(1/(1-Rho),2), dim = 3)
    
    for (jj in 1:NN){
      
      xij=rep(0,nchar(Type));kij=rep(1,nchar(Type))
      myMvd1=mvdc(copula=myCop, margins=c("weibull", "weibull", "weibull"), 
                  paramMargins=list(list(shape = W.shape[1],scale = W.scale[1]),
                                    list(shape = W.shape[2],scale = W.scale[2]),
                                    list(shape = W.shape[3],scale = W.scale[3])))
      w1=rMvdc(W.time,myMvd1)
      myMvd2=mvdc(copula=myCop, margins=c("weibull", "weibull", "weibull"), 
                  paramMargins=list(list(shape = W.shape[4],scale = W.scale[4]),
                                    list(shape = W.shape[5],scale = W.scale[5]),
                                    list(shape = W.shape[6],scale = W.scale[6])))
      w2=rMvdc(W.time,myMvd2)
      myMvd3=mvdc(copula=myCop, margins=c("weibull", "weibull", "weibull"), 
                  paramMargins=list(list(shape = W.shape[7],scale = W.scale[7]),
                                    list(shape = W.shape[8],scale = W.scale[8]),
                                    list(shape = W.shape[9],scale = W.scale[9])))
      w3=rMvdc(W.time,myMvd3)
      w<-cbind(w1,w2,w3)
      
      for (kk in 1:nchar(Type)){ 
        while (xij[kk] <= W.time) {
          YY[jj,kk]<-YY[jj,kk]+1
          xij[kk]<-xij[kk]+w[kij[kk],kk]
          kij[kk]=kij[kk]+1
        }
      }  
      
    } 
    
  }
  
  DY <- data.frame(YY)
  colnames(DY) <- c("y11", "y12", "y13","y21", "y22", "y23","y31", "y32", "y33")
  return(DY)
  
  
}


param=c(2,0,0,-0.7,-0.5,-0.1,-0.1)
xmat=matrix(c(rep(1,9), 1, rep(0,4),rep(1,2),rep(0,4),1,0,1,0,0,1,0,rep(c(0,1,0),3),rep(c(0,0,1),3),rep(0,3),rep(1,3),rep(0,3), rep(0,6),rep(1,3)), nrow = 7, ncol = 9,byrow = TRUE)
mean.true=exp(param%*%xmat)

Type='BACACBBCA';cor_par=0;seq=10000;W.t=50;d.BACACBBCA<-c(0.59,0.61,0.62,0.63,0.65,0.65,0.65,0.66,0.64)
W.aa<-rep(2,nchar(Type));W.bb<-mapply(w.scale,w.shape=W.aa,w.mean=1/(mean.true-d.BACACBBCA) * W.t)
Y<-Data.Wei.BACACBBCA(Rho=cor_par,NN=seq,W.shape=W.aa,W.scale=W.bb,W.time=W.t)

mean.true;colMeans(Y);cor(Y)


#-------------------------------------------
# Weibull Count Data BBAACBCAC
#-------------------------------------------
w.scale<-function(w.shape,w.mean) round(w.mean/factorial(1/w.shape),2)
Gen.ind.yy<-function(Wi.shape,Wi.scale,Wi.time){
  wi<-yi<-0
  while (wi <= Wi.time) { 
    yi<-yi+1
    wi<-wi+rweibull(1,shape = Wi.shape,scale = Wi.scale)}
  return(yi)
}
Data.Wei.BBAACBCAC<-function(Rho,NN,W.shape,W.scale,W.time){
  
  if (Rho == 0){
    
    YY<-t(replicate(n = NN, mapply(Gen.ind.yy, Wi.shape=W.shape , Wi.scale = W.scale , Wi.time= W.time)))
    
  }else{
    YY = matrix(0, nrow = NN, ncol = nchar(Type))
    myCop=gumbelCopula(param=round(1/(1-Rho),2), dim = 3)
    
    for (jj in 1:NN){
      
      xij=rep(0,nchar(Type));kij=rep(1,nchar(Type))
      myMvd1=mvdc(copula=myCop, margins=c("weibull", "weibull", "weibull"), 
                  paramMargins=list(list(shape = W.shape[1],scale = W.scale[1]),
                                    list(shape = W.shape[2],scale = W.scale[2]),
                                    list(shape = W.shape[3],scale = W.scale[3])))
      w1=rMvdc(W.time,myMvd1)
      myMvd2=mvdc(copula=myCop, margins=c("weibull", "weibull", "weibull"), 
                  paramMargins=list(list(shape = W.shape[4],scale = W.scale[4]),
                                    list(shape = W.shape[5],scale = W.scale[5]),
                                    list(shape = W.shape[6],scale = W.scale[6])))
      w2=rMvdc(W.time,myMvd2)
      myMvd3=mvdc(copula=myCop, margins=c("weibull", "weibull", "weibull"), 
                  paramMargins=list(list(shape = W.shape[7],scale = W.scale[7]),
                                    list(shape = W.shape[8],scale = W.scale[8]),
                                    list(shape = W.shape[9],scale = W.scale[9])))
      w3=rMvdc(W.time,myMvd3)
      w<-cbind(w1,w2,w3)
      
      for (kk in 1:nchar(Type)){ 
        while (xij[kk] <= W.time) {
          YY[jj,kk]<-YY[jj,kk]+1
          xij[kk]<-xij[kk]+w[kij[kk],kk]
          kij[kk]=kij[kk]+1
        }
      }  
      
    } 
    
  }
  
  DY <- data.frame(YY)
  colnames(DY) <- c("y11", "y12", "y13","y21", "y22", "y23","y31", "y32", "y33")
  return(DY)
  
  
}

param=c(2,0,0,-0.5,-0.7,-0.2,-0.1)
xmat=matrix(c(rep(1,9),rep(1,2),rep(0,3),1,rep(0,6),rep(0:1,3),rep(c(0,1,0),3),rep(c(0,0,1),3),rep(0,3),rep(1,3),rep(0,9),rep(1,3)), nrow = 7, ncol = 9,byrow = TRUE)
mean.true=exp(param%*%xmat)

Type='BBAACBCAC';cor_par=0.6;seq=10000;W.t=50;d.BBAACBCAC<-c(0.59,0.61,0.62,0.63,0.65,0.65,0.65,0.66,0.64)
W.aa<-rep(2,nchar(Type));W.bb<-mapply(w.scale,w.shape=W.aa,w.mean=1/(mean.true-d.BBAACBCAC) * W.t)
Y<-Data.Wei.BBAACBCAC(Rho=cor_par,NN=seq,W.shape=W.aa,W.scale=W.bb,W.time=W.t)

mean.true;colMeans(Y);cor(Y)
