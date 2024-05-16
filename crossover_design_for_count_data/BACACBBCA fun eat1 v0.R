rm(list=ls(all=TRUE)) 
require(faraway)
require(numDeriv)
require(MASS)
require(extraDistr)
require(simstudy)
library(emplik)
library(copula)
library(writexl)
#============================================
# function needed
#============================================
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
MatAB.Po.BACACBBCA<-function(DT,NN,YY,MLE,Xs,mat.type){
  y11<-YY[,1];y12<-YY[,2];y13<-YY[,3];y21<-YY[,4];y22<-YY[,5];y23<-YY[,6];y31<-YY[,7];y32<-YY[,8];y33<-YY[,9]
  Mhat<-exp(MLE%*%Xs)
  II<-matrix(0,nrow = length(MLE),ncol = length(MLE))
  for (j in 1:length(MLE)){II[j,]<-Mhat%*%(Xs[j,]*t(Xs))/3}
  
  ii.ee<-II[2:3,2:3]
  ii.ep<-II[2:3,-c(2,3)]
  ii.pp<-II[-c(2,3),-c(2,3)]
  
  AA = ii.ee - ii.ep %*% solve(ii.pp) %*% t(ii.ep)
  #DT=Type,NN=seq,YY=yy,MLE=c(t,0,g,d),Xs=xmat
  if (mat.type == 'null'){
    vcov<-matrix(0,nrow = nchar(Type),ncol = nchar(Type))
    for (l in 1:nchar(Type)){for(ll in 1:nchar(Type)){ vcov[l,ll]<-sum((YY[,l]-Mhat[,l])*(YY[,ll]-Mhat[,ll]))/NN}  }
  }else{  vcov<-var(YY) }
  # yii: var(y11),var(y12),var(y13),var(y21),var(y22),var(y23)
  # yij:cov(y11,y12),cov(y11,y13),cov(y12,y13),cov(y21,y22),cov(y21,y23),cov(y22,y23)
  yii<-diag(vcov)
  yij<-c(diag(vcov[-1,])[1],vcov[1,3],diag(vcov[-1,])[2],diag(vcov[-1,])[4],vcov[4,6],diag(vcov[-1,])[5],diag(vcov[-1,])[7],vcov[7,9],diag(vcov[-1,])[8])
  vv.tt =  sum(yii)/3+2*sum(yij)/3
  vv.te1 =  (yii[1]+yii[6]+yii[7])/3+(yij[1]+yij[2]+yij[5]+yij[6]+yij[7]+yij[8])/3
  vv.te2 =  (yii[3]+yii[5]+yii[8])/3+(yij[2]+yij[3]+yij[4]+yij[6]+yij[7]+yij[9])/3
  vv.tg1 = (yii[2]+yii[5]+yii[8])/3+(yij[1]+yij[3]+yij[4]+yij[6]+yij[7]+yij[9])/3
  vv.tg2 = (yii[3]+yii[6]+yii[9])/3+(yij[2]+yij[3]+yij[5]+yij[6]+yij[8]+yij[9])/3
  vv.td1 = vv.d1d1 =sum(yii[4:6])/3+2*sum(yij[4:6])/3
  vv.td2 = vv.d2d2 = sum(yii[7:9])/3+2*sum(yij[7:9])/3
  
  vv.e1e1 =  (yii[1]+yii[6]+yii[7])/3
  vv.e1e2 =  (yij[2]+yij[6]+yij[7])/3
  vv.e1g1 = (yij[1]+yij[6]+yij[7])/3
  vv.e1g2 = (yii[6]+yij[2]+yij[7])/3
  vv.e1d1 = (yii[6]+yij[5]+yij[6])/3
  vv.e1d2 = (yii[7]+yij[7]+yij[8])/3
  
  vv.e2e2 =  (yii[3]+yii[5]+yii[8])/3
  vv.e2g1 = (yii[5]+yii[8]+yij[3])/3
  vv.e2g2 = (yii[3]+yij[6]+yij[9])/3
  vv.e2d1 = (yii[5]+yij[4]+yij[6])/3
  vv.e2d2 = (yii[8]+yij[7]+yij[9])/3
  
  vv.g1g1 = (yii[2]+yii[5]+yii[8])/3
  vv.g1g2 = (yij[3]+yij[6]+yij[9])/3
  vv.g1d1 = (yii[5]+yij[4]+yij[6])/3
  vv.g1d2 = (yii[8]+yij[7]+yij[9])/3
  vv.g2g2 = (yii[3]+yii[6]+yii[9])/3
  vv.g2d1 = (yii[6]+yij[5]+yij[6])/3
  vv.g2d2 = (yii[9]+yij[8]+yij[9])/3
  vv.d1d2 = 0
  
  
  V<-matrix( c(vv.tt, vv.te1, vv.te2, vv.tg1, vv.tg2, vv.td1, vv.td2,
               vv.te1, vv.e1e1, vv.e1e2, vv.e1g1, vv.e1g2, vv.e1d1,vv.e1d2, 
               vv.te2, vv.e1e2, vv.e2e2, vv.e2g1, vv.e2g2, vv.e2d1,vv.e2d2, 
               vv.tg1, vv.e1g1, vv.e2g1, vv.g1g1, vv.g1g2, vv.g1d1,vv.g1d2,
               vv.tg2, vv.e1g2, vv.e2g2, vv.g1g2, vv.g2g2, vv.g2d1,vv.g2d2,
               vv.td1, vv.e1d1, vv.e2d1, vv.g1d1, vv.g2d1, vv.d1d1,vv.d1d2,
               vv.td2, vv.e1d2, vv.e2d2, vv.g1d2, vv.g2d2, vv.d1d2,vv.d2d2),nrow=7, ncol=7, byrow = TRUE)
  
  vv.ee = V[2:3,2:3]
  vv.ep = V[2:3,-c(2,3)]
  vv.pp = V[-c(2,3),-c(2,3)]
  
  
  BB = vv.ee - ii.ep %*% solve(ii.pp) %*% t(vv.ep) - vv.ep %*% solve(ii.pp) %*% t(ii.ep) +
    ii.ep %*% solve(ii.pp) %*% vv.pp %*% solve(ii.pp) %*% t(ii.ep)
  
  mat.res<-list("Mat.A" = AA,"Mat.B" = BB)
  
  return(mat.res)
  
}
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

#============================================
# mutual parameters
#============================================
Path="C:/0409pp"
# Path="C:/Users/User/Documents/Study_CrossoverDesign/BACACBBCA"
setwd(Path)

sim_time=10000;cor_par=0;seq=100;Type='BACACBBCA'
param=c(2, 0, 0, -0.5, -0.7, -0.2, -0.1)
xmat=matrix(c(rep(1,9), 1, rep(0,4),rep(1,2),rep(0,4),1,0,1,0,0,1,0,rep(c(0,1,0),3),rep(c(0,0,1),3),rep(0,3),rep(1,3),rep(0,3), rep(0,6),rep(1,3)), nrow = 7, ncol = 9,byrow = TRUE)
mean.true=exp(param%*%xmat)

#============================================
# Weibull Count Data parameters
#============================================
W.t=50;d.BACACBBCA<-c(0.59,0.61,0.62,0.63,0.65,0.65,0.65,0.66,0.64)
W.aa<-rep(2,nchar(Type));W.bb<-mapply(w.scale,w.shape=W.aa,w.mean=1/(mean.true-d.BACACBBCA) * W.t)
# i=1;seq=250;dataType="Poisson"

sim.BAC1<-function(seq,dataType){
  print(paste(seq,dataType))
  #MLE, null MLE, Matrix I, Matrix V, invI
  mle.1 <- mle.0 <- matrix(, nrow = sim_time, ncol = length(param))
  colnames(mle.1) <- c("tao", "eta1", "eta2", "gam1", "gam2", "del1", "del2")
  colnames(mle.0) <- c("tao.0", "eta1.0", "eta2.0", "gam1.0", "gam2.0", "del1.0", "del2.0")
  A<-B<-A0<-B0<-list()
  #variance of eta & type I error
  Lna1<-Lrb1<-Sna1<-Srb1<-Wna1<-Wrb1<-var.na<-var.rb<-c()
  
  #for C.I., AL, CP
  ci <- matrix(, nrow = sim_time, ncol = 18)
  colnames(ci)<-c('Lna.LB','Lna.UB','Lna.AL','Lrb.LB','Lrb.UB','Lrb.AL',
                  'Sna.LB','Sna.UB','Sna.AL','Srb.LB','Srb.UB','Srb.AL',
                  'Wna.LB','Wna.UB','Wna.AL','Wrb.LB','Wrb.UB','Wrb.AL')
  LRna.cp<-LRrb.cp<-Sna.cp<-Srb.cp<-Wna.cp<-Wrb.cp<-c()
  
  #==============================
  # simulation 
  #==============================
  set.seed(110225021)
  for (i in 1:sim_time){
    
    while(TRUE){
      
      CI<-tryCatch({
        if (dataType == "Poisson"){
          yy<-Data.Po(DT=Type,TM=mean.true,NN=seq,Rho=cor_par)
          Y<-as.vector(unlist(yy))
        }else{
          yy<-Data.Wei.BACACBBCA(Rho=cor_par,NN=seq,W.shape=W.aa,W.scale=W.bb,W.time=W.t)
          Y<-as.vector(unlist(yy))
        }
        X1 = c(rep(1,seq),rep(0,each=seq,time=4),rep(1,each=seq,time=2),rep(0,each=seq,time=2))
        X2 = c(rep(0,each=seq,time=2),rep(c(1,0),each=seq,time=2),rep(0,seq),rep(1,seq),rep(0,seq))
        Z1 = c(rep(c(0,1,0),each = seq,time=3))
        Z2 = c(rep(c(0,0,1),each = seq,time=3))
        G1 = c(rep(0,each=seq,time=3),rep(1,each=seq,time=3),rep(0,each=seq,time=3))
        G2 = c(rep(0,each=seq,time=6),rep(1,each=seq,time=3))
        df.cor = data.frame(Y,X1,X2,Z1,Z2,G1,G2)
        
        #---------------------------------
        #MLE
        #---------------------------------
        mod.1 <- glm(Y ~ X1 + X2 + Z1 + Z2 + G1 + G2, family = poisson(link = "log"), df.cor)
        mle.1[i,]<-mod.1$coefficients
        #---------------------------------
        #null MLE eta1=0
        #---------------------------------
        mod.0 <- glm(Y ~ X2 + Z1 + Z2 + G1 + G2, family = poisson(link = "log"), df.cor)
        mle.0[i,]<- append(mod.0$coefficients, 0, after=1) # eta1 =0 after 1 ,eta2 = 0 after 2
        #---------------------------------
        #Matrix I & Matrix V
        #---------------------------------
        Mat = MatAB.Po.BACACBBCA(DT=Type,NN=seq,YY=yy,MLE=mle.1[i,],Xs = xmat,mat.type = 'true')
        A[[i]]<-Mat$Mat.A;    B[[i]]<-Mat$Mat.B
        
        var.na[i]<-(solve(A[[i]])/(3*seq))[1,1]
        var.rb[i]<-(solve(A[[i]]) %*% B[[i]] %*% solve(A[[i]])/(3*seq))[1,1]
        #---------------------------------
        #log LR test 
        #---------------------------------
        lik<-function(par){ 
          ll=sum((par[1]+par[2])*yy$y11-exp(par[1]+par[2])+(par[1]+par[4])*yy$y12-exp(par[1]+par[4])+(par[1]+par[3]+par[5])*yy$y13-exp(par[1]+par[3]+par[5]))+
            sum((par[1]+par[6])*yy$y21-exp(par[1]+par[6])+ (par[1]+par[3]+par[4]+par[6])*yy$y22-exp(par[1]+par[3]+par[4]+par[6]) + (par[1]+par[2]+par[5]+par[6])*yy$y23-exp(par[1]+par[2]+par[5]+par[6]))+
            sum((par[1]+par[2]+par[7])*yy$y31-exp(par[1]+par[2]+par[7])+ (par[1]+par[3]+par[4]+par[7])*yy$y32-exp(par[1]+par[3]+par[4]+par[7]) + (par[1]+par[5]+par[7])*yy$y33-exp(par[1]+par[5]+par[7]))
          return(ll)
        }
        
        #=====================================
        #LR Test
        #=====================================
        l1 = lik(mle.1[i,]); l0 = lik(mle.0[i,])
        Lna1[i]<-2*(l1-l0)
        Lrb1[i]<-2 * (A[[i]] %*% solve(B[[i]]))[1,1] * (l1-l0)
        #=====================================
        #LR test C.I. lower and upper
        #=====================================
        #Naive
        #-------------------------------------
        
        LR.na  <- function(e){
          
          Mod.0 <- glm(formula = Y~ X2 + Z1 + Z2 + G1 + G2 + offset(e*X1),family = poisson(link = "log"), data = df.cor)
          mlee<- append(Mod.0$coefficients, e, after=1)
          l.0 = lik(mlee)
          u = 2*(l1-l.0)
          list("-2LLR"=u)
        }
        
        LR.naci<-findUL(step=0.01, fun=LR.na, MLE = mle.1[i,2])
        ci[i,c('Lna.LB','Lna.UB','Lna.AL')] = c(LR.naci$Low,LR.naci$Up,LR.naci$Up-LR.naci$Low)
        LRna.cp[i] = ifelse(( LR.naci$Low < 0 & LR.naci$Up > 0), 1, 0)
        #-------------------------------------
        #Robust
        #-------------------------------------
        LR.rb <- function(e){
          Mod.0 <- glm(formula = Y~ X2 + Z1 + Z2 + G1 + G2+offset(e*X1),family = poisson(link = "log"), data = df.cor)
          mlee<- append(Mod.0$coefficients, e, after=1)
          l.0 = lik(mlee)
          u = 2* (A[[i]] %*% solve(B[[i]]))[1,1]*(l1-l.0)
          list("-2LLR"=u)
        }
        
        LR.rbci<-findUL(step=0.01, fun=LR.rb, MLE = mle.1[i,2])
        ci[i,c('Lrb.LB','Lrb.UB','Lrb.AL')] = c(LR.rbci$Low,LR.rbci$Up,LR.rbci$Up-LR.rbci$Low)
        LRrb.cp[i] = ifelse(( LR.rbci$Low < 0 & LR.rbci$Up > 0), 1, 0)
        
        #---------------------------------
        # null Matrix I & Matrix V
        #---------------------------------
        Mat0 = MatAB.Po.BACACBBCA(DT=Type,NN=seq,YY=yy,MLE=mle.0[i,],Xs = xmat,mat.type = 'null')
        A0[[i]]<-Mat$Mat.A;    B0[[i]]<-Mat$Mat.B
        #=====================================
        #score test
        #=====================================
        s0.1 = seq * ( mean(yy$y11) - exp(mle.0[i,1])+ mean(yy$y23) - exp(mle.0[i,1]+mle.0[i,5]+mle.0[i,6]) + mean(yy$y31)- exp(mle.0[i,1]+mle.0[i,7]) )
        Sna1[i]<- s0.1 %*% solve(A0[[i]])[1,1] %*% t(s0.1) / (3*seq)
        Srb1[i]<- s0.1%*% solve(B0[[i]])[1,1] %*% t(s0.1) / (3*seq)
        #=====================================
        #score test C.I. lower and upper
        #=====================================
        #Naive
        #-------------------------------------
        S.na <- function(e){
          
          Mod.0 <- glm(formula = Y~ X2 + Z1 + Z2 + G1 + G2+offset(e*X1),family = poisson(link = "log"), data = df.cor)
          mlee<- append(Mod.0$coefficients, e, after=1)
          mat = MatAB.Po.BACACBBCA(DT=Type,NN=seq,YY=yy,MLE=mlee,Xs = xmat,mat.type = 'null')
          aa<-mat$Mat.A;    bb<-mat$Mat.B
          s01 = seq * ( mean(yy$y11) - exp(mlee[1]+e)+ mean(yy$y23) - exp(mlee[1]+e+mlee[5]+mlee[6]) + mean(yy$y31)- exp(mlee[1]+e+mlee[7]) )
          s.na = s01^2 * solve(aa)[1,1]  / (3*seq)
          list("-2LLR"=s.na)
        }
        
        S.naci<-findUL(step=0.01, fun=S.na, MLE = mle.1[i,2])
        ci[i,c('Sna.LB','Sna.UB','Sna.AL')] = c(S.naci$Low,S.naci$Up,S.naci$Up-S.naci$Low)
        Sna.cp[i] = ifelse(( S.naci$Low < 0 & S.naci$Up > 0), 1, 0)
        
        # #-------------------------------------
        # #Robust
        # #-------------------------------------
        S.rb <- function(e){
          Mod.0 <- glm(formula = Y~ X2 + Z1 + Z2 + G1 + G2+offset(e*X1),family = poisson(link = "log"), data = df.cor)
          mlee<- append(Mod.0$coefficients, e, after=1)
          mat = MatAB.Po.BACACBBCA(DT=Type,NN=seq,YY=yy,MLE=mlee,Xs = xmat,mat.type = 'null')
          aa<-mat$Mat.A;    bb<-mat$Mat.B
          s01 = seq * ( mean(yy$y11) - exp(mlee[1]+e)+ mean(yy$y23) - exp(mlee[1]+e+mlee[5]+mlee[6]) + mean(yy$y31)- exp(mlee[1]+e+mlee[7]) )
          s.rb = s01^2 * solve(bb)[1,1]  / (3*seq)
          list("-2LLR"=s.rb)
        }
        
        S.rbci<-findUL(step=0.01, fun=S.rb, MLE = mle.1[i,2])
        ci[i,c('Srb.LB','Srb.UB','Srb.AL')] = c(S.rbci$Low,S.rbci$Up,S.rbci$Up-S.rbci$Low)
        Srb.cp[i] = ifelse(( S.rbci$Low < 0 & S.rbci$Up > 0), 1, 0)
        
        #=====================================
        #wald test
        #=====================================
        Wna1[i]<-mle.1[i,2]^2 / ( solve(A0[[i]]) / (3*seq) )[1,1]
        Wrb1[i]<- mle.1[i,2]^2 / (solve(A0[[i]]) %*% B0[[i]] %*% solve(A0[[i]])/(3*seq))[1,1]
        
        #=====================================
        #Wald test C.I. lower and upper
        #=====================================
        #Naive
        #-------------------------------------
        W.na <- function(e){
          
          Mod.0 <- glm(formula = Y~ X2 + Z1 + Z2 + G1 + G2+offset(e*X1),family = poisson(link = "log"), data = df.cor)
          mlee<- append(Mod.0$coefficients, e, after=1)
          mat = MatAB.Po.BACACBBCA(DT=Type,NN=seq,YY=yy,MLE=mlee,Xs = xmat,mat.type = 'null')
          aa<-mat$Mat.A;    bb<-mat$Mat.B
          w.na = (mle.1[i,2]-e)^2 / ( solve(aa) / (3*seq) )[1,1]
          list("-2LLR"=w.na)
        }
        W.naci<-findUL(step=0.01, fun=W.na, MLE = mle.1[i,2])
        ci[i,c('Wna.LB','Wna.UB','Wna.AL')] = c(W.naci$Low,W.naci$Up,W.naci$Up-W.naci$Low)
        Wna.cp[i] = ifelse(( W.naci$Low < 0 & W.naci$Up > 0), 1, 0)
        
        #-------------------------------------
        #Robust
        #-------------------------------------
        W.rb <- function(e){
          Mod.0 <- glm(formula = Y~ X2 + Z1 + Z2 + G1 + G2+offset(e*X1),family = poisson(link = "log"), data = df.cor)
          mlee<- append(Mod.0$coefficients, e, after=1)
          mat = MatAB.Po.BACACBBCA(DT=Type,NN=seq,YY=yy,MLE=mlee,Xs = xmat,mat.type = 'null')
          aa<-mat$Mat.A;    bb<-mat$Mat.B
          w.rb = (mle.1[i,2]-e)^2 / (solve(aa) %*% bb %*% solve(aa)/(3*seq))[1,1]
          list("-2LLR"=w.rb)
        }
        
        W.rbci<-findUL(step=0.01, fun=W.rb, MLE = mle.1[i,2])
        ci[i,c('Wrb.LB','Wrb.UB','Wrb.AL')] = c(W.rbci$Low,W.rbci$Up,W.rbci$Up-W.rbci$Low)
        Wrb.cp[i] = ifelse(( W.rbci$Low < 0 & W.rbci$Up > 0), 1, 0)
        
  
      }, error=function(condi){ return('error')})
      if(CI!='error'){ break }
    }
  }
  
  #模擬sim_time後的所有值
  Estimates<-signif(colMeans(mle.1), digits = 6)
  VARs<-c(var(mle.1[,2]),sd(mle.1[,2]),mean(var.na),sqrt(mean(var.na)),mean(var.rb),sqrt(mean(var.rb)))
  names(VARs)<-c("sv.eta1","std.eta1","var.na.eta1","std.na.eta1","var.rb.eta1","std.rb.eta1")
  RES.sim.alpha<-data.frame(Lna1,Lrb1,Sna1,Srb1,Wna1,Wrb1)
  ALPHA<-apply(X = RES.sim.alpha, MARGIN = 2, FUN = function(x){sum(x > qchisq(0.95, 1))/sim_time})
  CI.sim<-signif(colMeans(ci), digits = 6)
  RES.sim.cp<-data.frame(LRna.cp,LRrb.cp,Sna.cp, Srb.cp,Wna.cp, Wrb.cp)
  CP<-apply(X = RES.sim.cp, MARGIN = 2, FUN = function(x){sum(x)/sim_time})
  
  #計算估計量
  # RES<-cbind(N=seq,t(Estimates),t(VARs),t(ALPHA))
  RES<-cbind(N=seq,t(Estimates),t(VARs),t(ALPHA),
             t(CI.sim[1:3]),t(CP[1]),t(CI.sim[4:6]),t(CP[2]),
             t(CI.sim[7:9]),t(CP[3]),t(CI.sim[10:12]),t(CP[4]),
             t(CI.sim[13:15]),t(CP[5]),t(CI.sim[16:18]),t(CP[6]))
  
  
}




# tt<-sim.ABC1(150,"Poisson")

res11<-do.call(rbind, lapply(c(25,50,150,250), sim.BAC1, dataType="Poisson")) 
write_xlsx(data.frame(res11),path = paste0(Path,"/BAC1_funPo_rho=",cor_par,"_",substr(Sys.Date(),6,10),".xlsx"))

res12<-do.call(rbind, lapply(c(25,50,150,250), sim.BAC1, dataType="Weibull")) 
write_xlsx(data.frame(res12),path = paste0(Path,"/BAC1_funWei_rho=",cor_par,"_",substr(Sys.Date(),6,10),".xlsx"))

t2<-Sys.time()
print(t2-t1)