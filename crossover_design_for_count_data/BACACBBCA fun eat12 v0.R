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

sim.BAC3<-function(seq,dataType){
  
  
  #MLE, null MLE, Matrix I, Matrix V, invI
  mle.1 <- mle.0 <- matrix(, nrow = sim_time, ncol = length(param))
  colnames(mle.1) <- c("tao", "eta1", "eta2", "gam1", "gam2", "del1", "del2")
  colnames(mle.0) <- c("tao.0", "eta1.0", "eta2.0", "gam1.0", "gam2.0", "del1.0", "del2.0")
  A<-B<-A0<-B0<-list()
  #variance of eta
  S.na<-S.rb<-W.na<-W.rb<-c()
  #for testing
  var.na<-var.rb<-0
  
  
  set.seed(110225021)
  
  for (i in 1:sim_time){
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
    #null MLE eta2=0
    #---------------------------------
    mod.0 <- glm(Y ~ Z1 + Z2 + G1 + G2, family = poisson(link = "log"), df.cor)
    mle.0[i,]<- append(append(mod.0$coefficients, 0, after=1), 0, after=2)
    #---------------------------------
    #Matrix I & Matrix V
    #---------------------------------
    Mat = MatAB.Po.BACACBBCA(DT=Type,NN=seq,YY=yy,MLE=mle.1[i,],Xs = xmat,mat.type = 'true')
    A[[i]]<-Mat$Mat.A;    B[[i]]<-Mat$Mat.B
    
    
    var.na<-var.na+(solve(A[[i]])/(3*seq))
    var.rb<-var.rb+(solve(A[[i]]) %*% B[[i]] %*% solve(A[[i]])/(3*seq))
    #---------------------------------
    # null Matrix I & Matrix V
    #---------------------------------
    Mat0 = MatAB.Po.BACACBBCA(DT=Type,NN=seq,YY=yy,MLE=mle.0[i,],Xs = xmat,mat.type = 'null')
    A0[[i]]<-Mat$Mat.A;    B0[[i]]<-Mat$Mat.B
    
    #=====================================
    #score test
    #=====================================
    # mle.0[i,] = c(tao.0,0,0,gam1.0,gam2.0,del1.0,del2.0)
    # = c(mle.0[i,1],0,0,mle.0[i,4],mle.0[i,5],mle.0[i,6],mle.0[i,7])
    s0.1 = seq * ( mean(yy$y11) - exp(mle.0[i,1])+ mean(yy$y23) - exp(mle.0[i,1]+mle.0[i,5]+mle.0[i,6]) + mean(yy$y31)- exp(mle.0[i,1]+mle.0[i,7]) )
    s0.2 = seq * ( mean(yy$y13) - exp(mle.0[i,1]+mle.0[i,5])+ mean(yy$y22) - exp(mle.0[i,1]+mle.0[i,4]+mle.0[i,6]) + mean(yy$y32)- exp(mle.0[i,1]+mle.0[i,4]+mle.0[i,7]) )
    s0 = matrix(c(s0.1,s0.2),ncol = 2,nrow = 1)
    S.na[i]<-s0 %*% solve(A0[[i]]) %*% t(s0) / (3*seq); S.rb[i]<- s0 %*% solve(B0[[i]]) %*% t(s0) / (3*seq)
    
    #=====================================
    #wald test
    #=====================================
    etai<-as.matrix(c(mle.1[i,2],mle.1[i,3]))
    W.na[i]<- t(etai)%*% A0[[i]] %*% etai * 3*seq
    W.rb[i]<- t(etai)%*% A0[[i]] %*% solve(B0[[i]]) %*% A0[[i]] %*% etai * 3*seq 
  }
  
  #模擬sim_time後的所有值
  Estimates<-signif(colMeans(mle.1), digits = 6)
  var.na0<-diag(var.na/sim_time);var.rb0<-diag(var.rb/sim_time)
  VARs<-c(var(mle.1[,2]),sd(mle.1[,2]),var.na0[1],sqrt(var.na0[1]),var.rb0[1],sqrt(var.rb0[1]),
          var(mle.1[,3]),sd(mle.1[,3]),var.na0[2],sqrt(var.na0[2]),var.rb0[2],sqrt(var.rb0[2]))
  names(VARs)<-c("sv.eta1","std.eta1","var.na.eta1","std.na.eta1","var.rb.eta1","std.rb.eta1",
                 "sv.eta2","std.eta2","var.na.eta2","std.na.eta2","var.rb.eta2","std.rb.eta2")
  RES.sim.alpha<-data.frame(W.na,W.rb,S.na,S.rb)
  ALPHA<-apply(X = RES.sim.alpha, MARGIN = 2, FUN = function(x){sum(x > qchisq(0.95, 2))/sim_time})
  
  #計算估計量
  RES<-cbind(N=seq,t(Estimates),t(VARs),t(ALPHA))
  
}

res31<-do.call(rbind, lapply(c(25,50,150,250), sim.BAC3, dataType="Poisson")) 
write_xlsx(data.frame(res31),path = paste0(Path,"/BAC3_funPo_rho=",cor_par,"_",substr(Sys.Date(),6,10),".xlsx"))

res32<-do.call(rbind, lapply(c(25,50,150,250), sim.BAC3, dataType="Weibull")) 
write_xlsx(data.frame(res32),path = paste0(Path,"/BAC3_funWei_rho=",cor_par,"_",substr(Sys.Date(),6,10),".xlsx"))
