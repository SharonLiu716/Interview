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
  if (nchar(DT) == 4){
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
MatAB.Po.ABBA<-function(DT,NN,YY,MLE,Xs,mat.type){
  #DT=Type,NN=seq,YY=yy,MLE=c(t,0,g,d),Xs=xmat
  y11<-YY[,1]; y12<-YY[,2]; y21<-YY[,3]; y22<-YY[,4]
  Mhat<-exp(MLE%*%Xs)
  II<-matrix(0,nrow = length(MLE),ncol = length(MLE))
  for (j in 1:4){II[j,]<-Mhat%*%(Xs[j,]*t(Xs))/2}
  ii.ee<-II[2,2]
  ii.ep<-matrix(II[2,-2],nrow = 1,ncol = length(MLE)-1)
  ii.pp<-II[-2,-2]
  AA = ii.ee - ii.ep %*% solve(ii.pp) %*% t(ii.ep)
  if (mat.type == 'null'){
    vcov<-matrix(0,nrow = nchar(Type),ncol = nchar(Type))
    for (l in 1:nchar(Type)){for(ll in 1:nchar(Type)){ vcov[l,ll]<-sum((YY[,l]-Mhat[,l])*(YY[,ll]-Mhat[,ll]))/NN}  }
  }else{ vcov<-var(YY) }
  
  vv.tt = sum(diag(vcov))/2+ vcov[1,2]+vcov[3,4]
  vv.ee = (diag(vcov)[2]+diag(vcov)[3])/2
  vv.gg = (diag(vcov)[2]+diag(vcov)[4])/2
  vv.dd = (diag(vcov)[3]+diag(vcov)[4])/2
  vv.te = (diag(vcov)[2]+diag(vcov)[3])/2 + vcov[1,2]/2+vcov[3,4]/2
  vv.tg = (diag(vcov)[2]+diag(vcov)[4])/2 + vcov[1,2]/2+vcov[3,4]/2
  vv.td = (diag(vcov)[3]+diag(vcov)[4])/2 + vcov[3,4]
  vv.eg = diag(vcov)[2]/2 + vcov[3,4]/2
  vv.ed = diag(vcov)[3]/2 + vcov[3,4]/2
  vv.gd = diag(vcov)[4]/2 + vcov[3,4]/2
  
  VV<-matrix( c(vv.tt, vv.te, vv.tg, vv.td,
                vv.te, vv.ee, vv.eg, vv.ed, 
                vv.tg, vv.eg, vv.gg, vv.gd,
                vv.td, vv.ed, vv.gd, vv.td),nrow=length(MLE), ncol=length(MLE), byrow = TRUE)
  
  vv.pe = matrix(VV[2,-2], nrow = length(MLE)-1)
  vv.pp = VV[-2,-2]
  
  BB = vv.ee - 2 * ii.ep %*% solve(ii.pp) %*% vv.pe + 
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
  DY <- data.frame(YY)
  x <- c("y11", "y12", "y21", "y22")
  colnames(DY) <- x
  return(DY)
  
  
}
Data.LuiAB<-function(NN,G.M,G.B,Po.M){
  ui1<-rgamma(NN, shape = G.M/G.B, scale = G.B)
  ui2<-rgamma(NN, shape = G.M/G.B, scale = G.B)
  YL11<-mapply(rpois,n=1,lambda=ui1*Po.M[1]);YL12<-mapply(rpois,n=1,lambda=ui1*Po.M[2])
  YL21<-mapply(rpois,n=1,lambda=ui2*Po.M[3]);YL22<-mapply(rpois,n=1,lambda=ui2*Po.M[4])
  YY <- data.frame(YL11,YL12,YL21,YL22)
  colnames(YY) <- c("y11", "y12", "y21", "y22")
  return(YY)
}
#============================================
# mutual parameters
#============================================
Path="C:/0409pp"
# Path="C:/Users/User/Documents/Study_final/Sim_res"
setwd(Path)

sim_time=10000;cor_par=0;Type='ABBA'
param=c(0,0,0.5,0)
xmat=matrix(c(1,1,1,1, 0,1,1,0, 0,1,0,1, 0,0,1,1), nrow = 4, ncol = 4,byrow = TRUE)
mean.true=exp(param%*%xmat)

#============================================
# Weibull Count Data parameters
#============================================
W.t=50;d.ABBA<-c(0.61,0.62,0.65,0.65)
W.aa<-rep(2,nchar(Type));W.bb<-mapply(w.scale,w.shape=W.aa,w.mean=1/(mean.true-d.ABBA) * W.t)
# i=1;seq=100;dataType='Weibull'
g.m<-3;g.b<-1
sim.Lui12<-function(seq,dataType){
  
  
  #MLE, null MLE, Matrix I, Matrix V, invI
  mle.1 <- mle.0 <- matrix(, nrow = sim_time, ncol = length(param))
  colnames(mle.1) <- c("tao", "eta", "gam", "del")
  colnames(mle.0) <- c("tao.0", "eta.0", "gam.0", "del.0")
  
  #variance of eta & type I error
  Lna1<-Lrb1<-Sna1<-Srb1<-Wna1<-Wrb1<-var.na<-var.rb<-A<-B<-A0<-B0<-c()

  #Lui
  eta.Lui<-var.Lui<-Lui7<-c();Lui7Z<-0
  
  set.seed(110225021)
  
  for (i in 1:sim_time){
    
    if (dataType == "Poisson"){
      yy<-Data.Po(DT=Type,TM=mean.true,NN=seq,Rho=cor_par)
      Y<-as.vector(unlist(yy))
    }else if (dataType == "Weibull"){
      yy<-Data.Wei.ABBA(Rho=cor_par,NN=seq,W.shape=W.aa,W.scale=W.bb,W.time=W.t)
      Y<-as.vector(unlist(yy))
    }else{
      yy<-Data.LuiAB(NN=seq,G.M = g.m,G.B = g.b,Po.M = mean.true)
      Y<-as.vector(unlist(yy))
    }
    X = c(rep(0,seq), rep(1,2*seq), rep(0,seq))
    Z = c(rep(0,seq), rep(1,seq), rep(0,seq), rep(1,seq))
    G = c(rep(0,2*seq), rep(1,2*seq))
    df.cor = data.frame(Y,X,Z,G)
    #---------------------------------
    #MLE
    #---------------------------------
    mod.1 <- glm(Y ~ X + Z + G, family = poisson(link = "log"), df.cor)
    mle.1[i,]<-mod.1$coefficients
    #---------------------------------
    #null MLE
    #---------------------------------
    mod.0 <- glm(Y ~ Z + G, family = poisson(link = "log"), df.cor)
    mle.0[i,]<- append(mod.0$coefficients, 0, after=1) 
    #---------------------------------
    #Matrix I & Matrix V
    #---------------------------------
    Mat = MatAB.Po.ABBA(DT=Type,NN=seq,YY=yy,MLE=mle.1[i,],Xs = xmat,mat.type = 'true')
    A[i]<-Mat$Mat.A;    B[i]<-Mat$Mat.B
    
    var.na[i]<-1/A[i]/(2*seq);    var.rb[i]<-B[i]/A[i]/A[i]/(2*seq)
    #============================================
    # Lui 2012
    #============================================
    syy<-colSums(yy)
    phat<-c(syy[2]/sum(syy[1:2]),syy[4]/sum(syy[3:4]))
    names(phat)<-c("P1","P2")
    
    eta.Lui[i]<-(log(syy[2])+log(syy[3])-log(syy[1])-log(syy[4]))/2
    pbar<-(syy[2]+syy[4])/sum(syy)
    var.Lui[i]<- ( 1/sum(syy[1:2])+1/sum(syy[3:4]))/ (4*(pbar*(1-pbar)))
    
    Lui7[i]<-eta.Lui[i]^2/var.Lui[i]
    zz<-eta.Lui[i]/ sqrt(var.Lui[i])
    if ( zz > -qnorm(0.05/2,lower.tail = FALSE) & zz < qnorm(0.05/2,lower.tail = FALSE) )  Lui7Z = Lui7Z+1
    #---------------------------------
    #log LR test 
    #---------------------------------
    lik<-function(par){ 
      ll=sum(par[1]*yy$y11-exp(par[1])+(par[1]+par[2]+par[3])*yy$y12-exp(par[1]+par[2]+par[3]))+
        sum((par[1]+par[2]+par[4])*yy$y21-exp(par[1]+par[2]+par[4])+ (par[1]+par[3]+par[4])*yy$y22-exp(par[1]+par[3]+par[4]))
      return(ll)
    }
    
    #=====================================
    #LR Test
    #=====================================
    l1 = lik(mle.1[i,]); l0 = lik(mle.0[i,])
    Lna1[i]<-2*(l1-l0)
    Lrb1[i]<-2 * A[i]/B[i] * (l1-l0)
    
    #---------------------------------
    # null Matrix I & Matrix V
    #---------------------------------
    Mat0 =  MatAB.Po.ABBA(DT=Type,NN=seq,YY=yy,MLE=mle.0[i,],Xs = xmat,mat.type = 'null')
    A0[i]<-Mat$Mat.A;    B0[i]<-Mat$Mat.B
    
    #=====================================
    #score test
    #=====================================
    # mle.0[i,] = c(tao.0,gam.0,del.0)
    # = c(mle.0[i,1],0,mle.0[i,3],mle.0[i,4])
    s0 = seq * ( mean(yy$y12) - exp(mle.0[i,1]+mle.0[i,3]) + mean(yy$y21)- exp(mle.0[i,1]+mle.0[i,4]) )
    Sna1[i]<- s0 / A0[i] * s0 / (2*seq)
    Srb1[i]<- s0 / B0[i] * s0 / (2*seq)
    
    #=====================================
    #wald test
    #=====================================
    Wna1[i]<- mle.1[i,2] * A0[i] * mle.1[i,2] * 2*seq
    Wrb1[i]<- mle.1[i,2] * A0[i]^2 / B0[i] * mle.1[i,2] * 2*seq
  }
  
  #模擬sim_time後的所有值
  eta.hat<-signif(colMeans(mle.1), digits = 6)
  Estimates<-c(seq,eta.hat,mean(eta.Lui))
  VARs<-c(var(mle.1[,2]),var(eta.Lui),mean(var.na),mean(var.rb),mean(var.Lui))
  names(VARs)<-c("sv.eta","sv.eta.Lui","var.na.eta", "var.rb.eta", "var.Lui.eta")
  STDs<-c( sd(mle.1[,2]), sd(eta.Lui),sqrt(mean(var.na)),sqrt(mean(var.rb)),sqrt(mean(var.Lui)) ) 
  names(STDs)<-c("std.eta","std.eta.Lui","std.na.eta","std.rb.eta","std.Lui.eta")
  RES.sim.alpha<-data.frame(Lna1,Lrb1,Sna1,Srb1,Wna1,Wrb1,Lui7)
  ALPHA<-apply(X = RES.sim.alpha, MARGIN = 2, FUN = function(x){sum(x > qchisq(0.95, 1))/sim_time})
  
  #計算估計量
  
  RES<-cbind(t(Estimates),t(VARs),t(STDs),t(ALPHA))
  # #模擬sim_time後的所有值
  # Estimates<-signif(colMeans(mle.1), digits = 6)
  # VARs<-c(var(mle.1[,2]),sd(mle.1[,2]),mean(var.na),sqrt(mean(var.na)),mean(var.rb),sqrt(mean(var.rb)))
  # names(VARs)<-c("sv.eta","std.eta","var.na.eta","std.na.eta","var.rb.eta","std.rb.eta")
  # Lui<- c(mean(eta.Lui),mean(var.Lui),sqrt(mean(var.Lui)))
  # names(Lui)<-c("eta.Lui","var.Lui.eta","std.Lui.eta")
  # RES.sim.alpha<-data.frame(Lna1,Lrb1,Sna1,Srb1,Wna1,Wrb1,Lui7)
  # 
  # ALPHA<-apply(X = RES.sim.alpha, MARGIN = 2, FUN = function(x){sum(x > qchisq(0.95, 1))/sim_time})
  # RES<-cbind(N=seq,t(Estimates),t(VARs),t(Lui),t(ALPHA),1-Lui7Z/sim_time)
  
}


# Po50AB<-sim.Lui12(50,"Poisson")
# Wei50AB<-sim.Lui12(50,"Weibull")

res1<-do.call(rbind, lapply(c(25,50,150,250), sim.Lui12, dataType="Poisson")) 
write_xlsx(data.frame(res1),path = paste0(Path,"/PoLui_AB_rho=",cor_par,"_",substr(Sys.Date(),6,10),".xlsx"))

res2<-do.call(rbind, lapply(c(25,50,150,250), sim.Lui12, dataType="Weibull")) 
write_xlsx(data.frame(res2),path = paste0(Path,"/WeiLui_AB_rho=",cor_par,"_",substr(Sys.Date(),6,10),".xlsx"))

B1<-do.call(rbind, lapply(c(15,30,50,100), sim.Lui12, dataType="Lui12")) 
write_xlsx(data.frame(B1),path = paste0(Path,"/Lui12_AB_gam0.5",substr(Sys.Date(),6,10),".xlsx"))
B2<-do.call(rbind, lapply(c(15,30,50,100), sim.Lui12, dataType="Lui12")) 
B5<-do.call(rbind, lapply(c(15,30,50,100), sim.Lui12, dataType="Lui12")) 


