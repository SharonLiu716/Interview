rm(list=ls(all=TRUE)) 
require(faraway);require(numDeriv);require(MASS);require(extraDistr);
require(simstudy);library(emplik);library(copula);library(writexl)
#============================================
# ABBA 
#============================================
xmat=matrix(c(1,1,1,1, 0,1,1,0, 0,1,0,1, 0,0,1,1), nrow = 4, ncol = 4,byrow = TRUE)
MatAB.Po.ABBA<-function(DT,NN,YY,MLE,Xs,mat.type){
  #DT=Type,NN=14,YY=yy,MLE=c(t,0,g,d),Xs=xmat
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
#============================================
# 
#============================================
# Path="C:/0409pp"
Path="C:/Users/User/Documents/Study_final/Sim_res"
setwd(Path)
library(BE)
data.abba<-NCAResult4BE
y11<-data.abba[data.abba$GRP == 'RT' & data.abba$PRD == 1,]$Cmax
y12<-data.abba[data.abba$GRP == 'RT' & data.abba$PRD == 2,]$Cmax 
y21<-data.abba[data.abba$GRP == 'TR' & data.abba$PRD == 1,]$Cmax 
y22<-data.abba[data.abba$GRP == 'TR' & data.abba$PRD == 2,]$Cmax 

#Albert data bioequivalence
y11<-c(1076,748,615,439,1097,814,635,812,549,640)
y12<-c(988,519,1284,734,1264,922,1040,719,413,971)
y21<-c(1127,580,778,831,1153,841,1023,677,862,913)
y22<-c(586,734,770,769,1088,805,1297,1140,662,595)

# not significant
y11<-c(74.675,96.4,101.95,79.05,79.05,85.95,69.725,86.275,112.675,99.525,89.425,55.175)
y12<-c(73.675,93.25,102.125,69.45,69.025,68.7,59.425,76.125,114.875,116.25,64.175,74.575)
y21<-c(74.825,86.875,81.675,92.7,50.45,66.125,122.45,99.075,86.35,49.925,42.7,91.725)
y22<-c(37.35,51.925,72.175,77.5,71.875,94.025,124.975,85.225,95.925,67.1,59.425,114.05)

# import data(use it finally)
y21<-c(360.11, 412.69, 423.98, 1359.86,2403.67, 409.05, 2434.96, 476.31,565.66, 1344.96,441.33,1458.82,500.45,139.61)
y22<-c(278.48, 196.19, 681.34, 1191.91, 1449.63, 468.31,2509.14,265.09,1070.06,1064.23,2057.38,1899.42,945.48,213.63)
y11<-c(605.45,608.67,392.45,571.51, 738.77, 364.8,884.73, 557.16,483.04, 123.9,2552.32, 403.34,247.49, 449.56)
y12<-c(326.19,578.66,1046.37,733.8,669.17,717.01,907.64,995.68,579.72,297.07,1702.07,656.33,905.35,688.03)

yy<-data.frame(y11,y12,y21,y22);Y<-as.vector(unlist(yy));Type='ABBA';nn<-length(y11)
X = c(rep(0,nn), rep(1,2*nn), rep(0,nn))
Z = c(rep(0,nn), rep(1,nn), rep(0,nn), rep(1,nn))
G = c(rep(0,2*nn), rep(1,2*nn))
df.cor = data.frame(Y,X,Z,G)
#---------------------------------
#MLE
#---------------------------------
mod.1 <- glm(Y ~ X + Z + G, family = poisson(link = "log"), df.cor)
mle.1<-mod.1$coefficients
#---------------------------------
#null MLE
#---------------------------------
mod.0 <- glm(Y ~ Z + G, family = poisson(link = "log"), df.cor)
mle.0<- append(mod.0$coefficients, 0, after=1) 
#---------------------------------
#Matrix I & Matrix V
#---------------------------------
Mat = MatAB.Po.ABBA(DT=Type,NN=nn,YY=yy,MLE=mle.1,Xs = xmat,mat.type = 'true')
A<-Mat$Mat.A;    B<-Mat$Mat.B

var.na<-1/A/(2*nn);    var.rb<-B/A/A/(2*nn)

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
l1 = lik(mle.1); l0 = lik(mle.0)
Lna<-2*(l1-l0);Lrb<-2 * A/B * (l1-l0)
pchisq(Lna,1,lower.tail = F);pchisq(Lrb,1,lower.tail = F)
#=====================================
#LR test C.I. lower and upper
#=====================================
#Naive
#-------------------------------------
LR.na  <- function(e){
  Mod.0 <- glm(formula = Y~Z+G+offset(e*X),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=1) 
  l.0 = lik(mlee)
  u = 2*(l1-l.0)
  list("-2LLR"=u)
}

LR.naci<-findUL(step=0.01, fun=LR.na, MLE = mle.1[2]) 
ci.Lna = c(LR.naci$Low,LR.naci$Up,LR.naci$Up-LR.naci$Low)# c('Lna.LB','Lna.UB','Lna.AL')

#-------------------------------------
#Robust
#-------------------------------------
LR.rb <- function(e){
  Mod.0 <- glm(formula = Y~Z+G+offset(e*X),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=1) 
  l.0 = lik(mlee)
  u = 2* (A %*% solve(B))*(l1-l.0)
  list("-2LLR"=u)
}

LR.rbci<-findUL(step=0.01, fun=LR.rb, MLE = mle.1[2]) 
ci.Lrb = c(LR.rbci$Low,LR.rbci$Up,LR.rbci$Up-LR.rbci$Low) #c('Lrb.LB','Lrb.UB','Lrb.AL')


#---------------------------------
# null Matrix I & Matrix V
#---------------------------------
Mat0 =  MatAB.Po.ABBA(DT=Type,NN=nn,YY=yy,MLE=mle.0,Xs = xmat,mat.type = 'null')
A0<-Mat$Mat.A;    B0<-Mat$Mat.B

#=====================================
#score test
#=====================================
# mle.0[i,] = c(tao.0,gam.0,del.0)
# = c(mle.0[i,1],0,mle.0[i,3],mle.0[i,4])
s0 = nn * ( mean(yy$y12) - exp(mle.0[1]+mle.0[3]) + mean(yy$y21)- exp(mle.0[1]+mle.0[4]) )
Sna<- s0 / A0 * s0 / (2*nn);Srb<- s0 / B0 * s0 / (2*nn)
pchisq(Sna,1,lower.tail = F);pchisq(Srb,1,lower.tail = F)

#=====================================
#score test C.I. lower and upper
#=====================================
#Naive
#-------------------------------------
S.na <- function(e){
  Mod.0 <- glm(formula = Y~Z+G+offset(e*X),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=1) 
  mat = MatAB.Po.ABBA(DT=Type,NN=nn,YY=yy,MLE=mlee,Xs = xmat,mat.type = 'null')
  aa<-mat$Mat.A;    bb<-mat$Mat.B
  ss0 = nn * ( mean(yy$y12) - exp(mlee[1]+e+mlee[3]) + mean(yy$y21)- exp(mlee[1]+e+mlee[4]) )
  s.na = ss0 / aa * ss0 / (2*nn)
  list("-2LLR"=s.na)
}

S.naci<-findUL(step=0.01, fun=S.na, MLE = mle.1[2]) 
ci.Sna = c(S.naci$Low,S.naci$Up,S.naci$Up-S.naci$Low) #c('Sna.LB','Sna.UB','Sna.AL')

#-------------------------------------
#Robust
#-------------------------------------
S.rb <- function(e){
  Mod.0 <- glm(formula = Y~Z+G+offset(e*X),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=1) 
  mat = MatAB.Po.ABBA(DT=Type,NN=14,YY=yy,MLE=mlee,Xs = xmat,mat.type = 'null')
  aa<-mat$Mat.A;    bb<-mat$Mat.B
  ss0 = 14 * ( mean(yy$y12) - exp(mlee[1]+e+mlee[3]) + mean(yy$y21)- exp(mlee[1]+e+mlee[4]) )
  s.rb = ss0 / bb * ss0 / (2*14)
  list("-2LLR"=s.rb)
}

S.rbci<-findUL(step=0.01, fun=S.rb, MLE = mle.1[2]) 
ci.Srb = c(S.rbci$Low,S.rbci$Up,S.rbci$Up-S.rbci$Low) #c('Srb.LB','Srb.UB','Srb.AL')
#=====================================
#wald test
#=====================================
Wna<- mle.1[2] * A0 * mle.1[2] * 2*nn
Wrb<- mle.1[2] * A0^2 / B0 * mle.1[2] * 2*nn
pchisq(Wna,1,lower.tail = F);pchisq(Wrb,1,lower.tail = F)
#=====================================
#Wald test C.I. lower and upper
#=====================================
#Naive
#-------------------------------------
W.na <- function(e){
  Mod.0 <- glm(formula = Y~Z+G+offset(e*X),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=1) 
  mat = MatAB.Po.ABBA(DT=Type,NN=nn,YY=yy,MLE=mlee,Xs = xmat,mat.type = 'null')
  aa<-mat$Mat.A;    bb<-mat$Mat.B
  w.na = 2*nn * (mle.1[2]-e)^2 * aa
  list("-2LLR"=w.na)
}
W.naci<-findUL(step=0.01, fun=W.na, MLE = mle.1[2])
ci.Wna = c(W.naci$Low,W.naci$Up,W.naci$Up-W.naci$Low) #c('Wna.LB','Wna.UB','Wna.AL')

#-------------------------------------
#Robust
#-------------------------------------
W.rb <- function(e){
  Mod.0 <- glm(formula = Y~Z+G+offset(e*X),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=1) 
  mat = MatAB.Po.ABBA(DT=Type,NN=nn,YY=yy,MLE=mlee,Xs = xmat,mat.type = 'null')
  aa<-mat$Mat.A;    bb<-mat$Mat.B
  w.rb = 2*nn * (mle.1[2]-e)^2 * aa^2/bb 
  list("-2LLR"=w.rb)
}
W.rbci<-findUL(step=0.01, fun=W.rb, MLE = mle.1[2])
ci.Wrb = c(W.rbci$Low,W.rbci$Up,W.rbci$Up-W.rbci$Low) #c('Wrb.LB','Wrb.UB','Wrb.AL')
#------------
# Lui
#------------
syy<-colSums(yy);pbar<-(syy[2]+syy[4])/sum(syy)
eta.Lui<-(log(syy[2])+log(syy[3])-log(syy[1])-log(syy[4]))/2
var.Lui<- ( 1/sum(syy[1:2])+1/sum(syy[3:4]))/ (4*(pbar*(1-pbar)))
Lui7<-eta.Lui^2/var.Lui;zz<-eta.Lui/ sqrt(var.Lui)
pnorm(Lui7,1,lower.tail = FALSE)
eta.Lui-1.96* sqrt(var.Lui);eta.Lui+1.96* sqrt(var.Lui)

mle.1[2];var.na;var.rb;eta.Lui;var.Lui
Wna;Wrb;Lna;Lrb;Sna;Srb;Lui7
pchisq(Wna,1,lower.tail = F);pchisq(Wrb,1,lower.tail = F)
pchisq(Lna,1,lower.tail = F);pchisq(Lrb,1,lower.tail = F)
pchisq(Sna,1,lower.tail = F);pchisq(Srb,1,lower.tail = F)
pnorm(Lui7,1,lower.tail = FALSE)
ci.Wna;ci.Wrb;ci.Lna;ci.Lrb;ci.Sna;ci.Srb
