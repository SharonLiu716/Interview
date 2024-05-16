rm(list=ls(all=TRUE)) 
require(faraway);require(numDeriv);require(MASS);require(extraDistr);
require(simstudy);library(emplik);library(copula);library(writexl)
#============================================
# ABBBAA
#============================================
MatAB.Po.ABBBAA<-function(DT,NN,YY,MLE,Xs,mat.type){
  y11<-YY[,1];y12<-YY[,2];y13<-YY[,3];y21<-YY[,4];y22<-YY[,5];y23<-YY[,6]
  Mhat<-exp(MLE%*%Xs)
  II<-matrix(0,nrow = length(MLE),ncol = length(MLE))
  for (j in 1:length(MLE)){II[j,]<-Mhat%*%(Xs[j,]*t(Xs))/2}
  ii.ee<-II[2,2]
  ii.ep<-matrix(II[2,-2],nrow = 1,ncol = length(MLE)-1)
  ii.pp<-II[-2,-2]
  AA = ii.ee - ii.ep %*% solve(ii.pp) %*% t(ii.ep)
  #DT=Type,NN=17,YY=yy,MLE=c(t,0,g,d),Xs=xmat
  if (mat.type == 'null'){
    vcov<-matrix(0,nrow = nchar(Type),ncol = nchar(Type))
    for (l in 1:nchar(Type)){for(ll in 1:nchar(Type)){ vcov[l,ll]<-sum((YY[,l]-Mhat[,l])*(YY[,ll]-Mhat[,ll]))/NN}  }
  }else{ vcov<-var(YY) }
  
  # yii: var(y11),var(y12),var(y13),var(y21),var(y22),var(y23)
  # yij:cov(y11,y12),cov(y11,y13),cov(y12,y13),cov(y21,y22),cov(y21,y23),cov(y22,y23)
  yii<-diag(vcov)
  yij<-c(diag(vcov[-1,])[1],vcov[1,3],diag(vcov[-1,])[2],diag(vcov[-1,])[4],vcov[4,6],diag(vcov[-1,])[5])
  vv.tt = sum(yii)/2 + sum(yij)
  vv.ee = (yii[2]+yii[3]+yii[4])/2 + yij[3]
  vv.g1g1 = (yii[2]+yii[5])/2
  vv.g2g2 = (yii[3]+yii[6])/2
  vv.dd = sum(yii[4:6])/2+sum(yij[4:6])
  vv.te = (yii[2]+yii[3]+sum(yij[1:3])+yij[3])/2+(yii[4]+yij[4]+yij[5])/2
  vv.tg1 = (yii[2]+yij[1]+yij[3])/2+(yii[5]+yij[4]+yij[6])/2
  vv.tg2 = (yii[3]+yij[2]+yij[3])/2+(yii[6]+yij[5]+yij[6])/2
  vv.td = sum(yii[4:6])/2+sum(yij[4:6])
  vv.eg1 = (yii[2]+yij[3]+yij[4])/2
  vv.eg2 = (yii[3]+yij[3]+yij[5])/2
  vv.ed = (yii[4]+yij[4]+yij[5])/2
  vv.g1g2 = (yij[3]+yij[6])/2
  vv.g1d = (yii[5]+yij[4]+yij[6])/2
  vv.g2d = (yii[6]+yij[5]+yij[6])/2
  
  VV<-matrix( c(vv.tt,  vv.te,  vv.tg1,  vv.tg2,  vv.td,
                vv.te,  vv.ee,  vv.eg1,  vv.eg2,  vv.ed, 
                vv.tg1, vv.eg1, vv.g1g1, vv.g1g2, vv.g1d,
                vv.tg2, vv.eg2, vv.g1g2, vv.g2g2, vv.g2d,
                vv.td,  vv.ed,  vv.g1d,  vv.g2d,  vv.td),nrow=length(MLE), ncol=length(MLE), byrow = TRUE)
  
  #---------------------------------
  #Matrix B
  #---------------------------------
  vv.pe = matrix(VV[2,-2], nrow = length(MLE)-1)
  vv.pp = VV[-2,-2]
  BB = vv.ee - 2 * ii.ep %*% solve(ii.pp) %*% vv.pe + 
    ii.ep %*% solve(ii.pp) %*% vv.pp %*% solve(ii.pp) %*% t(ii.ep)
  
  
  mat.res<-list("Mat.A" = AA,"Mat.B" = BB)
  
  return(mat.res)
  
}
xmat=matrix(c(rep(1,6), 0,rep(1,3),rep(0,2),rep(c(0,1,0),2),rep(c(0,0,1),2) ,rep(0,3) ,rep(1,3)), nrow = 5, ncol = 6,byrow = TRUE)
y11<-c(159,153,160,160,170,174,175,154,160,160,145,148,170,125,140,125,150)
y12<-c(140,172,156,200,170,132,155,138,170,160,140,154,170,130,112,140,150)
y13<-c(137,155,140,132,160,130,155,150,168,170,140,138,150,130,95,125,145)
y21<-c(165,160,140,140,158,180,170,140,126,130,144,140,120,145,155,168,150)
y22<-c(154,165,150,125,160,165,160,158,170,125,140,160,145,150,130,168,160)
y23<-c(173,140,180,130,180,160,160,148,200,150,120,140,120,150,140,168,180)

Type='ABBBAA'
yy<-data.frame(y11,y12,y13,y21,y22,y23);Y<-as.vector(unlist(yy))
X = c(rep(0,17), rep(1,3*17), rep(0,2*17))
Z1 = c(rep(0,17), rep(1,17), rep(0,2*17), rep(1,17), rep(0,17))
Z2 = c(rep(0,2*17),  rep(1,17), rep(0,2*17), rep(1,17))
G = c(rep(0,3*17), rep(1,3*17))
df.cor = data.frame(Y,X,Z1,Z2,G)
#---------------------------------
#MLE
#---------------------------------
mod.1 <- glm(Y ~ X + Z1 + Z2 + G, family = poisson(link = "log"), df.cor)
mle.1<-mod.1$coefficients
#---------------------------------
#null MLE
#---------------------------------
mod.0 <- glm(Y ~ Z1+Z2 + G, family = poisson(link = "log"), df.cor)
mle.0<- append(mod.0$coefficients, 0, after=1) 
#---------------------------------
#Matrix I & Matrix V
#---------------------------------
Mat = MatAB.Po.ABBBAA(DT=Type,NN=17,YY=yy,MLE=mle.1,Xs = xmat,mat.type = 'true')
A<-Mat$Mat.A;    B<-Mat$Mat.B

var.na<-1/A/(2*17);    var.rb<-B/A/A/(2*17)

#---------------------------------
#log LR test 
#---------------------------------
lik<-function(par){ 
  ll=sum(par[1]*yy$y11-exp(par[1])+(par[1]+par[2]+par[3])*yy$y12-exp(par[1]+par[2]+par[3])+(par[1]+par[2]+par[4])*yy$y13-exp(par[1]+par[2]+par[4]))+
    sum((par[1]+par[2]+par[5])*yy$y21-exp(par[1]+par[2]+par[5])+ (par[1]+par[3]+par[5])*yy$y22-exp(par[1]+par[3]+par[5]) + (par[1]+par[4]+par[5])*yy$y23-exp(par[1]+par[4]+par[5]))
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
  Mod.0 <- glm(formula = Y~Z1+Z2+G+offset(e*X),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=1) 
  l.0 = lik(mlee)
  u = 2*(l1-l.0)
  list("-2LLR"=u)
}

LR.naci<-findUL(step=0.01, fun=LR.na, MLE = mle.1[2]) 
ci.Lna = c(LR.naci$Low,LR.naci$Up,LR.naci$Up-LR.naci$Low)#c('Lna.LB','Lna.UB','Lna.AL')

#-------------------------------------
#Robust
#-------------------------------------
LR.rb <- function(e){
  Mod.0 <- glm(formula = Y~Z1+Z2+G+offset(e*X),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=1) # eta1 =0 after 1 ,eta2 = 0 after 2
  l.0 = lik(mlee)
  u = 2* (A %*% solve(B))*(l1-l.0)
  list("-2LLR"=u)
}

LR.rbci<-findUL(step=0.01, fun=LR.rb, MLE = mle.1[2]) # eta1 = mle.1[i,2]  ,eta2 = mle.1[i,3]
ci.Lrb = c(LR.rbci$Low,LR.rbci$Up,LR.rbci$Up-LR.rbci$Low) #c('Lrb.LB','Lrb.UB','Lrb.AL')

#---------------------------------
# null Matrix I & Matrix V
#---------------------------------
Mat0 =  MatAB.Po.ABBBAA(DT=Type,NN=17,YY=yy,MLE=mle.0,Xs = xmat,mat.type = 'null')
A0<-Mat$Mat.A;    B0<-Mat$Mat.B

#=====================================
#score test
#=====================================
# mle.0[i,] = c(tao.0,0,gam1.0,gam2.0,del)
# = c(mle.0[i,1],0,mle.0[i,3],mle.0[i,4],mle.0[i,5])
s0 = 17 * ( mean(yy$y12) - exp(mle.0[1]+mle.0[3])+ mean(yy$y13) - exp(mle.0[1]+mle.0[4]) + mean(yy$y21)- exp(mle.0[1]+mle.0[5]) )
Sna<- s0 / A0 * s0 / (2*17);Srb<- s0 / B0 * s0 / (2*17)
pchisq(Sna,1,lower.tail = F);pchisq(Srb,1,lower.tail = F)

#=====================================
#score test C.I. lower and upper
#=====================================
#Naive
#-------------------------------------
S.na <- function(e){
  Mod.0 <- glm(formula = Y~Z1+Z2+G+offset(e*X),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=1) 
  mat = MatAB.Po.ABBBAA(DT=Type,NN=17,YY=yy,MLE=mlee,Xs = xmat,mat.type = 'null')
  aa<-mat$Mat.A;    bb<-mat$Mat.B
  ss0 = 17 * ( mean(yy$y12) - exp(mlee[1]+e+mlee[3])+ mean(yy$y13) - exp(mlee[1]+e+mlee[4]) + mean(yy$y21)- exp(mlee[1]+e+mlee[5]) )
  s.na = ss0 / aa * ss0 / (2*17)
  list("-2LLR"=s.na)
}

S.naci<-findUL(step=0.01, fun=S.na, MLE = mle.1[2]) 
ci.Sna = c(S.naci$Low,S.naci$Up,S.naci$Up-S.naci$Low) #c('Sna.LB','Sna.UB','Sna.AL')

#-------------------------------------
#Robust
#-------------------------------------
S.rb <- function(e){
  Mod.0 <- glm(formula = Y~Z1+Z2+G+offset(e*X),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=1) 
  mat = MatAB.Po.ABBBAA(DT=Type,NN=17,YY=yy,MLE=mlee,Xs = xmat,mat.type = 'null')
  aa<-mat$Mat.A;    bb<-mat$Mat.B
  ss0 = 17 * ( mean(yy$y12) - exp(mlee[1]+e+mlee[3])+ mean(yy$y13) - exp(mlee[1]+e+mlee[4]) + mean(yy$y21)- exp(mlee[1]+e+mlee[5]) )
  s.rb = ss0 / bb * ss0 / (2*17)
  list("-2LLR"=s.rb)
}

S.rbci<-findUL(step=0.01, fun=S.rb, MLE = mle.1[2]) 
ci.Srb = c(S.rbci$Low,S.rbci$Up,S.rbci$Up-S.rbci$Low) #c('Srb.LB','Srb.UB','Srb.AL')

#=====================================
#wald test
#=====================================
Wna<- mle.1[2] * A0 * mle.1[2] * 2*17
Wrb<- mle.1[2] * A0^2 / B0 * mle.1[2] * 2*17
pchisq(Wna,1,lower.tail = F);pchisq(Wrb,1,lower.tail = F)
#=====================================
#Wald test C.I. lower and upper
#=====================================
#Naive
#-------------------------------------
W.na <- function(e){
  Mod.0 <- glm(formula = Y~Z1+Z2+G+offset(e*X),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=1) 
  mat = MatAB.Po.ABBBAA(DT=Type,NN=17,YY=yy,MLE=mlee,Xs = xmat,mat.type = 'null')
  aa<-mat$Mat.A;    bb<-mat$Mat.B
  w.na = 2*17 * (mle.1[2]-e)^2 * aa
  list("-2LLR"=w.na)
}
W.naci<-findUL(step=0.01, fun=W.na, MLE = mle.1[2])
ci.Wna = c(W.naci$Low,W.naci$Up,W.naci$Up-W.naci$Low) #c('Wna.LB','Wna.UB','Wna.AL')
#-------------------------------------
#Robust
#-------------------------------------
W.rb <- function(e){
  Mod.0 <- glm(formula = Y~Z1+Z2+G+offset(e*X),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=1) 
  mat = MatAB.Po.ABBBAA(DT=Type,NN=17,YY=yy,MLE=mlee,Xs = xmat,mat.type = 'null')
  aa<-mat$Mat.A;    bb<-mat$Mat.B
  w.rb = 2*17 * (mle.1[2]-e)^2 * aa^2/bb 
  list("-2LLR"=w.rb)
}

W.rbci<-findUL(step=0.01, fun=W.rb, MLE = mle.1[2])
ci.Wrb = c(W.rbci$Low,W.rbci$Up,W.rbci$Up-W.rbci$Low) #c('Wrb.LB','Wrb.UB','Wrb.AL')

# Test eta=0
var.na;var.rb
Wna;Wrb;Lna;Lrb;Sna;Srb
pchisq(Wna,1,lower.tail = F);pchisq(Wrb,1,lower.tail = F)
pchisq(Lna,1,lower.tail = F);pchisq(Lrb,1,lower.tail = F)
pchisq(Sna,1,lower.tail = F);pchisq(Srb,1,lower.tail = F)
ci.Wna;ci.Wrb;ci.Lna;ci.Lrb;ci.Sna;ci.Srb

