rm(list=ls(all=TRUE)) 
require(faraway);require(numDeriv);require(MASS);require(extraDistr);
require(simstudy);library(emplik);library(copula);library(writexl)
#============================================
# ABC 3 
#============================================
MatAB.Po.ABCBCACAB<-function(DT,NN,YY,MLE,Xs,mat.type){
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
  vv.te1 =  (yii[2]+yii[4]+yii[9])/3+(yij[1]+yij[3]+yij[4]+yij[5]+yij[8]+yij[9])/3
  vv.te2 =  (yii[3]+yii[5]+yii[7])/3+(yij[2]+yij[3]+yij[4]+yij[6]+yij[7]+yij[8])/3
  vv.tg1 = (yii[2]+yii[5]+yii[8])/3+(yij[1]+yij[3]+yij[4]+yij[6]+yij[7]+yij[9])/3
  vv.tg2 = (yii[3]+yii[6]+yii[9])/3+(yij[2]+yij[3]+yij[5]+yij[6]+yij[8]+yij[9])/3
  vv.td1 = sum(yii[4:6])/3+2*sum(yij[4:6])/3
  vv.td2 = sum(yii[7:9])/3+2*sum(yij[7:9])/3
  
  vv.e1e1 =  (yii[2]+yii[4]+yii[9])/3
  vv.e1e2 =  (yij[3]+yij[4]+yij[8])/3
  vv.e1g1 = (yii[2]+yij[4]+yij[9])/3
  vv.e1g2 = (yii[9]+yij[3]+yij[5])/3
  vv.e1d1 = (yii[4]+yij[4]+yij[5])/3
  vv.e1d2 = (yii[9]+yij[8]+yij[9])/3
  
  vv.e2e2 =  (yii[3]+yii[5]+yii[7])/3
  vv.e2g1 = (yii[5]+yij[3]+yij[7])/3
  vv.e2g2 = (yii[3]+yij[6]+yij[8])/3
  vv.e2d1 = (yii[5]+yij[4]+yij[6])/3
  vv.e2d2 = (yii[7]+yij[7]+yij[8])/3
  
  vv.g1g1 = (yii[2]+yii[5]+yii[8])/3
  vv.g1g2 = (yij[3]+yij[6]+yij[9])/3
  vv.g1d1 = (yii[5]+yij[4]+yij[6])/3
  vv.g1d2 = (yii[8]+yij[7]+yij[9])/3
  vv.g2g2 = (yii[3]+yii[6]+yii[9])/3
  vv.g2d1 = (yii[6]+yij[5]+yij[6])/3
  vv.g2d2 = (yii[9]+yij[8]+yij[9])/3
  vv.d1d1 = sum(yii[4:6])/3+2*sum(yij[4:6])/3
  vv.d1d2 = 0
  vv.d2d2 = sum(yii[7:9])/3+2*sum(yij[7:9])/3
  
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
Type='ABCBCACAB'
xmat=matrix(c(rep(1,9), rep(0:1,2),rep(0,4),1, 0,rep(c(0:1),3),rep(0,2), rep(c(0,1,0),3),rep(c(0,0,1),3),rep(0,3),rep(1,3),rep(0,3), rep(0,6),rep(1,3)), nrow = 7, ncol = 9,byrow = TRUE)
y11<-c(1376, 1655, 1342, 1863, 1384, 1180)
y12<-c(1246, 1517, 1294, 1755, 1535, 1245)
y13<-c(1151, 1366, 1317, 1462, 1289, 1082)
y21<-c(2088, 1938, 1344, 1748, 1640, 1287)
y22<-c(1864, 1804, 1312, 1353, 1284, 1000)
y23<-c(1392, 969, 903, 1339, 1370, 1078)
y31<-c(2238, 1855, 1627, 2012, 1677, 1547)
y32<-c(1724, 1298, 1186, 1626, 1497, 1297)
y33<-c(1272, 1233, 1066, 1010, 1059, 887)

yy<-data.frame(y11,y12,y13,y21,y22,y23,y31,y32,y33)
Y<-as.vector(unlist(yy))
X1 = c(rep(0:1,each=6,time=2),rep(0,each=6,time=4),rep(1,6))
X2 = c(rep(0,6),rep(c(0:1),each=6,time=3),rep(0,each=6,time=2))
Z1 = c(rep(c(0,1,0),each = 6,time=3))
Z2 = c(rep(c(0,0,1),each = 6,time=3))
G1 = c(rep(0,each=6,time=3),rep(1,each=6,time=3),rep(0,each=6,time=3))
G2 = c(rep(0,each=6,time=6),rep(1,each=6,time=3))
df.cor = data.frame(Y,X1,X2,Z1,Z2,G1,G2)

#---------------------------------
#MLE
#---------------------------------
mod.1 <- glm(Y ~ X1 + X2 + Z1 + Z2 + G1 + G2, family = poisson(link = "log"), df.cor)
mle.1<-mod.1$coefficients
#---------------------------------
#null MLE eta12=0
#---------------------------------
mod.0 <- glm(Y ~ Z1 + Z2 + G1 + G2, family = poisson(link = "log"), df.cor)
mle.0<- append(append(mod.0$coefficients, 0, after=1), 0, after=2)
#---------------------------------
#Matrix I & Matrix V
#---------------------------------
Mat = MatAB.Po.ABCBCACAB(DT=Type,NN=6,YY=yy,MLE=mle.1,Xs = xmat,mat.type = 'true')
A<-Mat$Mat.A;    B<-Mat$Mat.B

var.na<-solve(A)/(3*6)
var.rb<-(solve(A) %*% B %*% solve(A)/(3*6))

#---------------------------------
#log LR test 
#---------------------------------
lik<-function(par){ 
  ll=sum(par[1]*yy$y11-exp(par[1])+(par[1]+par[2]+par[4])*yy$y12-exp(par[1]+par[2]+par[4])+(par[1]+par[3]+par[5])*yy$y13-exp(par[1]+par[3]+par[5]))+
    sum((par[1]+par[2]+par[6])*yy$y21-exp(par[1]+par[2]+par[6])+ (par[1]+par[3]+par[4]+par[6])*yy$y22-exp(par[1]+par[3]+par[4]+par[6]) + (par[1]+par[5]+par[6])*yy$y23-exp(par[1]+par[5]+par[6]))+
    sum((par[1]+par[3]+par[7])*yy$y31-exp(par[1]+par[3]+par[7])+ (par[1]+par[4]+par[7])*yy$y32-exp(par[1]+par[4]+par[7]) + (par[1]+par[2]+par[5]+par[7])*yy$y33-exp(par[1]+par[2]+par[5]+par[7]))
  return(ll)
}
#---------------------------------
# null Matrix I & Matrix V
#---------------------------------
Mat0 = MatAB.Po.ABCBCACAB(DT=Type,NN=6,YY=yy,MLE=mle.0,Xs = xmat,mat.type = 'null')
A0<-Mat$Mat.A;    B0<-Mat$Mat.B

#=====================================
#score test
#=====================================
# mle.0[i,] = c(tao.0,0,0,gam1.0,gam2.0,del1.0,del2.0)
# = c(mle.0[1],0,0,mle.0[4],mle.0[5],mle.0[6],mle.0[7])
s0.1 = 6 * ( mean(yy$y12) - exp(mle.0[1]+mle.0[4])+ mean(yy$y21) - exp(mle.0[1]+mle.0[6]) + mean(yy$y33)- exp(mle.0[1]+mle.0[5]+mle.0[7]) )
s0.2 = 6 * ( mean(yy$y13) - exp(mle.0[1]+mle.0[5])+ mean(yy$y22) - exp(mle.0[1]+mle.0[4]+mle.0[6]) + mean(yy$y31)- exp(mle.0[1]+mle.0[7]) )

s0 = matrix(c(s0.1,s0.2),ncol = 2,nrow = 1)
Sna<-s0 %*% solve(A0) %*% t(s0) / (3*6);Srb<- s0 %*% solve(B0) %*% t(s0) / (3*6)
pchisq(Sna,2,lower.tail = F);pchisq(Srb,2,lower.tail = F)
#=====================================
#wald test
#=====================================
etai<-as.matrix(c(mle.1[2],mle.1[3]))
Wna<- t(etai)%*% A0 %*% etai * 3*6
Wrb<- t(etai)%*% A0 %*% solve(B0) %*% A0 %*% etai * 3*6 
pchisq(Wna,2,lower.tail = F);pchisq(Wrb,2,lower.tail = F)

# Test eta1=eta2=0
Wna;Wrb;Sna;Srb
pchisq(Sna,2,lower.tail = F);pchisq(Srb,2,lower.tail = F)
pchisq(Wna,2,lower.tail = F);pchisq(Wrb,2,lower.tail = F)
#=========================
# ABC1
#=========================
#---------------------------------
#null MLE eta1=0
#---------------------------------
mod.0 <- glm(Y ~ X2 +Z1 + Z2 + G1 + G2, family = poisson(link = "log"), df.cor)
mle.0<- append(mod.0$coefficients, 0, after=1)
#---------------------------------
#Matrix I & Matrix V
#---------------------------------
Mat = MatAB.Po.ABCBCACAB(DT=Type,NN=6,YY=yy,MLE=mle.1,Xs = xmat,mat.type = 'true')
A<-Mat$Mat.A;    B<-Mat$Mat.B

var.na1<-solve(A)[1,1]/(3*6)
var.rb1<-(solve(A) %*% B %*% solve(A)/(3*6))[1,1]
#---------------------------------
# null Matrix I & Matrix V
#---------------------------------
Mat0 = MatAB.Po.ABCBCACAB(DT=Type,NN=6,YY=yy,MLE=mle.0,Xs = xmat,mat.type = 'null')
A0<-Mat$Mat.A;    B0<-Mat$Mat.B
#---------------------------------
#log LR test 
#---------------------------------
lik<-function(par){ 
  ll=sum(par[1]*yy$y11-exp(par[1])+(par[1]+par[2]+par[4])*yy$y12-exp(par[1]+par[2]+par[4])+(par[1]+par[3]+par[5])*yy$y13-exp(par[1]+par[3]+par[5]))+
    sum((par[1]+par[2]+par[6])*yy$y21-exp(par[1]+par[2]+par[6])+ (par[1]+par[3]+par[4]+par[6])*yy$y22-exp(par[1]+par[3]+par[4]+par[6]) + (par[1]+par[5]+par[6])*yy$y23-exp(par[1]+par[5]+par[6]))+
    sum((par[1]+par[3]+par[7])*yy$y31-exp(par[1]+par[3]+par[7])+ (par[1]+par[4]+par[7])*yy$y32-exp(par[1]+par[4]+par[7]) + (par[1]+par[2]+par[5]+par[7])*yy$y33-exp(par[1]+par[2]+par[5]+par[7]))
  return(ll)
}
#=====================================
#LR Test
#=====================================
l1 = lik(mle.1); l0 = lik(mle.0)
Lna1<-2*(l1-l0);Lrb1<-2 * (A[1,1] %*% solve(B[1,1])) * (l1-l0)
pchisq(Lna1,1,lower.tail = F);pchisq(Lrb1,1,lower.tail = F)

#=====================================
#LR test C.I. lower and upper
#=====================================
#Naive
#-------------------------------------

LR.na  <- function(e){
  Mod.0 <- glm(formula = Y~ X2 + Z1 + Z2 + G1 + G2 + offset(e*X1),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=1)
  l.0 = lik(mlee);  u = 2*(l1-l.0)
  list("-2LLR"=u)
}

LR.naci<-findUL(step=0.01, fun=LR.na, MLE = mle.1[2])
ci.Lna1 = c(LR.naci$Low,LR.naci$Up,LR.naci$Up-LR.naci$Low)#c('Lna.LB','Lna.UB','Lna.AL')

#-------------------------------------
#Robust
#-------------------------------------
LR.rb <- function(e){
  Mod.0 <- glm(formula = Y~ X2 + Z1 + Z2 + G1 + G2+offset(e*X1),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=1)
  l.0 = lik(mlee)
  u = 2* (A[1,1] %*% solve(B[1,1]))*(l1-l.0)
  list("-2LLR"=u)
}

LR.rbci<-findUL(step=0.01, fun=LR.rb, MLE = mle.1[2])
ci.Lrb1 =c(LR.rbci$Low,LR.rbci$Up,LR.rbci$Up-LR.rbci$Low)#c('Lrb.LB','Lrb.UB','Lrb.AL')

#=====================================
#score test
#=====================================
# mle.0[i,] = c(tao.0,0,0,gam1.0,gam2.0,del1.0,del2.0)
# = c(mle.0[1],0,0,mle.0[4],mle.0[5],mle.0[6],mle.0[7])
s0.1 = 6 * ( mean(yy$y12) - exp(mle.0[1]+mle.0[4])+ mean(yy$y21) - exp(mle.0[1]+mle.0[6]) + mean(yy$y33)- exp(mle.0[1]+mle.0[5]+mle.0[7]) )
Sna1<-s0.1 %*% solve(A0[1,1]) %*% t(s0.1) / (3*6);Srb1<- s0.1 %*% solve(B0[1,1]) %*% t(s0.1) / (3*6)
pchisq(Sna1,1,lower.tail = F);pchisq(Srb1,1,lower.tail = F)
#=====================================
#score test C.I. lower and upper
#=====================================
#Naive
#-------------------------------------
S.na <- function(e){
  
  Mod.0 <- glm(formula = Y~ X2 + Z1 + Z2 + G1 + G2+offset(e*X1),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=1)
  mat = MatAB.Po.ABCBCACAB(DT=Type,NN=6,YY=yy,MLE=mlee,Xs = xmat,mat.type = 'null')
  aa<-mat$Mat.A;    bb<-mat$Mat.B
  s01 = 6 * ( mean(yy$y12) - exp(mlee[1]+e+mlee[4])+ mean(yy$y21) - exp(mlee[1]+e+mlee[6]) + mean(yy$y33)- exp(mlee[1]+e+mlee[5]+mlee[7]) )
  s.na = s01^2 * solve(aa)[1,1]  / (3*6)
  list("-2LLR"=s.na)
}

S.naci<-findUL(step=0.01, fun=S.na, MLE = mle.1[2])
ci.Sna1 = c(S.naci$Low,S.naci$Up,S.naci$Up-S.naci$Low)#c('Sna.LB','Sna.UB','Sna.AL')


# #-------------------------------------
# #Robust
# #-------------------------------------
S.rb <- function(e){
  Mod.0 <- glm(formula = Y~ X2 + Z1 + Z2 + G1 + G2+offset(e*X1),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=1)
  mat = MatAB.Po.ABCBCACAB(DT=Type,NN=6,YY=yy,MLE=mlee,Xs = xmat,mat.type = 'null')
  aa<-mat$Mat.A;    bb<-mat$Mat.B
  s01 = 6 * ( mean(yy$y12) - exp(mlee[1]+e+mlee[4])+ mean(yy$y21) - exp(mlee[1]+e+mlee[6]) + mean(yy$y33)- exp(mlee[1]+mlee[5]+e+mlee[7]) )
  s.rb = s01^2 * solve(bb)[1,1]  / (3*6)
  list("-2LLR"=s.rb)
}

S.rbci<-findUL(step=0.01, fun=S.rb, MLE = mle.1[2])
ci.Srb1 = c(S.rbci$Low,S.rbci$Up,S.rbci$Up-S.rbci$Low)#c('Srb.LB','Srb.UB','Srb.AL')

#=====================================
#wald test
#=====================================
Wna1<-mle.1[2]^2 / ( solve(A0) / (3*6) )[1,1]
#Wrb1<- mle.1[2]^2 / (solve(A0) %*% B0 %*% solve(A0)/(3*6))[1,1]
Wrb1<-mle.1[2]^2 / (solve(A0[1,1]) %*% B0[1,1] %*% solve(A0[1,1])/(3*6))
pchisq(Wna1,1,lower.tail = F);pchisq(Wrb1,1,lower.tail = F)
#=====================================
#Wald test C.I. lower and upper
#=====================================
#Naive
#-------------------------------------
W.na <- function(e){
  
  Mod.0 <- glm(formula = Y~ X2 + Z1 + Z2 + G1 + G2+offset(e*X1),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=1)
  mat = MatAB.Po.ABCBCACAB(DT=Type,NN=6,YY=yy,MLE=mlee,Xs = xmat,mat.type = 'null')
  aa<-mat$Mat.A;    bb<-mat$Mat.B
  w.na = (mle.1[2]-e)^2 / ( solve(aa) / (3*6) )[1,1]
  list("-2LLR"=w.na)
}
W.naci<-findUL(step=0.01, fun=W.na, MLE = mle.1[2])
ci.Wna1 = c(W.naci$Low,W.naci$Up,W.naci$Up-W.naci$Low) #c('Wna.LB','Wna.UB','Wna.AL')


#-------------------------------------
#Robust
#-------------------------------------
W.rb <- function(e){
  Mod.0 <- glm(formula = Y~ X2 + Z1 + Z2 + G1 + G2+offset(e*X1),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=1)
  mat = MatAB.Po.ABCBCACAB(DT=Type,NN=6,YY=yy,MLE=mlee,Xs = xmat,mat.type = 'null')
  aa<-mat$Mat.A;    bb<-mat$Mat.B
  w.rb = (mle.1[2]-e)^2 / (solve(aa[1,1]) %*% bb[1,1] %*% solve(aa[1,1])/(3*6))
  list("-2LLR"=w.rb)
}

W.rbci<-findUL(step=0.01, fun=W.rb, MLE = mle.1[2])
ci.Wrb1 = c(W.rbci$Low,W.rbci$Up,W.rbci$Up-W.rbci$Low) #c('Wrb.LB','Wrb.UB','Wrb.AL')

# Test eta1=0
var.na1;var.rb1
Wna1;Wrb1;Lna1;Lrb1;Sna1;Srb1
pchisq(Wna1,1,lower.tail = F);pchisq(Wrb1,1,lower.tail = F)
pchisq(Lna1,1,lower.tail = F);pchisq(Lrb1,1,lower.tail = F)
pchisq(Sna1,1,lower.tail = F);pchisq(Srb1,1,lower.tail = F)
ci.Wna1;ci.Wrb1;ci.Lna1;ci.Lrb1;ci.Sna1;ci.Srb1


#=========================
# ABC2
#=========================
#---------------------------------
#null MLE eta1=0
#---------------------------------
mod.0 <- glm(Y ~ X1 + Z1 + Z2 + G1 + G2, family = poisson(link = "log"), df.cor)
mle.0<- append(mod.0$coefficients, 0, after=2) # eta1 =0 after 1 ,eta2 = 0 after 2

#---------------------------------
#Matrix I & Matrix V
#---------------------------------
Mat = MatAB.Po.ABCBCACAB(DT=Type,NN=6,YY=yy,MLE=mle.1,Xs = xmat,mat.type = 'true')
A<-Mat$Mat.A;    B<-Mat$Mat.B

var.na2<-solve(A)[2,2]/(3*6)
var.rb2<-(solve(A) %*% B %*% solve(A)/(3*6))[2,2]

#---------------------------------
#log LR test 
#---------------------------------
lik<-function(par){ 
  ll=sum(par[1]*yy$y11-exp(par[1])+(par[1]+par[2]+par[4])*yy$y12-exp(par[1]+par[2]+par[4])+(par[1]+par[3]+par[5])*yy$y13-exp(par[1]+par[3]+par[5]))+
    sum((par[1]+par[2]+par[6])*yy$y21-exp(par[1]+par[2]+par[6])+ (par[1]+par[3]+par[4]+par[6])*yy$y22-exp(par[1]+par[3]+par[4]+par[6]) + (par[1]+par[5]+par[6])*yy$y23-exp(par[1]+par[5]+par[6]))+
    sum((par[1]+par[3]+par[7])*yy$y31-exp(par[1]+par[3]+par[7])+ (par[1]+par[4]+par[7])*yy$y32-exp(par[1]+par[4]+par[7]) + (par[1]+par[2]+par[5]+par[7])*yy$y33-exp(par[1]+par[2]+par[5]+par[7]))
  return(ll)
}
#=====================================
#LR Test
#=====================================
l1 = lik(mle.1); l0 = lik(mle.0)
Lna2<-2*(l1-l0);Lrb2<-2 * (A[2,2] %*% solve(B[2,2])) * (l1-l0)
pchisq(Lna2,1,lower.tail = F);pchisq(Lrb2,1,lower.tail = F)
#=====================================
#LR test C.I. lower and upper
#=====================================
#Naive
#-------------------------------------

LR.na  <- function(e){
  Mod.0 <- glm(formula = Y~ X1 + Z1 + Z2 + G1 + G2+offset(e*X2),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=2) # eta1 =0 after 1 ,eta2 = 0 after 2
  l.0 = lik(mlee);  u = 2*(l1-l.0)
  list("-2LLR"=u)
}

LR.naci<-findUL(step=0.01, fun=LR.na, MLE = mle.1[3])
ci.Lna2 = c(LR.naci$Low,LR.naci$Up,LR.naci$Up-LR.naci$Low)#c('Lna.LB','Lna.UB','Lna.AL')

#-------------------------------------
#Robust
#-------------------------------------
LR.rb <- function(e){
  Mod.0 <- glm(formula = Y~ X1 + Z1 + Z2 + G1 + G2+offset(e*X2),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=2) # eta1 =0 after 1 ,eta2 = 0 after 2
  l.0 = lik(mlee)
  u = 2* (A[2,2] %*% solve(B[2,2]))*(l1-l.0)
  list("-2LLR"=u)
}

LR.rbci<-findUL(step=0.01, fun=LR.rb, MLE = mle.1[3])
ci.Lrb2 =c(LR.rbci$Low,LR.rbci$Up,LR.rbci$Up-LR.rbci$Low)#c('Lrb.LB','Lrb.UB','Lrb.AL')

#---------------------------------
# null Matrix I & Matrix V
#---------------------------------
Mat0 = MatAB.Po.ABCBCACAB(DT=Type,NN=6,YY=yy,MLE=mle.0,Xs = xmat,mat.type = 'null')
A0<-Mat$Mat.A;    B0<-Mat$Mat.B

#=====================================
#score test
#=====================================
# mle.0[i,] = c(tao.0,0,0,gam1.0,gam2.0,del1.0,del2.0)
# = c(mle.0[1],0,0,mle.0[4],mle.0[5],mle.0[6],mle.0[7])
s0.2 = 6 * ( mean(yy$y13) - exp(mle.0[1]+mle.0[5])+ mean(yy$y22) - exp(mle.0[1]+mle.0[4]+mle.0[6]) + mean(yy$y31)- exp(mle.0[1]+mle.0[7]) )
Sna2<-s0.2 %*% solve(A0)[2,2] %*% t(s0.2) / (3*6);Srb2<- s0.2 %*% solve(B0)[2,2] %*% t(s0.2) / (3*6)
pchisq(Sna2,1,lower.tail = F);pchisq(Srb2,1,lower.tail = F)
#=====================================
#score test C.I. lower and upper
#=====================================
#Naive
#-------------------------------------
S.na <- function(e){
  
  Mod.0 <- glm(formula = Y~ X1 + Z1 + Z2 + G1 + G2+offset(e*X2),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=2) # eta1 =0 after 1 ,eta2 = 0 after 2
  mat = MatAB.Po.ABCBCACAB(DT=Type,NN=6,YY=yy,MLE=mlee,Xs = xmat,mat.type = 'null')
  aa<-mat$Mat.A;    bb<-mat$Mat.B
  s02 = 6 * ( mean(yy$y13) - exp(mlee[1]+e+mlee[5])+ mean(yy$y22) - exp(mlee[1]+e+mlee[4]+mlee[6]) + mean(yy$y31)- exp(mlee[1]+e+mlee[7]) )
  s.na = s02^2 * solve(aa)[2,2]  / (3*6) # eta1 = [1,1] ,eta2 = [2,2]
  list("-2LLR"=s.na)
}

S.naci<-findUL(step=0.01, fun=S.na, MLE = mle.1[3])
ci.Sna2 = c(S.naci$Low,S.naci$Up,S.naci$Up-S.naci$Low)#c('Sna.LB','Sna.UB','Sna.AL')


# #-------------------------------------
# #Robust
# #-------------------------------------
S.rb <- function(e){
  Mod.0 <- glm(formula = Y~ X1 + Z1 + Z2 + G1 + G2+offset(e*X2),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=2) # eta1 =0 after 1 ,eta2 = 0 after 2
  mat = MatAB.Po.ABCBCACAB(DT=Type,NN=6,YY=yy,MLE=mlee,Xs = xmat,mat.type = 'null')
  aa<-mat$Mat.A;    bb<-mat$Mat.B
  s02 = 6 * ( mean(yy$y13) - exp(mlee[1]+e+mlee[5])+ mean(yy$y22) - exp(mlee[1]+e+mlee[4]+mlee[6]) + mean(yy$y31)- exp(mlee[1]+e+mlee[7]) )
  s.rb = s02^2 * solve(bb)[2,2]  / (3*6) # eta1 = [1,1] ,eta2 = [2,2]
  list("-2LLR"=s.rb)
}

S.rbci<-findUL(step=0.01, fun=S.rb, MLE = mle.1[3])
ci.Srb2 = c(S.rbci$Low,S.rbci$Up,S.rbci$Up-S.rbci$Low)#c('Srb.LB','Srb.UB','Srb.AL')

#=====================================
#wald test
#=====================================
Wna2<-mle.1[3]^2 / ( solve(A0) / (3*6) )[2,2]
# Wrb2<- mle.1[3]^2 / (solve(A0) %*% B0 %*% solve(A0)/(3*6))[2,2]
Wrb2<- mle.1[3]^2 / (solve(A0[2,2]) %*% B0[2,2] %*% solve(A0[2,2])/(3*6))
pchisq(Wna2,1,lower.tail = F);pchisq(Wrb2,1,lower.tail = F)
#=====================================
#Wald test C.I. lower and upper
#=====================================
#Naive
#-------------------------------------
W.na <- function(e){
  Mod.0 <- glm(formula = Y~ X1 + Z1 + Z2 + G1 + G2+offset(e*X2),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=2) # eta1 =0 after 1 ,eta2 = 0 after 2
  mat = MatAB.Po.ABCBCACAB(DT=Type,NN=6,YY=yy,MLE=mlee,Xs = xmat,mat.type = 'null')
  aa<-mat$Mat.A;    bb<-mat$Mat.B
  w.na = (mle.1[3]-e)^2 / ( solve(aa) / (3*6) )[2,2] # eta1 = [1,1] ,eta2 = [2,2]
  list("-2LLR"=w.na)
}
W.naci<-findUL(step=0.01, fun=W.na, MLE = mle.1[3])
ci.Wna2 = c(W.naci$Low,W.naci$Up,W.naci$Up-W.naci$Low) #c('Wna.LB','Wna.UB','Wna.AL')


#-------------------------------------
#Robust
#-------------------------------------
W.rb <- function(e){
  Mod.0 <- glm(formula = Y~ X2 + Z1 + Z2 + G1 + G2+offset(e*X1),family = poisson(link = "log"), data = df.cor)
  mlee<- append(Mod.0$coefficients, e, after=1)
  mat = MatAB.Po.ABCBCACAB(DT=Type,NN=6,YY=yy,MLE=mlee,Xs = xmat,mat.type = 'null')
  aa<-mat$Mat.A;    bb<-mat$Mat.B
  w.rb = (mle.1[3]-e)^2 / (solve(aa[2,2] ) %*% bb[2,2]  %*% solve(aa[2,2] )/(3*6))
  list("-2LLR"=w.rb)
}

W.rbci<-findUL(step=0.01, fun=W.rb, MLE = mle.1[3])
ci.Wrb2 = c(W.rbci$Low,W.rbci$Up,W.rbci$Up-W.rbci$Low) #c('Wrb.LB','Wrb.UB','Wrb.AL')

# Test eta2=0
var.na2;var.rb2
Wna2;Wrb2;Lna2;Lrb2;Sna2;Srb2
pchisq(Wna2,1,lower.tail = F);pchisq(Wrb2,1,lower.tail = F)
pchisq(Lna2,1,lower.tail = F);pchisq(Lrb2,1,lower.tail = F)
pchisq(Sna2,1,lower.tail = F);pchisq(Srb2,1,lower.tail = F)
ci.Wna2;ci.Wrb2;ci.Lna2;ci.Lrb2;ci.Sna2;ci.Srb2;

s0.2 %*% solve(B)[2,2] %*% t(s0.2) / (3*6)
A1 <- aov(Y ~ X1 + X2 + Z1 + Z2 + G1 + G2,data=df.cor)
summary(A1)
