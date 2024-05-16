# -*- coding: utf-8 -*-
"""
Created on Wed Mar 16 16:59:29 2022

@author: 懿萱
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

class RVs():
    def __init__(self,Distri,Size):
        # data_version: original: monthly raw data from RQ, updated: 經過欄位縮短&變換
        self.Size = Size
        self.U=np.random.uniform(low=0,high=1,size=self.Size)
        self.mapping_dict = pd.read_excel(mapping_dict, sheet_name=None) #sheetname=none可以匯入所有sheet，但以dict呈現
        self.merge_columns = merge_columns
        self.action = action
        self.version = version
        self.key1,self.key2 = self.PartKey(self.mapping_dict['Part Type'])        
        self.updated=self.Merge(self.DF,self.mapping_dict['Child'],self.mapping_dict['Part Type'],self.mapping_dict['Defect Type'],self.mapping_dict['Product Type'],self.merge_columns)
    
    def Exponential(self,Lam):
        return (-np.log(self.U))/Lam

    def Cauchy(self,Mu,Tao):
        return Tao*np.tan((self.U-0.5)*np.pi)+Mu
    
    def Weibull(self,Alpha,Beta):
        return (-np.log(self.U)/Alpha)**(1/Beta)

    def DU(self,N):
        return int(self.U*N)+1

    def Geometric(self,P):
        return (int(np.log(self.U)/np.log(1-P))+1)
    
    def NB_fromGeo(self,R,P):        
        NB=[self.Geometric(P) for i in R]
        return sum(NB)
    
    def NBPmf(I,R,P):
        pj=P**R 
        for j in range(R,I):
            pj=j*(1-P)*pj/(j-R+1)
        return pj

    def NB_fromRecursive(self,R,P):
        i,j=int((R-1)/P)+1,0 #initial
        Previous=sum([self.NBPmf(x,R,P) for x in range(R,i)])
        Next=sum([self.NBPmf(x,R,P) for x in range(R,i+1)])
        while not( self.U[j]<=Next and self.U[j]>Previous):
            
            if self.U[j]<=Previous:
                Next=Previous
                Previous=Previous-self.NBPmf(i,R,P)
                i=i-1
            elif self.U[j]>Next:
                Previous=Next#k的設定
                i=i+1
                Next=Next+self.NBPmf(i,R,P)
            j+=1
        return i
    
    def NB_fromDef(self,R,P):
        xi,j=0,0
        while j<R:
            U=np.random.uniform(low=0,high=1)
            if U<P:
                j=j+1
            xi=xi+1
        return xi

    def PoissonPmf(self,nj,lam):
        pj=np.exp(-lam)
        for j in range(0,nj):
            pj=pj*lam/(j+1)
        return pj

    def Poisson(self,U,Lam):
        i,count=Lam,1
        Previous=sum([self.PoissonPmf(n=x, lam=Lam) for x in range(1,i)])
        Next=sum([self.PoissonPmf(n=x, lam=Lam) for x in range(1,i+1)])
        while not( self.U<=Next and self.U>Previous):
            count=count+1
            if U<=Previous:
                Next=Previous
                Previous=Previous-self.PoissonPmf(n=i, lam=Lam)
                i=i-1
            elif  U>Next:
                Previous=Next#k的設定
                i=i+1
                Next=Next+self.PoissonPmf(n=i+1, lam=Lam)
        return i
    
    def Bernoulli(self,P):
        j,G=0,0
        Xj=[0]*self.Size if P<0.5 else [1]*self.Size 
        while j<self.Size :            
            Grvs=self.Geometric(self.U[G], P=1-P) if P>=0.5 else self.Geometric(self.U[G], P=P)
            if Grvs<=self.Size :
                Xj[j-1]=1 if P<0.5 else 0
            j+=Grvs
            G=G+1
        return Xj
    
    def BinomialPmf(N,P):    
        pi=P
        for i in range(0,N):
            pi=(P*(N-i)/(i+1)*(1-P))*pi
        return pi

    #Didn't use recursive BinomialPmf 
    def Binomial(self,N,P):    
        Bin=[self.Bernoulli(self,P) for i in N]
        return sum(Bin)
    
    