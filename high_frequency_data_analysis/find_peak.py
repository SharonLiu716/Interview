# -*- coding: utf-8 -*-
"""
Created on Tue Aug  3 15:55:14 2021

@author: yihsuan.liu
"""

import os
os.environ["KMP_DUPLICATE_LIB_OK"]  =  "TRUE"
import glob2
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import torch 
from torch.utils.data import Dataset,DataLoader
from scipy.misc import electrocardiogram
from scipy.signal import find_peaks

#所有路徑存取
fileSet = ['Train_A', 'Train_B','Test']
tmp_path,tmp_name,csv_file=[],[],[]
for Set in fileSet:
    path = 'C:/Users/yihsuan.liu/Documents/IAI/raw data/'+Set+'/'+Set
    csv_file.append(path+'_wear.csv')
    #trainY = pd.read_csv(path+'_wear.csv')
    file_path = glob2.glob(path+'/*.csv') 
    tmp_path.append(file_path)    
    file_name = list(map(os.path.basename,file_path))
    tmp_name.append(file_name)
    del path,file_name,file_path

allpath=pd.DataFrame(tmp_path).T
allfilename=pd.DataFrame(tmp_name).T
allpath.columns,allfilename.columns = fileSet,fileSet
del tmp_name,tmp_path,Set

#資料存取
class MyDataset(Dataset):
    def __init__(self,model,path_list,name_list,csv_file=None):
        self.model=model  #(train/test)        
        self.path_list = path_list   #train data file path(csv*315):allpath
        self.name_list=name_list     #train data file name(csv*315):allfilename
        if model=='train':
            self.wear =pd.read_csv(csv_file)
                
    def __len__(self):
        return self.path_list.shape[0]  #315 row    
    def __getitem__(self,idx):
        variable_name=['X方向的力量','Y方向的力量','Z方向的力量','X方向的震動','Y方向的震動','Z方向的震動','音洩訊號']
        data=pd.read_csv(self.path_list[idx],header=None)
        data.columns=variable_name
        if self.model=='train':
            wear = self.wear.iloc[idx, 1:4]
            dataSet={'data':data,'flute':wear,'file': self.name_list[idx],'signal':data['音洩訊號']}
        else:
            dataSet={'data':data,'file':self.name_list[idx],'signal':data['音洩訊號']}
        return dataSet
#需要用到的所有資料
Data_A = MyDataset('train',allpath['Train_A'], allfilename['Train_A'], csv_file = csv_file[0])
Data_B = MyDataset('train',allpath['Train_B'], allfilename['Train_B'], csv_file = csv_file[1])
Data_T = MyDataset('test',allpath['Test'], allfilename['Test'],csv_file=None)
del allfilename,allpath,csv_file,fileSet

#=============================================================================
#把音洩訊號取出來另外做特徵擷取


## function for step 1
def findSignal(DF, prop=0.2, Condi=0):
    DF=Data_A[7]['signal']
    ni = DF.shape[0]
    x = DF.iloc[0:np.int(ni*0.2)]
    noS,x_properties = find_peaks(x,prominence=.03,width=20)
    
    plt.plot(x)
    plt.plot(noS, x[noS], "x")
    plt.vlines(x=noS, ymin=x[noS] - x_properties["prominences"],ymax = x[noS], color = "C1")
    plt.hlines(y=x_properties["width_heights"], xmin=x_properties["left_ips"],xmax=x_properties["right_ips"], color = "C1")
    plt.show()

    y = DF.iloc[np.int(ni*(1-0.2)):]
    y=y.reset_index(drop=1)
    noE,y_properties = find_peaks(y,prominence=.03,width=20)
    plt.plot(y)
    plt.plot(noE, y[noE], "x")
    plt.vlines(x=noE, ymin=y[noE] - y_properties["prominences"],ymax = y[noE], color = "C1")
    plt.hlines(y=y_properties["width_heights"], xmin=y_properties["left_ips"],xmax=y_properties["right_ips"], color = "C1")
    plt.show()
    if Condi == 0: #切最小
        if noE.size == 0:
            ansE=np.int(ni*prop)
            noS=[0]
        else:
            ansE = noE[-1]  
    elif Condi == 1:   #切區域中相對極大
        ansE = noE[np.argmax(np.diff(noE))]

    return noS[0], np.int(ni*(1-prop))+ansE

## function for step 2
def cutPts(DF,idx,trainSet): #DF=Data_A[i]['signal']
    if(trainSet=='Train_A')|(trainSet=='Test'):
        initial,end=findSignal(DF)
    else:
        if(idx in np.array([304,308,314])):
            initial,end=findSignal(DF,Condi=1)
        else:
            initial,end=findSignal(DF)        
    cut_signal=DF[initial:end]    
    return cut_signal

plt.ion()
plt.figure(figsize=(6,2),linewidth=1)
plt.plot(df[columns],lw=2,alpha=0.8)#color=color_name[columns]
plt.title(SetName+' '+str(cutNo)+' '+variable_name[columns], fontproperties=font)          
         
         
plt.savefig(SetName+' '+str(cutNo)+' '+variable_name[columns]+'.png')              
#ax.set_title(SetName+' '+str(cutNo)+' '+variable_name[columns], fontproperties=font)          
#fig.savefig(SetName+' '+str(cutNo)+' '+variable_name[columns]+'.png')
plt.clf()
plt.close('all')
