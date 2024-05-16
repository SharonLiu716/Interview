# -*- coding: utf-8 -*-
"""
Created on Tue Jul 20 09:02:12 2021

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
#待修：取音洩訊號

## function for step 1
def findSignal(DF, prop=0.2, Condi=0):

    ni = DF.shape[0]
    x = DF.iloc[0:np.int(ni*prop)]
    noS,_ = find_peaks(x,prominence=.03,width=20)
    
    x = DF.iloc[np.int(ni*(1-prop)):]
    noE,_ = find_peaks(x,prominence=.03,width=20)
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


#統計特徵擷取
def featureTD(DF):
    
    colnames = DF.columns
    factorName = []
    rslt = []
    for (colIDX,cols) in enumerate(colnames):
        
        df = DF.iloc[:,colIDX]
        
        # maximum
        factorName.append(cols+'_Max')
        rslt.append(df.max())
        
        # mean
        factorName.append(cols+'_Mean')
        rslt.append(df.mean())
        
        # Root Mean Square
        factorName.append(cols+'_RMS')
        rslt.append(np.sqrt((df**2).mean()))
        
        # Standard Deviation
        factorName.append(cols+'_Std')
        rslt.append(df.std()) 
        
        # Skewness
        factorName.append(cols+'_Skewness')
        rslt.append(df.skew())
        
        # Kurtosis
        factorName.append(cols+'_Kurtosis')
        rslt.append(df.kurtosis())
        
        # Peak to Peak
        factorName.append(cols+'_P2P')
        rslt.append(df.max()-df.min())
        
        # Crest Factor
        factorName.append(cols+'_CF')
        rslt.append(df.max()/np.sqrt((df**2).mean()))
    
    return factorName, rslt

#Fourier Transformation後特徵擷取
def featureFD(DF):

    n = DF.shape[0]
    colnames = DF.columns
    
    factorName = []
    rslt = []
    for (colIDX,cols) in enumerate(colnames):
        # FFT
        df = DF.iloc[:,colIDX]
        yf = np.abs(np.fft.fft(df))/n
        yf2 = yf[range(int(n/2))]
        
        # Maximum below 5000 Hz
        factorName.append(cols+'_Value1')
        rslt.append(np.max(yf2[:5000]))
        
        # Relative Spectral Peak Per Band below 5000 Hz
        factorName.append(cols+'_RSPPB1')
        rslt.append(np.max(yf2[:5000])/np.mean(yf2[:5000]))
        
        if 2 < colIDX < 6:
            # Maximum between 60000 and 80000 Hz
            factorName.append(cols+'_Value2')
            rslt.append(np.max(yf2[60000:80000]))
	        # Relative Spectral Peak Per Band between 60000  
            # and 80000 Hz
            factorName.append(cols+'_RSPPB2')
            rslt.append(np.max(yf2[60000:80000])/np.mean(yf2[60000:80000]))
        
    return factorName, rslt

##把特徵整理成excel output
def FeatureSelect(DF,trainSet):
    TD_allvalue=[]
    FD_allvalue=[]
    i=0
    for i in range(315):
        tmp_data=DF[i]['data']        
        tmp_data=tmp_data.drop(labels='音洩訊號',axis=1) #除去音洩訊號這個欄位後的df計算特徵
        tmp_signal=DF[i]['signal']      #音洩訊號剪枝
        cutSing=cutPts(tmp_signal,i,trainSet).to_frame() #出來是series，需轉成df才能做特徵擷取
        #data的特徵擷取，需補上signal的特徵擷取後合併成一個dataframe
        TD_name,TD_value=featureTD(tmp_data)
        TD_signal_name,TD_signal_value=featureTD(cutSing)
        TD_allvalue.append((TD_value+TD_signal_value))
        FD_name,FD_value=featureFD(tmp_data)
        FD_signal_name,FD_signal_value=featureFD(cutSing)
        FD_allvalue.append((FD_value+FD_signal_value))

    DF_TD=pd.DataFrame(TD_allvalue,columns=(TD_name+TD_signal_name))
    DF_FD=pd.DataFrame(FD_allvalue,columns=(FD_name+FD_signal_name))
    df = pd.concat( [DF_TD,DF_FD], axis=1 )    
    del TD_allvalue,FD_allvalue,TD_name,TD_value,FD_name,FD_value,i
    return df

featureEtra_A=FeatureSelect(Data_A,'Train_A')
featureEtra_B=FeatureSelect(Data_B,'Train_B')
featureEtra_T=FeatureSelect(Data_T,'Test')
with pd.ExcelWriter('特徵擷取.xlsx') as writer:
    featureEtra_A.to_excel(writer,sheet_name='Train_A')
    featureEtra_B.to_excel(writer,sheet_name='Train_B')
    featureEtra_T.to_excel(writer,sheet_name='Test')









