# -*- coding: utf-8 -*-
"""
Created on Tue Dec 28 16:13:29 2021

@author: 懿萱
"""


import os
os.environ["KMP_DUPLICATE_LIB_OK"]  =  "TRUE"
import glob2
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import sys
import math
from torch.utils.data import Dataset
from matplotlib.font_manager import FontProperties
font=FontProperties(fname=r'c:\windows\fonts\simsun.ttc',size=20)
from statsmodels.tsa.arima_model import ARIMA
from statsmodels.tsa.stattools import adfuller
from pmdarima.arima.utils import ndiffs
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
from statsmodels.tsa.stattools import acf, pacf
from numpy import log
import pmdarima as pm
from pmdarima.model_selection import train_test_split
from pmdarima.arima import auto_arima
from math import sqrt
from sklearn.metrics import mean_squared_error,mean_absolute_error
from statsmodels.stats.diagnostic import het_arch


def get_path(PATH,FOLDER):
    DATA_PATH=PATH+'\\'+FOLDER
    dataset_list=glob2.glob(DATA_PATH+'\\*')    
    #dataset_namelist=['BeijingPM2013_20151231.csv','ChengduPM2012_20151231.csv','GuangzhouPM2011_20151231.csv']
    dataset_namelist=list(map(os.path.basename,dataset_list))
    dataset_namelist=[x.strip('.xlsx') for x in dataset_namelist]
   # dataset_namelist=[x.strip('new') for x in dataset_namelist]
    dataset_info = pd.DataFrame({'FilePath':dataset_list,'City':dataset_namelist})
    return dataset_info
    

class DataSet(Dataset):
    def __init__(self, file_list,option):

        self.file_list = file_list
        self.option=option
    
    def __len__(self):
        return len(self.file_list)

    def __getitem__(self, index):
        
        if self.option == 'origin':
            data = pd.read_excel(self.file_list[index])            
        else:
            data = self.PM_mean(self.file_list[index],self.option)
        return data
    def PM_mean(self,path,opt):
        df=pd.read_excel(path)
        df.rename({'pmAvg': 'PM_mean'}, axis=1, inplace=True)
        '''
        col=[]
        for (idx,name) in enumerate(df.columns):
            if 'PM' in name:
                col.append(name)                
        PM_mean=pd.DataFrame(np.nanmean(df[col], axis=1),columns=['PM_mean'])        
        PM_mean=pd.concat([df[['year','month','day']],PM_mean],axis=1)
        '''
        if opt =='day':
            PM_mean=df[['year','month','day','PM_mean']]
            PM_mean=PM_mean.groupby(['year','month','day']).mean().reset_index()
        return PM_mean

    
path='C:\\Users\\cherl\\GitHub\\Statistcal Practice 110\\ARIMA'
folder='data'
Files=get_path(path, folder)
dataset=DataSet(Files['FilePath'],option='day') #0:北京BJ、1:成都CD、2:廣州GZ、3:上海SH、4:瀋陽SY



for (idx,City) in enumerate(Files['City']):
    df=dataset[2]
    city = 'Beijing'#City.partition('PM')[0]
    train = df[:int(2/3*(len(df)))]
    valid = df[int(2/3*(len(df))):]
    plot_acf(train['PM_mean'].dropna(),title=city+' '+'Autocorrelation')
    plt.savefig(city+ ' train ACF(origin).png')
    plt.close()
    plot_pacf(train['PM_mean'].dropna(),title=city+' '+'Partial Autocorrelation')
    plt.savefig(city+ ' train PACF(origin).png')
    plt.close()
    #變異數不平穩的問題：對y做不同轉換
    #original data plot
    plt.figure(figsize=(12,8))
    train['PM_mean'].plot()
    plt.title(city+ ' 2013-2014 PM2.5')
    plt.savefig(city+ ' 2013-2014 PM2.5.png')
    plt.close()
    #y=log(y)
    plt.figure(figsize=(12,8))
    np.log(train['PM_mean']).plot()
    plt.title(city+ ' 2013-2014 PM2.5(log)')
    plt.savefig(city+ ' 2013-2014 PM2.5(log).png')
    plt.close()
    #y=y^0.5
    plt.figure(figsize=(12,8))
    (train['PM_mean']**0.5).plot()
    plt.title(city+ ' 2013-2014 PM2.5(sqrt)')
    plt.savefig(city+ ' 2013-2014 PM2.5(sqrt).png')
    plt.close()
    #y=y^0.25
    plt.figure(figsize=(12,8))
    (train['PM_mean']**0.25).plot()
    plt.title(city+ ' 2013-2014 PM2.5(0.25)')
    plt.savefig(city+ ' 2013-2014 PM2.5(0.25).png')
    plt.close()

#平均數不平穩的問題：差分


df=dataset[3]
train = df[:int(2/3*(len(df)))]
valid = df[int(2/3*(len(df))):]

plt.figure(figsize=(12,8))
(np.log(train['PM_mean'])).dropna().plot()
plt.title(city+' 2013-2014 PM2.5(log)')
plt.savefig(city+' 2013-2014 PM2.5(log).png')
plt.close()
data=(train['PM_mean']**0.25).dropna()#轉換完的response
plot_acf(data,title=city+' Autocorrelation(log)')
plt.savefig(city+' train ACF(log) .png')
plt.close()
plot_pacf(data,title=city+' Partial Autocorrelation(log) ')
plt.savefig(city+' train PACF(log).png')
plt.close()
#做arch_effect_test轉換後的data，若拒絕H0表示有arch effect要用arch model，若不拒絕H0則用arima
het_arch((data-data.mean())**2)
#fpvalue:0.5863688552994891
import statsmodels.api as sm
from statsmodels.stats.diagnostic import acorr_ljungbox
data=(np.log(train['PM_mean'])).dropna()
    '''ARIMA Model'''
    model = ARIMA(data, order=(0,1,2))
    model_fit = model.fit(disp=0)
    print(model_fit.summary())
    # Actual vs Fitted
    fcast = np.array([])
    # Step through the rest of the samplec
    history = np.array([x for x in (np.log(train['PM_mean']))])
    test=np.array([x for x in (np.log(valid['PM_mean']))])
    for t in range(len(test)):
        model = ARIMA(history, order=(1,0,1))
        model_fit = model.fit(disp=0)
        output = model_fit.forecast()
        yhat = np.exp(output[0])
        fcast = np.append(fcast, yhat)
        obs = test[t]
        history = np.append(history, obs)
        
    
    mse=mean_squared_error(valid['PM_mean'],fcast)
    rms = sqrt(mean_squared_error(valid['PM_mean'],fcast))
    mae = mean_absolute_error(valid['PM_mean'],fcast)
    mape = np.mean(np.abs( fcast- valid['PM_mean'])/np.abs(valid['PM_mean']))*100
    #殘差檢定
    residuals = pd.DataFrame(model_fit.resid)
    het_arch((residuals-residuals.mean())**2) #檢定殘差變異數是否同質
    sm.stats.acorr_ljungbox(residuals, lags=[30],return_df=True)#檢定殘差是否獨立
    plot_acf(residuals,title=city+'residuals ACF')
    ##
    mape = np.mean(np.abs( (predictions**4)- valid)/np.abs(valid))*100    
    # plot predictions and actual values
    pred = pd.DataFrame([x**4 for x in fcast],columns=['pred'])
    pred['pred'].plot(legend = True)
    valid=valid.reset_index()
    valid['PM_mean'].plot(legend = True)

    '''ARCH MODEL'''
    #ref:https://kknews.cc/zh-tw/code/6arq3zp.html
    from arch import arch_model
    am = arch_model(data,vol='ARCH',) 
    fit = am.fit()
    print(fit.summary())
    res = pd.DataFrame(fit.resid)
    het_arch((res-res.mean())**2) #檢定殘差變異數是否同質
    sm.stats.acorr_ljungbox(res,return_df=True)
    fit.resid.plot(figsize=(12,5)) 
    plt.title('residual GARCH(1,1) '+city,size=15) 
    plt.show() 
    fit.conditional_volatility.plot(figsize=(12,5),color='r') 
    plt.title('variance GARCH(1,1) '+city,size=15) 
    plt.show()
    
    '''MAPE'''
    train.drop(['year','month','day'],axis=1,inplace=True)
    valid.drop(['year','month','day'],axis=1,inplace=True)
    model.fit((train['PM_mean']**0.25))
    forecast = model.predict(n_periods=len(valid['PM_mean']))
    forecast = pd.DataFrame(forecast,index = valid.index,columns=['Prediction'])
    mape = np.mean(np.abs(forecast - valid)/np.abs(valid))
    
    plot_acf(data**2,title=' Chengdu'+'2013-2014 Autocorrelation(0.25)')
    plot_pacf(data**2,title='Chengdu '+'2013-2014 Partial Autocorrelation(0.25)')
    plt.figure(figsize=(12,8))
    valid['PM_mean'].plot()
    plt.title(city+ ' 2015 PM2.5')
    
    
    plt.close()


        #fit model
        model = ARIMA((train['PM_mean']**0.25).dropna(), order=(2,1,2))
        model_fit = model.fit(disp=0)
        fit_summary = city+' ARIMA Fit Result.txt'
        with open(fit_summary, 'a') as f:
            f.write(city+'\n')
            f.write('------------------------------------------------------------\n\n')
           # f.write('ADF Statistic: %f \n' % result[0])
           # f.write('p-value: %f \n' % result[1])
           #f.write('parameter d in ARIMA: %d \n' % ndiffs(tmp['PM_mean'].dropna(), test='adf'))
            f.write('------------------------------------------------------------\n\n')
            print(model_fit.summary(),file=f)
            f.write('\n============================================================\n\n')
            
 '''計算box pierce 和 box ljung統計量'''         
from statsmodels.stats.diagnostic import acorr_ljungbox   
def whitenoise_test(ts):
     
    q,p=acorr_ljungbox(ts) 
    with plt.style.context('ggplot'): 
        fig = plt.figure(figsize=(10, 4)) 
        axes = fig.subplots(1,2) 
        axes[0].plot(q, label='Q統計量') 
        axes[0].set_ylabel('Q') 
        axes[1].plot(p, label='p值') 
        axes[1].set_ylabel('P') 
        axes[0].legend() 
        axes[1].legend() 
        plt.tight_layout()
    return 
    
ret=df.ret.dropna() 
whitenoise_test((train['PM_mean']**0.25).dropna().diff().dropna())

原文網址：https://kknews.cc/code/6arq3zp.html