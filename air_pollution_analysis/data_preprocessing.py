# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""


import os
os.environ["KMP_DUPLICATE_LIB_OK"]  =  "TRUE"
import glob2
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import sys
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


def get_path(PATH,FOLDER):
    DATA_PATH=PATH+'\\'+FOLDER
    dataset_list=glob2.glob(DATA_PATH+'\\*')    
    #dataset_namelist=['BeijingPM2013_20151231.csv','ChengduPM2012_20151231.csv','GuangzhouPM2011_20151231.csv']
    dataset_namelist=list(map(os.path.basename,dataset_list))
    dataset_namelist=[x.strip('.csv') for x in dataset_namelist]
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
            data = pd.read_csv(self.file_list[index])            
        else:
            data = self.PM_mean(self.file_list[index],self.option)
        return data
    def PM_mean(self,path,opt):
        df=pd.read_csv(path)
        col=[]
        for (idx,name) in enumerate(df.columns):
            if 'PM' in name:
                col.append(name)                
        PM_mean=pd.DataFrame(np.nanmean(df[col], axis=1),columns=['PM_mean'])        
        PM_mean=pd.concat([df[['year','month','day']],PM_mean],axis=1)
        if opt =='day':
            PM_mean=PM_mean.groupby(['year','month','day']).mean().reset_index()
        return PM_mean


def countNA(dataset):
    #計算每個城市的na值 by column
    percentNA = pd.DataFrame()
    for i in range(len(dataset)):    
        df=dataset[i]
        city=Files['City'][i]
        column=list(df.columns)
        countNA=[df[x].isna().sum() for x in column]
        cityNA_ = pd.DataFrame({"Variable of "+city:column,'NA of '+city:countNA})
        #分季節計算NA
        g = df.groupby(['SeasonCode'])
        groupNA=g.count().rsub(g.size(), axis=0)
        savepath = os.path.join(os.getcwd(),city+' NA count.xlsx') # 設定路徑及檔名
        writer = pd.ExcelWriter(savepath, engine='openpyxl') # 指定引擎openpyxl
        groupNA.to_excel(writer, sheet_name='count NA by season') # 存到指定的sheet
        #計算每個城市NA比例
        for (idx,col) in enumerate(column):
            if col in groupNA.columns:
                percentNA[col+' % by newSeason']=100*(groupNA[col]/cityNA_.iloc[idx,1])
        percentNA.to_excel(writer, sheet_name='percentage of count NA by season')
        writer.save()
        writer.close()
        #將所有NA合併為一個表
        #totalNA = pd.concat([totalNA,new ], axis=1)
        
def SortedDate(dataset):
    for i in range(len(dataset)):
        df=dataset[i]    
        df=df[[ 'newSeasonCode','merge.'+Files['City'][i],'DEWP', 'HUMI', 'PRES','TEMP', 'Iws']]
        df=df.rename(columns={'merge.'+Files['City'][i]:'PM2.5(mean)','newSeason':'Season','newSeasonCode':'SeasonCode'})
        df.to_excel('Season'+Files['City'][i]+'.xlsx',index=False)
    
path='C:\\Users\\cherl\\GitHub\\Statistcal Practice 110'
folder='speed1124'
Files=get_path(path, folder)
dataset=DataSet(Files['FilePath'],option='day') #0:北京BJ、1:成都CD、2:廣州GZ、3:上海SH、4:瀋陽SY
countNA(dataset)

for (i,c) in enumerate(Files['City']):
    tmp=dataset[i].to_excel(c+'.xlsx')

'''knn-na values'''
from sklearn.impute import KNNImputer
import seaborn as sns
import matplotlib.pyplot as plt
import missingno as msno

#分層補NA

def KNNResult(_df,N,city):
    result=pd.DataFrame()
    for n in N:
        imputer=KNNImputer(n_neighbors=n)
        groups = _df.groupby(_df['SeasonCode'])
        for i in range(1,14):
            layer=groups.get_group(i)
            layer_filled= pd.DataFrame(imputer.fit_transform(layer[['SeasonCode','PM2.5(mean)', 'DEWP', 'HUMI', 'PRES', 'TEMP', 'Iws']]),columns=layer.columns)
            frames = [result, layer_filled]
            result = pd.concat(frames)
            
        corr_filled = result.corr(method='pearson')
        mask = np.zeros_like(corr_filled)
        mask[np.triu_indices_from(mask)] = True
        with sns.axes_style("white"):
            ax=sns.heatmap(corr_filled, mask=mask,vmax=0.3, annot=True,cmap="RdBu")
            ax.set_title(city+'as k='+str(n), fontsize =18)
            ax.figure.savefig(city+'as k='+str(n)+'.png',dpi=600)
            plt.close('all')

N=[2,8,10,20]

for (idx,c) in enumerate(Files['City']):
    df=dataset[idx]
    origin=msno.heatmap(df, cmap='YlGnBu')
    plt.savefig(c+'with NA.png', dpi=600)
    plt.close('all')
    KNNResult(df,N,c)
    plt.close('all')
    print (df.describe())

year=[2013,2014,2015]
for (idx,City) in enumerate(Files['City']):
    df=dataset[idx]
    city = City.partition('PM')[0]
    for y in year: 
        tmp=df[df['year']==y]        
        fig = plt.figure(figsize=(20, 15))
        plt.title(city+'  '+str(y)+' Original Series', fontsize=25)
        plt.xlabel('index', fontsize=20)
        plt.ylabel('PM 2.5 mean', fontsize=20)
        plt.plot(tmp['PM_mean'].dropna())
        plt.show()
        if ndiffs(tmp['PM_mean'].dropna())==1:
            plot_acf(tmp['PM_mean'].dropna().diff().dropna(),title=city+' '+str(y)+'Autocorrelation(diff=1)')
            plot_pacf(tmp['PM_mean'].dropna(),title=city+' '+str(y)+'Partial Autocorrelation(diff=1)')
            print( city+' '+str(y)+str(ndiffs(tmp['PM_mean'].dropna().diff().dropna())))
            
        else:
            plot_acf(tmp['PM_mean'].dropna(),title=city+' '+str(y)+' Autocorrelation(origin)')
            plot_pacf(tmp['PM_mean'].dropna(),title=city+' '+str(y)+'Partial Autocorrelation')
            print(city+' '+str(y)+str(ndiffs(tmp['PM_mean'].dropna())))
        plt.close()


        #fit model
        model = ARIMA(tmp['PM_mean'].dropna(), order=(2,1,2))
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


#load the data

df=dataset[0].dropna().diff().dropna()
for (idx,City) in enumerate(Files['City']):
    df=dataset[idx].dropna()
    city = City.partition('PM')[0]
    train = df[:int(2/3*(len(df)))]
    train.drop(['year','month','day'],axis=1,inplace=True)
    if city!='Beijing':        
        train=train.diff().dropna()
        model = auto_arima(train, trace=True, error_action='ignore', suppress_warnings=True)
        plot_acf(train,title=city+' '+'Autocorrelation(diff=1)')
        plot_pacf(train,title=city+' '+'Partial Autocorrelation(diff=1)')
        fit_summary = city+' ARIMA Fit Result.txt'
        with open(fit_summary, 'a') as f:
            print(city+'2013 & 2014 ,d = '+str(ndiffs(train)),file=f)
            f.write('------------------------------------------------------------\n\n')
            print(model.summary(),file=f)
            f.write('\n============================================================\n\n')
    else:
        model = auto_arima(train, trace=True, error_action='ignore', suppress_warnings=True)
        plot_acf(train,title=city+' '+'Autocorrelation')
        plot_pacf(train,title=city+' '+'Partial Autocorrelation')
        fit_summary = city+' ARIMA Fit Result.txt'
        with open(fit_summary, 'a') as f:
            print(city+'2013 & 2014 ,d = '+str(ndiffs(train)),file=f)
            f.write('------------------------------------------------------------\n\n')
            print(model.summary(),file=f)
            f.write('\n============================================================\n\n')
    plt.close()

for (idx,City) in enumerate(Files['City']):
    df=dataset[idx].dropna()
    city = City.partition('PM')[0]
    train = df[:int(2/3*(len(df)))]
    valid = df[int(2/3*(len(df))):]
    train.drop(['year','month','day'],axis=1,inplace=True)
    valid.drop(['year','month','day'],axis=1,inplace=True)
    train['PM_mean'].plot()
    valid['PM_mean'].plot()
    model = auto_arima(train, trace=True, error_action='ignore', suppress_warnings=True)
    model.fit(train)

    forecast = model.predict(n_periods=len(valid))
    forecast = pd.DataFrame(forecast,index = valid.index,columns=['Prediction'])

    #plot the predictions for validation set
    plt.plot(train, label='Train')
    plt.plot(valid, label='Valid')
    plt.plot(forecast, label='Prediction')
    plt.show()
    mse=mean_squared_error(valid,forecast)
    rms = sqrt(mean_squared_error(valid,forecast))
    mae = mean_absolute_error(valid,forecast)
    fit_summary = city+' ARIMA Fit Result.txt'
    with open(fit_summary, 'a') as f:
        print(city+' 2013 & 2014 ,d = '+str(ndiffs(train)),file=f)
        f.write('------------------------------------------------------------\n\n')
        print(model.summary(),file=f)
        f.write('\n============================================================\n\n')
        print(city+' MSE = '+str(mse),file=f)
        print(city+' RMSE = '+str(rms),file=f)
        print(city+' MAE = '+str(mae),file=f)
        


'''
#divide into train and validation set
train = df[:int(2/3*(len(df)))]
valid = df[int(2/3*(len(df))):]

#preprocessing (since arima takes univariate series as input)
train.drop(['year','month','day'],axis=1,inplace=True)
valid.drop(['year','month','day'],axis=1,inplace=True)

#plotting the data
train['PM_mean'].plot()
valid['PM_mean'].plot()


def p_q_values_estimator(timeseries):
    p=0
    q=0
    lag_acf = acf(timeseries, nlags=20)
    lag_pacf = pacf(timeseries, nlags=20, method='ols')
    y=1.96/np.sqrt(len(timeseries))

    if lag_acf[0] < y:
        for a in lag_acf:
            if a < y:
                q = q + 1
                break 
    elif lag_acf[0] > y:
        for c in lag_acf:
            if c > y:
                q = q + 1
                break

    if lag_pacf[0] < y:
        for b in lag_pacf:
            if b < y:
                p = p + 1
                break
    elif lag_pacf[0] > y:
        for d in lag_pacf:
            if d > y:
                p = p + 1
                break

    p_q=[p,q]
    return(p_q)

df=df[df['year']==2014]
p_q_values=p_q_values_estimator(df['PM_mean'].dropna())
p_value=p_q_values[0]
q_value=p_q_values[1]

#building the model

model = auto_arima(train, trace=True, error_action='ignore', suppress_warnings=True)
model.fit(train)

forecast = model.predict(n_periods=len(valid))
forecast = pd.DataFrame(forecast,index = valid.index,columns=['Prediction'])

#plot the predictions for validation set
plt.plot(train, label='Train')
plt.plot(valid, label='Valid')
plt.plot(forecast, label='Prediction')
plt.show()

#calculate rmse
from math import sqrt
from sklearn.metrics import mean_squared_error

rms = sqrt(mean_squared_error(valid,forecast))
print(rms)
'''