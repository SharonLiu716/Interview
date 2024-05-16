# -*- coding: utf-8 -*-
"""
Created on Tue Aug 10 10:12:22 2021

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
from sklearn.decomposition import PCA
from statsmodels.tsa.arima_model import ARIMA
from sklearn import datasets
from sklearn import preprocessing
import numpy as np, pandas as pd
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
import matplotlib.pyplot as plt
import matplotlib as mpl
from pandas.plotting import lag_plot
#'C:\\Users\\yihsuan.liu\\Documents\\IAI\\raw data\\'
path='C:\\Users\\cherl\\Desktop\\'

'''匯入預測資料'''
Wear_A=pd.read_csv(path+'Train_A_wear.csv').drop(columns='cut')
Wear_B=pd.read_csv(path+'Train_B_wear.csv').drop(columns='cut')

'''標準化samples'''
def Standardization(PATH,SET):
    DF=pd.read_excel(PATH,sheet_name=SET)
    columns=DF.columns
    DF = pd.DataFrame(preprocessing.scale(DF))
    DF.columns=columns
    return DF
TrainSets=['Train_A','Train_B','Test']
Train_A=Standardization(path+'特徵擷取.xlsx',TrainSets[0])
Train_B=Standardization(path+'特徵擷取.xlsx',TrainSets[1])
Test=Standardization(path+'特徵擷取.xlsx',TrainSets[2])

'''主成分個數為7可解釋90%變異
add=0
for idx in range(len(var)):
    add+=var[idx]
    if add>=0.9:
        print(add,'index：',idx)
        break
pca.n_components_
var=pca.explained_variance_ratio_'''
#用scree plot 看解釋變異
pca= PCA(n_components=7)
x_labels=['PC1', 'PC2','PC3','PC4','PC5','PC6','PC7']
var=pca.explained_variance_ratio_.tolist()
cum_var=np.cumsum(var).tolist()
plt.plot(x_labels, list(var), marker='o', markersize=6, color='skyblue', linewidth=2, label='Proportion of variance')
plt.plot(x_labels, list(cum_var), marker='o', color='orange', linewidth=2, label="Cumulative variance")
plt.legend()
plt.title('Scree plot')
plt.xlabel('Principal components')
plt.ylabel('Proportion of variance')
plt.show()
plt.close()


TrainA = pd.DataFrame(data = pca.fit_transform(Train_A), columns = ['PC1', 'PC2','PC3','PC4','PC5','PC6','PC7'])
TrainB = pd.DataFrame(data = pca.fit_transform(Train_B), columns = ['PC1', 'PC2','PC3','PC4','PC5','PC6','PC7'])
Test = pd.DataFrame(data = pca.fit_transform(Test), columns = ['PC1', 'PC2','PC3','PC4','PC5','PC6','PC7'])
y_trainA=Wear_A.min(axis = 1).to_frame().rename(columns={0 :'y'}, inplace = False)
y_trainB=Wear_B.min(axis = 1).to_frame().rename(columns={0 :'y'}, inplace = False)
TrainA = pd.concat([TrainA, y_trainA], axis = 1)
TrainB = pd.concat([TrainB, y_trainB], axis = 1)
#TrainA['Set']='A'
#TrainB['Set']='B'
#train = TrainA.append(TrainB,ignore_index=True)

mycolors = np.random.choice(list(mpl.colors.XKCD_COLORS.keys()), len(x_labels), replace=False)
lines=[]
plt.figure()
for i,x in enumerate(x_labels):        
        #lines+=plt.plot(Test[x])
        plot_pacf(TrainA["PC1"])

plt.legend(lines, x_labels,bbox_to_anchor=(1.0, 1.0))
plt.title('Test -PCA')
plt.show()


#用回歸找出特徵的y再做arima
import statsmodels.formula.api as smf
model = smf.ols('y~PC1+PC2+PC3+PC4+PC5+PC6+PC7',data=TrainA).fit()
predictions = model.predict(Test) 
print_model = model.summary()
print(print_model)

from statsmodels.graphics.tsaplots import plot_pacf, plot_acf
from pmdarima.arima import ndiffs
plot_pacf(TrainA["PC1"])
plt.show()

# 算出推薦的差分次數
d =  ndiffs(TrainA["PC2"],  test="adf")
print(d) # 1

#  觀察PACF圖，參數是差分之後的資料
plot_pacf(d =  ndiffs(TrainA["PC1"],  test="adf").diff(1))
plt.show()

#  觀察ACF圖，參數是差分之後的資料
plot_acf(data["time_imputed"].diff(1))
plt.show()
