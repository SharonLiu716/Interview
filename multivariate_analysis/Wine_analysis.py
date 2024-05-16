# -*- coding: utf-8 -*-
"""
Created on Wed Jun  8 11:46:12 2022

@author: 懿萱
"""

import numpy as np 
import pandas as pd 
import seaborn as sns
import scipy
from scipy import stats
import matplotlib.pyplot as plt
from sklearn.preprocessing import LabelEncoder #knn imputation for categorical data
from sklearn.impute import KNNImputer
from sklearn.neighbors import KNeighborsClassifier
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor, RandomForestClassifier
from sklearn.metrics import mean_squared_error
from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import IterativeImputer

'''
Q: predic price or quality of wine
Y = price 
machine learing  logistic regeression/ SVM 
analysis component : PCA


Data Preprocessing
  -Attribute Info
    -Category:['winery', 'wine', 'year', 'region', 'type'] (remove country)
    -Numerical:['rating', 'num_reviews', 'price', 'body', 'acidity']
  -NA Value--KNN
      #categorical variable missing columns: 7(type) trans to numerical then imputed
      #numerrical variable missing columns:2(year)、 8(body)、9(acidity)
  -choose features to predict price or quality
'''
#------------------------------------------------------------------------------
#data preprocessing
#------------------------------------------------------------------------------
#NA status
raw = pd.read_csv("C:/Users/cherl/GitHub/NCU/wines_SPA.csv")
raw.columns #['winery', 'wine', 'year', 'rating', 'num_reviews', 'country', 'region','price', 'type', 'body', 'acidity']
raw = raw.drop(columns=['country'])
categorical = ['winery', 'wine',  'region', 'type']
numerical = ['year', 'rating', 'num_reviews', 'price', 'body', 'acidity']
raw['year'] = raw['year'].replace('N.V.', np.nan)
raw['year'] = raw['year'].astype(np.float64)
raw_category=raw[['winery', 'wine', 'region', 'type']]
raw.isnull().sum()
raw.info()

#https://stackoverflow.com/questions/64900801/implementing-knn-imputation-on-categorical-variables-in-an-sklearn-pipeline
''' use knn imputate missing value
Q: why we use k=3
A:it is output least na value after imputation 

'''

rmse = lambda y, yhat: np.sqrt(mean_squared_error(y, yhat))

def optim_k(data, target,regressor,range_k):
    errors,r2s = [],[]
    for k in range(1, range_k):
        imputed = data.apply(lambda series: pd.Series(LabelEncoder().fit_transform(series[series.notnull()]),index=series[series.notnull()].index))
        imputer = KNNImputer(n_neighbors=k) 
        imputed = pd.DataFrame(imputer.fit_transform(imputed), columns=raw.columns)
        df_imputed = stats.zscore(imputed)
        
        X = df_imputed.drop(target, axis=1)
        y = df_imputed[target]
        X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3)

        model = regressor()
        model.fit(X_train, y_train)
        y_preds = model.predict(X_test)
        error = rmse(y_test, y_preds)
        errors.append({'K': k, 'RMSE': error})
        r2=r2_score(y_test, y_preds)
        r2s.append({'K': k, 'R-squared': r2})
    return errors, r2s

k_errors,k_r2s = optim_k(raw, 'price', KNeighborsRegressor, 10)
k_rmse_plot=[ list(k_errors[i].values())[1]  for i in range(len(k_errors))]
k_r2_plot=[ list(k_r2s[i].values())[1]  for i in range(len(k_r2s))]
#Plot RMSE and R2
plt.figure(figsize=(20, 15))
plt.plot(k_rmse_plot,linestyle='-',color='green',label='RMSE')
plt.xlabel('number of k',fontsize=16)  
plt.ylabel('values ',fontsize=16)
plt.title("Optim k",fontsize=16) 
plt.show() 

plt.figure(figsize=(20, 15))
plt.plot(k_r2_plot,linestyle='-',color='yellow',alpha=0.6,label='R-squared')
plt.xlabel('number of k',fontsize=16)  
plt.ylabel('values ',fontsize=16)
plt.title("Optim k",fontsize=16) 
plt.show() 



imputed = raw.apply(lambda series: pd.Series(LabelEncoder().fit_transform(series[series.notnull()]),index=series[series.notnull()].index))
imputer = KNNImputer(n_neighbors=3) 
imputed = pd.DataFrame(imputer.fit_transform(imputed), columns=raw.columns)
imputed.isnull().sum()
imputed = stats.zscore(imputed)


def mapping_imputed(df_imputed,df_raw,list_category,list_numerical):
    ''' recover categorical data from numerical to object'''
    for (idx,cat) in enumerate(list_category):
        category_map= pd.merge(df_imputed[cat], df_raw[cat], right_index = True,left_index = True).drop_duplicates(keep='last').reset_index(drop=True).sort_values(by=[str(cat+'_x')])
        category_map.index =category_map[str(cat+'_x')]
        category_map=category_map[str(cat+'_y')].to_dict()
        df_imputed[cat]= df_imputed[cat].map(category_map)
        if df_imputed[cat].isnull().sum() != 0:
            index=np.where(df_imputed[cat].isnull())[0]
            for i in range(len(index)):
                df_imputed[cat].iloc[index] = df_raw[cat].iloc[index]
    return df_imputed
df=imputed
df.isnull().sum()
df=mapping_imputed(df,raw_category,categorical,numerical)
imputed.isnull().sum()
index=np.where(df.isnull())[0]
temp=imputed.drop(index, axis=0)



#------------------------------------------------------------------------------
#preprocessing done
#------------------------------------------------------------------------------

''' choose features and model to predict
--split method: stratified 、proportion、shuffled split
   note!! inspect that data is imbalance or not
'''
from sklearn.linear_model import LinearRegression, Lasso, Ridge, LogisticRegression
from sklearn.tree import DecisionTreeRegressor
from sklearn.neighbors import KNeighborsRegressor
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
#------------
#EDA
#------------
sns.heatmap(imputed.corr(),annot=True, cmap='Blues', fmt='.1f')
sns.pairplot(imputed,hue='rating',kind="reg",diag_kind="hist",palette="husl")
imputed.groupby('winery')['price'].mean().sort_values(ascending=False).head(30).plot.bar()
imputed.groupby('wine')['price'].mean().sort_values(ascending=False).head(30).plot.bar()
imputed.groupby('type')['price'].mean().sort_values(ascending=False).head(20).plot.bar()
imputed.groupby('region')['price'].mean().sort_values(ascending=False).head(20).plot.bar()
##choose split method

#https://clay-atlas.com/blog/2020/07/17/python-cn-scikit-learn-shufflesplit-cross-validation/
X = imputed.drop(columns=['price'])
y = imputed['price']

#https://www.analyticsvidhya.com/blog/2021/11/top-7-cross-validation-techniques-with-python-code/
# coding: utf-8
# ShuffleSplit
# result of different model
#r2、mse、rmse
from sklearn.model_selection import ShuffleSplit,cross_val_score
models_shuffle = {}
def train_validate_predict_shuffle(regressor, x, y, index,scores):
    model = regressor()
    shuffle_split=ShuffleSplit(test_size=0.3,train_size=0.5,n_splits=10)
    scores=cross_val_score(model,X,y,cv=shuffle_split, scoring=scores)
    models_shuffle[index] = np.mean(scores)
    
model_list = [LinearRegression, Ridge,  KNeighborsRegressor, RandomForestRegressor]
model_names = ['Linear Regression', 'Ridge','KNeighbors Regressor', 'Random Forest Regressor']

index = 0
for regressor in model_list:
    train_validate_predict_shuffle(regressor, X, y, model_names[index],'r2')
    index+=1
    
'''
lasso = Lasso()
shuffle_split=ShuffleSplit(test_size=0.3,train_size=0.5,n_splits=10)
scores=cross_val_score(lasso,X,y,cv=shuffle_split, scoring='r2')
print("cross Validation scores:n {}".format(scores))
print("Average Cross Validation score :{}".format(scores.mean()))
np.var(scores)
'''


'''output'''
ss=ShuffleSplit(test_size=0.3,train_size=0.5,n_splits=10)
for train_index, test_index in ss.split(X):
    print("%s %s" % (train_index, test_index))

X = imputed[categorical]
X = imputed[numerical].drop(columns=['price'])
X = imputed.drop(columns=['price'])
y = imputed['price']
X_train, X_test, y_train, y_test = train_test_split(X, y, train_size = .7)
for reg in model_list:
    model = reg()
    model.fit(X_train, y_train)
    print(model.score(X_train, y_train))
    print(model.score(X_test, y_test))
    y_pred = model.predict(X_test)
    print('mse:',mean_squared_error(y_test, y_pred))


model =KNeighborsRegressor()
model.fit(X_train, y_train)
print(model.score(X_train, y_train))
print(model.score(X_test, y_test))
y_pred = model.predict(X_test)
print('mse:',mean_squared_error(y_test, y_pred))
print('rmse:',rmse(y_test, y_pred))
preds = pd.DataFrame({'y_pred': y_pred, 'y_test':y_test})
preds = preds.sort_values(by='y_test')
preds = preds.reset_index()

plt.figure(figsize=(15, 5))
plt.plot(preds['y_pred'], label='pred')
plt.plot(preds['y_test'], label='actual')
plt.legend()
plt.show()
'''implement'''
#another way to imputed value
'''
from sklearn.experimental import enable_iterative_imputer
from sklearn.impute import IterativeImputer
from sklearn.ensemble import RandomForestRegressor, RandomForestClassifier
categorical = ['winery', 'wine', 'year', 'region', 'type']
numerical = ['rating', 'num_reviews', 'price', 'body', 'acidity']
raw[categorical] = raw[categorical].apply(lambda series: pd.Series(LabelEncoder().fit_transform(series[series.notnull()]),index=series[series.notnull()].index))
imp_num = IterativeImputer(estimator=RandomForestRegressor(),initial_strategy='mean',max_iter=10, random_state=0)
imp_cat = IterativeImputer(estimator=RandomForestClassifier(),initial_strategy='most_frequent',max_iter=10, random_state=0)
raw[numerical] = imp_num.fit_transform(raw[numerical])
raw[categorical] = imp_cat.fit_transform(raw[categorical])
    '''