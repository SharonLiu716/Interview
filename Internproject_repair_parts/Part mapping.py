# -*- coding: utf-8 -*-
"""
Created on Fri Aug  6 13:26:28 2021

@author: yihsuan.liu
"""

import os
os.environ["KMP_DUPLICATE_LIB_OK"]  =  "TRUE"
import glob2
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import sys
import torch 
from torch.utils.data import Dataset,DataLoader

'''
Work：整合維修效益Parts與物料Parts
'''
path='C:/Users/yihsuan.liu/Documents/RQ物料資料清理/repair-parts/'
df_part3 = pd.read_excel(path+'Parts_Mapping.xlsx',sheet_name='維修效益_Parts')
df_part4 = pd.read_excel(path+'Parts_Mapping.xlsx',sheet_name='物料_Parts')
df_part4.drop(df_part4.loc[df_part4['物料類別']=='物料類別'].index, inplace=True)

'''
#物料part mapping table整理
補上所有nan的物料類別、刪除child & 料號為nan的row、更新index、轉換料號type
'''
df_part4['物料類別']=df_part4['物料類別'].fillna(method='pad',axis=0)
df_part4 = df_part4[df_part4['Child Description'].notna()]
df_part4 = df_part4[df_part4['料號前四碼'].notna()]
df_part4 = df_part4.reset_index(drop=True)
df_part4['料號前四碼']=df_part4['料號前四碼'].astype(str)
'''
#維修效益base parts df：串接點_part no & 料號前四碼
四碼跟三碼分開處理
'''
for idx in range(df_part4.shape[0]):
    if df_part4.at[idx,'料號前四碼'] in list(df_part3['PART_NO']):        
        index = list(df_part3['PART_NO']).index(df_part4.at[idx,'料號前四碼'])        
        df_part4.at[idx, "維修效益Part_type"]=df_part3.at[index,'Parts Type']
    elif df_part4.at[idx,'料號前四碼'][0:3] in list(df_part3['PART_NO']):
        index = list(df_part3['PART_NO']).index(df_part4.at[idx,'料號前四碼'][0:3])     
        df_part4.at[idx, "維修效益Part_type"]=df_part3.at[index,'Parts Type']
'''重新排列欄位、刪除對不上物料類別的row匯出兩版mapping table'''
df_part4=df_part4[['維修效益Part_type', '料號前四碼','物料類別', 'Key word', 'Child Description']]     
df_part4= df_part4[df_part4['維修效益Part_type'].notna()]
df_part4 = df_part4.reset_index(drop=True)

with pd.ExcelWriter(engine='openpyxl', path=path+'Parts_Mapping.xlsx', mode='a') as writer: # mode='a'現有檔案讀寫
    # 寫入新資料到sheet
    child.to_excel(writer, sheet_name='Child Check', index=False) # 存到指定的sheet
    group.to_excel(writer, sheet_name='Child group result') # 存到指定的sheet
    

