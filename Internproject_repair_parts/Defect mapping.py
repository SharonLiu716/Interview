# -*- coding: utf-8 -*-
"""
Created on Mon Aug  9 09:14:21 2021

@author: yihsuan.liu
"""

import os
os.environ["KMP_DUPLICATE_LIB_OK"]  =  "TRUE"
import glob2
import pandas as pd
import numpy as np
import sys
import torch 
from torch.utils.data import Dataset,DataLoader
from data_preprocessing import Dataset_monthly

class Dataset_monthly(Dataset):
    def __init__(self, dir_list, file_list, data_version = "original", mapping_dict = None, columns = "all"):
        # data_version: original: monthly raw data from RQ, updated: 經過欄位縮短&變換
        self.dir_list = dir_list
        self.file_list = file_list
        self.data_version = data_version
        self.mapping_dict = mapping_dict       
        self.columns = columns
        
    def __len__(self):
        return len(self.file_list)
        
    def __getitem__(self, idx):
        yearmonth = int(self.file_list[idx])
        # year month < 202010 = 舊版
        if self.data_version == "original" and yearmonth < 202010:
            # 舊版的一個月的data 分為3 個Sheet
            data = pd.DataFrame()
            for i in range(1,4):
                sheetname = "Sheet" + str(i)                
                try: 
                    # 即使舊版也不一定都有3個sheet
                    sheet = self.read_one_sheet(self.dir_list[idx], the_sheet = sheetname)
                    data = data.append(sheet, ignore_index=True)
                except: continue
        else:          
            data = self.read_one_sheet(self.dir_list[idx], version = "new")
        return data
    
    def read_one_sheet(self, the_path, the_sheet = 0, version = "old"):
        if version == "old":  
            data1 = pd.read_excel(the_path, engine='openpyxl', sheet_name=the_sheet)
            data1 = data1.rename(columns=self.mapping_dict)
        elif self.data_version == "updated":
            data1 = pd.read_excel(the_path, index_col = 0, engine='openpyxl')
        else:
            data1 = pd.read_excel(the_path, engine='openpyxl')
        if self.columns != "all":
            data1 = data1[self.columns]
        return data1
'''
WORK: symptom & defect type mapping table
讀取data_V0資料夾內所有excel取得symtom 的unique list
'''
def get_path():
    '''
    取得檔案路徑
    '''
    PATH='C:/Users/yihsuan.liu/Documents/RQ物料資料清理/'
    DATA_PATH = PATH + "/data_V0" # subfolder with shorten version of columns
    
    dataset_list = glob2.glob(DATA_PATH+'/*.xlsx')
    # ['D:\\repair-parts\\data_V0\\201908.xlsx', ...]
    dataset_namelist = list(map(os.path.basename, dataset_list))
    dataset_namelist = [x.strip('.xlsx') for x in dataset_namelist]
    return dataset_list, dataset_namelist

'''讀取所有dataV0的路徑
   抽出所有data的symptom對應維修效益的defect type輸出mapping table
'''
path=get_path()
columns = ["RC_ID","SERIAL_NO", "REPAIR_FINISHED_DATE"] #,'SYMPTOM','FAILURE_STAGE'
data_loader_1 = Dataset_monthly(path[0],path[1], data_version = "updated", columns = columns)
df = pd.DataFrame()

for k in range(len(data_loader_1)):
    df = df.append(data_loader_1[k], ignore_index=True)
    sys.stdout.write('\rReading : '+path[1][k])
Path='C:/Users/yihsuan.liu/Documents/RQ物料資料清理/'
map_df=pd.read_excel(Path+'repair-parts/'+'Defect_Mapping.xlsx')
symptom=df['SYMPTOM'].drop_duplicates()
symptom=(symptom.reset_index(drop=True)).to_frame()

for idx in range(symptom.shape[0]):
    if symptom.at[idx,'SYMPTOM'] in list(map_df['SYMPTOM']):        
        index = list(map_df['SYMPTOM']).index(symptom.at[idx,'SYMPTOM'])        
        symptom.at[idx, "Defect_Type"]=map_df.at[index,'Defect Type']
no_nansymptom = symptom[symptom['Defect_Type'].notna()]
'''新增克翔add 的defect type'''
add_defect=pd.read_excel(Path+'repair-parts/'+'Defect_Mapping.xlsx',sheet_name='新增defect type')
add_defect=add_defect.dropna(subset=['Defect_Type','Defect_Type(新增)'],how='all',axis=0)
add_defect['Defect_Type']=add_defect['Defect_Type'].fillna(add_defect['Defect_Type(新增)'])
add_defect=add_defect.drop(["Defect_Type(新增)"],axis=1).reset_index(drop=True)

with pd.ExcelWriter(engine='openpyxl', path=Path+'repair-parts/'+'Defect_Mapping.xlsx', mode='a') as writer: # mode='a'現有檔案讀寫
    # 寫入新資料到sheet
    #symptom.to_excel(writer, sheet_name='with nan Defect_type', index=False) # 存到指定的sheet
    #no_nansymptom.to_excel(writer, sheet_name='NO nan Defect_type', index=False) # 存到指定的sheet
    add_defect.to_excel(writer, sheet_name='Defect_type(update)', index=False)

#找出ACCENTECH轉換到IGS的時間點
j=11
element='IGS'

for j in range(len(data_loader_1)):
    df=data_loader_1[j]
    if element in list(df['RC_ID']):
        index = list(df['RC_ID']).index(element)
        date=df.at[index, "REPAIR_FINISHED_DATE"]
        print(path[1][j],index,date)
        break
        
    df = df.append(data_loader_1[j], ignore_index=True)
    sys.stdout.write('\rReading : '+path[1][j])
