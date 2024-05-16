# -*- coding: utf-8 -*-
"""
Created on Wed Aug 18 09:51:07 2021

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
from data_loader import Dataset_monthly, get_path, get_data, update_raw_column
'''
Child:用來補上現有data沒有的child description
Defect:defect type的mapping table
Producttype:將機種分為non-md / md的依據
mainRC:將主要預估的RC的data分出來'''

class AddColumns():
    
    '''def __init__
    Parameters
    ----------
    DF : 每月份資料(dataframe)
    mapping_dict : mapping table.xlsx的路徑
    merge_columns：用來merge出defect group的欄位
    action:
        -'merge':預設，新增欄位
        -'delete':刪除異常資料(待補)
    version:
        -'all':所有RC
        -'mainRC':只有主要RC(待補)
    
    Returns
    -------
    更新完的dataframe
    
    '''
    
    def __init__(self, DF, mapping_dict,merge_columns,action = "merge",version='all'):
        # data_version: original: monthly raw data from RQ, updated: 經過欄位縮短&變換
        self.DF = DF                
        self.mapping_dict = pd.read_excel(mapping_dict, sheet_name=None) #sheetname=none可以匯入所有sheet，但以dict呈現
        self.merge_columns = merge_columns
        self.action = action
        self.version = version
        self.key1,self.key2 = self.PartKey(self.mapping_dict['Part Type'])        
        self.updated=self.Merge(self.DF,self.mapping_dict['Child'],self.mapping_dict['Part Type'],self.mapping_dict['Defect Type'],self.mapping_dict['Product Type'],self.merge_columns)
        
        
    #從mapping table 分出key word 分為1個字和兩個字的list跟PART MAP
    def PartKey(self,PART):    
        KEY1,KEY2=[],[]
        for (idx,parts) in enumerate(PART['PART_TYPE']):
            count=len(PART.at[idx,'Key Word'].split(','))
            if count==1:
                KEY1.append(PART.at[idx,'Key Word'])
            else:
                KEY2.append(PART.at[idx,'Key Word'])
        return KEY1,KEY2


#取料號四碼+key word
    def PartMapping(self,DF,PART,KEY1,KEY2):
        
        DF['料號前四碼'],DF['Key Word']='',''
        #取料號四碼+keyword
        DF['PART_NO']=DF['PART_NO'].fillna('-1').astype(str)
        DF['Child Description']=DF['Child Description'].fillna('-1').astype(str)
        for (idx,child) in enumerate(DF['Child Description']):
            DF.at[idx,'料號前四碼']=str(np.where(DF.at[idx,'PART_NO']=='-1', '', str(DF.at[idx,'PART_NO'][0:4])))  #若PART_NO!=nan,取料號前四碼
            subchild=np.where(child=='-1', '',child.split(',')).tolist()  #若child!=nan,分割child字串
            if subchild!=['']:
                if subchild[0] in KEY1: #keyword只有一個字
                    DF.at[idx, 'Key Word']=subchild[0]
                elif (subchild[0]+','+subchild[1]) in KEY2: #keyword有兩個字
                    DF.at[idx, 'Key Word']=subchild[0]+','+subchild[1]
                elif (subchild[0]=='Bezel') and (subchild[1][0:3]=='PBG'): #Special Case:Bezel,PBG
                    DF.at[idx, 'Key Word']=subchild[0]+','+subchild[1][0:3]
            else:
                DF.at[idx, 'Key Word']=np.nan
        DF['PART_NO']=DF['PART_NO'].replace('-1', np.nan)
        DF['Child Description']=DF['Child Description'].replace('-1', np.nan)
        # map part type
        PART=PART[['PART_TYPE','料號前四碼', 'Key Word']]
        DF = pd.merge(DF, PART,on=['料號前四碼', 'Key Word'], how='left')
        return DF
    
    def DefectGroup(self,DF,column):
        temp = DF[DF['Defect_Type'].notna()].groupby(column)
        temp = temp.apply(lambda x: ','.join(sorted(x['Defect_Type'].unique()))).reset_index()
        column.append('Defect Group')
        temp.columns =column
        column.remove('Defect Group')    
        DF = DF.merge(temp, how = 'left',on = column)
        return DF

    def Merge(self,DF,CHILD,Part,Defect,Product,merge_columns):
        #根據Part_No新增child description
        DF= pd.merge(DF, CHILD, how='left')
        #新增欄位：PART_TYPE、DEFECT_TYPE、PRODUCT TYPE
        DF=self.PartMapping(DF, Part, self.key1, self.key2)
        DF = pd.merge(DF, Defect, how='left')
        DF['Product_id_start_with'] = DF['PRODUCT_ID'].str.slice(stop=2)
        DF = pd.merge(DF,Product, how='left')
        DF=self.DefectGroup(DF,merge_columns)
        return DF


        
'''待修
    if action == "delete":
        def Abnormal(DF,ABNORMAL):
            #刪除PART+keyword對不到parttype的data
            Ab1=DF[(DF['料號前四碼'].notna() & DF['Key Word'].notna()) & (DF['PART_TYPE'].isna())]
            DF=DF.drop(Ab1.index)
            #刪除no defect卻有用料的data
            Ab2=DF[(DF['SYMPTOM']=='No Defect') & (DF['PART_NO'].notna())]
            DF=DF.drop(Ab2.index)
            #匯入defect type 不會用到的 Parts type_20210518檢查異常資料(待補)

            return DF,AB1,AB2

class Split():
    def __init__(self, DF, list_of_RC,merge_columns,action = "mainRC"):
        # data_version: original: monthly raw data from RQ, updated: 經過欄位縮短&變換
        self.DF = DF                
        self.list_of_RC = list_of_RC
        self.action = action        
        self.mainRC_df = self. mainRC(self.DF,self.list_of_RC)        
        self.updated=self.Merge(self.DF,self.mapping_dict['Part Type'],self.mapping_dict['Defect Type'],self.mapping_dict['Product Type'],self.merge_columns)
        
    def mainRC(self,DF,list_of_RC):
        if self.version=='mainRC':
            DF=DF[DF['RC_ID'].isin(list_of_RC)]
            
    def Abnormal(DF,ABNORMAL):
            #刪除PART+keyword對不到parttype的data
            Ab1=DF[(DF['料號前四碼'].notna() & DF['Key Word'].notna()) & (DF['PART_TYPE'].isna())]
            DF=DF.drop(Ab1.index)
            #刪除no defect卻有用料的data
            Ab2=DF[(DF['SYMPTOM']=='No Defect') & (DF['PART_NO'].notna())]
            DF=DF.drop(Ab2.index)
            #匯入defect type 不會用到的 Parts type_20210518檢查異常資料(待補)

            return DF,AB1,AB2 
'''

#=================================================================
#main function read all file path and data
path='C:\\Users\\yihsuan.liu\\Documents\\RQ物料資料清理'
subfolder="data_with_qty"

data_path=get_path(path, subfolder)
columns = ["RC_ID","SERIAL_NO",'MODEL_NAME','PRODUCT_ID','FAILURE_STAGE','ORDER_NO','ORDER_TYPE','REPAIR_STATUS_ITEM','REPAIR_GRADE','NUMBER_OF_TRANSFER', "REPAIR_FINISHED_DATE",'month_year','FROM_RC','PART_NO','DAY_FROM_RC','PART_QTY','SYMPTOM','SYMPTOM_NO']
data_loader=Dataset_monthly(data_path[0],data_path[1], data_version = "updated", columns = columns)
mapping_dict='C:\\Users\\yihsuan.liu\\Documents\\RQ物料資料清理\\repair-parts\\Mapping Tables.xlsx'
merge_columns=['SERIAL_NO','month_year']
mainRC=['ACCU','UPLUS','EASCON','IGS','SMM','TGO','ZZHC']
output_path=path+'\\final'
for k in range(len(data_loader)):
    sys.stdout.write('\rReading : '+data_path[1][k])
    df = data_loader[k]
    df=AddColumns(DF=df, mapping_dict=mapping_dict,merge_columns=merge_columns).updated
    df.to_excel(output_path+'\\{}.xlsx'.format(data_path[1][k]), engine='openpyxl', encoding='utf_8_sig')
    
    #mainRC_df=df[df['RC_ID'].isin(mainRC)]     
    #df.groupby(by=['RC_ID','SERIAL_NO','month_year']).to_excel(writer, sheet_name='Group')

