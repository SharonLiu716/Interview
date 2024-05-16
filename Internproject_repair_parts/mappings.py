# -*- coding: utf-8 -*-
"""
Created on Sat Aug 14 10:05:13 2021

@author: 懿萱
"""

'''0813 child test 
擷取PART_NO 料號前四碼+Key Word對照物料類別
Part Mapping Table columns：PART_TYPE、料號前四碼、Key Word、Child Description
Data columns：PART_TYPE、PART_NO、Child Description
'''
import os
os.environ["KMP_DUPLICATE_LIB_OK"]  =  "TRUE"
import glob2
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import sys
import torch 
from torch.utils.data import Dataset,DataLoader
from data_loader import Dataset_monthly, get_path, get_data

#從mapping table 分出key word 分為1個字和兩個字的list跟PART MAP
def PartKey(PATH):    
    
    PART=pd.read_excel(PATH+'Parts_Mapping.xlsx',sheet_name='Part Type')    
    KEY1,KEY2=[],[]
    for (idx,child) in enumerate(PART['Child Description']):
        count=len(PART.at[idx,'Key Word'].split(','))
        if count==1:
            KEY1.append(PART.at[idx,'Key Word'])
        else:
            KEY2.append(PART.at[idx,'Key Word'])    
    return PART,KEY1,KEY2

#取料號四碼+key word
def PartMapping(DF,PART,KEY1,KEY2):
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


    

#read all file path and data
path='C:\\Users\\yihsuan.liu\\Documents\\RQ物料資料清理'
subfolder="data_with_qty"
data_path=get_path(path, subfolder)
columns = ["RC_ID","SERIAL_NO",'MODEL_NAME','PRODUCT_ID','FAILURE_STAGE','ORDER_NO','ORDER_TYPE','REPAIR_STATUS_ITEM','REPAIR_GRADE','NUMBER_OF_TRANSFER', "REPAIR_FINISHED_DATE",'month_year','FROM_RC','PART_NO','DAY_FROM_RC','PART_QTY','SYMPTOM','SYMPTOM_NO']
data_loader=Dataset_monthly(data_path[0],data_path[1], data_version = "updated", columns = columns)
df_alldata=get_data(path, subfolder, columns)

'''mapping
Child:用來補上現有data沒有的child description
Defect:defect type的mapping table
Producttype:將機種分為non-md / md的依據
mainRC:將主要預估的RC的data分出來
    '''
Child=pd.read_excel(path+'\\repair-parts\\'+'Description.xlsx')
Defect=pd.read_excel(path+'\\repair-parts\\'+'Defect_Mapping.xlsx',sheet_name='Defect_type(update)')
Producttype=pd.read_excel(path+'\\repair-parts\\'+'Mapping Tables.xlsx',sheet_name='Product Type')
Abnormal=pd.read_excel(path+'\\repair-parts\\'+'defect type 不會用到的 Parts type_20210518.xlsx')
mainRC=['ACCU','UPLUS','EASCON','IGS','SMM','TGO','ZZHC']

#物料mapping table讀取並分出1個字和2個字的key word list
part,key1,key2=PartKey(path+'\\repair-parts\\')
#------------------------------------------------------------------------

'''
work
data： all / by month---> mapping child、defect type、part type、product type
first: main RC/ all RC
second: check no_child,no_parttype,abnormal_no_defect (group by serial no+month)
third: non-md/ all
file output:mainRC、allRC
'''
#all data check
#新增child description
df_alldata = pd.merge(df_alldata, Child, how='left')
#新增PART_TYPE
df_alldata=PartMapping(df_alldata, part, key1, key2)
#新增defect type
df_alldata = pd.merge(df_alldata, Defect, how='left')
#取product id前兩碼
df_alldata['Product_id_start_with'] = df_alldata['PRODUCT_ID'].str.split('').str.get(1)+df_alldata['PRODUCT_ID'].str.split('').str.get(2)
#新增product id type
df_alldata = pd.merge(df_alldata, Producttype, how='left')
#匯入defect type 不會用到的 Parts type_20210518檢查異常資料(待修)
abnormal=df_alldata.reset_index().merge(Abnormal).set_index('index')
new=testDF.drop(abnormal.index)
#計算PART_NO對不上child筆數、抽出PART_NO對不上child的data
check_all1=sum((df_alldata['PART_NO'].notna()) & (df_alldata['Child Description'].isna()))
check_all1_data=df_alldata[(df_alldata['PART_NO'].notna()) & (df_alldata['Child Description'].isna())]
#計算有多少PART+keyword對不到parttype的筆數、抽出PART+keyword對不到parttype的data
check_all2=sum((df_alldata['料號前四碼'].notna() & df_alldata['Key Word'].notna()) & (df_alldata['PART_TYPE'].isna()))
check_all2_data=df_alldata[(df_alldata['料號前四碼'].notna() & df_alldata['Key Word'].notna()) & (df_alldata['PART_TYPE'].isna())]
#計算有多少no defect卻有用料的筆數、抽出no defect卻有用料的data
check_all3=sum((df_alldata['SYMPTOM']=='No Defect') & (df_alldata['PART_NO'].notna()))
check_all3_data=df_alldata[(df_alldata['SYMPTOM']=='No Defect') & (df_alldata['PART_NO'].notna())]
#group by serial no & month
savepath = os.path.join(os.getcwd(),'Group result of all data.xlsx') # 設定路徑及檔名
writer = pd.ExcelWriter(savepath, engine='openpyxl') # 指定引擎openpyxl
check_all1_data.groupby(by=['RC_ID','SERIAL_NO','month_year']).size().reset_index(name='counts').to_excel(writer, sheet_name='count No_child')
check_all2_data.groupby(by=['RC_ID','SERIAL_NO','month_year']).size().reset_index(name='counts').to_excel(writer, sheet_name='count No_Part Type')
check_all3_data.groupby(by=['RC_ID','SERIAL_NO','month_year']).size().reset_index(name='counts').to_excel(writer, sheet_name='count Abnormal No_defect')
check_all1_data.to_excel(writer, sheet_name='No_child') # 存到指定的sheet
check_all2_data.to_excel(writer, sheet_name='No_part_type')
check_all3_data.to_excel(writer, sheet_name='Abnormal No_defect')
check_all3_data[check_all3_data['RC_ID'].isin(mainRC)].to_excel(writer, sheet_name='main RC Abnormal No_defect')
writer.save() # 存檔生成excel檔案

#將dataV0內所有資料加上child、parttype、defect type，檢查有沒有對不到child、parttype、defect異常的資料
no_child,no_parttype,no_defect=[],[],[]

#分每月份資料統計
for k in range(len(data_loader)):
    sys.stdout.write('\rReading : '+data_path[1][k])
    df = data_loader[k]
    #新增child description
    df = pd.merge(df, Child, how='left')
    #新增PART_TYPE
    df=PartMapping(df, part, key1, key2)
    #新增defect type
    df = pd.merge(df, Defect, how='left')
    #取product id前兩碼
    df['Product_id_start_with'] = df['PRODUCT_ID'].str.split('').str.get(1)+df['PRODUCT_ID'].str.split('').str.get(2)
    #新增product id type
    df = pd.merge(df, Producttype, how='left')
    df.to_excel(data_path[1][k]+'.xlsx')
    print('\r'+data_path[1][k]+' merge ok')
    #計算PART_NO對不上child筆數、抽出PART_NO對不上child的data
    check1=sum((df['PART_NO'].notna()) & (df['Child Description'].isna()))
    no_child.append(check1)
    check1_data=df[(df['PART_NO'].notna()) & (df['Child Description'].isna())]    
    #計算有多少PART+keyword對不到parttype的筆數、抽出PART+keyword對不到parttype的data
    check2=sum((df['料號前四碼'].notna() & df['Key Word'].notna()) & (df['PART_TYPE'].isna()))
    no_parttype.append(check2)
    check2_data=df[(df['料號前四碼'].notna() & df['Key Word'].notna()) & (df['PART_TYPE'].isna())]
    #計算有多少no defect卻有用料的筆數、抽出no defect卻有用料的data
    check3=sum((df['SYMPTOM']=='No Defect') & (df['PART_NO'].notna()))
    no_defect.append(check3)
    check3_data=df[(df['SYMPTOM']=='No Defect') & (df['PART_NO'].notna())]    
    print('\r'+data_path[1][k]+' check ok')
    # 開一個新的excel並把多個df寫到同excel不同sheet
    savepath = os.path.join(os.getcwd(),data_path[1][k]+' Group.xlsx') # 設定路徑及檔名
    writer = pd.ExcelWriter(savepath, engine='openpyxl') # 指定引擎openpyxl
    check1_data.groupby(by=['RC_ID','SERIAL_NO','month_year']).size().reset_index(name='counts').to_excel(writer, sheet_name='count No_child')
    check2_data.groupby(by=['RC_ID','SERIAL_NO','month_year']).size().reset_index(name='counts').to_excel(writer, sheet_name='count No_Part Type')
    check3_data.groupby(by=['RC_ID','SERIAL_NO','month_year']).size().reset_index(name='counts').to_excel(writer, sheet_name='count Abnormal No_defect')
    check1_data.to_excel(writer, sheet_name='No_child') # 存到指定的sheet
    check2_data.to_excel(writer, sheet_name='No_part_type')
    check3_data.to_excel(writer, sheet_name='Abnormal No_defect')
    writer.save()
    writer.close()


check = pd.DataFrame(list(zip(data_path[1],no_child, no_parttype,no_defect)),
               columns =['file','no_child','no_part_type','no_defect'])
check.to_excel(subfolder+'check.xlsx',index=False)
    


