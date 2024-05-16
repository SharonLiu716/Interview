# -*- coding: utf-8 -*-
"""
Created on Thu Apr 18 15:52:39 2024

@author: cherl
"""

import os, shutil, glob,csv,datetime
import pandas as pd
import numpy as np
from datetime import *
import xlsxwriter



def mean_and_time(col_data, init_time):
    
    col_data_new = col_data.iloc[:, 1].drop_duplicates(keep='first').tolist()
    
    if col_data.shape[0] == 0:
        return  0, 0, 0
    else:
        time_start= datetime.strptime(col_data.loc[0,'time'] , "%H:%M:%S")
        col_time = (time_start - init_time).seconds
        col_mean = col_data.iloc[:, 1].mean()    
        return col_time, col_mean, col_data_new 

def combined_to_info(df_hr, df_sbp, df_dbp, df_br, init_time, file_date,machine_num):
    
    hr_time, hr_mean, hr_data = mean_and_time(df_hr,init_time)
    sbp_time, sbp_mean, sbp_data = mean_and_time(df_sbp,init_time)
    dbp_time, dbp_mean, dbp_data = mean_and_time(df_dbp,init_time)
    br_time, br_mean, br_data = mean_and_time(df_br,init_time)
    
    
    
    info_col= {'machine': machine_num, 'file_date':file_date,
               'interval_HR':hr_time, 'HR_mean':hr_mean, 'HR_raw':hr_data,
               'interval_SBP':sbp_time, 'SBP_mean':sbp_mean, 'SBP_raw':sbp_data,
               'interval_DBP':dbp_time, 'DBP_mean':dbp_mean, 'DBP_raw':dbp_data,
               'interval_BR':br_time, 'BR_mean':br_mean,'BR_raw':br_data} 
     
    return info_col 


def clean_data(df):
    # 初始化四個空的DataFrame來存放各項測量資料
    df_hr = pd.DataFrame(columns=['time', 'HR'])
    df_sbp = pd.DataFrame(columns=['time', 'SBP'])
    df_dbp = pd.DataFrame(columns=['time', 'DBP'])
    df_br = pd.DataFrame(columns=['time', 'BR'])
    
    for index, row in data.iterrows():
        
        if row['HR'] != -1 and row['HR'] != 0:        
            df_hr = pd.concat([ df_hr, pd.DataFrame([{'time': row['time'], 'HR': row['HR']}])], ignore_index=True)
        if row['SBP'] != -1 and row['SBP'] != 0:        
            df_sbp = pd.concat([ df_sbp, pd.DataFrame([{'time': row['time'], 'SBP': row['SBP']}])], ignore_index=True)
        if row['DBP'] != -1 and row['DBP'] != 0:        
            df_dbp = pd.concat([ df_dbp, pd.DataFrame([{'time': row['time'], 'DBP': row['DBP']}])], ignore_index=True)
        if row['BR'] != -1 and row['BR'] != 0:        
            df_br = pd.concat([ df_br, pd.DataFrame([{'time': row['time'], 'BR': row['BR']}])], ignore_index=True)
    
    return df_hr, df_sbp, df_dbp, df_br



def save_to_excel(df_hr, df_sbp, df_dbp, df_br, filename):
    # 建立一個Excel Writer物件，用於將四個DataFrame分別存儲為不同的工作表
    with pd.ExcelWriter(filename, engine='xlsxwriter') as writer:
        # 將每個DataFrame存儲為一個工作表
        df_hr.to_excel(writer, sheet_name='HR', index=False)
        df_sbp.to_excel(writer, sheet_name='SBP', index=False)
        df_dbp.to_excel(writer, sheet_name='DBP', index=False)
        df_br.to_excel(writer, sheet_name='BR', index=False)
    
    print(filename)


# 讀取bpm受試者資訊的總表
bpm_info = pd.read_excel(r"result/subjectok.xlsx",sheet_name='bpmok_info_correct') 
# 讀取bpm受試者測量結果的csv路徑    
bpm_ok_files = []
main_bpm_path = r"C:\Users\cherl\Chintek_Work\BPM\bpm"
for bpm_file in glob.glob( os.path.join(main_bpm_path,'**/**.csv'), recursive=True):
    if bpm_file.split('\\')[-1] != 'personal_infomation.csv':
        bpm_ok_files.append(bpm_file) 
        


bpm_info_new = pd.DataFrame()

for file in bpm_ok_files:
       
    csv_file_name = file.split('\\')[-1].strip('.csv')
    data = pd.read_csv(file,header=0, usecols=["time","HR","BR", "SBP", "DBP"])    
    init_time = datetime.strptime(data.loc[0,'time'], "%H:%M:%S")
    # 將HR、BR、SBP、DBP去除-1與0後分別存成只有time+測量資料的df
    hr, sbp, dbp, br = clean_data(data)
    
    if file.split('\\')[-3] == 'machine1_OK':
        # 將去除後的量測資料分為四個df，以4個sheet存在同個excel輸出
        path_to = r'C:\Users\cherl\Chintek_Work\BPM\bpm\excel_ok_m1'        
        path_to = os.path.join(path_to,file.split('\\')[-1].strip('.csv')+'.xlsx')
        save_to_excel(hr, sbp, dbp, br, path_to)
        
        # 計算HR、BR、SBP、DBP四個測量值的平均與間隔時間
        info_from_raw = combined_to_info(hr, sbp, dbp, br, init_time, csv_file_name ,1) 
        bpm_info_new = pd.concat([ bpm_info_new, pd.DataFrame([info_from_raw])], ignore_index=True)
        
    elif file.split('\\')[-3] == 'machine2_OK':
        # 將去除後的量測資料分為四個df，以4個sheet存在同個excel輸出
        path_to = r'C:\Users\cherl\Chintek_Work\BPM\bpm\excel_ok_m2'        
        path_to = os.path.join(path_to,file.split('\\')[-1].strip('.csv')+'.xlsx')
        save_to_excel(hr, sbp, dbp, br, path_to)
        
        # 計算HR、BR、SBP、DBP四個測量值的平均與間隔時間
        info_from_raw = combined_to_info(hr, sbp, dbp, br, init_time, csv_file_name ,2) 
        bpm_info_new = pd.concat([ bpm_info_new, pd.DataFrame([info_from_raw])], ignore_index=True)
        
    with open(os.path.join(main_bpm_path, 'excel_ok_info.txt'), 'a') as log_file:
        log_file.write(f"{path_to}\n")    


# 輸出合併後的df
bpm_info=pd.merge(left=bpm_info,right=bpm_info_new,how="outer")
bpm_info_correct = bpm_info[bpm_info['SBP_mean']!=0].dropna()

with pd.ExcelWriter(r"C:\Users\cherl\Chintek_Work\BPM\result\bpm_data_240422.xlsx", engine='xlsxwriter') as writer:
    # 將每個DataFrame存儲為一個工作表
    bpm_info.to_excel(writer, sheet_name='bpm_full_data', index=False, float_format="%.2f")
    bpm_info_correct.to_excel(writer, sheet_name='bpm_full_data_correct', index=False, float_format="%.2f")
    
#-----------------新增欄位：保存HR、SBP、DBP、BR的數值與順序--------------------------------------

for file in bpm_ok_files:
       
    csv_file_name = file.split('\\')[-1].strip('.csv')
    data = pd.read_csv(file,header=0, usecols=["time","HR","BR", "SBP", "DBP"])    
    init_time = datetime.strptime(data.loc[0,'time'], "%H:%M:%S")
    # 將HR、BR、SBP、DBP去除-1與0後分別存成只有time+測量資料的df
    hr, sbp, dbp, br = clean_data(data)
    
    if file.split('\\')[-3] == 'machine1_OK':      
        
        # 計算HR、BR、SBP、DBP四個測量值的平均與間隔時間
        info_from_raw = combined_to_info(hr, sbp, dbp, br, init_time, csv_file_name ,1) 
        bpm_info_new = pd.concat([ bpm_info_new, pd.DataFrame([info_from_raw])], ignore_index=True)
        
    elif file.split('\\')[-3] == 'machine2_OK':        
        
        # 計算HR、BR、SBP、DBP四個測量值的平均與間隔時間
        info_from_raw = combined_to_info(hr, sbp, dbp, br, init_time, csv_file_name ,2) 
        bpm_info_new = pd.concat([ bpm_info_new, pd.DataFrame([info_from_raw])], ignore_index=True)
        



# 輸出合併後的df
bpm_info=pd.merge(left=bpm_info,right=bpm_info_new,how="outer")
bpm_info_correct = bpm_info[bpm_info['SBP_mean']!=0].dropna()

# 新增sheet至現有的excel
with pd.ExcelWriter(r"C:\Users\cherl\Chintek_Work\BPM\result\bpm_data_240423.xlsx", engine='openpyxl', mode='a') as writer:
    #bpm_info.to_excel(writer, sheet_name='bpmok_info', index=False, float_format="%.2f")
    bpm_info_correct.to_excel(writer, sheet_name='bpm_full_data_v1', index=False, float_format="%.2f")