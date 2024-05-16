# -*- coding: utf-8 -*-
"""
Created on Tue Apr 23 09:11:47 2024

@author: cherl
"""

import os, shutil, glob,csv,datetime,pymongo,xlsxwriter
import pandas as pd
import numpy as np
from datetime import *
from pymongo import MongoClient

bpm_info = pd.read_excel(r"result/bpm_data_240423.xlsx",sheet_name='bpm_full_data_v1') 
radar_info = pd.read_excel(r"result/radar_data_240423.xlsx",sheet_name='radar_full_data') 



bpm_info.rename(columns = { 'interval_HR': 'hr_interval_bpm' ,'HR_mean':'hr_mean_bpm', 'HR_raw':'hr_raw_bpm',       
                           'interval_SBP': 'sbp_interval_bpm', 'SBP_mean':'sbp_mean_bpm', 'SBP_raw':'sbp_raw_bpm',
                           'interval_DBP': 'dbp_interval_bpm', 'DBP_mean':'dbp_mean_bpm', 'DBP_raw':'dbp_raw_bpm',
                           'interval_BR': 'br_interval_bpm', 'BR_mean':'br_mean_bpm', 'BR_raw':'br_raw_bpm'}, inplace = True) 


radar_info.rename(columns = {'hr_mean':'hr_mean_radar','br_mean':'br_mean_radar','hr_raw':'hr_raw_radar','br_raw':'br_raw_radar'}, inplace = True) 
radar_info = radar_info[radar_info['hr_mean_radar']!= -1]

# radar and bpm compare file date
bpm_rader_merge=pd.merge(left=bpm_info,right=radar_info,how="outer")
bpmradar_subject = bpm_rader_merge.groupby(['height', 'weight', 'age','gender'])
len(bpmradar_subject.groups.keys())


# 針對只有bpm或只有radar的資料特別抽出來處理 
def extract_one_subject(subject_detail):
    if subject_df.iloc[0,0] == 'bpm':
        result_one = subject_df.rename(columns = {'file_date':'bpm_file'}).drop(columns=['info_from','machine'])
        result_one.insert(1, "radar_file", np.nan)
    elif subject_df.iloc[0,0] == 'radar':
        result_one = subject_df.rename(columns = {'file_date':'radar_file'}).drop(columns=['info_from','machine'])
        result_one.insert(1, "bpm_file", np.nan )
    
    return result_one
   
# 對所有資料進行處理  
    
def process_subject_data(subject_information,subject_detail):
    
    result_col_order =['height', 'weight', 'age', 'gender', 'bpm_file', 'radar_file', 'HR',
                       'hr_interval_bpm', 'hr_mean_bpm', 'hr_raw_bpm', 'hr_mean_radar',
                       'hr_raw_radar', 'br_mean_bpm', 'br_raw_bpm', 'br_mean_radar',
                       'br_raw_radar', 'br_interval_bpm', 'SBP', 'sbp_interval_bpm',
                       'sbp_mean_bpm', 'sbp_raw_bpm', 'DBP', 'dbp_interval_bpm',
                       'dbp_mean_bpm', 'dbp_raw_bpm']
    # 受使者身高、體重、年齡、性別
    subject = {'height': subject_information[0],'weight':subject_information[1],'age':subject_information[2],'gender':subject_information[3]}
    filtered_bpm = subject_detail[subject_detail['info_from'] == 'bpm'].dropna(axis=1).rename(columns = {'file_date':'bpm_file'}).drop(columns=['info_from'])
    filtered_radar = subject_detail[subject_detail['info_from'] == 'radar'].dropna(axis=1).rename(columns = {'file_date':'radar_file'}).drop(columns=['info_from'])
    # if 受測者有bpm也有radar的資料 else 受測者只有radar或bpm
    if (filtered_bpm.shape[0] > 1) and (filtered_radar.shape[0] > 1):
        subject_detail = subject_detail.drop(columns=['machine','height','weight','age','gender'])  
        # 以HR為依據合併bpm與radar的資料
        result = pd.merge(filtered_bpm, filtered_radar, on='HR', how='outer')
        # 新增受試者資訊到合併後的表格，讓同一位受試者的資訊整併在同個表格
        # 重新排column order
        # 去除表格重複資料
        result[['height', 'weight', 'age', 'gender']] = subject['height'], subject['weight'], subject['age'], subject['gender']   
        result.drop_duplicates(subset = 'bpm_file',keep='first',inplace = True)
    else:
        result = extract_one_subject(subject_detail)
                
    result = result[result_col_order] 
    result = result.reset_index(drop=True)
    
    return result

bpmradar = pd.DataFrame()    
for subject_info,subject_df in bpmradar_subject:
    subject_merge = process_subject_data(subject_information = subject_info, subject_detail = subject_df )
    bpmradar = pd.concat([bpmradar, subject_merge], ignore_index=True)

# 確認是否與單純merge bpm 和 radar的資料人數相符
len(bpmradar.groupby(['height', 'weight', 'age','gender']).groups.keys())
with pd.ExcelWriter(r"C:\Users\cherl\Chintek_Work\BPM\result\bpm_rader_merge_240423.xlsx", engine='openpyxl', mode='a') as writer:
    #bpm_info.to_excel(writer, sheet_name='bpmok_info', index=False, float_format="%.2f")
    bpmradar.to_excel(writer, sheet_name='0424', index=False, float_format="%.2f")