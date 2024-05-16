# -*- coding: utf-8 -*-
"""
Created on Thu Apr 18 11:17:15 2024

@author: cherl
"""

import os, shutil, glob,csv
import pandas as pd
from statistics import mean 
from scipy.stats import zscore
import numpy as np


radar_info = pd.read_excel(r"result/subjectok.xlsx",sheet_name='radarok_info_correct')

radarok_path = r"C:\Users\cherl\Chintek_Work\BPM\radar\OK"
radar_files, file_date, num_data =[],[],[]
for file in glob.glob(os.path.join(radarok_path,'**.csv'), recursive=True):
    radar_files.append(file) 
    
    

"""擷取關鍵字所在的row的資料"""

def search_keyword_in_csv(csv_file, keyword):
    # 儲存符合條件的行
    matching_rows = []

    # 開啟 CSV 檔案並進行讀取
    with open(csv_file, newline='') as csvfile:
        reader = csv.reader(csvfile)
        
        # 逐行檢查
        for row in reader:
            # 檢查是否有關鍵字在該行中
            if any(keyword in cell for cell in row):
                matching_rows.append(row)
    
    return matching_rows

# 輸入檔案路徑和關鍵字，執行搜索並印出符合條件的行
def process_subject_data(csv_file):
    
    csv_file_name = csv_file.split('\\')[-1].strip('.csv')
    
    rri = search_keyword_in_csv(csv_file , 'rri')
    rri = pd.DataFrame(rri, columns=['name', 'rri1', 'rri2', 'rri3', 'rri4']).drop(columns='name').astype(float)
    
    
    bri = search_keyword_in_csv(csv_file ,  'bri')
    bri = pd.DataFrame(bri, columns=['name', 'bri1', 'bri2', 'bri3', 'bri4']).drop(columns='name').astype(float) 
    
    hrbr = search_keyword_in_csv(csv_file , 'hr')
    hr = pd.DataFrame([ int(h[0].strip('hr='))  for h in hrbr], columns=['hr'])
    br = pd.DataFrame([ int(b[1].strip(' br='))  for b in hrbr], columns=['br'])

    df = pd.concat([rri,bri,hr,br], axis=1)
    df['file_date'] = [csv_file_name]*df.shape[0]
    
    return df

radar_data = pd.DataFrame()
for file in radar_files:
    subject = process_subject_data(file)
    radar_data = pd.concat([ radar_data, subject ], ignore_index=True)

radar_full = pd.merge(left=radar_info,right=radar_data,how="outer")
radar_full = radar_full.dropna(subset =["height"])
with pd.ExcelWriter("./result/radar_data_240418.xlsx") as writer:
    radar_data.to_excel(writer, sheet_name="radar_subject_data", index=False)
    radar_full.to_excel(writer, sheet_name="radar_full_data", index=False) 

# date = radar_full.groupby(['file_date']).size().reset_index(name='count')['file_date']
# print("\ndate not present in record :") 
# res = radar_info['file_date'] [~radar_info['file_date'].isin(date)] 
# res = date[~date.isin(radar_info['file_date'])] 
# print(res) 
# radar_data.to_excel('./result/radar_data_240418.xlsx')

def remove_outliers(test_list):
    z_scores = zscore(test_list)
    outliers_indices = np.where(np.abs(z_scores) > 2)[0] #大於三倍標準差都離群值
    filtered_data = [x for i, x in enumerate(test_list) if i not in outliers_indices]

    return filtered_data


def remove_invalid_hrbr(test_list):
    
    test_list = [x for x in test_list.tolist() if x != -1]  
    test_list =  remove_outliers(test_list)      
    if len(test_list)>1 :
        list_mean = round(mean(test_list), 2)        
    elif len(test_list)==0:
        list_mean = -1
    else:
        list_mean = int(test_list[0])
    
    return list_mean


radar_info = pd.read_excel(r"result/radar_data_240418.xlsx",sheet_name='radar_info') 
#radar_info_new = pd.DataFrame(columns=['info_from','file_date','height','weight','age','gender','hr_mean','br_mean'])

radar_group = radar_info.groupby(['info_from','file_date','height','weight','age','gender','HR'])#.get_group("NO")[['machine','file_date']]
len(radar_group.groups.keys())

radar_info_new = pd.DataFrame()
radar_subjects = radar_group.groups.keys()

for rs in radar_subjects:
    
    group = radar_group.get_group(rs)
    hr_mean =  remove_invalid_hrbr(group['hr'])
    br_mean =  remove_invalid_hrbr(group['br'])
    
    radar_suject = {'info_from': rs[0], 'file_date':rs[1],'height':rs[2],'weight':rs[3],'age':rs[4],'gender':rs[5], 'HR':rs[6],
                    'hr_mean':hr_mean, 'br_mean':br_mean, 'hr_raw':group['hr'].tolist(),'br_raw':group['br'].tolist()} 
    
    radar_info_new = pd.concat([radar_info_new, pd.DataFrame([radar_suject])], ignore_index=True)
    
with pd.ExcelWriter("./result/radar_data_240423.xlsx") as writer:
    radar_info_new.to_excel(writer, sheet_name="radar_full_data", index=False)
    radar_info.to_excel(writer, sheet_name="radar_info", index=False)