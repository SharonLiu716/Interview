# -*- coding: utf-8 -*-
"""
Created on Mon Apr  8 17:44:21 2024

@author: cherl
"""



import os, shutil, glob,csv, openpyxl
import pandas as pd
import matplotlib.pyplot as plt
from openpyxl import load_workbook
import pymongo

bpmok = pd.read_excel(r"bpmok_info_240408.xlsx",sheet_name=0)
bpmok.rename(columns={'weight': 'age','age': 'HR','HR': 'weight'}, index={'ONE': 'one'}, inplace=True)
bpm = bpmok[bpmok['height']!=0]

df = bpm.groupby(['height', 'age', 'weight']).size().reset_index(name='count')
#df.to_excel('bpm_subject_info.xlsx')
age_count = df.groupby([ 'age']).size().reset_index(name='count_age')

plt.title('Age ditribution of 180 count of BPM valid data')
plt.xlabel('Age')
plt.ylabel('valid data')
plt.bar(age_count[age_count['age']<100].age,age_count[age_count['age']<100].count_age,color='g',width = 0.5)
plt.show()
plt.close()


# radar = pd.read_excel(r"radarok_info_240410.xlsx",sheet_name=0)
# with pd.ExcelWriter("radarok_info_240410.xlsx") as writer:
#    radarok_info.to_excel(writer, sheet_name="radar_info_origin", index=False)
#    radar.to_excel(writer, sheet_name="radar_info_correct", index=False) #手動篩選後

df = radar.groupby(['height', 'age', 'weight']).size().reset_index(name='count')
df.to_excel('radar_subject_info.xlsx', index=False)  
age_count = df.groupby([ 'age']).size().reset_index(name='count_age')

plt.title('Age ditribution of 147 count of radar valid data')
plt.xlabel('Age')
plt.ylabel('valid data')
plt.bar(age_count.age,age_count.count_age,color='g',width = 0.5)
plt.savefig("Radar有效資料年齡分布圖.jpg")
plt.show()
plt.close()

# 新增sheet至現有的excel
with pd.ExcelWriter('test.xlsx', engine='openpyxl', mode='a') as writer:
    bpm.to_excel(writer, sheet_name='bpmok_info_correct', index=False)

# radar and bpm compare file date
bpm = pd.read_excel(r"test.xlsx",sheet_name='bpmok_info_correct')
radar = pd.read_excel(r"test.xlsx",sheet_name='radarok_info_correct')
outer_join=pd.merge(left=bpm,right=radar,how="outer")
outer_join['machine'].fillna(1221, inplace=True)
outer_join[ ['machine',  'height', 'age', 'HR', 'weight']].astype(int)
bpmradar = outer_join.groupby(['height', 'weight', 'age'])

outer_join.columns
len(bpmradar.groups.keys())
# connect to MongoDB
net_name = 'mongodb+srv://cherliu1998:WW8E8g9tJkPCPhBM@cluster0.9posls2.mongodb.net/'
myclient = pymongo.MongoClient(net_name)
mydb = myclient["CT2402"]
mycol = mydb["bpmradar"]



common_subject = []
num_of_subject,count_ind = 1,1
for  subject,bio_info in bpmradar:   
    
    info = bio_info[['info_from', 'machine', 'file_date',  'HR',  'gender']].to_dict('list')
    
    if bio_info.shape[0] == 1:       
        info = {key: value[0] for key, value in info.items()}
        count_ind = count_ind+1        
    info["_id"] = num_of_subject
    info['subject'] = [int(s) for s in subject]
    info['merge_index'] = int(bio_info.index[0])
    
    #to_db = mycol.insert_one(info)
    
    common_subject.append(info)
    num_of_subject = num_of_subject+1


