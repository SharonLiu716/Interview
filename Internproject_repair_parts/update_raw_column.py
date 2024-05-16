# -*- coding: utf-8 -*-
"""
Created on Fri Aug  6 11:16:07 2021

@author: hiuching.cheng
"""

import sys
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from torch.utils.data import Dataset
from matplotlib.font_manager import FontProperties
from PIL import Image
from scipy.signal import find_peaks
font = FontProperties(fname = r"c:\windows\fonts\simsun.ttc", size = 20)
# solve error
os.environ["KMP_DUPLICATE_LIB_OK"]  =  "TRUE"

'''
Get the file path for data set
'''
import glob

PATH = "D:\\repair-parts"
DATA_PATH = PATH + "\\data"

dataset_list = glob.glob(DATA_PATH+"\\*")
# ['D:\\repair-parts\\data\\L3_RAW_DATA_201908.xlsx', ...]
dataset_namelist = list(map(os.path.basename, dataset_list))
dataset_namelist = [x.strip('L3_RAW_DATA_').strip('.xlsx') for x in dataset_namelist]
# ['L3_RAW_DATA_201908.xlsx', ...]
# print(dataset_list)
# print(dataset_namelist)

'''
# Turn mapping table into dictionary {舊: 新,...}
'''
mapping_table = pd.read_excel(PATH+"\\欄位對照表.xlsx", header = 0, engine='openpyxl').iloc[: , :2]
# [新版欄位名稱, 舊版版欄位名稱]
mapping_table1 = mapping_table.set_index('舊版版欄位名稱')
mapping_dict = mapping_table1["新版欄位名稱"].to_dict()
del mapping_dict["(無)"] # Delete (無)
# print(mapping_dict)

class Dataset_monthly(Dataset):
    def __init__(self, dir_list, file_list):
        self.dir_list = dir_list
        self.file_list = file_list
        
    def __len__(self):
        return len(self.file_list)
        
    def __getitem__(self, idx):
        yearmonth = int(self.file_list[idx])
        # year month < 202010 = 舊版
        if yearmonth < 202010:
            data = pd.DataFrame()
            for i in range(1,4):
                sheetname = "Sheet" + str(i)                
                try: 
                    sheet = self.read_one_sheet(self.dir_list[idx], the_sheet = sheetname)
                    data = data.append(sheet, ignore_index=True)
                except: continue
        else:          
            data = self.read_one_sheet(self.dir_list[idx], version = "new")
        return data
    
    def read_one_sheet(self, the_path, the_sheet = 0, version = "old"):
        if version == "old":  
            data1 = pd.read_excel(the_path, engine='openpyxl', sheet_name=the_sheet)
            data1 = data1.rename(columns=mapping_dict)
        else:
            data1 = pd.read_excel(the_path, engine='openpyxl')
        shorten = ["ORDER_NO","ORDER_TYPE","RC_ID","SERIAL_NO","MODEL_NAME",\
                   "PRODUCT_ID","FAILURE_STAGE","REPAIR_FINISHED_DATE",\
                "REPAIR_STATUS_ITEM","SYMPTOM","SYMPTOM_NO","PART_NO", "REPAIR_GRADE"]
        data1 = data1[shorten]
        return data1

data_raw = Dataset_monthly(dataset_list, dataset_namelist)
for k in range(0, len(data_raw)):
    file_name = PATH + "\\data_V0\\" + dataset_namelist[k]+ ".xlsx"
    print("start")
    curr_data = data_raw[k]
    print(curr_data.head())
    curr_data.to_excel(file_name, engine='openpyxl', encoding='utf_8_sig')
    print("Finished" + file_name)

        

