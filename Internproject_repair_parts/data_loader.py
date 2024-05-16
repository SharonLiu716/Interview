# -*- coding: utf-8 -*-
"""
Created on Sat Aug  7 09:57:03 2021

@author: Stephan
"""
import pandas as pd
import numpy as np
import os
import glob
import sys
from torch.utils.data import Dataset

#======================================================
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

def get_path(PATH, FOLDER):
    '''
    INPUT
    =======
    PATH: 檔案路徑
    FOLDER: raw data 資料夾名稱
    
    OUTPUT
    ======
    dataset_list, dataset_namelist: data 路徑清單,data 檔案名稱清單
    '''
    # PATH = "D:\\repair-parts"
    DATA_PATH = PATH + "\\"+ FOLDER # subfolder with shorten version of columns
    
    dataset_list = glob.glob(DATA_PATH+"\\*")
    # ['D:\\repair-parts\\data_V0\\201908.xlsx', ...]
    dataset_namelist = list(map(os.path.basename, dataset_list))
    dataset_namelist = [x.strip('.xlsx') for x in dataset_namelist]
    return dataset_list, dataset_namelist

def get_data(PATH, FOLDER, columns):
    '''
    INPUT
    =======
    PATH: 檔案路徑
    FOLDER: data 資料夾名稱
    Columns: "all", 或者希望取得的column list
    
    OUTPUT
    ======
    dataframe, 2年資料
    '''
    dataset_list, dataset_namelist = get_path(PATH, FOLDER)
    data_loader_1 = Dataset_monthly(dataset_list, dataset_namelist, data_version = "updated", columns = columns)
    df = pd.DataFrame()
    for k in range(len(data_loader_1)):
        df = df.append(data_loader_1[k], ignore_index=True)
        sys.stdout.write('\rReading : '+dataset_namelist[k])
    return df

def mapping_table(PATH):
    # 
    '''
    Turn mapping table into dictionary {舊: 新,...}
    INPUT
    =======
    PATH: 檔案路徑
    FOLDER: data 資料夾名稱
    Columns: "all", 或者希望取得的column list
    
    OUTPUT
    ======
    dataframe, 2年資料
    '''
    mapping_table = pd.read_excel(PATH+"\\欄位對照表.xlsx", header = 0, engine='openpyxl').iloc[: , :2]
    # [新版欄位名稱, 舊版版欄位名稱]
    mapping_table1 = mapping_table.set_index('舊版版欄位名稱')
    mapping_dict = mapping_table1["新版欄位名稱"].to_dict()
    del mapping_dict["(無)"] # Delete 
    return mapping_dict

def update_raw_column(PATH, FOLDER, OUTPUT_FOLDER):
    '''
    整理欄位
    INPUT
    =======
    PATH: 檔案路徑
    FOLDER: data 資料夾名稱
        
    OUTPUT
    ======
    
    '''
    
    dataset_list, dataset_namelist = get_path(PATH, FOLDER)
    mapping_dict = mapping_table(PATH)
    columns = ["ORDER_NO","ORDER_TYPE","RC_ID","SERIAL_NO","MODEL_NAME",\
                   "PRODUCT_ID","FAILURE_STAGE","REPAIR_FINISHED_DATE",\
                "REPAIR_STATUS_ITEM","SYMPTOM","SYMPTOM_NO","PART_NO", "REPAIR_GRADE"]
    data_raw = Dataset_monthly(dataset_list, dataset_namelist, mapping_dict = mapping_dict, columns = columns)
    for k in range(0, len(data_raw)):
        file_name = PATH + "\\" + OUTPUT_FOLDER + "\\" + dataset_namelist[k]+ ".xlsx"
        print("start")
        curr_data = data_raw[k]
        # print(curr_data.head())
        curr_data.to_excel(file_name, engine='openpyxl', encoding='utf_8_sig')
        print("Finished" + file_name)       
    
def main():
    PATH = "D:\\repair-parts"
    FOLDER = "data"
    update_raw_column(PATH, FOLDER, "data_V0")

if __name__ == '__main__':
    main()