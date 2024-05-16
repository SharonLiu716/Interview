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
from scipy import stats
from data_loader import Dataset_monthly, get_path, get_data


def add_from_rc(dataset_list, dataset_namelist, output_name):
    
    '''
    搜尋同SERIAL_NO 在資料中的前一個RC ，並加入from rc 與RC 之間repair finish date 的天數差異
    output :  unique RC+serial no 的dataframe, with From rc, day from rc
    '''
    
    # 把2年資料整合成1個dataframe
    columns = ["RC_ID","SERIAL_NO", "REPAIR_FINISHED_DATE"] # 只取得3個欄位
    data_loader_1 = Dataset_monthly(dataset_list, dataset_namelist, data_version = "updated", columns = columns)
    df = pd.DataFrame()
    for k in range(len(data_loader_1)):
        df = df.append(data_loader_1[k], ignore_index=True)
        sys.stdout.write('\rReading : '+dataset_namelist[k])
    
    
    # Get all unique RCID+SERIAL_NO    
    df1 = df.groupby(["RC_ID", "SERIAL_NO"]).agg({'REPAIR_FINISHED_DATE': np.max}).reset_index()
    del df
    
    '''
    - Get all route number , grpup by SERIAL_NO, 再按日期排序
    1 = 該panel 經過的第1個RC 
    2 = 該panel 經過的第2個RC
    
    - 加入from_rc
    - 加入RC 之間repair finish date 的差異
    ...
    '''
    # convert date from object to datetime
    df1['REPAIR_FINISHED_DATE'] = df1['REPAIR_FINISHED_DATE'].astype('datetime64[ns]')
    # print(df1.dtypes)
    
    # 取得該panel 是第幾個
    df1["NUMBER_OF_TRANSFER"] = df1.groupby("SERIAL_NO")["REPAIR_FINISHED_DATE"].rank("dense", ascending=True).astype(int)
    
    # 把serial no + rank 設成index 並sort 再search 會快很多
    df2 = df1.set_index(["SERIAL_NO",'NUMBER_OF_TRANSFER'])
    df2 = df2.sort_index()
    
    add_from_rc = []
    add_date = []
    num_data = df1.shape[0]
    for index, row in df1.iterrows():   
        if row["NUMBER_OF_TRANSFER"] == 1: # 如果只有1, 即2年data 沒找到同一panel / no Transfer/ 是該panel 經過的第1個RC )
            add_from_rc.append(np.NaN)
            add_date.append(np.NaN)        
        else:
            # The following will run forever (index not set)
            # match = df1[(df1['SERIAL_NO'] == row["SERIAL_NO"]) & (df1['RANK'] == row["RANK"] -1)]
            
            match = df2.loc[(row["SERIAL_NO"],row["NUMBER_OF_TRANSFER"] -1)]  # 取得同一Panel rank 是-1 的記錄 
            from_rc = match["RC_ID"].values[0] # 取得同一Panel rank 是-1 的rcid
            from_rc_date = match["REPAIR_FINISHED_DATE"].values[0] # 取得同一Panel 前個RC 完修時間
            row_date = row["REPAIR_FINISHED_DATE"] # 取得同一Panel 目前RC 完修時間
            delta = (row_date - from_rc_date).days # RC 之間repair finish date 的差異
            add_from_rc.append(from_rc)
            add_date.append(delta)
        # del delta, row_date, from_rc_date, from_rc, match
        if index%10000 == 0:
            sys.stdout.write("\rprocess" + str(index/num_data *100))
    df1["FROM_RC"] = add_from_rc
    df1["DAY_FROM_RC"] = add_date
    
    # 存到CSV
    df1.to_csv(output_name, encoding='utf_8_sig')
    return df1

def count_from_rc(OUTPUTPATH, dataframe):
    '''
    Parameters
    ----------
    OUTPUTPATH : output excel 結果的資料夾路徑
    dataframe : 一個結案面版為一行的dataframe (by rcid + serialno + from rc + REPAIR_STATUS_ITEM)

    Returns
    -------
    None.
    計算by RCID 平均完修日期差異與面板總數
    計算by RCID+From rc 平均完修日期差異與面板總數
    計算by RCID+From rc+reapir status 平均完修日期差異與面板總數
    會把結果存到excel "day_by_rc.xlsx"
    '''
    
    day_by_rc = dataframe.groupby("RC_ID").agg({"DAY_FROM_RC": ['mean', 'count']})
    day_by_rc_from_rc = dataframe.groupby(["RC_ID", "FROM_RC"]).agg({"DAY_FROM_RC": ['mean', 'count']})
    day_by_rc_from_rc_allocation = dataframe.groupby(["RC_ID", "FROM_RC", "REPAIR_STATUS_ITEM"]).agg({"DAY_FROM_RC": ['mean', 'count']})
    
    with pd.ExcelWriter(OUTPUTPATH+'\\day_by_rc.xlsx') as writer:  
        day_by_rc.to_excel(writer, sheet_name='day by RC')
        day_by_rc_from_rc.to_excel(writer, sheet_name='by_RC_fromRC')
        day_by_rc_from_rc_allocation.to_excel(writer, sheet_name='by_RC_fromRC_status')
        
def merge_from_rc(df, dataset_list, dataset_namelist, output_folder):
    # 把from rc 與raw data merge
    columns = "all"
    data_loader_monthly = Dataset_monthly(dataset_list, dataset_namelist, data_version = "updated", columns = columns)
    
    df = df.drop(columns=['REPAIR_FINISHED_DATE'])
    
    # folder_name = PATH + "\\data_with_from_rc"
    for k in range(len(data_loader_monthly)):
        df_monthly = data_loader_monthly[k]
        sys.stdout.write('\rReading : '+dataset_namelist[k])        
        unique_key = ["RC_ID", "SERIAL_NO"]
        df_monthly_merged = pd.merge(df_monthly, df, how = "left", on= unique_key)
        df_monthly_merged.to_excel(output_folder+'\\{}.xlsx'.format(dataset_namelist[k]), engine='openpyxl', encoding='utf_8_sig')



def unique_repair_item(df):
    unique_repair_status = pd.DataFrame(df["REPAIR_STATUS_ITEM"].unique())
    unique_repair_status.to_excel("D:\\repair-parts\\status_mapping.xlsx")
    
def count_NA(df):
    '''
    count missing from rc each month
    return: total panel each month (Allocation), Allocation and from rc = none    
    '''
    df_counting = df.loc[df["ORDER_TYPE"] == 'Allocation']
    df2 = df_counting.groupby(["RC_ID", "SERIAL_NO"]).agg({'REPAIR_FINISHED_DATE': np.max, "DAY_FROM_RC": np.max}).reset_index()
    df2['month_year'] = df2['REPAIR_FINISHED_DATE'].dt.to_period('M')
    df_count_panel = df2.groupby(["RC_ID", 'month_year']).count().sort_index()
    df_count_na = df2.set_index(['RC_ID', 'month_year']).isna().sum(level=[0, 1]).sort_index()
    
    merged = pd.merge(df_count_panel, df_count_na, left_index=True, right_index=True)
    merged = merged [["SERIAL_NO_x","DAY_FROM_RC_y"]]
    merged.columns = ['TOTAL_allocation', 'NA_count']
    merged.to_excel("D:\\repair-parts\\countna.xlsx")
    '''
    with pd.ExcelWriter("D:\\repair-parts\\countna.xlsx") as writeNo documr:  
    df_count_panel.to_excel(writer, sheet_name='count_all')
    df_count_na.to_excel(writer, sheet_name='count_na')  '''   

'''
# 進行ad hoc 統計
# 取得2年資料
PATH = "D:\\repair-parts"
FOLDER = 'data_with_from_rc\\merged_from_rc'
columns = ["RC_ID","SERIAL_NO", "ORDER_TYPE", "REPAIR_FINISHED_DATE", "REPAIR_STATUS_ITEM",\
           "SYMPTOM", "FROM_RC", "DAY_FROM_RC", "PART_NO", "PRODUCT_ID"] 
df = get_data(PATH, FOLDER, columns)
df['REPAIR_FINISHED_DATE'] = df['REPAIR_FINISHED_DATE'].astype('datetime64[ns]')

# 計算from rc NA 數量
count_NA(df)

# 計算rc 平均相隔日期
df_counting = df.loc[df["ORDER_TYPE"] == 'Allocation']
df_counting = df_counting[["RC_ID", "SERIAL_NO", "FROM_RC", "REPAIR_FINISHED_DATE", 'DAY_FROM_RC', "REPAIR_STATUS_ITEM"]]
df_counting = df_counting.groupby(["RC_ID", "SERIAL_NO", "FROM_RC", "REPAIR_STATUS_ITEM"]).agg({'REPAIR_FINISHED_DATE': np.max, 'DAY_FROM_RC': np.max}).reset_index()
count_from_rc(PATH + "\\data_with_from_rc\\count", df_counting)

# 計算同一面板用同一物料
df_count_qty = df.groupby(["RC_ID", "SERIAL_NO", "REPAIR_STATUS_ITEM", "SYMPTOM", "PART_NO"], dropna=False).agg({'REPAIR_FINISHED_DATE': np.max, 'DAY_FROM_RC': np.max})
df_count_qty1 = df.groupby(["RC_ID", "SERIAL_NO", "REPAIR_STATUS_ITEM", "SYMPTOM"], dropna=False)['PART_NO'].value_counts()
df_count_qty1 = pd.DataFrame(df_count_qty1)
df_count_qty1 = df_count_qty1.rename(columns=({'PART_NO': 'PART_QTY'}))
df_qty = pd.merge(df_count_qty, df_count_qty1, how = 'left', left_index = True, right_index = True)
df_count_qty2 = df_count_qty1.reset_index()
df_count_qty2.to_excel(PATH+'\\unique_qty.xlsx', engine='openpyxl', encoding='utf_8_sig')
'''

def merge_qty(PATH, FOLDER, output_folder, df = None, save_result = False):
    # 取得data loader
    dataset_list, dataset_namelist = get_path(PATH, FOLDER)
    columns = "all"
    data_loader_monthly = Dataset_monthly(dataset_list, dataset_namelist, data_version = "updated", columns = columns)
    
    if df is None:
        # 如果之前未取得dataframe
        df_columns = ["RC_ID","SERIAL_NO", "ORDER_TYPE", "REPAIR_FINISHED_DATE", "REPAIR_STATUS_ITEM",\
           "SYMPTOM", "FROM_RC", "DAY_FROM_RC", "PART_NO", "PRODUCT_ID"] 
        df = get_data(PATH, FOLDER, df_columns)        
        df['REPAIR_FINISHED_DATE'] = df['REPAIR_FINISHED_DATE'].astype('datetime64[ns]')
        
    '''
    計算同一面板用同一物料
    '''
    # 加入month_year 欄位
    df['month_year'] = df['REPAIR_FINISHED_DATE'].astype('datetime64[ns]').dt.to_period('M')
    
    # 取得RCID+SERIAL+STATUS+SYMPTOM+PART 的組合
    df_count_qty = df.groupby(["RC_ID", "SERIAL_NO", "REPAIR_STATUS_ITEM", "SYMPTOM", "PART_NO", 'month_year'], dropna=False).agg({'REPAIR_FINISHED_DATE': np.max, 'DAY_FROM_RC': np.max})
    # 取得RCID+SERIAL+STATUS+SYMPTOM 的組合 按part 進行加總 (有part 才會有加總)
    df_count_qty1 = df.groupby(["RC_ID", "SERIAL_NO", "REPAIR_STATUS_ITEM", "SYMPTOM", 'month_year'], dropna=False)['PART_NO'].value_counts()
    df_count_qty1 = pd.DataFrame(df_count_qty1)
    df_count_qty1 = df_count_qty1.rename(columns=({'PART_NO': 'PART_QTY'}))
    
    if save_result != False:
        # 把結果保存
        df_count_qty2 = df_count_qty1.reset_index()
        df_count_qty2.to_excel(PATH+'\\unique_qty.xlsx', engine='openpyxl', encoding='utf_8_sig')
    
    # 把加總數字merge 到RCID+SERIAL+STATUS+SYMPTOM+PART 的組合 data frame
    df_qty = pd.merge(df_count_qty, df_count_qty1, how = 'left', left_index = True, right_index = True)
    
    # Group by month_year
    df_qty_bymonth = df_qty.reset_index().groupby(['month_year'])
    
    # 把檔案名稱list 變成period 格式
    month_list = pd.to_datetime(dataset_namelist, yearfirst = True, format='%Y%m', errors='ignore').to_period('M')       
    

    for k in range(len(data_loader_monthly)):     
               
        df_curr_month =  df_qty_bymonth.get_group(month_list[k]) # 組合過的data by month
        
        output_path = PATH + "\\" + output_folder
        df_monthly = data_loader_monthly[k] # original data
        df_monthly['month_year'] = df_monthly['REPAIR_FINISHED_DATE'].astype('datetime64[ns]').dt.to_period('M')
        
        # join 兩個dataframe
        key = ["RC_ID", "SERIAL_NO", "REPAIR_STATUS_ITEM", "SYMPTOM", "PART_NO", "month_year"]        
        merged = pd.merge(df_curr_month, df_monthly, how = 'left', on = key, suffixes = ('', '_y'))
        merged = merged.drop_duplicates(subset= key, keep='last')        
        
        # 取得需要的column
        columns = ['RC_ID', 'SERIAL_NO', 'REPAIR_STATUS_ITEM', 'SYMPTOM', 'PART_NO', 'PART_QTY',\
               'REPAIR_FINISHED_DATE', 'month_year', 'FROM_RC', 'DAY_FROM_RC', \
               'ORDER_NO', 'ORDER_TYPE', 'MODEL_NAME', 'PRODUCT_ID', 'FAILURE_STAGE',\
               'SYMPTOM_NO', 'REPAIR_GRADE', 'NUMBER_OF_TRANSFER']
        merged = merged[columns]                
        merged.to_excel(output_path+'\\{}.xlsx'.format(dataset_namelist[k]), engine='openpyxl', encoding='utf_8_sig')
        sys.stdout.write('\rDone merging : {}.xlsx'.format(dataset_namelist[k]))
        
        
    def count_same_serial(PATH, FOLDER, df_columns = None):
                
        if df_columns is None:
            # 如果沒有指定Column
            df_columns = ["RC_ID","SERIAL_NO", "ORDER_TYPE", "REPAIR_FINISHED_DATE", "REPAIR_STATUS_ITEM",\
               "SYMPTOM", "FROM_RC", "DAY_FROM_RC", "PART_NO", "PRODUCT_ID", "ORDER_NO"] 
                
            df = get_data(PATH, FOLDER, df_columns)        
            df['REPAIR_FINISHED_DATE'] = df['REPAIR_FINISHED_DATE'].astype('datetime64[ns]')
               
        '''
        計算同一面板修幾次
        '''
        # 加入month_year 欄位
        df['month_year'] = df['REPAIR_FINISHED_DATE'].astype('datetime64[ns]').dt.to_period('M')
        
        # 取得RCID+SERIAL+ORDER NO 的組合，計算有多少不同的repair_finished_date
        df_count_qty = df.groupby(["RC_ID", "SERIAL_NO", "ORDER_NO", "PRODUCT_ID"], dropna=False)['REPAIR_FINISHED_DATE'].nunique()
        df_count_qty = df_count_qty.reset_index()
        df_count_qty = pd.DataFrame(df_count_qty).rename(columns=({'REPAIR_FINISHED_DATE': 'NUMBER_OF_REPAIR'}))
        
        # 只取同一面板修多於一次的
        df_count_qty1 = df_count_qty[df_count_qty["NUMBER_OF_REPAIR"] > 1]
        
        # 只取要預測的RC
        mainRC=['ACCU','UPLUS','EASCON','IGS','SMM','TGO','ZZHC']
        mainRC_DF=df_count_qty1[df_count_qty1['RC_ID'].isin(mainRC)]
        
        if save_result != False:
            # 把結果保存
            mainRC_DF.to_excel(PATH+'\\一片面版修多於一次.xlsx', engine='openpyxl', encoding='utf_8_sig')

# =========================================Main program====================

def create_merge_from_rc():
    PATH = "D:\\repair-parts"
    FOLDER = 'data_V0'
    output_name = PATH + "\\data_with_from_rc\\201908to202107.csv"
    dataset_list, dataset_namelist = get_path(PATH, FOLDER)
    df1 = add_from_rc(dataset_list, dataset_namelist, output_name)
    merge_from_rc(df1, dataset_list, dataset_namelist, PATH + "\\data_with_from_rc")
    
def create_qty():
    PATH = "D:\\repair-parts"
    FOLDER = 'data_with_from_rc\\merged_from_rc'
    output_folder = 'data_with_qty\\data'
    merge_qty(PATH, FOLDER, output_folder, df = None, save_result = False)

PATH = "D:\\repair-parts"
FOLDER = 'map dataV1'
df_columns = "all"
                
df = get_data(PATH, FOLDER, df_columns)  
df_check = df[df["SERIAL_NO"] == "0DC3BLL08T8003J"]

if __name__ == '__main__':
    create_merge_from_rc()
    create_qty()