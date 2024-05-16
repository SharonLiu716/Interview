# -*- coding: utf-8 -*-
"""
Created on Fri Mar 29 11:07:18 2024

@author: cherl


0329 匯出record與所有personal_information.csv的整合結果
"""


import os, shutil, glob,csv
import pandas as pd

"""
Tasks
1.Bpm資料分ok and fail，Radar資料也分ok and fail移到  "C:\Users\cherl\Documents\Chintek\BPM\bpm"
2.擷取bpm與雷達的受試者資訊 """


"""       raw data重新分類

分類後路徑："C:\Users\cherl\Chintek_Work\BPM"
bpm: machine1_OK、machine2_OK
    - 內含以日期為名的資料夾，每個資料夾下cheek為受測者眼下區域照片，日期.csv檔案為計算出的血壓資料，personal_information.csv為受測者資訊
radar: NO、OK
    - 日期.csv為雷達偵測資料

"""
raw_data_main_path = r"C:\Users\cherl\Documents\Work\BPM_raw_data"
# 根據0329彙整結果重新整理符合量測標準之資料檔名
# "C:\Users\cherl\Documents\CT2402_BPM_data\machine1\20240223\bpm_raw_data\2024-02-02-13-54-12\personal_infomation.csv"
bpm_record = pd.read_excel(r"record.xlsx",sheet_name=0)
# "C:\Users\cherl\Documents\CT2402_BPM_data\machine1\20240223\radar_raw_data\2024-02-02-13-55-33.csv"
radar_record = pd.read_excel(r"record.xlsx",sheet_name=1)


# 找出所有raw data csv檔存在的路徑
radar_files,bpm_files = [],[]

for radar_file in glob.glob( os.path.join(raw_data_main_path,'**/radar_raw_data/*.csv'), recursive=True):
    radar_files.append(radar_file)  

for bpm_file in glob.glob( os.path.join(raw_data_main_path,'**/bpm_raw_data/**/personal_infomation.csv'), recursive=True):    
    bpm_files.append(bpm_file) 

# 血壓raw date為ok、no、血壓no的資料存放路徑
bpm_files_ok, bpm_files_no, ng_bpm = [],[],[]
for bpm_file in bpm_files:
    if len(bpm_file.split('\\')) == 11:
        bpm_files_ok.append(bpm_file)
    elif bpm_file.split('\\')[-3] == 'no':
        bpm_files_no.append(bpm_file)
    else:
        ng_bpm.append(bpm_file)


# 根據record歸類資料到相對應的資料夾

## radar file 歸類
## record為ok複製檔案至radar/ok，record為no複製檔案至radar/no
ok_radar = radar_record.groupby('status').get_group("OK")['file_date']
no_radar = radar_record.groupby('status').get_group("NO")['file_date']
for radar_file in glob.glob( os.path.join(raw_data_main_path,'**/radar_raw_data/*.csv'), recursive=True):
    file_name = radar_file.split('\\')[-1].strip('.csv')
    if ok_radar.str.contains(file_name).any():
        shutil.copy(radar_file,'./radar/OK')
        print(file_name,'OK')
    if no_radar.str.contains(file_name).any():
        shutil.copy(radar_file,'./radar/NO')
        print(file_name,'NO')



## bpm file 歸類
## bpm為ok且為machine1複製檔案至bpm/machine1_ok， bpm為ok且為machine2複製檔案至bpm/machine2_ok
ok_bpm = bpm_record.groupby('status').get_group("OK")[['machine','file_date']]
no_bpm = bpm_record.groupby('status').get_group("NO")[['machine','file_date']]

for bpm_file in bpm_files_ok:
    
    file_name = bpm_file.split('\\')[-2]
    
    if ok_bpm['file_date'].str.contains(file_name).any():
    
        ix = ok_bpm.isin([file_name]).any(axis=1).idxmax()
        
        if ok_bpm.loc[ix, 'machine'] == 1:
            try:
                shutil.copytree(bpm_file.strip('\\personal_infomation.csv'),'./bpm/machine1_OK/'+file_name)
                print(file_name ,'_machine1_OK')
            except FileExistsError :
                print(file_name ,'_machine1_OK無需複製。')     
        
        elif ok_bpm.loc[ix, 'machine'] == 2: 
            try:
                shutil.copytree(bpm_file.strip('\\personal_infomation.csv'),'./bpm/machine2_OK/'+file_name)
                print(file_name ,'_machine2_OK')
            except FileExistsError :
                 print(file_name ,'_machine2_OK無需複製。')
"""   BPM受測者資料擷取

整理後machin1_ok與machine2_ok下的csv路徑 共728份
path於"C:\Users\cherl\Documents\Work\BPM\bpm"下machine1_OK 與 machine2_OK的資料夾下
"""        


def bpm_info(csv_file_path):      
    
    if csv_file_path.split('\\')[-3] == 'machine1_OK':
        info = {'info_from': 'bpm','machine': 1, 'file_date':csv_file_path.split('\\')[-2]}        
        file = open(csv_file_path)
        data = list(csv.reader(file))
        # info = {row[0]: row[1] for row in zip(*data)}
        info.update({row[0]: row[1] for row in zip(*data)})
        
    elif  csv_file_path.split('\\')[-3] == 'machine2_OK':
        info = {'info_from': 'bpm','machine': 2, 'file_date':csv_file_path.split('\\')[-2]}        
        file = open(csv_file_path)
        data = list(csv.reader(file))
        # info = {row[0]: row[1] for row in zip(*data)}
        info.update({row[0]: row[1] for row in zip(*data)})   

    return info

# 存取bpm下的受測者資訊 from personal_infomation.csv
bpm_ok_files = []
main_bpm_path = r"C:\Users\cherl\Documents\Work\BPM\bpm"
for bpm_file in glob.glob( os.path.join(main_bpm_path,'**/personal_infomation.csv'), recursive=True):    
    bpm_ok_files.append(bpm_file) 


bpmok_info = pd.DataFrame()
for file in bpm_ok_files:
    bpmok_info = pd.concat([ bpmok_info, pd.DataFrame([bpm_info(file)])], ignore_index=True)

# personal_infomation.csv存到的受測者資料存至bpmok_info_240408.xlsx
bpmok_info.to_excel('bpmok_info_240408.xlsx',index=False)
# 結合彙整結果record的受測者資料彙整至bpm_merge_240408.xlsx
outer_join=pd.merge(left=bpm_record,right=bpmok_info,how="outer")
outer_join.to_excel('bpm_merge_240408.xlsx')


# 打开文件寫入所有歸類後的bpm資料存放路徑
fo = open("bpmok_path.txt", "w")
fo.writelines([line+'\n' for line in bpm_ok_files])
fo.close()


"""   BPM受測者資料擷取

整理後OK與NO下的csv最後一行即受測者資訊，but不是每份radar都有受測者資訊

path於"C:\Users\cherl\Documents\Work\BPM\radar"下 OK 與 NO的資料夾內
"""        


path = r"C:\Users\cherl\Chintek_Work\BPM\radar\OK"
radarok_no_info = []
radar_col = ['info_from','file_date','height', 'weight', 'age', 'gender','HR']
info = {key: None for key in radar_col}
radarok_info = pd.DataFrame()

for csv_file_path in glob.glob( os.path.join(path,'**.csv'), recursive=True):    
    file = open(csv_file_path, newline='')
    data = list(csv.reader(file))[-1:]
    
    csv_file_name = csv_file_path.split('\\')[-1].strip('.csv')
    if 'height' in data[0][0]:
        height = data[0][0].strip('height=')
        weight = data[0][1].strip('weight=')
        age = data[0][2].strip('age=')
        gender = data[0][3].strip('gender=')
        HR = data[0][4].strip('HR=')
        info.update({'info_from': 'radar', 'file_date':csv_file_name, 'height': int(height), 'weight': int(weight), 'age' : int(age),'gender' : gender, 'HR': int(HR)})
        radarok_info = pd.concat([ radarok_info, pd.DataFrame([info])], ignore_index=True)
    else:        
        radarok_no_info.append(csv_file_path)
    file.close() 

radarok_info = radarok_info[radarok_info['HR']!= 0]   
radarok_info = radarok_info[radarok_info['HR']!= 1]   
radarok_info['gender']  = radarok_info['gender'] .replace(to_replace = ['Femal', 'Femaleemal', 'Mal', 'Malel'], value = ['Female', 'Female','Male','Male'])
radarok_info.to_excel('radarok_info_240410.xlsx',index=False)

fo = open("radarok_noinfo_path.txt", "w")
fo.writelines([line+'\n' for line in radarok_no_info])
fo.close()


# 以下未用

def radar_info(csv_file_path):
    print(csv_file_path)
    #csv_file_path = radar_file
    file = open(csv_file_path, newline='')
    data = list(csv.reader(file))[-1:]
    csv_file_name = csv_file_path.split('\\')[-1].strip('.csv')
    height = data[0][0].strip('height=')
    weight = data[0][1].strip('weight=')
    age = data[0][2].strip('age=')
    gender = data[0][3].strip('gender=')
    HR = data[0][4].strip('HR=')
    file.close() 
        
    info  = {'info_from': 'radar', 'file_date':csv_file_name, 'height': height, 'weight': weight, 'age' : age,'gender' : gender, 'HR':HR}
    return info 
   


#
# 整理後machin1_ok與machine2_ok下的radar csv路徑



radarok_info = pd.DataFrame()
for file in radar_ok_files:
    radarok_info = pd.concat([ radarok_info, pd.DataFrame([radar_info(file)])], ignore_index=True)

radarok_info.to_excel('radarok_info_240408.xlsx',index=False)
outer_join=pd.merge(left=radar_record,right=radarok_info,how="outer")
outer_join.to_excel('radar_merge_240408.xlsx')


# 打开文件
fo = open("radarok_path.txt", "w")
fo.writelines([line+'\n' for line in bpm_ok_files])
fo.close()

"""待修 不ok的還需要校正數量

### bpm file 歸類
ok_bpm = bpm_record.groupby('status').get_group("OK")[['machine','file_date']]
no_bpm = bpm_record.groupby('status').get_group("NO")[['machine','file_date']]
for bpm_file in glob.glob( os.path.join(raw_data_main_path,'**/bpm_raw_data/**/personal_infomation.csv'), recursive=True):
    
    file_name = bpm_file.split('\\')[-2]
    
    if ok_bpm['file_date'].str.contains(file_name).any():
    
        ix = ok_bpm.isin([file_name]).any(axis=1).idxmax()
        
        if ok_bpm.loc[ix, 'machine'] == 1:
            try:
                shutil.copytree(bpm_file.strip('\\personal_infomation.csv'),'./bpm/machine1_OK/'+file_name)
                print(file_name ,'_machine1_OK')
            except FileExistsError:
                print(file_name ,'_machine1_OK無需複製。')     
        
        elif ok_bpm.loc[ix, 'machine'] == 2: 
            try:
                shutil.copytree(bpm_file.strip('\\personal_infomation.csv'),'./bpm/machine2_OK/'+file_name)
                print(file_name ,'_machine2_OK')
             except FileExistsError:
                 print(file_name ,'_machine2_OK無需複製。')
            
            
    if no_bpm['file_date'].str.contains(file_name).any():
        
        ix = no_bpm.isin([file_name]).any(axis=1).idxmax()
        
        if no_bpm.loc[ix, 'machine'] == 1:
            try:
                shutil.copytree(bpm_file.strip('\\personal_infomation.csv'),'./bpm/machine1_NO/'+file_name)
                print(file_name ,'_machine1_NO')
            except FileExistsError:
                print(file_name ,'_machine1_NO無需複製。')
                
        elif no_bpm.loc[ix, 'machine'] == 2:     
            try:
                shutil.copytree(bpm_file.strip('\\personal_infomation.csv'),'./bpm/machine2_NO/'+file_name)
                print(file_name ,'_machine2_NO')
                    
            except FileExistsError:
                print(file_name ,'_machine2_NO無需複製。')
        

"""




# 讀取radar資料最後四行，返回受試者資訊。
def radar_info(radar_data,csv_file_path):
    # csv_file_path from radar_files[i] is 'CT2402_BPM_data\\machine1\\20240223\\radar_raw_data\\2024-02-02-14-05-38.csv'
    
    if csv_file_path.split('\\')[-2] == 'radar_raw_data':
        
        machine_number = csv_file_path.split('\\')[1].strip('machine')
        csv_file_name = csv_file_path.split('\\')[-1]
        rri = [ item.strip(' ') for item in radar_data[0][1:] ]
        bri = [ item.strip(' ') for item in radar_data[1][1:] ]
        hr = radar_data[2][0].strip('hr=')
        br = radar_data[2][1].strip(' br=')
        height = radar_data[3][0].strip('height=')
        weight = radar_data[3][1].strip('weight=')
        age = radar_data[3][2].strip('age=')
        gender = radar_data[3][3].strip('gender=')
        HR = radar_data[3][4].strip('HR=')
        
        
        Person_Info  = {'info_from': 'radar','machine': machine_number, 'file_date':csv_file_name.strip('.csv'), 
                        'height': height, 'weight': weight, 'age' : age,'gender' : gender, 'HR':HR, 
                        'hr': hr, 'br': br, 'csv_file_name': csv_file_name, 
                        'RRI': rri, 'BRI': bri}
        return Person_Info 
    else:
        print('it is',csv_file_path)



def get_info(csv_file_path):
    
    file_path = os.path.join(os.getcwd(),csv_file_path)   
    
    if csv_file_path.split('\\')[3] == 'bpm_raw_data':
        info = {'info_from': 'bpm','machine': int(csv_file_path.split('\\')[1].strip('machine')), 'file_date':csv_file_path.split('\\')[-2]}        
        file = open(file_path)
        data = list(csv.reader(file))
        #info = {row[0]: row[1] for row in zip(*data)}
        info.update({row[0]: row[1] for row in zip(*data)})
    
    else:
        
        file = open(file_path, newline='')
        data = list(csv.reader(file))[-4:]
        info = radar_info(data, csv_file_path)         
        
    file.close()
    
    return info


bpm_info = pd.DataFrame()
for file in bpm_files:
    bpm_info = pd.concat([ bpm_info, pd.DataFrame([get_info(file)])], ignore_index=True)

outer_join=pd.merge(left=bpm_record,right=bpm_info,how="outer")
bpm_info = outer_join[outer_join["SBP"].notna()]
bpm_info=bpm_info[bpm_info["SBP"] != '000' ]
# 手動更改file_date有no的資料，考慮用程式改
bpm_info.to_excel('bpm_info_240329.xlsx')
