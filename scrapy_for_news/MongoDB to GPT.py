# -*- coding: utf-8 -*-
"""
Created on Tue Dec  5 14:52:32 2023

@author: User
"""

import json,re,requests,pymongo,time
from lxml import html
from datetime import date,datetime
from openai import AzureOpenAI    
    


class AskGPT(object):

    def __init__(self, client_name,db_name,collection_name):
        self.client_name,self.db_name,self.collection_name = client_name,db_name,collection_name
        self.conn = pymongo.MongoClient(client_name)
        db = self.conn[db_name]
        self.collection = db[collection_name]
        #self.conn = pymongo.MongoClient('mongodb://localhost:27017')
        #db = self.conn['News_ScrapyXpath']
        #self.collection = db['EY_news_test_1127']
# =============================================================================
#         self.gpt_client = AzureOpenAI(azure_endpoint="https://eaaasgpt4turbo.openai.azure.com/",
#                                       api_key="1df05334a7054ad9b4f4f24a4ff074e3",api_version="2023-07-01-preview",)
#         self.model = "EAUSERGPT4Turbo"
# =============================================================================
        self.gpt_client = AzureOpenAI(azure_endpoint="https://eaaasgpt4turbo.openai.azure.com/",
                             api_key="api_key",api_version="2023-05-15",)

        self.model = "EAUSERGPT432K"
        self.temperature = 0.5
        self.max_tokens = 4000
        self.frequency_penalty = 0.5
        self.sleep_time = 5
        
        self.question = "幫我整理以下文章的內容：標題，標題請從文章內容整理出重點人物、事件、地點，並不得多於20字；發佈機構；日期及時間點，日期如果是中華民國紀元的話請改為西元紀年，格式為年-月-日 hh:mm:ss；文章的重點並且分兩種方式呈現，一種以條列式的方式整合文章內容列出三點重點，內容必須包含重要人物、事件、地點、重要的數字，內容若有提到「交通、港灣、觀光」則優先度為最高，並且扼要在30字內，一種用敘述的方式，內容必須包含重要人物、事件、地點、重要的數字，內容若有提到「交通、港灣、觀光」則優先度為最高，字數在200字以內。回答時輸出此格式「{\"標題\": 從文章整理出的標題, \"發佈機構\": 從文章整理出的發佈機構, \"日期\": 從文章整理出的發佈日期, \"文章重點（條列式）\": 從文章整理出的條列式重點, \"文章重點（敘述式）\": 從文章整理出的敘述式重點 }」。"
        self.request_count = 0
        self.stop_time = None
        self.keys_to_check = ['標題', '日期', '發佈機構','文章重點（條列式）','文章重點（敘述式）']
    def generate_reply(self, question, news_content):
        input_text = f"{question}\n{news_content}"
        result = self.gpt_client.chat.completions.create(
            model=self.model,
            messages=[{"role": "user", "content": input_text}],
            temperature=self.temperature,
            max_tokens=self.max_tokens,
            top_p=0.95,
            frequency_penalty=self.frequency_penalty,
            presence_penalty=0,
            stop=None,
            ).choices[0].message.content
        self.request_count += 1
        print(self.request_count)
        if 'RateLimitError' in result.lower():
            # If so, update stop time
            self.stop_time = datetime.strftime(datetime.now(),"%Y/%m/%d %H:%M:%S")           
        time.sleep(self.sleep_time)
        return result
# =============================================================================
#     def update_reply(self):        
#         for data in self.collection.find():
#             print(data['Date'],data['Title'])
#             GPT_Q = data['Date']+data['Author']+data['Title']+data['Article']
#             GPT_A = json.loads(AskGPT(self.client_name,self.db_name,self.collection_name ).generate_reply(self.question, GPT_Q))
#             print(GPT_A)
#             GPT_A['Title'] = data['Title']
#             self.collection.update_one({'Title': data['Title']},  {'$set': GPT_A,"$currentDate":{"lastModified":True}}, upsert=True )
#     
# =============================================================================
# =============================================================================
#1206根據下方 &&修改後的 update_reply，還得補齊如何執行
    def update_reply(self):    
        with self.conn.start_session() as session:
            try:
                cursor = self.collection.find(session=session)        
                for data in cursor:
                    if not(all(key in list(data) for key in self.keys_to_check)):
                        print(data['Date'],data['Title'])
                        GPT_Q = data['Date']+data['Author']+data['Title']+data['Article']
                        GPT_A = json.loads(AskGPT(self.client_name,self.db_name,self.collection_name ).generate_reply(self.question, GPT_Q))
                        print(GPT_A)
                        GPT_A['Title'] = data['Title']
                        self.collection.update_one({'Title': data['Title']},  {'$set': GPT_A,"$currentDate":{"lastModified":True}}, upsert=True )
                
            except Exception as e:
                print(f"An error occurred: {e}")    
  
    
# =============================================================================
#     def token_request(self,text):
#         encoding = tiktoken.get_encoding('cl100k_base')
#         num_tokens = len(encoding.encode(text))
#         return num_tokens
# =============================================================================i

for i in range(0,5):
         
    AskGPT(client_name='mongodb://localhost:27017',db_name='News_ScrapyXpath',collection_name='KGOV_1204').update_reply()
    i+=1


#============Testing for reconnect DB and update original data====
from pymongo import MongoClient

# Replace these with your MongoDB connection details
mongo_uri, mongo_db,mongo_collection  = "mongodb://localhost:27017", "News_ScrapyXpath", "EY_news_test_1127"

# Create a MongoClient
client = MongoClient(mongo_uri)
db = client[mongo_db]
keys_to_check = ['標題', '日期', '發佈機構','文章重點（條列式）','文章重點（敘述式）']
def perform_operations_with_session():
    # Start a new session
    with client.start_session() as session:
        try:
            # Use the session in your operations
            eycol = db.get_collection("EY_news_test_1127")
            ey_cursor = eycol.find(session=session)

            # Process the results
            for data in ey_cursor :
                if not(all(key in list(data) for key in keys_to_check)):
                    GPT_Q = data['Date']+data['Author']+data['Title']+data['Article']        
                    GPT_A = json.loads( AskGPT(client_name='mongodb://localhost:27017',db_name='News_ScrapyXpath',collection_name='EY_news_test_1127').generate_reply(question, GPT_Q))
                    GPT_A['Title'] = data['Title']
                    print(GPT_A)
                    eycol.update_one({'Title': data['Title']},  {'$set': GPT_A,"$currentDate":{"lastModified":True}}, upsert=True )

            # Periodically refresh the session to prevent it from expiring
            session.refresh()

        except Exception as e:
            print(f"An error occurred: {e}")

# Perform operations periodically
while True:
    perform_operations_with_session()
#&&============Testing for reconnect DB and update original data====

# 從MongeDB找到最近更新的post日期
client = pymongo.MongoClient('mongodb://localhost:27017')
db = client['News_ScrapyXpath']
eycol = db.get_collection("EY_news_test_1127")
# =============================================================================
# #doc = eycol.find().sort('_id',1).limit(1) #find latest data
# doc = eycol.find().sort('_id',1).limit(1) #find oldest data
# for record in doc:    
#     latest_post = str(int(record.get('Date')[:3])+1911)+record.get('Date')[3:] #轉為西元年
#     delta = (datetime.today()-datetime.strptime(latest_post,"%Y-%m-%d")).days #計算幾天沒更新
# =============================================================================
question = "幫我整理以下文章的內容：標題，標題請從文章內容整理出重點人物、事件、地點，並不得多於20字；發佈機構；日期及時間點，日期如果是中華民國紀元的話請改為西元紀年，格式為年-月-日 hh:mm:ss；文章的重點並且分兩種方式呈現，一種以條列式的方式整合文章內容列出三點重點，內容必須包含重要人物、事件、地點、重要的數字，內容若有提到「交通、港灣、觀光」則優先度為最高，並且扼要在30字內，一種用敘述的方式，內容必須包含重要人物、事件、地點、重要的數字，內容若有提到「交通、港灣、觀光」則優先度為最高，字數在200字以內。回答時輸出此格式「{\"標題\": 從文章整理出的標題, \"發佈機構\": 從文章整理出的發佈機構, \"日期\": 從文章整理出的發佈日期, \"文章重點（條列式）\": 從文章整理出的條列式重點, \"文章重點（敘述式）\": 從文章整理出的敘述式重點 }」。"
raw_ey = eycol.find({'Title':'主持毒品防制會報 蘇揆：反毒宣導應與時俱進 盼從源頭阻斷毒害、抑制毒品危害'})

for i in range(0,5):
    for data in raw_ey:
        if not(all(key in list(data) for key in keys_to_check)):
            print(data['Title'])
            GPT_Q = data['Date']+data['Author']+data['Title']+data['Article']        
            GPT_A = json.loads( AskGPT(client_name='mongodb://localhost:27017',db_name='News_ScrapyXpath',collection_name='EY_news_test_1127').generate_reply(question, GPT_Q))
            GPT_A['Title'] = data['Title']
            print(GPT_A)
            eycol.update_one({'Title': data['Title']},  {'$set': GPT_A}, upsert=True )


