import scrapy
import time
from scrapy.http import FormRequest
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from webdriver_manager.chrome import ChromeDriverManager
from scrapy.spiders import CrawlSpider, Rule
from scrapy.linkextractors import LinkExtractor
class KgovSpider(scrapy.Spider):
    name = "kgov"
    allowed_domains = ["www.kcg.gov.tw"]
    start_urls = ["http://www.kcg.gov.tw/CityNews1.aspx?n=3A379BB94CA5F12D"]    
    download_delay = 1.5 
    

    def parse(self, response):
        # Extract form data for the initial request
        form_data = self.extract_form_data(response)
        # Submit the initial request
        yield scrapy.FormRequest(
            url='https://www.kcg.gov.tw/CityNews1.aspx?n=3A379BB94CA5F12D',
            formdata=form_data,
            callback=self.parse_page,
            meta={'page_number': 1})  # Initial page number        
 
    def parse_page(self, response):
                    
        post_titles = response.xpath("//td[@align='left']//a/text()").getall()
        print(post_titles)
        print(self.logger.info(f"Page {response.meta['page_number']} titles: {post_titles}"))

        # 爬取文章來源&日期
        post_authors = response.xpath("//td[@align='center']/text()").getall()[::2]
        print(post_authors)        
        # 爬取文章日期
        post_dates = response.xpath("//td[@align='center']/text()").getall()[1::2]
        print(post_dates)
        # 爬取文章連結
        post_links = response.xpath("//td[@align='left']//a/@href").getall()     
        
                
        for data in zip(post_titles, post_authors, post_dates, post_links):
            NewsScraperItem = {
                    "post_title": data[0],
                    "post_author": data[1],
                    "post_date": data[2],
                    "post_link": data[3]
                }

            yield NewsScraperItem
        # Extract form data for the next request
        form_data = self.extract_form_data(response)

        # Extract the current page number from the meta
        current_page = response.meta.get('page_number')
        print(current_page)
        # Update the page number for the next request
        next_page = current_page + 1

        # Update form data to simulate clicking the "Next" button
        form_data['__EVENTTARGET'] = 'ctl00$ContentPlaceHolder1$lbtNext'
        form_data['__EVENTARGUMENT'] = ''

        # Submit the next request
        yield scrapy.FormRequest(
            url='https://www.kcg.gov.tw/CityNews1.aspx?n=3A379BB94CA5F12D',
            formdata=form_data,
            callback=self.parse_page,
            meta={'page_number': next_page}  # Update the page number in the meta
        )
    def extract_form_data(self, response):
        # Extract and return form data from the current page
        form_data = {
            'ToolkitScriptManager1_HiddenField': response.xpath("//*[@name='ToolkitScriptManager1_HiddenField']/@value").get(),
            '__EVENTTARGET': response.xpath("//*[@name='__EVENTTARGET']/@value").get(),
            '__EVENTARGUMENT': response.xpath("//*[@name='__EVENTARGUMENT']/@value").get(),
            '__LASTFOCUS': response.xpath("//*[@name='__LASTFOCUS']/@value").get(),
            '__VIEWSTATE': response.xpath("//*[@name='__VIEWSTATE']/@value").get(),
            'ctl00$hdManagerURL': response.xpath("//*[@name='ctl00$hdManagerURL']/@value").get(),
            'ctl00$uscSiteMap1$hdNodeID': response.xpath("//*[@name='ctl00$uscSiteMap1$hdNodeID']/@value").get(),
            '__VIEWSTATEGENERATOR': response.xpath("//*[@name='__VIEWSTATEGENERATOR']/@value").get(),
            '__VIEWSTATEENCRYPTED': response.xpath("//*[@name='__VIEWSTATEENCRYPTED']/@value").get(),
            '__EVENTVALIDATION': response.xpath("//*[@name='__EVENTVALIDATION']/@value").get(),
        }
        return form_data
# open another file   
#新聞內文
   #//div[@class="data_midlle_news_box02"]//p/text()
   #post_content = response.xpath("//div[@class='data_midlle_news_box02']//p/text()").getall()
   # 根據link搜尋新聞內文
   
   #for links in post_links:
    #   yield scrapy.Request(links, self.parse_content)

# =============================================================================
#     def parse_content(self, response):
#  
#         # 文章內容
#         content = response.xpath("//div[@class='data_midlle_news_box02']//p/text()").getall()
#         print(content)      
#         return content
# =============================================================================
# 1119 測試結果 maximum：抓取為期半年的資料ex 5/19-11/19              