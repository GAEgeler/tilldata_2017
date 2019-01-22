# Analysis till data
# stand: 20.03 // egel

import numpy as np
import pandas as pd
import os

#import subprocess as sp
#tmp = sp.call('cls',shell=True)

# First step: load data
os.chdir('S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files')
d = pd.read_csv('180320_joined_04egel.csv', sep=';', encoding='latin1',parse_dates=['date','trans_date'])

os.chdir('S:/pools/n/N-IUNR-nova-data/06_add_var/01_dokumentation')
info=pd.read_csv('menu_inhalt_180215_08vori.csv', sep=';', encoding='latin1', parse_dates=['date']).rename(columns={'meal':'article_description','place':'shop_description'})
 
info=info.replace("Grüntal Mensa","Grüental Mensa")

# filter data => only lunch meals

test=pd.read_csv('test_checktomorrow_filtered.csv',sep=',',encoding='latin1',parse_dates=['trans_date','date'])

articles = [
    "A10001" ,# Favorite
    "A10010", # Kitchen 0
    "A10011", # Kitchen 1
    "A10040" ,# Garden 
    "A10012" ,# Kitchen 2
    "A10013" ,# Kitchen 3
    "A10014" ,# Kitchen 4
    "A10025" ,# Local Favorite
    "A10026" ,# Local World
    "A10027" ,# Local Tössfeld ??
    "A10015" ,# World
    "A10020" ,# Local 0
    "A10021" ,#	Local 1
    "A10022" ,#	Local 2
    "A10023" ,#	Local 3
    "A10024"  # Local 4
]

test2=d[d['code_description'].isin(articles)] # filter articles

test3=test2.set_index('trans_date') # change Date to DateIndex for between time function
test4=test3.between_time("9:00", "15:00", include_start=True, include_end=True) # select between 10 and 15 

t=test4.groupby(['article_description','date','shop_description_x']).count() #does it for every column => find a better solution


# merge information out of documentation

test=pd.merge(d, info[['date','article_description','label_content','shop_description']],on=['article_description','shop_description','date'], how='inner')

#count single card_num => 1905

len((test['card_num'].unique()))
