## Filter Data in Python

# Stand 2.05.18 // egel

import pandas as pd
import os

# First step: load data
os.chdir('S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files')
d = pd.read_csv('180320_joined_05egel.csv', sep=';', encoding='latin1',parse_dates=['date','trans_date']).rename(columns={'price_articles':'price_article','price_payments':'price_payment'})

# filter meals => only lunch meals

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

df=d[d['art_code'].isin(articles)] # different outcome than in R => 70 less cases, dont know why

os.chdir('S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/bereinigen')   
df.to_csv('filtered_data_dub_python_180320_04egel.csv', sep=';',encoding = 'latin1') # some information is getting lost while loading with R


# filter time

df=d.set_index('trans_date') # change Date to DateIndex for between time function
df1=df.between_time("9:00", "15:00", include_start=True, include_end=True) # select between 10 and 15 ?? does it contains strat and end time?

df1.to_csv('filtered_data_time_python_180419_egel.csv', sep=';',encoding = 'latin1') # some information is getting lost while loading with R


