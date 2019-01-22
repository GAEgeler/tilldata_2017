# Analysis till data
# stand: 28.03 // egel

import numpy as np
import pandas as pd
import seaborn as sns
import os


# First step: load data
os.chdir('S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files')
d = pd.read_csv('180320_joined_04egel.csv', sep=';', encoding='latin1',parse_dates=['date','trans_date'])

os.chdir('S:/pools/n/N-IUNR-nova-data/06_add_var/01_dokumentation')
info=pd.read_csv('menu_inhalt_180215_09egel.csv', sep=';', encoding='latin1', parse_dates=['date']).rename(columns={'meal':'art_description','place':'shop_description'})
 
# filter data => only lunch meals

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

df=d[d['code_description'].isin(articles)] # different outcome than in R => 70 less cases, dont know why
df=df.set_index('trans_date') # change Date to DateIndex for between time function
df1=df.between_time("9:00", "15:00", include_start=True, include_end=True) # select between 10 and 15 

    
df1.to_csv('filtered_data_dub_python_180320_03egel.csv', sep=';',encoding = 'latin1') # some information is getting lost while loading with R

#--------------------------------------------------- # change some in python

#df1=pd.read_csv("filtered_data_dub_180320_03egel.csv", sep=';',encoding = 'latin1', parse_dates=['date','trans_date']) # wrong Dataset
df1= pd.read_csv("filtered_data_dub_python_180320_04egel.csv", sep=',',encoding = 'latin1', parse_dates=['date','trans_date']).rename(columns={'article_description':'art_description'})

# add cycle of canteen to data with list comprehension => there should be a better solution   
result=[1 if (x >= 40 | x <= 45) else 2 for x in df1['week']]   

df1['cycle'] = result # add result to dataframe
    
# add intervention to data frame, there is sure a better solution

result=[]
for num in df1[df1['cycle']==1]['week']: # subset data for 1 cycle and loop through it
    if num %2 == 0:
       result.append('Basis')
    else:
        result.append('Intervention')

result2=[]
for num in df1[df1['cycle']==2]['week']: # subset data for 2 cycle and loop through it
    if num %2 == 1:
       result2.append('Basis')
    else:
        result2.append('Intervention')

result3=result+result2 # concatenate this lists
df1['condit'] = result3 # add to dataframe


# merge information out of documentation

test=pd.merge(df1, info[['date','art_description','cycle','shop_description','content','label_content']],on=['date','art_description','cycle','shop_description'], how='inner')


diff=set(df1['transaction_id']).difference(test['transaction_id'])
where_diff = df1['transaction_id'].isin(diff)



df1.to_csv("filtered_data_dub_python_180320_04egel.csv", sep=';',encoding = 'latin1', index=False)















#count single card_num only for lunch time => 1603

len((df1['card_num'].unique()))

# count frequency of card_num => max possible is 60
frequ=df1['card_num'].value_counts()
frequ1=frequ[frequ>60]
df2=df1[df1['card_num'].isin(frequ1.index)] # dataframe with more than 60 transactions


# group by card_num => only single cases plus frequence => not working yet

df3=df1[df1.groupby('card_num').count()]


# check for doublictated data form R
os.chdir('S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files')
double=pd.read_csv("duplicate transaction_id 180320 egel.csv", sep=';',encoding='latin1',parse_dates=['date','trans_date'])
df3=df1[df1['card_num'].isin(double.card_num)]


# check for differences between R and Python datasets
os.chdir('S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files')
df_r=pd.read_csv("filtered_data_dub_180320_02egel.csv", sep=',',encoding='latin1',parse_dates=['date','trans_date'])



# try antoher way
#==============================================================================
# df_tot = pd.concat([df1, df_r]) # first change column names so that it can be fitted
# df_tot2 = df_tot.reset_index(drop=True)
# df_gpby = df_tot2.groupby(list(df.columns))
# idx = [x[0] for x in df_gpby.groups.values() if len(x) == 1]
# df_tot3=df_tot.iloc[idx]
#==============================================================================
