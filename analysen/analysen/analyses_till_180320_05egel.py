# Analysis till data
# stand: 28.03 // egel

import numpy as np
import pandas as pd
import seaborn as sns
import os


os.chdir('S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files')

# saving result
df=pd.read_csv("filtered_data_r_notime_180403_egel.csv", sep=';', index=False)

# merge information out of documentation
os.chdir('S:/pools/n/N-IUNR-nova-data/06_add_var/01_dokumentation')
info=pd.read_csv('menu_inhalt_180215_10egel.csv', sep=';', encoding='latin1', parse_dates=['date']).rename(columns={'meal':'art_description','place':'shop_description'})

# filter time
df1=df1.set_index('trans_date') # change Date to DateIndex for between time function
df2=df1.between_time("11:00", "14:00", include_start=True, include_end=True) # select between 10 and 15 

# merge
df3=pd.merge(df2, info[['date','art_description','cycle','shop_description','content','label_content']],on=['date','art_description','cycle','shop_description'], how='left')

# save
df3.to_csv("filtered_data_r_merged_180403_02egel.csv", sep=';', index=False)

sns.countplot(x='art_description',data=df2)
sns.countplot(x='art_description', data=df3)


#==============================================================================
# # difference between dataframes not working
# df2.columns = df3.columns
# df2 = df2.set_index(df2.columns.tolist())
# df3 = df3.set_index(df3.columns.tolist())
# 
# diff=df2.index.difference(df3.index)
# diff2=df3.index.difference(df2.index)
# 
# pd.concat([df2, df3]).loc[
#     df2.index.symmetric_difference(df3.index)
# ]
# 
# # not working either
# diff=set(df1['transaction_id']).difference(df2['transaction_id'])
# where_diff = df1['transaction_id'].isin(diff)
# test=df1[where_diff]
#==============================================================================



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
