# Analysis till data
# stand: 17.4 // egel

import numpy as np
import pandas as pd
import seaborn as sns
import os


os.chdir('S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files')

df1= pd.read_csv("filtered_data_dub_notime_180320_07egel.csv", sep=';', parse_dates=['date','trans_date'])

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

# saving result
df1.to_csv("filtered_r_180502_egel.csv", sep=';', index=False)

