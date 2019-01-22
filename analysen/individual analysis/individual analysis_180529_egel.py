# Analysis till data: individual analysis
# stand: 29.5.18 // egel

# load packages
import numpy as np
import pandas as pd
import seaborn as sns
import os

# load data
os.chdir('S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files/')
df1= pd.read_csv("r_final_180503_egel.csv", sep=';', parse_dates=['date','trans_date'])


# subsample
np.random.seed(4)
sample=df1['card_num'].sample(10, replace=False).index # somehow not working
# tolist() convert to list
# pd.np.array() convert to array
sample=[1121476, 1670487 ,1069507 ,1042499 ,1121467 , 786184 , 971017,  972867, 729667] # generated in r
df=df1[df1['card_num'].isin(sample)] 

# loop through sample

ids=set(df['card_num']) # unique values
ids=list(ids)

 dat_tot = pd.DataFrame() # define empty data frame

for card in ids:   
    dat_card = df.loc[(df['card_num'] == card),'label_content'].to_frame() #search all valid meal chocies resp. buyings
    dat_card['card'] = card # add new row with card number
    dat_tot=dat_card.append(dat_tot) # append new rows


# loop through sample and select only card nums which appear in basis and intervention
# however lots of nan arise => reasons?
dat_tot = pd.DataFrame()
for card in ids:
    dat_card = df.loc[(df['card_num'] == card) & (df['condit']=="Basis") | (df['condit']=="Intervention"),'label_content'].to_frame()
    dat_card['card_nr'] = card
    dat_card['condition'] = df.loc[(df['card_num'] == card),'condit']
    dat_tot= dat_card.append(dat_tot).dropna()

