# -*- coding: utf-8 -*-
"""
Created on Mon Aug 13 10:26:47 2018

@author: egel
"""

## merge transactions from patrick buenter (e-mail from 24.7.18)

# status: 13.8.18 // egel

# import packages
import os
import pandas as pd
import numpy as np
import string
import datetime

# load data
# problem zhaw_pers mseriennr has at least one number with underscore in it => problem with merging
zhaw_pers = pd.read_excel("S:/pools/n/N-IUNR-nova-data/02_kassendaten/02_tilldata_2017/raw data/Kaufverhalten_2018-01-19_egel.xlsx", 'Mapping-Person', doublequote=False) # information about people from zhaw (info from patrik buenter)

# .count and .replace is not working why?
zhaw_pers.loc[2143,'mseriennr'] =  29090741 # one special case of mrseriennrz
zhaw_pers.loc[3039,'mseriennr'] =  29046121
zhaw_pers.loc[3501,'mseriennr'] =  27288231
zhaw_pers.loc[4940,'mseriennr'] =  29090751
zhaw_pers.loc[6589,'mseriennr'] =  24964031
zhaw_pers.loc[7991,'mseriennr'] =  19958861
zhaw_pers.loc[35936,'mseriennr'] =  27251121
zhaw_pers.loc[36911,'mseriennr'] =  22354121
zhaw_pers.loc[36923,'mseriennr'] =  24970231
zhaw_pers.loc[39088,'mseriennr'] =  17993361
zhaw_pers.loc[39524,'mseriennr'] =  24952701
zhaw_pers.loc[43444,'mseriennr'] =  24985101
zhaw_pers.loc[47536,'mseriennr'] =  24985081
zhaw_pers.loc[49938,'mseriennr'] =  24988951
zhaw_pers.loc[57815,'mseriennr'] =  22354531
zhaw_pers.loc[57816,'mseriennr'] =  22354521
zhaw_pers.loc[59360,'mseriennr'] =  10240301
zhaw_pers.loc[60294,'mseriennr'] =  17963561
zhaw_pers.loc[67447,'mseriennr'] =  27251531

zhaw_pers['mseriennr'] = pd.to_numeric(zhaw_pers['mseriennr']) # convert to numeric


zhaw_trans = pd.read_excel("S:/pools/n/N-IUNR-nova-data/02_kassendaten/02_tilldata_2017/raw data/Kaufverhalten_2018-01-19.xlsx", 'Transaktionen') # all transaction from zhaw (info from patrik buenter)
zhaw_trans['TRANSACTION_DATE_TIME'] = pd.to_datetime(zhaw_trans.TRANSACTION_DATE_TIME) # parse to dateformat
zhaw_trans['date'] = zhaw_trans['TRANSACTION_DATE_TIME'].dt.date # only date without time stamp
zhaw_trans['date'] = pd.to_datetime(zhaw_trans['date'], format = "%Y-%M-%d")


# merge data
df = pd.merge(zhaw_trans, zhaw_pers, left_on = "SERIAL_NUMBER", right_on = "mseriennr", how = "inner")


# read info shop code and till code
till = pd.read_excel("S:/pools/n/N-IUNR-nova-data/02_kassendaten/02_tilldata_2017/raw data/ZHAW_Tills.xlsx")[['id', 'code', 'shop_id', 'description']]
till['terminal_id'] = till['description'].str.slice(start = -3) # information of cash station in the last 3 letters of description
till['terminal_id'] = pd.to_numeric(till['terminal_id']) # for merging change into int


# merge info to rest of data
df1 = pd.merge(df, till[['terminal_id', "shop_id"]], left_on = "CASH_STATION", right_on = 'terminal_id', how = "inner")

# select only grüental und vista (34 818)
df2 = df1.loc[(df1['terminal_id'] == 185) | (df1['terminal_id'] == 186) | (df1['terminal_id'] == 181) | (df1['terminal_id'] == 187)]

# read sample from sv
sv_sample = pd.read_csv("S:/pools/n/N-IUNR-nova-data/02_kassendaten/02_tilldata_2017/augmented data/test_data_sv.csv", sep=';') #, parse_dates = ['date']


#merge with df1
#df1.rename(columns={'mseriennr': 'card_num', 'TURNOVER': 'total_amount'}, inplace=True)# change names
df2 = pd.merge(sv_sample, df1, left_on = ['date', 'total_amount'], right_on = ['date', 'TURNOVER'], how = "left")

df1.sort_index(inplace=True)
sv_sample.sort_index(inplace=True)
sv_sample['card_num']==df1['mseriennr']


    

print(df1['mseriennr'].reset_index(drop=True) == sv_sample['card_num'].reset_index(drop=True))


for v in [ i.start() for i in re.finditer(r'' + sv_sample['card_num'] + '',df1['mseriennr']) ]:
    print "at index {0} found a slice {1}".format(v,df['mseriennr']) 


def expand_list(complete_list, to_be_expand_list):
    expanded_list = []
    for i in complete_list:
        if i in be_expand_list:
            expanded_list.append(i)
        else:
            expanded_list.append(0)
    return expanded_list

expand_list(df1['mseriennr'], sv_sample['card_num'])


test = [x if x in df1['mseriennr'] else 0 for x in sv_sample['card_num']] # search for match in sv_sample

test2= pd.merge()


