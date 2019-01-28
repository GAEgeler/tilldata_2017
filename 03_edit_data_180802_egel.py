##### edit special cases in data
#### Manuelle Anpassungen an die Kassendaten:

###
# state: january 2019
# author: gian-Andrea egeler
###

# see documentation: "anpassungen an datensatz 180502.txt"

# required packages
import seaborn as sns
import numpy as np
import pandas as pd
import string
from datetime import datetime
import random
import os
import re


# load data
os.chdir('S:/pools/n/N-IUNR-nova-data/02_kassendaten/02_tilldata_2017/clean data')

# individual data set
d = pd.read_csv('data_filtered_180929_egel.csv',sep=';', parse_dates=['trans_date']) # do not parse date


# agg data set
d = pd.read_csv('data_filtered_180802_egel.csv',sep=';', parse_dates=['trans_date',"date"])
# the shop_description contained two words "grüental/vista MENSA"; to delete the mensa word see code below 
d['shop_description'] = d['shop_description'].str.replace(" .*","") # select only fist word of string 

# two special cases:
# 1. take 65 random local favorites from data frame and change them to favorite
## am 23.10 Local Favorite to Favorite
random.seed(3) # set seed, for replication
spec=d.loc[(d['date']=='2017-10-23') & (d['shop_description']=='Vista') & (d['article_description']=="Local Favorite")].sample(65) # select random 65 cases of local favorite
spec['article_description'] = 'Favorite' # rename label
d.update(spec)# merge it back to data frame

# 2. 10 vegi-burgers where sold as world instead of local world only in Vista canteen
## am 17.11 World to local World
random.seed(3)
rand = d.loc[(d['date']=='2017-11-17') & (d['shop_description']=='Vista') & (d['article_description']=="World")].sample(10)
rand['article_description'] = 'Local World'
d.update(rand)

# adjust data frame according to the description above
## Kitchen
# 	2017-10-09: local tössfeld to kitchen in individual dataset (10x)
# 	2017-10-09: local tössfeld to kitchen in aggregated dataset (12x)
d.loc[(d['date']=='2017-10-09') & (d['article_description']=="Local Tössfeld"),'article_description'] ='Local Kitchen'

# 	2017-10-17
d.loc[(d['date']=='2017-10-17') & (d['article_description']=="Kitchen 4") & (d['shop_description']=='Grüental'),'article_description'] ='Local Kitchen'

# 	2017-10-20
d.loc[(d['date']=='2017-10-20') & (d['shop_description']=='Vista') & (d['article_description']=="Kitchen 4"),'article_description'] ='Local Kitchen'

# 	2017-11-20
d.loc[(d['date']=='2017-11-20') & (d['article_description']=="Kitchen 2"),'article_description'] ='Local Kitchen'

# 	2017-12-19: in individual data set (14)
#   2017-12-19: in aggregated data set (16)
d.loc[(d['date']=='2017-12-19') & (d['shop_description']=='Grüental') & (d['article_description']=="Kitchen 2"),'article_description'] = 'Local Kitchen'

# 	2017-12-20: in individual data set (14)
# 	2017-12-20: in aggregated data set (15)
d.loc[(d['date']=='2017-12-20') & (d['shop_description']=='Vista') & (d['article_description']=="Kitchen 2"),'article_description'] ='Local Kitchen'

# change all other kitchen 1,2,3,4 to kitchen
d['article_description'] = d['article_description'].str.replace("Kitchen \d", "Kitchen")


## faults of system, or workers
# 2017-10-09
d.loc[(d['date']=='2017-10-09') & (d['shop_description']=='Vista') & (d['article_description'] == "Local World"),'article_description']='Local Favorite'

# 	2017-10-26: 2 in individual data set
# 	2017-10-26: 3 in aggregated data set
d.loc[(d['date']=='2017-10-26') & (d['article_description']=="Local Favorite"),'article_description'] = 'Local World'

# 	2017-10-30
d.loc[(d['date']=='2017-10-30') & (d['article_description']=="Local World"),'article_description'] = 'Local Favorite'

# 	2017-11-02: only for aggregated data set (1)
d.loc[(d['date']=='2017-11-02') & (d['shop_description']=='Vista') & (d['article_description'] =="Local Favorite"),'article_description'] = 'Local World'

# 	2017-11-03: 
d.loc[(d['date']=='2017-11-03') & (d['shop_description']=='Grüental') & (d['article_description'] =="Local Favorite"),'article_description'] = 'Local World'

# 	2017-11-07: 5 observations in individual data set
#   2017-11-07: 6 observations in aggrageted data set
d.loc[(d['date']=='2017-11-07') & (d['article_description']=="Local World"),'article_description'] = 'Local Favorite'

# 	2017-11-15
d.loc[(d['date']=='2017-11-15') & (d['article_description']=="Local World"),'article_description']='Local Favorite'

# 	2017-11-16
d.loc[(d['date']=='2017-11-16') & (d['article_description']=="Local World"),'article_description']='Local Favorite'

# 	2017-11-17: only for aggregated data set (1)
d.loc[(d['date']=='2017-11-17') & (d['article_description']=="Local Favorite"),'article_description'] ='Local World'

# 	2017-11-21
d.loc[(d['date']=='2017-11-21') & (d['article_description']=="Local Favorite"),'article_description'] = 'Favorite'

# 	2017-11-22
d.loc[(d['date']=='2017-11-22') & (d['article_description']=="Local Favorite"),'article_description'] = 'Local World'

# 	2017-12-04: 24 observations in individual data set 
#   2017-12-04: 25 observations in aggregated data set 
d.loc[(d['date']=='2017-12-04') & (d['article_description']=="Local World"),'article_description'] = 'Local Favorite'

# 	2017-12-07 
d.loc[(d['date']=='2017-12-07') & (d['article_description']=="Local World"),'article_description'] = 'Local Favorite'

# 	2017-12-13
d.loc[(d['date']=='2017-12-13') & (d['article_description']=="Local World"),'article_description'] = 'Favorite'

# 	2017-12-15
d.loc[(d['date']=='2017-12-15') & (d['article_description']=="Local Favorite"),'article_description'] = 'Local World'

# 	2017-12-20: 2 observations in individual data set  (its a duplicate) 
# 	2017-12-20: 1 observations in aggregated data set   
d.loc[(d['date']=='2017-12-20') & (d['shop_description']=='Vista') & (d['article_description']=="Local Favorite"),'article_description'] = 'Favorite'


### save individual data
d.to_csv('S:/pools/n/N-IUNR-nova-data/02_kassendaten/02_tilldata_2017/augmented data/data_edit_180929_egel.csv',sep=';', index=False) 

### save agg data set
d.to_csv('S:/pools/n/N-IUNR-nova-data/02_kassendaten/02_tilldata_2017/augmented data/data_edit_180802_egel.csv',sep=';', index=False) 	

