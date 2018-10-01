##### edit special cases in data
#### Manuelle Anpassungen an die Kassendaten:

# Stand: 1.10.18 // egel    

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
# new data set
d = pd.read_csv('data_filtered_180929_egel.csv',sep=';', parse_dates=['trans_date']) # dont parse date
# d['date2'] = d['date'].dt.date
# d['date2'] = pd.to_datetime(d['date2'], format = "%Y-%m-%d")

# old data set
d1 = pd.read_csv('data_filtered_180802_egel.csv',sep=';', parse_dates=['trans_date',"date"])

# two special cases:
# 1. take 65 random local favorites from data frame and change them to favorite
## am 23.10 Local Favorite to Favorite
random.seed(3) # set seed, for replication
spec=d.loc[(d['date']=='2017-10-23') & (d['shop_description']=='Vista') & (d['article_description']=="Local Favorite")].sample(65) # select random 65 cases of local favorite
spec['article_description'] = 'Favorite' # rename label
d.update(spec)# merge it back to data frame

# 2. not handled yet
## am 17.11 ???????


# adjust data frame according to the description above
## Kitchen
# 	2017-10-09: local tössfeld to kitchen (10x)
d.loc[(d['date']=='2017-10-09') & (d['article_description']=="Local Tössfeld"),'article_description'] ='Local Kitchen'

# 	2017-10-17
d.loc[(d['date']=='2017-10-17') & (d['article_description']=="Kitchen 4") & (d['shop_description']=='Grüental'),'article_description'] ='Local Kitchen'

# 	2017-10-20
d.loc[(d['date']=='2017-10-20') & (d['shop_description']=='Vista') & (d['article_description']=="Kitchen 4"),'article_description'] ='Local Kitchen'

# 	2017-11-20
d.loc[(d['date']=='2017-11-20') & (d['article_description']=="Kitchen 2"),'article_description'] ='Local Kitchen'

# 	2017-12-19: in comparison to the old data set (16), the new data set has only 14 kitchen 2
d.loc[(d['date']=='2017-12-19') & (d['shop_description']=='Grüental') & (d['article_description']=="Kitchen 2"),'article_description'] = 'Local Kitchen'

# 	2017-12-20: in comparison to the old data set (15), the new data set has only 14 kitchen 2
d.loc[(d['date']=='2017-12-20') & (d['shop_description']=='Vista') & (d['article_description']=="Kitchen 2"),'article_description'] ='Local Kitchen'

# change all other kitchen 1,2,3,4 to kitchen
d['article_description'] = d['article_description'].str.replace("Kitchen \d", "Kitchen")


## faults of system, or workers
# 2017-10-09
d.loc[(d['date']=='2017-10-09') & (d['shop_description']=='Vista') & (d['article_description'] == "Local World"),'article_description']='Local Favorite'

# 	2017-10-26
d.loc[(d['date']=='2017-10-26') & (d['article_description']=="Local Favorite"),'article_description'] = 'Local World'

# 	2017-10-30
d.loc[(d['date']=='2017-10-30') & (d['article_description']=="Local World"),'article_description'] = 'Local Favorite'

# 	2017-11-02: not anymore there in new data set (because it was payed cash)
# d.loc[(d['date']=='2017-11-02') & (d['shop_description']=='Vista') & (d['article_description'] =="Local Favorite"),'article_description']# = 'Local World'

# 	2017-11-03
d.loc[(d['date']=='2017-11-03') & (d['shop_description']=='Grüntal Mensa') & (d['article_description'] =="Local Favorite"),'article_description'] = 'Local World'

# 	2017-11-07: 5 observations in new data set (before 6)
d.loc[(d['date']=='2017-11-07') & (d['article_description']=="Local World"),'article_description'] = 'Local Favorite'

# 	2017-11-15
d.loc[(d['date']=='2017-11-15') & (d['article_description']=="Local World"),'article_description']='Local Favorite'

# 	2017-11-16
d.loc[(d['date']=='2017-11-16') & (d['article_description']=="Local World"),'article_description']='Local Favorite'

# 	2017-11-17: not anymore there in new data set (because it was payed cash)
d.loc[(d['date']=='2017-11-17') & (d['article_description']=="Local Favorite"),'article_description'] ='Local World'

# 	2017-11-21
d.loc[(d['date']=='2017-11-21') & (d['article_description']=="Local Favorite"),'article_description'] = 'Favorite'

# 	2017-11-22
d.loc[(d['date']=='2017-11-22') & (d['article_description']=="Local Favorite"),'article_description'] = 'Local World'

# 	2017-12-04: 24 observations in new data set  (before 26) 
d.loc[(d['date']=='2017-12-04') & (d['article_description']=="Local World"),'article_description'] = 'Local Favorite'

# 	2017-12-07 
d.loc[(d['date']=='2017-12-07') & (d['article_description']=="Local World"),'article_description'] = 'Local Favorite'

# 	2017-12-13
d.loc[(d['date']=='2017-12-13') & (d['article_description']=="Local World"),'article_description'] = 'Favorite'

# 	2017-12-15
d.loc[(d['date']=='2017-12-15') & (d['article_description']=="Local Favorite"),'article_description'] = 'Local World'

# 	2017-12-20: 2 observations in new data set  (before 1) 
d.loc[(d['date']=='2017-12-20') & (d['shop_description']=='Vista Mensa') & (d['article_description']=="Local Favorite"),'article_description'] = 'Favorite'

### save data
d.to_csv('S:/pools/n/N-IUNR-nova-data/02_kassendaten/02_tilldata_2017/augmented data/data_edit_180929_egel.csv',sep=';', index=False) # not including time filter

