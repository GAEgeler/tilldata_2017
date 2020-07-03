##### edit special cases in data
#### Manuelle Anpassungen an die Kassendaten:

###
# state: december 2019
# author: gian-Andrea egeler
###

# see documentation: "Egeler, 2019. Dokumentation (NOVANIMAL Kurzbericht). Wädenswil: ZHAW."

# required packages
import os
import pandas as pd
import string
from datetime import datetime # not sure if needed
import random
import numpy as np


# load data
os.chdir('S:/pools/n/N-IUNR-nova-data/02_kassendaten/02_tilldata_2017/')

# load individual data set
d = pd.read_csv('clean data/data_filtered_ind_180929_egel_check.csv', sep=';', parse_dates=['trans_date']) # do not parse date


# one special cases:
# take 65 random local favorites from data frame and change them to favorite
## am 23.10 Local Favorite to Favorite, attention 2 persons bought a Favorite at that day
np.random.seed(3) # set seed, for replication (attention take that function from numpy not random library)
spec=d.loc[(d['date']=='2017-10-23') & (d['shop_description']=='Vista') & (d['article_description']=="Local Favorite")].sample(65) # select random 65 cases of local favorite, without replacement
spec['article_description'] = 'Favorite' # rename label
d.update(spec)# merge it back to data frame, in total 67 cases had favorites at that day (slightly differs from the dokumentation from Michael Krauer with 65 cases)

# 2. 10 vegi-burgers where sold as world instead of local world only in Vista canteen
## am 17.11 World to local World
## was however not taken into account (see Egeler 2019. Dokuemnation (NOVANIMAL Kurzbericht). Wädenswil: ZHAW)

###############
# adjust data set
## Kitchen
# 	2017-10-09: local tössfeld to kitchen in individual dataset (10x)
d.loc[(d['date']=='2017-10-09') & (d['article_description']=="Local Tössfeld"),'article_description'] ='Local Kitchen'

# 	2017-10-17
d.loc[(d['date']=='2017-10-17') & (d['article_description']=="Kitchen 4") & (d['shop_description']=='Grüental'),'article_description'] ='Local Kitchen'

# 	2017-10-20
d.loc[(d['date']=='2017-10-20') & (d['shop_description']=='Vista') & (d['article_description']=="Kitchen 4"),'article_description'] ='Local Kitchen'

# 	2017-11-20
d.loc[(d['date']=='2017-11-20') & (d['article_description']=="Kitchen 2"),'article_description'] ='Local Kitchen'

#   2017-12-19: in aggregated data set (16)
d.loc[(d['date']=='2017-12-19') & (d['shop_description']=='Grüental') & (d['article_description']=="Kitchen 2"),'article_description'] = 'Local Kitchen'

# 	2017-12-20: in individual data set (14)
d.loc[(d['date']=='2017-12-20') & (d['shop_description']=='Vista') & (d['article_description']=="Kitchen 2"),'article_description'] ='Local Kitchen'

# change all other kitchen 1,2,3,4 to kitchen
d['article_description'] = d['article_description'].str.replace("Kitchen \d", "Kitchen")

#############
## adjust data set
## all other meal lines
# 2017-10-09
d.loc[(d['date']=='2017-10-09') & (d['shop_description']=='Vista') & (d['article_description'] == "Local World"),'article_description']='Local Favorite'

# 	2017-10-26 
d.loc[(d['date']=='2017-10-26') & (d['article_description']=="Local Favorite"),'article_description'] = 'Local World'

# 	2017-10-30
d.loc[(d['date']=='2017-10-30') & (d['article_description']=="Local World"),'article_description'] = 'Local Favorite'

# 	2017-11-03:
d.loc[(d['date']=='2017-11-03') & (d['shop_description']=='Grüental') & (d['article_description'] =="Local Favorite"),'article_description'] = 'Local World'

# 	2017-11-07: 5 observations in individual data set
d.loc[(d['date']=='2017-11-07') & (d['article_description']=="Local World"),'article_description'] = 'Local Favorite'

# 	2017-11-15
d.loc[(d['date']=='2017-11-15') & (d['article_description']=="Local World"),'article_description']='Local Favorite'

# 	2017-11-16
d.loc[(d['date']=='2017-11-16') & (d['article_description']=="Local World"),'article_description']='Local Favorite'

# 	2017-11-21
d.loc[(d['date']=='2017-11-21') & (d['article_description']=="Local Favorite"),'article_description'] = 'Favorite'

# 	2017-11-22
d.loc[(d['date']=='2017-11-22') & (d['article_description']=="Local Favorite"),'article_description'] = 'Local World'

# 	2017-12-04: 24 observations in individual data set 
d.loc[(d['date']=='2017-12-04') & (d['article_description']=="Local World"),'article_description'] = 'Local Favorite'

# 	2017-12-07 
d.loc[(d['date']=='2017-12-07') & (d['article_description']=="Local World"),'article_description'] = 'Local Favorite'

# 	2017-12-13
d.loc[(d['date']=='2017-12-13') & (d['article_description']=="Local World"),'article_description'] = 'World'

# 	2017-12-15
d.loc[(d['date']=='2017-12-15') & (d['article_description']=="Local Favorite"),'article_description'] = 'Local World'

# 	2017-12-20: 2 observations in individual data set  (its a duplicate) 
d.loc[(d['date']=='2017-12-20') & (d['shop_description']=='Vista') & (d['article_description']=="Local Favorite"),'article_description'] = 'Favorite'

### save individual data
d.to_csv('S:/pools/n/N-IUNR-nova-data/02_kassendaten/02_tilldata_2017/augmented data/data_edit_ind_180929_egel_check.csv',sep=';', index=False)


#########################################################
#########################################################----------------------------------------------
#########################################################

#############
# load agg data set
d = pd.read_csv('clean data/data_filtered_agg_180802_egel_check.csv', sep=';', parse_dates=['trans_date',"date"])
# the shop_description contained two words "grüental/vista MENSA"; to delete the mensa word see code below
#d['shop_description'] = d['shop_description'].str.replace(" .*","") # select only fist word of string

# two special cases:
# 1. take 65 random local favorites from data frame and change them to favorite
## am 23.10 Local Favorite to Favorite
np.random.seed(3) # set seed, for replication
spec_2=d.loc[(d['date']=='2017-10-23') & (d['shop_description']=='Vista') & (d['article_description']=="Local Favorite")].sample(65) # select random 65 cases of local favorite
spec['article_description'] = 'Favorite' # rename label
d.update(spec)# merge it back to data frame

# 2. 10 vegi-burgers where sold as world instead of local world only in Vista canteen
## am 17.11 World to local World
# was not taken into account

###############
# adjust data set
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

#   2017-12-19: in aggregated data set (16)
d.loc[(d['date']=='2017-12-19') & (d['shop_description']=='Grüental') & (d['article_description']=="Kitchen 2"),'article_description'] = 'Local Kitchen'

# 	2017-12-20: in aggregated data set (15)
d.loc[(d['date']=='2017-12-20') & (d['shop_description']=='Vista') & (d['article_description']=="Kitchen 2"),'article_description'] ='Local Kitchen'

# change all other kitchen 1,2,3,4 to kitchen
d['article_description'] = d['article_description'].str.replace("Kitchen \d", "Kitchen")

#############
## adjust data set
## all other meal lines
# 2017-10-09
d.loc[(d['date']=='2017-10-09') & (d['shop_description']=='Vista') & (d['article_description'] == "Local World"),'article_description']='Local Favorite'

# 	2017-10-26
d.loc[(d['date']=='2017-10-26') & (d['article_description']=="Local Favorite"),'article_description'] = 'Local World'

# 	2017-10-30
d.loc[(d['date']=='2017-10-30') & (d['article_description']=="Local World"),'article_description'] = 'Local Favorite'

# 	2017-11-02: only for aggregated data set (1)
d.loc[(d['date']=='2017-11-02') & (d['shop_description']=='Vista') & (d['article_description'] =="Local Favorite"),'article_description'] = 'Local World'

# 	2017-11-03:
d.loc[(d['date']=='2017-11-03') & (d['shop_description']=='Grüental') & (d['article_description'] =="Local Favorite"),'article_description'] = 'Local World'

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

#   2017-12-04: 25 observations in aggregated data set (reason not clear yet)
d.loc[(d['date']=='2017-12-04') & (d['article_description']=="Local World"),'article_description'] = 'Local Favorite'

# 	2017-12-07
d.loc[(d['date']=='2017-12-07') & (d['article_description']=="Local World"),'article_description'] = 'Local Favorite'

# 	2017-12-13
d.loc[(d['date']=='2017-12-13') & (d['article_description']=="Local World"),'article_description'] = 'World'

# 	2017-12-15
d.loc[(d['date']=='2017-12-15') & (d['article_description']=="Local Favorite"),'article_description'] = 'Local World'

# 	2017-12-20: 1 observations in aggregated data set
d.loc[(d['date']=='2017-12-20') & (d['shop_description']=='Vista') & (d['article_description']=="Local Favorite"),'article_description'] = 'Favorite'

### save agg data set
d.to_csv('S:/pools/n/N-IUNR-nova-data/02_kassendaten/02_tilldata_2017/augmented data/data_edit_agg_180802_egel_check.csv',sep=';', index=False)