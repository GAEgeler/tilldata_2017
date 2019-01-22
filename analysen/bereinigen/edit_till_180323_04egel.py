##### edit till data
#### Manuelle Anpassungen an die Kassendaten

# Stand: 2.08.18 // egel    

# see documentation: "anpassungen an datensatz 180502.txt"

# required packages
import seaborn as sns
import numpy as np
import pandas as pd
import random
import os


# load data
os.chdir('S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files')
d= pd.read_csv('filtered_r_180502_egel.csv',sep=';', parse_dates=['trans_date','date'])

# two special cases:
# 1. take 65 random local favorites from data frame and change them to favorite
## am 23.10 Local Favorite to Favorite
random.seed(3) # set seed, for replication
spec=d.loc[(d['date']=='2017-10-23') & (d['shop_description']=='Vista Mensa') & (d['article_description']=="Local Favorite")].sample(65) # select random 65 cases of local favorite
spec['article_description'] = 'Favorite' # rename label
d.update(spec)# merge it back to data frame

# 2. not handled yet
## am 17.11


# adjust data frame according to the description above
## Kitchen
# 	2017-10-09
d.loc[(d['date']=='2017-10-09') & (d['article_description']=="Local Tössfeld"),'article_description'] ='Local Kitchen'

# 	2017-10-17
d.loc[(d['date']=='2017-10-17') & (d['article_description']=="Kitchen 4") & (d['shop_description']=='Grüntal Mensa'),'article_description'] ='Local Kitchen'

# 	2017-10-20
d.loc[(d['date']=='2017-10-20') & (d['shop_description']=='Vista Mensa') & (d['article_description']=="Kitchen 4"),'article_description'] ='Local Kitchen'

# 	2017-11-20
d.loc[(d['date']=='2017-11-20') & (d['article_description']=="Kitchen 2"),'article_description'] ='Local Kitchen'

# 	2017-12-19
d.loc[(d['date']=='2017-12-19') & (d['shop_description']=='Grüntal Mensa') & (d['article_description']=="Kitchen 2"),'article_description'] ='Local Kitchen'

# 	2017-12-20
d.loc[(d['date']=='2017-12-20') & (d['shop_description']=='Vista Mensa') & (d['article_description']=="Kitchen 2"),'article_description'] ='Local Kitchen'


## faults of system, or workers
# 2017-10-09
d.loc[(d['date']=='2017-10-09') & (d['shop_description']=='Vista Mensa') & (d['article_description'] == "Local World"),'article_description']='Local Favorite'

# 	2017-10-26
d.loc[(d['date']=='2017-10-26') & (d['article_description']=="Local Favorite"),'article_description']='Local World'

# 	2017-10-30
d.loc[(d['date']=='2017-10-30') & (d['article_description']=="Local World"),'article_description']='Local Favorite'

# 	2017-11-02
d.loc[(d['date']=='2017-11-02') & (d['shop_description']=='Vista Mensa') & (d['article_description'] =="Local Favorite"),'article_description']='Local World'

# 	2017-11-03
d.loc[(d['date']=='2017-11-03') & (d['shop_description']=='Grüntal Mensa') & (d['article_description'] =="Local Favorite"),'article_description'] = 'Local World'

# 	2017-11-07
d.loc[(d['date']=='2017-11-07') & (d['article_description']=="Local World"),'article_description']='Local Favorite'

# 	2017-11-15
d.loc[(d['date']=='2017-11-15') & (d['article_description']=="Local World"),'article_description']='Local Favorite'

# 	2017-11-16
d.loc[(d['date']=='2017-11-16') & (d['article_description']=="Local World"),'article_description']='Local Favorite'

# 	2017-11-17
d.loc[(d['date']=='2017-11-17') & (d['article_description']=="Local Favorite"),'article_description'] ='Local World'

# 	2017-11-21
d.loc[(d['date']=='2017-11-21') & (d['article_description']=="Local Favorite"),'article_description'] = 'Favorite'

# 	2017-11-22
d.loc[(d['date']=='2017-11-22') & (d['article_description']=="Local Favorite"),'article_description'] = 'Local World'

# 	2017-12-04 
d.loc[(d['date']=='2017-12-04') & (d['article_description']=="Local World"),'article_description'] = 'Local Favorite'

# 	2017-12-07 
d.loc[(d['date']=='2017-12-07') & (d['article_description']=="Local World"),'article_description'] = 'Local Favorite'

# 	2017-12-13
d.loc[(d['date']=='2017-12-13') & (d['article_description']=="Local World"),'article_description'] = 'Favorite'

# 	2017-12-15
d.loc[(d['date']=='2017-12-15') & (d['article_description']=="Local Favorite"),'article_description'] = 'Local World'

# 	2017-12-20
d.loc[(d['date']=='2017-12-20') & (d['shop_description']=='Vista Mensa') & (d['article_description']=="Local Favorite"),'article_description'] = 'Favorite'

### save data
d.to_csv('filtered_r_edit_180502_egel.csv',sep=';', index=False) # not including time filter

