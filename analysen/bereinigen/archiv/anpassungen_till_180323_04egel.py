
#### Manuelle Anpassungen an die Kassendaten:

# Stand: 2.05.18 // egel    


#==============================================================================
# Spezialfälle
# # am 23.10 wurden in der Mensa Vista ausversehen 65 Favorites als Local Favorites getippt
# # Am 17.11 gemäss Michael Krauer im Vista Local World (Vegi Burger, 10) => Tauchen nicht in den Kassendaten auf => Nachfragen wohin?

#==============================================================================
# Local Kitchen => wurde von hand angepasst

# # Am 9.10 Grüental: Local Kitchen = Local Tössfeld > wechsel zu Local Kitchen
# # am 17.10 Grüental: local Kitchen = Kitchen 4  > wechsel zu Local Kitchen
# # Am 20.10 Vista: local Kitchen = Kitchen 4 > wechsel zu Local Kitchen
# # Am 20.11 Grüental & Vista: Local Kitchen = Kitchen 2 > wechsel zu Local Kitchen
# # Am 19.12 Grüental: Local Kitchen = Kitchen 2 > wechsel zu Local Kitchen
# # Am 20.12 Vista: Local Kitchen = Kitchen 2 > wechsel zu Local Kitchen

#==============================================================================
# Tippfehler von Mitarbeitenden => wurde von hand angepasst

# # 	2017-10-09  	Local World	Vista Mensa	>		Taucht nicht in Dokumentation auf  > wechsel zu Local Favorite (1x)
# # 	2017-10-26  	Local Favorite	Grüntal Mensa >		Taucht nicht in Dokumentation auf > wechsel zu Local World (2x)
# # 	2017-10-26  	Local Favorite	Vista Mensa	>		Taucht nicht in Dokumentation auf > wechsel zu Local World (1x)
# # 	2017-10-30  	Local World	Grüntal Mensa	 >		Taucht nicht in Dokumentation auf > wechsel zu Local Favorite (1x)
# # 	2017-11-02  	Local Favorite	Vista Mensa	>		Taucht nicht in Dokumentation auf > wechsel zu Local World (plus in Dokumentation 09_egel angepasst) (1x)
# # 	2017-11-03  	Local Favorite	Grüntal Mensa	 >		Taucht nicht in Dokumentation auf > wechsel zu Local World (1x)
# # 	2017-11-07  	Local World	Grüntal Mensa	 >		Stimmt nicht mit Zahlen von Michael überein > wechsel von Local World zu Local Favorite (Doku von Michael (als Local World), falsch eingetragen) (1x)
# # 	2017-11-07  	Local World	Vista Mensa	>		Stimmt nicht mit Zahlen von Michael überein > wechsel von Local World zu Local Favorite (Doku von Michael (als Local World), falsch eingetragen) (5x)
# # 	2017-11-15  	Local World	Grüntal Mensa	 >		Taucht nicht in Dokumentation auf  > wechsel zu Local Favorite (2x)
# # 	2017-11-16  	Local World	Grüntal Mensa	 >		Taucht nicht in Dokumentation auf > wechsel zu Local Favorite (1x)
# # 	2017-11-17  	Local Favorite	Grüntal Mensa	 >		Taucht nicht in Dokumentation auf > wechsel zu Local World (1x)
# # 	2017-11-21  	Local Favorite	Vista Mensa	>		Taucht nicht in Dokumentation auf > wechsel zu Favorite (keine Locals an diesem Tag) (2x)
# # 	2017-11-22  	Local Favorite	Grüntal Mensa	 >		Taucht nicht in Dokumentation auf > wechsel zu Local World (2x)
# # 	2017-12-04  	Local World	Vista Mensa	 >		Taucht nicht in Dokumentation auf > wechsel zu Local Favorite (26x)
# # 	2017-12-07  	Local World	Grüntal Mensa	 >		Taucht nicht in Dokumentation auf > wechsel zu Local Favorite (1x)
# # 	2017-12-13  	Local World	Grüntal Mensa	 >		Taucht nicht in Dokumentation auf  > wechsel zu World (keine Locals an diesem Tag) (1x)
# # 	2017-12-15  	Local Favorite	Grüntal Mensa	 >		Taucht nicht in Dokumentation auf > wechsel zu Local World (1x)
# # 	2017-12-20  	Local Favorite	Vista Mensa	>		Taucht nicht in Dokumentation auf  > wechsel zu Favorite (an diesem Tag nur Local Kitchen im Vista) (1x)
 
#==============================================================================

import seaborn as sns
import numpy as np
import pandas as pd
import random
import os


# load data
os.chdir('S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files')
# d = pd.read_csv('data_r_timefilter_180403_egel.csv', sep=';', encoding='latin1',parse_dates=['trans_date','date'])
d= pd.read_csv('data_r_nofilter_180404_egel.csv',sep=';', parse_dates=['trans_date','date']).drop('X1', axis=1) # latin1 crates wired signs

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

# save data
#==============================================================================
# d.to_csv('data_r_time_change_180403_egel.csv',sep=';', encoding ='latin1', index=False)
# d.to_csv('data_r_time_change_180403_egel.csv',sep=';', encoding ='latin1').drop(['Unnamed: 0'],axis=1)
# #==============================================================================

d.to_csv('data_r_change_180502_egel.csv',sep=';', index=False) # not including time filter
