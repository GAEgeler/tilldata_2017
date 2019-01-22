
#### Manuelle Anpassungen an die Kassendaten:

# Stand: 28.03 // egel    
    
#==============================================================================
# Local Kitchen => wurde von hand angepasst

# # Am 9.10 Grüental: Local Kitchen = Local Tössfeld > wechsel zu Local Kitchen
# # Am 20.11 Grüental & Vista: Local Kitchen = Kitchen 2 > wechsel zu Local Kitchen
# # Am 17.11 gemäss Michael Krauer im Vista Local World (Vegi Burger, 10) => Tauchen nicht in den Kassendaten auf => Nachfragen wohin?
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
# # 	2017-12-07  	Local World	Grüntal Mensa	 >		Taucht nicht in Dokumentation auf > wechsel zu Local Favorite (1x)
# # 	2017-12-13  	Local World	Grüntal Mensa	 >		Taucht nicht in Dokumentation auf  > wechsel zu World (keine Locals an diesem Tag) (1x)
# # 	2017-12-15  	Local Favorite	Grüntal Mensa	 >		Taucht nicht in Dokumentation auf > wechsel zu Local World (1x)
# # 	2017-12-20  	Local Favorite	Vista Mensa	>		Taucht nicht in Dokumentation auf  > wechsel zu Favorite (an diesem Tag nur Local Kitchen im Vista) (4x)
 
#==============================================================================

import seaborn as sns
import numpy as np
import pandas as pd
import os


# load data
os.chdir('C:/Users/GianAndrea/Desktop')
os.chdir('S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files')
d = pd.read_csv('filtered_data_dub_180320_02egel.csv', sep=';', encoding='latin1',parse_dates=['trans_date','date'])

# adjust data frame according to the description above
# 2017-10-09
d.loc[(d['date']=='2017-10-09') & (d['art_description'].str.endswith("Local World")),'art_description']='Local Favorite'

# 	2017-10-26
d.loc[(d['date']=='2017-10-26') & (d['art_description'].str.endswith("Local Favorite")),'art_description']='Local World'

# 	2017-10-30
d.loc[(d['date']=='2017-10-30') & (d['art_description'].str.endswith("Local World")),'art_description']='Local Favorite'

# 	2017-11-02
d.loc[(d['date']=='2017-11-02') & (d['shop_description']=='Vista Mensa') & (d['art_description'].str.endswith("Local Favorite")),'art_description']='Local World'

# 	2017-11-03
d.loc[(d['date']=='2017-11-03') & (d['shop_description']=='Grüental Mensa') & (d['art_description'].str.endswith("Local Favorite")),'art_description'] = 'Local World'

# 	2017-11-07 another code, but seems not to work
d.loc[(d['date']=='2017-11-07') & (d['art_description']=="Local World")]['art_description']='Local Favorite'

# 	2017-11-15
d[(d['date']=='2017-11-15') & (d['art_description']=="Local World")]['art_description']='Local Favorite'

# 	2017-11-16
d[(d['date']=='2017-11-16') & (d['art_description']=="Local World")]['art_description']='Local Favorite'

# 	2017-11-17
d.loc[(d['date']=='2017-11-17') & (d['art_description']=="Local Favorite")]['art_description'] ='Local World'

# 	2017-11-21
d.loc[(d['date']=='2017-11-21') & (d['art_description']=="Local Favorite")]['art_description'] = 'Favorite'

# 	2017-11-22
d.loc[(d['date']=='2017-11-22') & (d['art_description']=="Local Favorite")]['art_description'] = 'Local World'

# 	2017-12-07 
d.loc[(d['date']=='2017-12-07') & (d['art_description']=="Local World")]['art_description'] = 'Local Favorite'

# 	2017-12-13
d.loc[(d['date']=='2017-12-13') & (d['art_description']=="Local World")]['art_description'] = 'Favorite'

# 	2017-12-15
d.loc[(d['date']=='2017-12-15') & (d['art_description']=="Local Favorite")]['art_description'] = 'Local World'

# 	2017-12-20
d.loc[(d['date']=='2017-12-20') & (d['art_description']=="Local Favorite")]['art_description']# = 'Favorite'

