
#### Manuelle Anpassungen an die Kassendaten:

# Stand: 23.03 // egel    
    
#==============================================================================
# Local Kitchen

# # Am 9.10 Grüental: Local Kitchen = Local Tössfeld
# # Am 20.11 Grüental & Vista: Local Kitchen = Kitchen 2
# # Am 17.11 gemäss Michael Krauer im Vista Local World (Vegi Burger, 10) => Tauchen nicht in den Kassendaten auf => Nachfragen wohin?
# # Am 19.12 Grüental: Local Kitchen = Kitchen 2
# # Am 20.12 Vista: Local Kitchen = Kitchen 2

#==============================================================================
# Tippfehler von Mitarbeitenden

# # 	2017-10-09  	Local World	Vista Mensa	>		Taucht nicht in Dokumentation auf -> evtl Fehler durch Kassenfrau > wechsel zu Local Favorite 
# # 	2017-10-26  	Local Favorite	Grüntal Mensa >		Taucht nicht in Dokumentation auf > wechsel zu Local 
# # 	2017-10-26  	Local Favorite	Vista Mensa	>		Taucht nicht in Dokumentation auf
# # 	2017-10-30  	Local World	Grüntal Mensa	 >		Taucht nicht in Dokumentation auf
# # 	2017-11-02  	Local Favorite	Vista Mensa	>		Taucht nicht in Dokumentation auf
# # 	2017-11-03  	Local Favorite	Grüntal Mensa	 >		Taucht nicht in Dokumentation auf
# # 	2017-11-07  	Local World	Grüntal Mensa	 >		Stimmt nicht mit Zahlen von Michael überein
# # 	2017-11-07  	Local World	Vista Mensa	>		Stimmt nicht mit Zahlen von Michael überein
# # 	2017-11-15  	Local World	Grüntal Mensa	 >		Taucht nicht in Dokumentation auf
# # 	2017-11-16  	Local World	Grüntal Mensa	 >		Taucht nicht in Dokumentation auf
# # 	2017-11-17  	Local Favorite	Grüntal Mensa	 >		Taucht nicht in Dokumentation auf
# # 	2017-11-21  	Local Favorite	Vista Mensa	>		Taucht nicht in Dokumentation auf
# # 	2017-11-22  	Local Favorite	Grüntal Mensa	 >		Taucht nicht in Dokumentation auf
# # 	2017-12-07  	Local World	Grüntal Mensa	 >		Taucht nicht in Dokumentation auf
# # 	2017-12-13  	Local World	Grüntal Mensa	 >		Taucht nicht in Dokumentation auf
# # 	2017-12-15  	Local Favorite	Grüntal Mensa	 >		Taucht nicht in Dokumentation auf
# # 	2017-12-20  	Local Favorite	Vista Mensa	>		Taucht nicht in Dokumentation auf
 
#==============================================================================

import pandas as pd
import os


# load data
os.chdir('S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files')
d = pd.read_csv('name.csv', sep=';', encoding='latin1',parse_dates=['date'])

test4.date=pd.to_datetime(test4['date']) # if not in read_csv, do it here


hal=test4.loc[test4.date=='2017-10-09', 'article_description'].replace("Local World","Local Favorite")# works however how to put back in dataframe? => merge or join as possibility!

lsf=test4[(test4.date == "2017-10-09") | (test4.article_description== 'Local World')]# try to subset right, problem with dates!!
