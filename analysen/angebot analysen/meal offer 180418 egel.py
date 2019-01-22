#####
## Menu offer analysis
#####

# status: 20.04 // egel

import pandas as pd
import os

os.chdir("S:/pools/n/N-IUNR-nova-data/06_add_var/01_dokumentation/final")
d=pd.read_csv("menu_inhalt_protein_180420_egel_final.csv",sep=';',parse_dates=['date'], encoding='latin1')


# only one place and without locals
d1=d[(d['article_description']=='Favorite')|(d['article_description']=='Kitchen')|(d['article_description']=='World')&(d['shop_description']=='Vista Mensa')] # is not filtering right, why?
d1=d1[d1['shop_description']=='Vista Mensa'] 

# cross tabs
pd.crosstab(d1['Tier_1'],d1['Milchprodukt_1'],margins=True)
pd.crosstab(d1['Tier_1'],d1['Milchprodukt_1'], normalize='columns',margins=True)

pd.crosstab(d1['Tier_1'],d1['Ei'],margins=True)
pd.crosstab(d1['Tier_1'],d1['Ei'], normalize='columns',margins=True)

pd.crosstab(d1['Tier_1'],d1['Vegi_Protein'],margins=True)
pd.crosstab(d1['Tier_1'],d1['Vegi_Protein'], normalize='columns',margins=True)

pd.crosstab(d1['Tier_1'],d1['Tier_2'],margins=True)
pd.crosstab(d1['Tier_1'],d1['Tier_2'], normalize='columns',margins=True)

pd.crosstab(d1['Tier_1'],d1['Verarbeitet_1'],margins=True)
pd.crosstab(d1['Tier_1'],d1['Verarbeitet_1'], normalize='columns',margins=True)

pd.crosstab(d1['Milchprodukt_1'],d1['Ei'],margins=True)
pd.crosstab(d1['Milchprodukt_1'],d1['Ei'], normalize='columns',margins=True)

pd.crosstab(d1['Milchprodukt_1'],d1['Vegi_Protein'],margins=True)
pd.crosstab(d1['Milchprodukt_1'],d1['Vegi_Protein'], normalize='columns',margins=True)



