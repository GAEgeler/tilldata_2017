# Datamerge
# stand: 17.04 // egel

import pandas as pd
import os

# First step: load data
# set working directory
os.chdir('S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW')

# load transaction variables
t = pd.read_csv('ZHAW_Transactions_180320.csv',sep=';', encoding = 'latin1').rename(columns=lambda x: x.strip()) # remove withe space at the beginning
t=t[['id','till_id','shop_id','operator_id','trans_date','total_amount','bookkeeping_date','pricelevel_id','Geschlecht','card_num','Geburtsjahr2','Kategorisierung']]
t=t.rename(columns = {'id':'transaction_id','bookkeeping_date':'date','Kategorisierung':'member'})        
t['trans_date'] = pd.to_datetime(t['trans_date']) # change in dateformat

# load shop location
s = pd.read_excel('ZHAW_Shops.xlsx',sep=';', encoding = 'latin1')
s = s.rename(columns=lambda x: x.strip())
s = s.rename(columns={'id':'shop_id','description':'shop_description'})

# load articles data
dt = pd.read_csv('ZHAW_Trans_Articles.csv',sep=';').rename(columns=lambda x: x.strip()) # remove withe space at the beginning
d = dt[['transaction_id','article_id','qty_weight','price']] # use subset of columns

# load article descriptions
a = pd.read_excel('ZHAW_Articles.xlsx',sep=';',encoding='latin1')
a = a.rename(columns={'id':'article_id','description':'article_description','code':'code_description'})
a = a[['article_id','code_description','article_description']]

#load payment levels(rabate)
pl=pd.read_excel('ZHAW_Pricelevels.xlsx')
pl = pl[['id','code','description']].rename(columns={'id':'pricelevel_id','code':'rab_code','description':'rab_description'})

#load price information (empfohlener Kaufpreis) 
pr=pd.read_excel('ZHAW_Prices.xlsx')
pr=pr[['article_id','pricelevel_id','price']].rename(columns={'price':'prop_price'})

#load payment transaction data
pt=pd.read_csv('ZHAW_Trans_Payments.csv', sep=';').rename(columns=lambda x: x.strip())
p=pt[['transaction_id','payment_id','amount']]

#load payment description
os.chdir('S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_testdaten_original/Versand an ZHAW 180103')
pde=pd.read_csv('ZHAW_payments.csv', sep=';').rename(columns={'id':'payment_id', 'code':'payment_code','description' : 'pay_description'})

##############
# Second step: merge data
##############

#merge shop info and transaction (person info) 
tm = pd.merge(t,s[['shop_id','shop_description']],on='shop_id',how='left') # uses use only keys from left frame

# merge zhaw transaction with article transaction
df = pd.merge(tm,d, on='transaction_id',how='left')

# merge article info with all articles 
df1 = pd.merge(df,a,on=['article_id'],how='left')

# merge article info with rabate and proposed price
a2=pd.merge(pr,pl, on='pricelevel_id',how='inner')
df2=pd.merge(df1,a2, on=['article_id','pricelevel_id'], how='left')

# merge payment description with payment data
p = pd.merge(p,pde[['payment_id','pay_description']],on='payment_id',how='left')

# merge t and p
df3 = pd.merge(df2,p,on='transaction_id',how='left')

############
# Third step: subset data
############


# subset only for mensa grüental and vista
data = df3[(df3.shop_description == 'Vista Mensa') | (df3.shop_description == 'Grüntal Mensa')]
data=data[['transaction_id','trans_date','date', 'article_description','code_description','qty_weight', 
           'card_num','Geschlecht','Geburtsjahr2','member', 'rab_description',
           'total_amount','price','amount','prop_price',
           'pay_description','shop_description',]].rename(columns={'code_description':'art_code','rab_description':'rab_descript','amount':'price_payment','price':'price_article'})

os.chdir('S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW')
data.to_csv('joined_180320_05egel.csv',index=False, encoding = 'latin1', sep=';')
