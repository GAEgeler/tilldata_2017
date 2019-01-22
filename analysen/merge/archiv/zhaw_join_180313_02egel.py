# Datamerge
# stand: 20.03 // egel

import pandas as pd
import os

# set working directory
os.chdir('S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW')

##### attention; check different merge methods => not alwys inner!!


# load articles data
dt = pd.read_csv('ZHAW_Trans_Articles.csv',sep=';').rename(columns=lambda x: x.strip()) # remove withe space at the beginning
d = dt[['transaction_id','article_id','qty_weight','price','menu_id']] # use subset of columns

#load payment data
pt=pd.read_csv('ZHAW_Trans_Payments.csv', sep=';').rename(columns=lambda x: x.strip())
p=pt[[]]

#load payment description
os.chdir('S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_testdaten_original/Versand an ZHAW 180103')
pde=pd.read_csv('ZHAW_payments.csv', sep=';').rename(columns={'id':'payment_id', 'code':'payment_code','description' : 'pay_description'})

# merge payment description with payment data and after with articles
p = pd.merge(p,pde[['payment_id','pay_description']],on='payment_id',how='inner')
p=pd.merge(dt,p[['transaction_id','pay_description','amount']], on='transaction_id',how='inner')

# load additional transaction variables
t = pd.read_csv('ZHAW_Transactions_180320.csv',sep=';', encoding = 'latin1')
t=t.rename(columns=lambda x: x.strip()) # remove withe space at the beginning
 
t=t[['id','till_id','shop_id','operator_id','trans_date','total_amount','bookkeeping_date','pricelevel_id','Geschlecht','card_num','Geburtsjahr2','Kategorisierung']]
t=t.rename(columns = {'id':'transaction_id','bookkeeping_date'='date','Kategorisierung'='member'})        
t['trans_date'] = pd.to_datetime(t['trans_date']) # change in dateformat

# this results in a loss of 98% of articles (see d.shape before and after merge)
d = pd.merge(t,p[['transaction_id','article_id','qty_weight','price']],on='transaction_id',how='inner')


# get shop name from shop.csv
s = pd.read_excel('ZHAW_Shops.xlsx',sep=';', encoding = 'latin1')
s = s.rename(columns=lambda x: x.strip())
s = s.rename(columns={'id':'shop_id','description':'shop_description'})

# get article descriptions
a = pd.read_excel('ZHAW_Articles.xlsx',sep=';',encoding='latin1')
a = a[['article_id','code','article_description']]
a = a.rename(columns={'id':'article_id','description':'article_description','code':'code_description'})

# merge missing info: article and shop 
d = pd.merge(d,a[['article_id','article_description']],on='article_id',how='left')
d = pd.merge(d,s[['shop_id','shop_description']],on='shop_id',how='left') # uses use only keys from left frame

# subset only for mensa grüental and vista
data = d[(d.shop_description == 'Vista Mensa') | (d.shop_description == 'Grüntal Mensa')]
data.to_csv('180320_joined_02egel.csv',index=False, encoding = 'latin1', sep=';')
