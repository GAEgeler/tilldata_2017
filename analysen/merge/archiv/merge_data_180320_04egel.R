##############
# Kassendaten: all
#############

###
# Stand: 17.4.18 // egel
###

# required packages

library(dplyr)
library(feather)
library(readr)
library(readxl)
#library(tidyverse)

#####################
################# First Step: Loading data
#####################


## transactions of ZHAW
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW")
trans_dat <- read_delim("ZHAW_Transactions_180320.csv",delim=';',col_names = T,trim_ws = T)# trim_ws removes white space
trans_dat <- select(trans_dat,id, till_id ,  shop_id ,  operator_id ,  trans_date ,  total_amount ,  bookkeeping_date ,  pricelevel_id , Geschlecht,card_num,Geburtsjahr2, Kategorisierung)
trans_dat <- rename(trans_dat, transaction_id = id, till_id=  till_id , operator_id=  operator_id , total_amount =   total_amount , date =   bookkeeping_date , member = Kategorisierung)

# location of shop
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW")
shop <- read_excel("ZHAW_Shops.xlsx")# good for small datasets
shop <- select(shop,id,code, description)
shop <- rename(shop,shop_id = id, shop_code=code, shop_description = description)

# load transaction articles
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW")
df_a <- read_delim("ZHAW_Trans_Articles.csv",delim = ';',trim_ws = T) # throw back errors, however reading it with csv2 same results 
df_a <- select(df_a, transaction_id,   article_id  ,   qty_weight ,   price )
df_a <- rename(df_a, article_id=  article_id  , qty_weight=  qty_weight , price_article =   price )

# load article information
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW")
df_ai <-  read_excel("ZHAW_Articles.xlsx")
df_ai <- select(df_ai, id, code, description)
df_ai <- rename(df_ai, article_id=id, art_code = code, art_description = description)

# load price levels
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW")
price_lev <- read_excel("ZHAW_Pricelevels.xlsx")
price_lev <- select(price_lev,id, code, description )
price_lev <- rename(price_lev, pricelevel_id=id, price_code = code,price_descript = description)

# load article price information
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW")
df_ap <- read_excel("ZHAW_prices.xlsx") # conversts some variables into characters => reasons?
df_ap <- select(df_ap, article_id, pricelevel_id,price)
df_ap <- rename(df_ap, single_price = price)
df_ap$single_price <- as.numeric(df_ap$single_price)
df_ap$pricelevel_id <- as.integer(df_ap$pricelevel_id)
df_ap$article_id <- as.integer(df_ap$article_id)

# load trans payments => difference to ZHAW_payments, same variables however df_p has more entries resp. observations
df_p <- read_delim("ZHAW_Trans_Payments.csv",delim=';',trim_ws = T)
df_p <- select(df_p, transaction_id,   payment_id ,   amount )
df_p <- rename(df_p, payment_id=  payment_id , price_p=  amount  )

# load payments however same information as above
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW")
df_p2 <- read_delim("ZHAW_payments.csv",delim = ';',trim_ws = T)
df_p2 <- select(df_p2,transaction_id,   payment_id ,   amount )
df_p2 <- rename(df_p2, payment_id=  payment_id , price_p=  amount  )

difference=anti_join(df_p, df_p2, by="transaction_id") # ask Bruno, however take df_p

# load payments information
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_testdaten_original/Versand an ZHAW 180103")
df_pay <- read_delim("ZHAW_payments.csv", delim = ';',trim_ws = T)
df_pay <- select(df_pay,id, code, description)
df_pay <- rename(df_pay, payment_id=id, payment_code=code, pay_description = description)


#####################
################# Second Step: SQL Merging
#####################

## for information see: https://communities.sas.com/t5/SAS-Procedures/Proc-SQL-equiv-of-Merge-If-A-or-B/td-p/255720 


# merge transactions and shops
df <- left_join(trans_dat, shop, by='shop_id') # another possiblity => see dokumentation, semi_join could be interessting

# merge transaction and trans_article
df1 <- left_join(df,df_a, by="transaction_id") 
# df1_ <- inner_join(df,df_a, by="transaction_id") # same as inner_join, why?

# merge df1 with atricle information
df2 <- left_join(df1, df_ai, by="article_id")

# merge df1 with price_level and article prices information
price_inf <- inner_join(df_ap, price_lev, by="pricelevel_id")
df3 <- left_join(df2, price_inf, by=c("article_id", "pricelevel_id")) # to avoid double transaction id's, take more than one key to join

# merge data with price_levels and payment desciption
pay_inf <- left_join(df_p,df_pay, by="payment_id")

df4 <- left_join(df3,pay_inf, by="transaction_id") # some new observations came along => why

write_delim(df4,"ZHAW_transactions_170419_egel.csv",delim = ';') # contains all transaction over ZHAW


#####################
################# last Step: clean data and subset
#####################

# subset for grüental and vista + filter only important variables

df_dat <- subset(df4,shop_description == "Grüntal Mensa" | shop_description== "Vista Mensa")
df_dat <- select(df_dat, transaction_id,trans_date,date,art_description, art_code, qty_weight, 
                 card_num, Geschlecht,Geburtsjahr2,member,price_descript,
                 total_amount,price_article,price_p,single_price, pay_description,shop_description)
df_dat <- rename(df_dat,article_description=art_description,rab_descript=price_descript, prop_price=single_price, price_payment=price_p)


####----------------
## change variables
# set #NV to lehrnender in variable member
df_dat$member[df_dat$member == "#NV"] <- "Lernende"


# change 0, NV and NULL to NA
df_dat$Geschlecht[df_dat$Geschlecht == "0"] <- NA # replace "0" with NA
df_dat$Geschlecht[df_dat$Geschlecht == "#NV"] <- NA
df_dat$Geschlecht[df_dat$Geschlecht == "NULL"] <- NA
df_dat$card_num[df_dat$card_num == "NULL"] <- NA
df_dat$member[df_dat$member == "NULL"] <- NA
df_dat$Geburtsjahr2[df_dat$Geburtsjahr2 == "#NV"] <- NA
df_dat$Geburtsjahr2[df_dat$Geburtsjahr2 == "NULL"] <- NA

# as integer
df_dat$card_num <- as.integer(df_dat$card_num)
df_dat$Geburtsjahr2 <- as.integer(df_dat$Geburtsjahr2) 

# drop useless levels
df_dat <- droplevels(df_dat)

# save last data frame
write_delim(df_dat,"data_180320_05egel.csv",delim=';')
# write_rds(df_dat,"data_180320_04egel.Rda")
