##############
# Kassendaten: all
#############

###
# Stand: 27.3.18 // egel
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
trans_dat <- read_delim("ZHAW_Transactions_180320.csv",delim=';')
trans_dat <- select(trans_dat,id,till_id,shop_id,operator_id,trans_date,total_amount,bookkeeping_date,pricelevel_id,Geschlecht,card_num,Geburtsjahr2,Kategorisierung)
trans_dat <- rename(trans_dat, transaction_id = id, date = bookkeeping_date, member = Kategorisierung)

# location of shop
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW")
shop <- read_excel("ZHAW_Shops.xlsx")# good for small datasets
shop <- select(shop,id,code, description)
shop <- rename(shop,shop_id = id, shop_code=code, shop_description = description)

# load transaction articles
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW")
df_a <- read_delim("ZHAW_Trans_Articles.csv",delim = ';') # throw back errors, however reading it with csv2 same results 
df_a <- select(df_a, transaction_id, article_id, qty_weight, price)
df_a <- rename(df_a, price_article = price)

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

# load trans payments
df_p <- read_delim("ZHAW_Trans_Payments.csv",delim=';')
df_p <- select(df_p, transaction_id, payment_id, amount)

# load payments information
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_testdaten_original/Versand an ZHAW 180103")
df_pay <- read_csv2("ZHAW_payments.csv")
df_pay <- select(df_pay,id, code, description)
df_pay <- rename(df_pay, payment_id=id, payment_code=code, pay_description = description)


#####################
################# Second Step: SQL Merging
#####################

## for information see: https://communities.sas.com/t5/SAS-Procedures/Proc-SQL-equiv-of-Merge-If-A-or-B/td-p/255720 

# merge transactions and shops

df <- left_join(trans_dat, shop, by='shop_id') # another possiblity => see dokumentation, semi_join could be interessting

# merge transaction and trans_article
# df <- base::merge(df1,df_a, by.x="transaction_id", all.x=T)
df1 <- inner_join(df,df_a, by="transaction_id") # same, however faster

# merge df1 with atricle information
df2 <- left_join(df1, df_ai, by="article_id")

# merge df1 with price_level and article prices information

price_inf <- inner_join(df_ap, price_lev, by="pricelevel_id")
df3 <- left_join(df2, price_inf, by=c("article_id", "pricelevel_id")) # to avoid double transaction id's, take more than one key to join

# merge data with price_levels and payment desciption
pay_inf <- left_join(df_p,df_pay, by="payment_id")

df4 <- left_join(df3,pay_inf, by="transaction_id")

write.csv2(df4,"ZHAW_transactions_170327_egel.csv") # contains all transaction over ZHAW


#####################
################# last Step: clean data and subset
#####################

# subset for grüental and vista + filter only important variables

df_dat <- subset(df4,shop_description == "Grüntal Mensa" | shop_description== "Vista Mensa")
df_dat <- select(df_dat, transaction_id,trans_date,
                 total_amount,date,Geschlecht,
                 card_num,Geburtsjahr2,member,price_descript
                 shop_description,amount,pay_description,
                 qty_weight,art_code,art_description,
                 pay_description)


####----------------
## change variables
# set #NV to lehrnender in variable member
# as factors
df_dat$member[df_dat$member == "#NV"] <- "Lernende"
df_dat$member <- as.factor(df_dat$member)

# as factor
df_dat$Geschlecht <- as.factor(df_dat$Geschlecht)
df_dat$Geschlecht[df_dat$Geschlecht == "0"] <- NA # replace "0" with NA
df_dat$Geschlecht[df_dat$Geschlecht == "#NV"] <- NA

#as numeric
df_dat$total_amount <- as.numeric(df_dat$total_amount)

# as integer
df_dat$Geburtsjahr2[df_dat$Geburtsjahr2 == "#NV"] <- NA
df_dat$Geburtsjahr2 <- as.numeric(as.character(df_dat$Geburtsjahr2)) 

# as date
df_dat$trans_date = parse_datetime(df_dat$trans_date)
df_dat$date <- parse_datetime(df_dat$date)

# as integer
# df_dat$card_num <- as.integer(df_dat$card_num)

# drop useless levels
df_dat <- droplevels(df_dat)

# save last data frame
write.csv2(df_dat,"data_180320_03egel.csv")
write_rds(df_dat,"data_180320_03egel.Rda")
