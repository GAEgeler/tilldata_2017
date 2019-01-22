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
################# 1. Step
#####################

### go trough smoothly!!
### tripple check everything => problems with double transaction id's => try another method


## load data
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW")
# transactions of ZHAW
trans_dat <- read_excel("ZHAW_Transactions.xlsx")# takes ages, better read_csv
trans_dat <- read_csv2("ZHAW_Transactions_180320.csv")
# location
shop <- read_excel("ZHAW_Shops.xlsx")# good for small datasets

# select and rename variables

trans_dat <- select(trans_dat,id,till_id,shop_id,operator_id,trans_date,total_amount,bookkeeping_date,pricelevel_id,Geschlecht,card_num,Geburtsjahr2,Kategorisierung)
trans_dat <- rename(trans_dat, transaction_id = id, date = bookkeeping_date, member = Kategorisierung)
shop <- select(shop,id,code, description)
shop <- rename(shop,shop_id = id, shop_code=code, shop_description = description)

# merge transactions, tills and shops

df <- merge(trans_dat, shop, by="shop_id") # inner join
df1 <- inner_join(trans_dat, shop, by='shop_id') # another possiblity => see dokumentation, semi_join could be interessting

## change variables
# set #NV to lehrnender in variable member
# as factors
df$member[df$member == "#NV"] <- "Lernende"
df$member <- as.factor(df$member)

# as factor
df$Geschlecht <- as.factor(df$Geschlecht)
df$Geschlecht[df$Geschlecht == "0"] <- NA # replace "0" with NA
df$Geschlecht[df$Geschlecht == "#NV"] <- NA

#as numeric
df$total_amount <- as.numeric(df$total_amount)

# as integer
df$Geburtsjahr2[df$Geburtsjahr2 == "#NV"] <- NA
df$Geburtsjahr2 <- as.numeric(as.character(df$Geburtsjahr2)) 

# as date
df$trans_date = parse_datetime(df$trans_date)
df$date <- parse_datetime(df$dat)
# drop useless levels
df <- droplevels(df)

# save data frame
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files")
write_rds(df, "transactions_180320_02egel.Rda")

#####################
################# 2. Step
#####################

# load trans payments
df_p <- read_csv2("ZHAW_Trans_Payments.csv")
df_p <- select(df_p, transaction_id, payment_id, amount)


# load payments information
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_testdaten_original/Versand an ZHAW 180103")
df_pay <- read_csv2("ZHAW_payments.csv")
df_pay <- select(df_pay,id, code, description)
df_pay <- rename(df_pay, payment_id=id, payment_code=code, pay_description = description)

# merge data
df_p1 <- merge(df_p,df_pay, by="payment_id",all.x=T)

setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW")
write_rds(df_p1,"ZHAW_trans_Payments_180320_egel.Rda")
write_feather(df_p1,"ZHAW_trans_Payments_180320_02egel.feather")

#####################
################# 3. Step
#####################

# load transaction articles
df_a <- read_csv2("ZHAW_Trans_Articles.csv") # throw back errors, however reading it with csv2 same results 
df_a <- select(df_a, transaction_id, article_id, qty_weight, price)
df_a <- rename(df_a, price_article = price)

# load article price information
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW")
df_ap <- read_excel("ZHAW_prices.xlsx") # conversts some variables into characters => reasons?
df_ap <- select(df_ap, article_id, pricelevel_id,price)
df_ap <- rename(df_ap, single_price = price)
df_ap$single_price <- as.numeric(df_ap$single_price)
df_ap$pricelevel_id <- as.integer(df_ap$pricelevel_id)
df_ap$article_id <- as.integer(df_ap$article_id)

# load article information
df_ai <-  read_excel("ZHAW_Articles.xlsx")
df_ai <- select(df_ai, id, code, description)
df_ai <- rename(df_ai, article_id=id, art_code = code, art_description = description)

# load price levels
price_lev <- read_excel("ZHAW_Pricelevels.xlsx")
price_lev <- select(price_lev,id, code, description )
price_lev <- rename(price_lev, pricelevel_id=id, price_code = code,price_descript = description)

# merge price article and price_levels and article information

df_ <- inner_join(df_ap, price_lev, by='pricelevel_id')
df_ <- merge(df_ap, price_lev, by="pricelevel_id")

df_2 <- merge(df_,df_ai, by ="article_id")
df_2 <- inner_join(df_,df_ai, by='article_id')

# merge rest article transaction with information, price information

df_a2 <- base::merge(df_a,df_2, by.x="article_id", all.x=T) # problems with merging! id's are showing up multiple times
df_a2_test <- semi_join(df_a, df_2, by = 'article_id') # should be the right join option, however dont takes in the second datafram

setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files")
write_feather(df_a2,"ZHAW_trans_articles_180320_02egel.feather")

#####################
################# 4. Step
#####################

# load payment data
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files")
df_p <- read_feather("ZHAW_trans_Payments_180320_egel.feather")
df_a <- read_feather("ZHAW_trans_articles_180320_02egel.feather")
df_t <- read_rds("transactions_180320_02egel.Rda")


# merge payment and articles
#one method (faster)
df_fin <- merge(df_t, df_a, by="transaction_id", all.x=T)
df_fin_  <- left_join(df_t, df_a, by="transaction_id") # problems that id's shows up multiple times => why?

df_fin1 <- merge(df_fin, df_p, by = "transaction_id", all.x = T)
df_fin1_test <- left_join(df_fin, df_p, by="transaction_id")


setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files")
write_feather(df_fin1,"ZHAW_transactions_170320_03egel.feather")
write_csv(df_fin,"ZHAW_transactions_170320_03egel.csv")

#####################
################# 5. Step
#####################

# subset for grüental and vista + filter only important variables

df_dat <- subset(df_fin1,df_fin1$shop_description == "Grüntal Mensa" | df_fin1$shop_description== "Vista Mensa")
df_dat <- select(df_dat, transaction_id,trans_date,
               total_amount,date,Geschlecht,
               card_num,Geburtsjahr2,member,
               shop_description,amount,pay_description,
               qty_weight,art_code,art_description,
               pay_description)

# # exclude double payment methods, in particular payed with voucher (python handle that better)
# seted("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/validation")
# diff <- read.table("difference_180227_egel.txt") # for controll
# testdata$trans_date <- as.POSIXct(testdata$trans_date) # to use dplyr date need to be changed
# testdata <- filter(testdata, !(grepl("Gutschein",testdata$pay_descript))) # delets 11 cases, all 9 out of diff and 2 which do not matter

write.csv2(df_dat,"data_180320_02egel.csv")
write_rds(df_dat,"data_180320_02egel.Rda")
