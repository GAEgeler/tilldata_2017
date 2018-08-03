##############
# merge tilldata 2017
#############

###
# Stand: 2.8.18 // egel
###

# required packages

pack <- c("dplyr", "readr", "readxl", "here")
lapply(pack, function(x){do.call("library",list(x))})


#####################
################# First Step: Loading data
#####################

# all transactions ZHAW 2.Okt 2017 till 22. Dec 2017
trans_dat <- read_delim(here("raw data", "ZHAW_Transactions_180320.csv"), delim = ';',col_names = T,trim_ws = T) %>% # trim_ws removes white space
    select(id, till_id, shop_id, operator_id, trans_date, total_amount, bookkeeping_date, pricelevel_id, Geschlecht, card_num, Geburtsjahr2, Kategorisierung) %>%
    rename(transaction_id = id, till_id = till_id , operator_id = operator_id , total_amount = total_amount , date = bookkeeping_date , member = Kategorisierung)

# shop location
shop <- read_excel(here("raw data", "ZHAW_Shops.xlsx")) %>% # good for small datasets
    select(id,code, description) %>%
    rename(shop_id = id, shop_code = code, shop_description = description)

# load all acrticle transaction
trans_art <- read_delim(here("raw data", "ZHAW_Trans_Articles.csv"), delim = ';',trim_ws = T) %>% # throw back errors, however reading it with csv2 same results 
    select(transaction_id, article_id, qty_weight, price ) %>%
    rename(article_id = article_id, qty_weight = qty_weight, price_article = price )

# load article information => names of articles
art_info <-  read_excel(here("raw data", "ZHAW_Articles.xlsx")) %>%
    select(id, code, description) %>%
    rename(article_id = id, art_code = code, art_description = description)

# load price levels => rabatt information
price_lev <- read_excel(here("raw data", "ZHAW_Pricelevels.xlsx")) %>%
    select(id, code, description) %>%
    rename(pricelevel_id = id, price_code = code, price_descript = description)

# load article price information
art_price <- read_excel(here("raw data", "ZHAW_prices.xlsx"), sheet = 1, col_names = T) %>% # conversts some variables into characters => in the excel file is everything stored as text
    select(article_id, pricelevel_id ,price) %>%
    rename(single_price = price) %>%
    mutate(single_price = as.numeric(.$single_price), # change format of three variables
           pricelevel_id = as.integer(.$pricelevel_id),
           article_id = as.integer(.$article_id))

# load all payments transactions => there are two files which differ (ZHAW_trans_payments > ZHAW_payments)
# one guess is, that Bruno names the file wrong, because ZHAW_payments should include payment information e.g. payed cash or with card
trans_pay <- read_delim(here("raw data", "ZHAW_Trans_Payments.csv"), delim = ';', trim_ws = T) %>%
    select(transaction_id, payment_id, amount) %>%
    rename(payment_id = payment_id, price_p = amount)

# load payments of ZHAW and check differences
trans_pay2 <- read_delim(here("raw data", "ZHAW_payments_180313.csv"), delim = ';', trim_ws = T) %>%
    select(transaction_id, payment_id, amount) %>%
    rename(payment_id = payment_id, price_p = amount)

# check difference
df_diff <- anti_join(trans_pay, trans_pay2, by = "transaction_id")

# load payments information
pay_method <- read_delim(here("raw data", "ZHAW_payments_171214.csv"), delim = ';',trim_ws = T) %>%
    select(id, code, description) %>%
    rename(payment_id = id, payment_code = code, pay_description = description)

#####################
################# Second Step: SQL Merging
#####################

## for information see: https://communities.sas.com/t5/SAS-Procedures/Proc-SQL-equiv-of-Merge-If-A-or-B/td-p/255720 

# merge transactions and shops
df <- inner_join(trans_dat, shop, by = 'shop_id') # another possiblity => see dokumentation, semi_join could be interessting

# merge transaction and trans_article
df1 <- inner_join(df, trans_art, by = "transaction_id") 

# merge df1 with atricle information
df2 <- inner_join(df1, art_info, by = "article_id")

# merge df1 with price_level and article prices information
price_inf <- inner_join(art_price, price_lev, by = "pricelevel_id")
df3 <- left_join(df2, price_inf, by = c("article_id", "pricelevel_id")) # to avoid double transaction id's, take more than one key to join

# merge data with price_levels and payment desciption
pay_inf <- left_join(trans_pay, pay_method, by = "payment_id") %>%
    rename(total_amount = price_p) # price_p should be the total payment per transaction (however not the same as total_amount of trans_dat)
df4 <- left_join(df3, pay_inf, by = "transaction_id", "total_amount") # some new observations came along => reason is pay_description (some entries are now double e.g. 2373186)

write_delim(df4, here("raw data", "data_trans_180802_egel.csv"), delim = ';') # contains all transaction over ZHAW
