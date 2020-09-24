# merge tilldata 2017----------


###
# state: december 2019
# author: gian-Andrea egeler
###

#version control
checkpoint::checkpoint("2019-12-06")


y# required packages
pack <- c("dplyr", "readr", "readxl", "stringr", "here") # attention here and lubridate has same functions here()
lapply(pack, function(x){do.call("library",list(x))})


#### First Step: Loading all data sets: all transactions ZHAW 2.Okt 2017 till 22. Dec 2017--------
# shop location
till <- read_xlsx(here("raw data/ZHAW_Tills.xlsx")) %>% # good for small datasets
    select(id, description) %>%
    rename(till_id = id, shop_description = description) %>%
    mutate(shop_description = str_replace(.$shop_description, " .*", ""), # take only first word of shop_description
           till_id = as.integer(.$till_id)) # change string to integer

# load all acrticle transaction
trans_art <- read_delim(here("raw data/ZHAW_Trans_Articles.csv"), delim = ';', trim_ws = T, na = "NULL") %>% # throw back errors: becaise of NULL string (use na = "NULL") => can be ignored (only vat_id and vat_percent are affected)
    select(transaction_id, article_id, qty_weight, price ) %>%
    rename(article_id = article_id, qty_weight = qty_weight, price_article = price )

# load article information => names of articles
art_info <-  read_excel(here("raw data/ZHAW_Articles.xlsx")) %>%
    select(id, code, description) %>%
    rename(article_id = id, art_code = code, art_description = description) %>%
    mutate(article_id = as.integer(.$article_id))

# load price levels => rabatt information
price_lev <- read_excel(here("raw data/ZHAW_Pricelevels.xlsx")) %>%
    select(id, code, description) %>%
    rename(pricelevel_id = id, price_code = code, price_descript = description)

# load article price information
art_price <- read_excel(here("raw data/ZHAW_prices.xlsx"), sheet = 1, col_names = T) %>% # conversts some variables into characters => in the excel file is everything stored as text
    select(article_id, pricelevel_id ,price) %>%
    rename(single_price = price) %>%
    mutate(single_price = as.numeric(.$single_price), # change format of three variables
           pricelevel_id = as.integer(.$pricelevel_id),
           article_id = as.integer(.$article_id))

# load all payments transactions => there are two files which differ (ZHAW_trans_payments > ZHAW_payments)
# one guess is, that Bruno named the file wrong, because ZHAW_payments should include payment information e.g. payed cash or with card
trans_pay <- read_delim(here("raw data/ZHAW_Trans_Payments.csv"), delim = ';', trim_ws = T) %>%
    select(transaction_id, payment_id, amount) %>%
    rename(payment_id = payment_id, price_p = amount)

# load payments of ZHAW and check differences
trans_pay2 <- read_delim(here("raw data/ZHAW_payments_180313.csv"), delim = ';', trim_ws = T) %>%
    select(transaction_id, payment_id, amount) %>%
    rename(payment_id = payment_id, price_p = amount)

# check difference
df_diff <- anti_join(trans_pay, trans_pay2, by = "transaction_id") # they differ by 3990 Transaktions, take the first set

# load payments information
pay_method <- read_delim(here("raw data/ZHAW_payments_171214.csv"), delim = ';',trim_ws = T) %>%
    select(id, code, description) %>%
    rename(payment_id = id, payment_code = code, pay_description = description)


# aggregated dataset (data from March 2018)
# inluced all cash payers
trans_dat <- read_delim(here("raw data/ZHAW_Transactions_180320.csv"), delim = ';',col_names = T,trim_ws = T) %>% # trim_ws removes white space
    select(id, till_id, shop_id, operator_id, trans_date, total_amount, trans_num, bookkeeping_date, pricelevel_id, Geschlecht, card_num, Geburtsjahr2, Kategorisierung) %>%
    rename(transaction_id = id, member = Kategorisierung, date = bookkeeping_date) %>%
    mutate(trans_num = as.integer(.$trans_num), # parse to integer otherwise r saves it as numeric (e.g. 2.67e3)
           transaction_id = as.integer(.$transaction_id))


# individual dataset (data from 27.09.18)
# (No column name) = ccnr | its a number to identify a distinct person (important for indivdiaul analyses)
# Bruno says trans_num is better for merge as transaction_id => however there is no trans_num in either of the other data sets
# do not inluced all cash payers
# dob = date of birth
trans_dat1 <- read_xlsx(here("raw data/Kaufverhalten_2018-09-27_V01.xlsx"), sheet = 5, col_names = T, trim_ws = T) %>% # trim_ws removes white space
    select(`(No column name)`, id, till_id, DatumSV, trans_date, total_amount, trans_num, pricelevel_id, Geschlecht, card_num, Geburtsjahr2, Kategorisierung) %>%
    rename(ccrs = `(No column name)`, transaction_id = id, member = Kategorisierung, gender = Geschlecht, Dob = Geburtsjahr2, date = DatumSV) %>%
    mutate(ccrs = as.integer(.$ccrs), # parse to integer otherwise r saves it as numeric (e.g. 2.67e3)
           transaction_id = as.integer(.$transaction_id),
           card_num = as.integer(.$card_num))


################# Second Step: SQL Merging: aggregated data-------
## for information see: https://communities.sas.com/t5/SAS-Procedures/Proc-SQL-equiv-of-Merge-If-A-or-B/td-p/255720 

# merge transactions and shops
df <- left_join(trans_dat, till, by = 'till_id') # another possiblity => see dokumentation, semi_join could be interessting

# merge transaction and trans_article
df1 <- left_join(df, trans_art, by = "transaction_id") # merge it on trans_num (is like the bon number), however there is no variable called like that?

# merge df1 with atricle information
df2 <- inner_join(df1, art_info, by = "article_id")

# merge df1 with price_level information
price_inf <- inner_join(art_price, price_lev, by = "pricelevel_id")
df3 <- left_join(df2, price_inf, by = c("article_id", "pricelevel_id")) # to avoid double transaction id's, take more than one key to join

# around 50000 transactions have no single_price (reasons: for some of the articles there are no combo between pricelevel and article_id e.g. stück brot has only price for external people) => lunchs are not affected (nas <- filter(df3, is.na(df3$single_price)))


# merge data with price_levels and payment desciption
pay_inf <- left_join(trans_pay, pay_method, by = "payment_id") %>%
    rename(total_amount = price_p) # price_p should be the total payment per transaction (however not the same as total_amount of trans_dat)
df4_agg <- left_join(df3, pay_inf, by = "transaction_id", "total_amount") %>%# some new observations came along => reason is pay_description (some entries are now double e.g. 2373186)
    select(-total_amount.y) %>% # drop double variable
    rename(total_amount = total_amount.x)

# save aggregated data-------------
write_delim(df4_agg, here::here("augmented data/data_trans_agg_180802.csv"), delim = ';') # contains all transaction over ZHAW


################# Second Step: SQL Merging: individual data-------
# merge transactions and shops
df <- left_join(trans_dat1, till, by = 'till_id') # another possiblity => see dokumentation, semi_join could be interessting

# merge transaction and trans_article
df1 <- left_join(df, trans_art, by = "transaction_id") # merge it on trans_num (is like the bon number), however there is no variable called like that?

# merge df1 with atricle information
df2 <- inner_join(df1, art_info, by = "article_id")

# merge df1 with price_level information
price_inf <- inner_join(art_price, price_lev, by = "pricelevel_id")
df3 <- left_join(df2, price_inf, by = c("article_id", "pricelevel_id")) # to avoid double transaction id's, take more than one key to join

# around 50000 transactions have no single_price (reasons: for some of the articles there are no combo between pricelevel and article_id e.g. stück brot has only price for external people) => lunchs are not affected (nas <- filter(df3, is.na(df3$single_price)))

# merge data with price_levels and payment desciption
pay_inf <- left_join(trans_pay, pay_method, by = "payment_id") %>%
    rename(total_amount = price_p) # price_p should be the total payment per transaction (however not the same as total_amount of trans_dat)
df4_ind <- left_join(df3, pay_inf, by = "transaction_id", "total_amount") %>%# some new observations came along => reason is pay_description (some entries are now double e.g. 2373186)
    select(-total_amount.y) %>% # drop double variable
    rename(total_amount = total_amount.x)

# save individual data---------        
write_delim(df4_ind, here::here("augmented data/data_trans_ind_180929.csv"), delim = ';') # contains all transaction over ZHAW