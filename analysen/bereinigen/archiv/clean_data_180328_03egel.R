########
### Clean Data from Python
########


# required packages

library(stringr)
library(readr)
library(dplyr)
library(eeptools)

## Status: 02.5.18 // egel

##-----------##-----------
##-----------##-----------

# load data (from python, claned and change for single cases)
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files")
# df_new2 <- read_delim("data_r_time_change_180403_egel.csv",delim=';',locale = locale(encoding='LATIN1')) # Problems with the Geburtsjahr2 => there is a 0 behind => read_csv2 drops the decimal => you can use read_delim
# df_new2 <- read_delim('data_r_change_180404_egel.csv',delim=';',locale = locale(encoding='LATIN1')) # problems parcing price_payments and total amount

df_new2 <- read_delim(
    'data_r_change_180502_egel.csv', delim=';',locale = locale(encoding='LATIN1'),
    col_types = cols(
        transaction_id = col_double(),
        trans_date = col_datetime(format = ""),
        date = col_datetime(format = ""),
        article_description = col_character(),
        art_code = col_character(),
        qty_weight = col_double(),
        card_num = col_double(),
        Geschlecht = col_character(),
        Geburtsjahr2 = col_double(),
        member = col_character(),
        rab_descript = col_character(),
        total_amount = col_character(),
        price_article = col_double(),
        price_payment = col_character(),
        prop_price = col_double(),
        pay_description = col_character(),
        shop_description = col_character()
    )
)

df_new2$total_amount <- sub(",",".",df_new2$total_amount) # change comma quote to point
df_new2$total_amount <- as.numeric(df_new2$total_amount)  # change to numeric
df_new2$price_payment <- sub(",",".",df_new2$price_payment)
df_new2$price_payment <- as.numeric(df_new2$price_payment)


# Change names of kitchen and hot and cold
df_new2$article_description = str_replace_all(df_new2$article_description,c('Kitchen 1' = 'Kitchen','Kitchen 2' = 'Kitchen','Kitchen 3' = 'Kitchen', 'Kitchen 4' = 'Kitchen'))
df_new2$article_description = str_replace(df_new2$article_description,"Garden","Hot and Cold")

# change levels of gender
df_new2$Geschlecht= toupper(df_new2$Geschlecht) # change f and m to uppercase
df_new2$Geschlecht=str_replace_all(df_new2$Geschlecht,c('F' = 'female','M'='male'))
df_new2$Geschlecht[df_new2$Geschlecht == "#NV" ] <- NA


# change variable date to date format and add week number
# df_new2$test<- parse_date(df_new2$date)
df_new2$trans_date <- parse_datetime(df_new2$trans_date)
df_new2$week <- as.integer(strftime(df_new2$trans_date, format= "%V"))
df_new2$year <- as.integer(strftime(df_new2$trans_date, format= "%Y"))

# change some variables to factors, there are surely better ways
df_new2$shop_description = str_replace_all(df_new2$shop_description,'Grüntal Mensa', 'Grüental Mensa')

#age calculation => attention max age is 118, takes alwys date of today for calculation, e.g. born "actual today" in 1977
# problem with zero behind the birthyear
df_new2$Geburtsjahr2 <- parse_date(df_new2$Geburtsjahr2,format = "%Y")
age <- age_calc(na.omit(df_new2$Geburtsjahr2),enddate = Sys.Date(), units = "years") # omit NA for calculation
df_new2$age[!is.na(df_new2$Geburtsjahr2)] <- age # insert NA values again

# # change #NV to lernende => already made in the merge script
# df_new2$member = str_replace_all(df_new2$member,"#NV","Lernende")

write_delim(df_new2,'filtered_data_dub_notime_180320_07egel.csv', delim = ";") # no timefilter

#add canteen cycle and intervention period
# => see python script: analyses_till_180320_03egel


