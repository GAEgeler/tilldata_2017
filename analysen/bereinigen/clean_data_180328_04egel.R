########
### Clean Data from Python
########


## Status: 02.5.18 // egel

# required packages -----------
library(stringr)
library(readr)
library(dplyr)
library(eeptools)


# load data ---------
df_new2 <- read_delim(
    'S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files/filtered_r_edit_180502_egel.csv', delim=';',locale = locale(encoding='LATIN1'),
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

# change structure of some variables -------------
df_new2$total_amount <- sub(",",".",df_new2$total_amount) # change comma quote to point
df_new2$total_amount <- as.numeric(df_new2$total_amount)  # change to numeric
df_new2$price_payment <- sub(",",".",df_new2$price_payment) # change comma quote to point
df_new2$price_payment <- as.numeric(df_new2$price_payment) # change to numeric

# Change names of kitchen and hot and cold -----------
df_new2$article_description = str_replace_all(df_new2$article_description,c('Kitchen 1' = 'Kitchen','Kitchen 2' = 'Kitchen','Kitchen 3' = 'Kitchen', 'Kitchen 4' = 'Kitchen'))
df_new2$article_description = str_replace(df_new2$article_description,"Garden","Hot and Cold")

# change name of grüntal to grüental ------------
df_new2$shop_description = str_replace_all(df_new2$shop_description,'Grüntal Mensa', 'Grüental Mensa')

# change names of gender ----------
df_new2$Geschlecht=str_replace_all(toupper(df_new2$Geschlecht),c('F' = 'female','M'='male')) # first set all letters uppercase than change them according the pattern

# add week number and year as new variables ------------
df_new2$week <- as.integer(strftime(df_new2$trans_date, format= "%V"))
df_new2$year <- as.integer(strftime(df_new2$trans_date, format= "%Y"))

#age calculation => attention max age is 118
# takes difference between year of birth and enddate, enddate is the end of the quasi experiment -> lets say 31.12.2017
startdate <- parse_date(df_new2$Geburtsjahr2,format = "%Y") #set startdate, here everyone is born on the 1st January 
enddate <- as.Date("2017-12-31") # set enddate
age <- age_calc(na.omit(startdate),enddate = enddate, units = "years") # omit NA for calculation
df_new2$age[!is.na(df_new2$Geburtsjahr2)] <- age # insert age, there where is no NA


# save data ------
write_delim(df_new2,'S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files/filtered_r_clean_180502_egel.csv', delim = ";") # no timefilter




