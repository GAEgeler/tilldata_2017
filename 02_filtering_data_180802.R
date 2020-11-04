### filtering data ------------

###
# state: january 2019
# author: gian-Andrea egeler
###

# required packages------
library(dplyr)
library(readr)
library(lubridate)
library(here)

#####article filter--------
# subset data: Favorite, Garden (= Hot n Cold), Kitchen (0,1,2,3,4), LocalFavorite, LocalKitchen, LocalWorld, World => Local Kitchen is missing?
# individual data set: 23736 observations
# agg data set: 28199 observations (many differences because of the cash payers)
articles <- c(
    "A10001" ,# Favorite
    "A10010", # Kitchen 0
    "A10011", # Kitchen 1
    "A10040" ,# Garden 
    "A10012" ,# Kitchen 2
    "A10013" ,# Kitchen 3
    "A10014" ,# Kitchen 4
    "A10025" ,# Local Favorite
    "A10026" ,# Local World
    "A10027" ,# Local Tössfeld = Local Kitchen
    "A10015" ,# World
    "A10020" ,# Local 0
    "A10021" ,#	Local 1
    "A10022" ,#	Local 2
    "A10023" ,#	Local 3
    "A10024"  # Local 4
)

###############
### filter for individual dara
# load data set-------------
# individual dataset
df_ind <- read_delim(here::here("clean data/data_clean_ind_180929.csv"), delim = ';')

# 1. article filter--------
df_i <- filter(df_ind, art_code %in% articles) # attention no time filter

# 2. time filter (only lunch meals)-----
# between 8 and 9 oclock are weiered transactions (total amount over 1000). it seems that michael krauer then feed in missing sold meals of the evenings
# filter all meals between 9 and 10 to see how many are affected 
df_in <- filter(df_i, (hour(trans_date) >= 9 & hour(trans_date) <= 14)) # starts at 9:00 and end at 14:59

# 3. change date format (because it causes several problems while merging)----------
df_in$date <- as.Date(df_in$date) # change date format from POSIXct to Date

# 4. save data-------
# individual data set: 23683 obs
write_delim(df_in, here::here("clean data/", "data_filtered_ind_180929.csv"), delim = ';')

##################################################
##################################################--------------------------------
##################################################

#load data set------------
# aggreagted dataset
df_agg <- read_delim(here::here("clean data/data_clean_agg_180802.csv"), delim = ';')

###############
### filter for aggregated dara: 
# 1. article filter-----
df_a <- filter(df_agg, art_code %in% articles) # attention no time filter

# 2. time filter (only lunch meals)-----
# between 8 and 9 oclock are weiered transactions (total amount over 1000). it seems that michael krauer then feed in missing sold meals of the evenings
# filter all meals between 9 and 10 to see how many are affected 
df_ag <- filter(df_a, (hour(trans_date) >= 9 & hour(trans_date) <= 14)) # starts at 9:00 and end at 14:59, and excluds cases (only one) which the total_amount is higher than 1000

# 3. one strange transactions in total_amount (1) 1700 CHF 
# exclude that one
df_ag <- filter(df_ag, total_amount < 1000)

# 4. exclude Gutschein payers for agg data set, becaus of double entries
df_ag <- filter(df_ag, !grepl("Gutschein",df_ag$pay_description))

# 5. change date format (because it causes several problems while merging)----------
df_ag$date <- as.Date(df_ag$date) # change date format from POSIXct to Date

# 6. save data-------
# agg data set: 26234 obs
write_delim(df_ag, here::here("clean data/data_filtered_agg_180802.csv"), delim = ';')
