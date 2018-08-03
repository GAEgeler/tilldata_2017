### filtering data ------------

## Status: 2.8.18 // egel

# required packages------
library(dplyr)
library(readr)
library(lubridate)

# load data -------------
df <- read_delim(here("clean data/data_clean_180802_egel.csv"), delim = ';')

# subset data: Favorite, Garden (= Hot n Cold), Kitchen (0,1,2,3,4), LocalFavorite, LocalKitchen, LocalWorld, World => Local Kitchen is missing?
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
df_ <- filter(df, art_code %in% articles) # attention no time filter

# double check----------
### as control, filter data with article names => same result as above
articl_name <- c(
    "Favorite",
    "Kitchen",
    "Hot and Cold",
    "Local Favorite",
    "Local World",
    "Local Tössfeld",
    "World")
df2_ <- filter(df, article_description %in% articl_name) # attention no time filter

# time filter (only lunch meals)-----
# between 8 and 9 oclock are weiered transactions (total amount over 1000). it seems that michael krauer then feed in missing sold meals of the evenings
# filter all meals between 9 and 10 to see how many are affected 
# thus filter total amount over 1000 chf
df_ <- filter(df_, (hour(trans_date) >= 9 & hour(trans_date) <= 14) & total_amount_trans < 1000) # starts at 9:00 and end at 14:59, and excluds cases (only one) which the total_amount is higher than 1000

# exclude all pay methods with Gutschein
df_ <- filter(df_, !grepl("Gutschein*", df_$pay_description))

# change date format (because it causes several problems while merging)----------
df_$date <- as.Date(df_$date) # change date format from POSIXct to Date

# save data-------
write_delim(df_, here("clean data/data_filtered_180802_egel.csv"), delim = ';')
