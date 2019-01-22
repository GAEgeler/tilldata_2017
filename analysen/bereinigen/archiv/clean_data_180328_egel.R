########
### Clean Data from Python
########


# required packages

library(stringr)
library(readr)
library(dplyr)
library(eeptools)

## Status: 29.03 // egel

setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files")
df_new2 <- read_csv2("filtered_data_dub_python_180320_03egel.csv", locale = locale(encoding = 'LATIN1'), na=c("NULL","0","")) # differences between datasets => 70 Hot and COld are missing in df_new2 (see below)
df_new1 <- read_rds("filtered_data_dub_180320_02egel.Rda")


# difference between two dataframes
diff=setdiff(df_new1$transaction_id, df_new2$transaction_id)
diff=anti_join(df_new1, df_new2, by="transaction_id") # 70 hot and cold in df_new2 are missing => why? 
# test=df_new1[duplicated(df_new1$transaction_id),] same code as anti_join

##-----------
# change some variables of df_new2
df_new2$article_description = str_replace_all(df_new2$article_description,c('Kitchen 1' = 'Kitchen','Kitchen 2' = 'Kitchen','Kitchen 3' = 'Kitchen', 'Kitchen 4' = 'Kitchen')) # how is Kitchen 1 replaced 
df_new2$article_description = str_replace(df_new2$article_description,"Garden","Hot and Cold")

# change levels of gender
df_new2$Geschlecht= toupper(df_new2$Geschlecht) # change f to uppercase
df_new2$Geschlecht=str_replace_all(df_new2$Geschlecht,c('F' = 'female','M'='male'))
df_new2$Geschlecht[df_new2$Geschlecht == "#NV" ] <- NA


# change variable date to date format and add week number
df_new2$date <- parse_date(df_new2$date, format="%d.%m.%Y")
df_new2$trans_date <- parse_datetime(df_new2$trans_date, format="%d.%m.%Y %H:%M")
df_new2$week <- as.integer(strftime(df_new2$date, format= "%V"))
df_new2$year <- as.integer(strftime(df_new2$date, format= "%Y"))

# change some variables to factors, there are surely better ways
df_new2$shop_description = str_replace_all(df_new2$shop_description,'Grüntal Mensa', 'Grüental Mensa')

#age calculation => attention max age is 118, takes alwys date of today for calculation, e.g. born "actual today" in 1977

df_new2$Geburtsjahr2 <-as.Date(df_new2$Geburtsjahr2,'%Y') # change date to right format
age <- age_calc(na.omit(df_new2$Geburtsjahr2),enddate = Sys.Date(), units = "years") # omit NA for calculation
df_new2$age[!is.na(df_new2$Geburtsjahr2)] <- age # insert NA values again

# change #NV to lernende
df_new2$member = str_replace_all(df_new2$member,"#NV","Lernende")


write_csv(df_new2,"filtered_data_dub_python_180320_04egel.csv")

#add canteen cycle and intervention period

# see python script: analyses_till_180320_03egel



##-----------

