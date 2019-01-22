#######
# Difference between R and Python: Overall
#######


# Status: 3.5.18 // egel

# required packages
library(readr)
library(dplyr)
library(lubridate)

##load data => see that both dataframes have same structure
dat <- read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files/data_180320_05egel.csv",delim = ';') # some major differences in str between dat and dat2
dat2 <- read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files/joined_180320_05egel.csv",delim = ';',locale = locale(encoding='LATIN1'), trim_ws = T) # there are some problems with the variable article_description


#compare structure of data sets
m <- sapply(list(dat = dat, dat2 = dat2), sapply, class)
m[m[, "dat"] != m[, "dat2"], , drop = FALSE]


#change dat2 to same structure
dat2$Geburtsjahr2 <- as.integer(dat2$Geburtsjahr2)
dat2$card_num <- as.integer(dat2$card_num)
# dat2 <- rename(dat2, price_article=price_articles,price_payment=price_payments)


# test if identical
identical(dat,dat2) # not true, differences in article_description seems to be the problem => see comments below
length(unique(dat$transaction_id)) # seems to have same lenght of TA_ids
length(unique(dat2$transaction_id))
anti_join(dat,dat2,by="card_num") # returns empty dataframe, means the same card_nums in both dataframes
# => seems to have the same transaction_id and card_num

# Differences are in the variable article_description => not any more
diffr=anti_join(dat,dat2,by="article_description") # returns data frame with differences, is empty
diffr2=setdiff(dat$article_description,dat2$article_description) # returns a list of all articles, which are in dat2 missing

# luckily the differences dont affect the main meals => when time check the error in python


#######
# Difference between R and Python: Timefilter
#######

## load data
# from R
library(lubridate)
df_17 <- filter(dat, (hour(trans_date) >= 9) & (hour(trans_date) <= 14)) # 9.00 is included but not 15.00 is not included, dont know how to handle it

# from Python => wrong data set!!
df_17_py <- read_delim('S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/bereinigen/filtered_py_time_180502_egel.csv', delim=';',locale = locale(encoding='LATIN1'),col_types = cols(date = col_datetime(format = "")),trim_ws = T)

#### differences between data sets
diff_ <- setdiff(df_17,df_17_py)
diff_2=anti_join(df_17,df_17_py,by="article_description")
diff_3=anti_join(df_17,df_17_py,by="article_description") # with trim_ws=T => seems to have missings in article_description

# => seems that in python some merging is not taking the information of the ZHAW_article.csv => dont know why!?, it might be a problem of structure!


#######
# Difference between R and Python: Artikelfilter
#######

## load data
# from R 

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

df_R <- filter(dat, art_code %in% articles) # no time filter
df_PY <- filter(dat2, art_code %in% articles)# same result

#unique values in transaction id's => same lengths
length(unique(df_R$transaction_id))
length(unique(df_PY$transaction_id))


# search for difference => no differences
dif=anti_join(df_R,df_PY,by ="transaction_id")


# makes no sense to compare, thus lots of variables are missing e.g. Kitchen, local Kitchen etc.
# #######
# # Difference between R and Python: Merging Dokumentation 
# #######
# 
# 
# setwd("S:/pools/n/N-IUNR-nova-data/06_add_var/01_dokumentation")
# info <- read_delim("menu_inhalt_180215_10egel.csv", delim=';',locale = locale(encoding = 'LATIN1'), col_types = cols(date=col_datetime(format = "%d.%m.%Y"))) # attention load always actual version
# info <- select(info,date,art_description,label_content,cycle,meal_name,shop_description)
# info <- rename(info, article_description = art_description)
# 
# 
# df_info_R <- left_join(df_R,info,by=c("shop_description","date","article_description"))
# df_info_PY <- left_join(df_PY, info, by=c("shop_description","date","article_description"))


