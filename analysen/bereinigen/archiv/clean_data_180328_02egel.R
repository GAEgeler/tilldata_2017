########
### Clean Data from Python
########


# required packages

library(stringr)
library(readr)
library(dplyr)
library(eeptools)

## Status: 3.04 // egel

# load data hs17
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files")
dat <- read_rds("data_180320_03egel.Rda")
dat2 <- read_csv2("180320_joined_04egel.csv", locale = locale(encoding='LATIN1'))

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


articl_name <- c(
    "Favorite",
    "Kitchen 0",
    "Kitchen 1",
    "Garden" ,
    "Kitchen 2",
    "Kitchen 3",
    "Kitchen 4",
    "Local Favorite",
    "Local World",
    "Local Tössfeld",
    "World")


# df_new1 <- filter(dat, art_description %in% articl_name)
df_new <- filter(dat, art_code %in% articles) # no time filter
df_new1 <- filter(dat, art_code %in% articles) # differences between R generated-file and Python generated-file
df_new2 <- filter(dat2, code_description %in% articles)

# subset data II: only meals during lunch time, ATTENTION: dataset only using for individual comparisons. If we compare with the last years we need to filter only meals sold during 11 am and 2 pm 

df_new1 <- subset(df_new1, (strftime(df_new1$trans_date, '%H') %in% c('9','10','11','12','13','14','15'))) # differences between R generated-file and Python generated-file
df_new1 <- droplevels(df_new1) # drop useless levels
df_new2 <- subset(df_new2, (strftime(df_new2$trans_date, '%H') %in% c('9','10','11','12','13','14','15'))) # differences between R generated-file and Python generated-file
df_new2 <- droplevels(df_new2) # drop useless levels

write_csv(df_new,"data_r_nofilter_180403_egel.csv")
write.csv2(df_new1, "data_r_timefilter_180403_egel.csv") # for working in python
write_rds(df_new1,"data_r_timefilter_180403_egel.rda") # for working in r
write.csv2(df_new2,"data_phy_timefilter_180403_egel.csv")

##-----------##-----------
##-----------##-----------
##-----------##-----------
##-----------##-----------
##-----------##-----------
# load data (from python, claned and change for single cases)

# df_new2 <- read_delim("data_r_time_change_180403_egel.csv",delim=';',locale = locale(encoding='LATIN1')) # Problems with the Geburtsjahr2 => there is a 0 behind => read_csv2 drops the decimal => you can use read_delim
df_new2 <- read_delim('data_r_change_180403_egel.csv',delim=';',locale = locale(encoding='LATIN1')) # is coming from python
df_new2 <- rename(df_new2,article_description = art_description, code_description = art_code)


# Change names of kitchen
df_new2$article_description = str_replace_all(df_new2$article_description,c('Kitchen 1' = 'Kitchen','Kitchen 2' = 'Kitchen','Kitchen 3' = 'Kitchen', 'Kitchen 4' = 'Kitchen'))
df_new2$article_description = str_replace(df_new2$article_description,"Garden","Hot and Cold")

# change levels of gender
df_new2$Geschlecht= toupper(df_new2$Geschlecht) # change f to uppercase
df_new2$Geschlecht=str_replace_all(df_new2$Geschlecht,c('F' = 'female','M'='male'))
df_new2$Geschlecht[df_new2$Geschlecht == "#NV" ] <- NA


# change variable date to date format and add week number
df_new2$test<- parse_date(df_new2$date)
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

# change #NV to lernende
df_new2$member = str_replace_all(df_new2$member,"#NV","Lernende")

write_csv(df_new2,'filtered_data_dub_notime_180320_05egel.csv') # no timefilter
write_csv2(df_new2,"filtered_data_dub_180320_05egel.csv")
write_rds(df_new2,"filtered_data_dub_180320_05egel.rda")
#write_csv(df_new2,"filtered_data_dub_python_180320_05egel.csv")

#add canteen cycle and intervention period
# => see python script: analyses_till_180320_03egel

##-----------

