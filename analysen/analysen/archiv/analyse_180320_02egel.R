########
## First Analyses with till data
#######

#Stand: 28.3 // egel


# required libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(feather)
library(reshape2)
library(tidyr)
library(eeptools)
library(readr)
library(Hmisc)
library(gmodels)

###########
### Preparation for analyses
###########

# load data hs17
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files")
dat <- read_rds("data_180320_03egel.Rda")
# dat2 <- read_csv2("180320_joined_04egel.csv", locale = locale(encoding='LATIN1'), )

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
df_new1 <- filter(dat, art_code %in% articles)

# subset data II: only meals during lunch time, ATTENTION: dataset only using for individual comparisons. If we compare with the last years we need to filter only meals sold during 11 am and 2 pm 
rest <- subset(df_new1, !(strftime(df_new1$trans_date, '%H') %in% c('9','10','11','12','13','14','15'))) # are breaks includes? e.g. 9.00 or not?
write.csv2(rest,"data_dinner_wädenswil_180221.csv")
df_new1 <- subset(df_new1, (strftime(df_new1$trans_date, '%H') %in% c('9','10','11','12','13','14','15')))
df_new1 <- droplevels(df_new1) # drop useless levels

# change names of levels
df_new1$art_description <- as.factor(df_new1$art_description)
levels(df_new1$art_description)[3:6] <- c('Kitchen')
levels(df_new1$art_description)[levels(df_new1$art_description) == "Garden"] <- 'Hot and Cold'

# change levels of gender
levels(df_new1$Geschlecht)[1:2] <- 'female'
levels(df_new1$Geschlecht)[2:3] <- 'male'
levels(df_new1$Geschlecht)[levels(df_new1$Geschlecht)=="NULL"] <- NA

# change variable date to date format and add week number
df_new1$week <- as.integer(strftime(df_new1$date, format= "%V"))
df_new1$year <- as.integer(strftime(df_new1$date, format= "%Y"))

# change some variables to factors

df_new1$shop_description <- as.factor(df_new1$shop_description)
levels(df_new1$shop_description)[1] <- 'Grüental Mensa'

#age calculation => attention max age is 118, takes alwys date of today for calculation, e.g. born "actual today" in 1977
df_new1$Geburtsjahr1 <- as.Date(as.character(df_new1$Geburtsjahr2),"%Y") # change geburtsjahr in date format
age <- age_calc(na.omit(df_new1$Geburtsjahr1), units = "years") # omit NA for calculation
df_new1$age[!is.na(df_new1$Geburtsjahr2)] <- age # insert NA values again


# change null to na
df_new1$card_num <- replace(df_new1$card_num,df_new1$card_num == 'NULL',values= NA)
df_new1$member <- replace(df_new1$member,df_new1$member == 'NULL',values= NA)
df_new1$Geschlecht <- replace(df_new1$Geschlecht,df_new1$Geschlecht == 'NULL',values= NA)
df_new1$card_num <- as.integer(df_new1$card_num)


write_rds(df_new1,"filtered_data_dub_180320_02egel.Rda") # include double transaction_id's
write.csv2(df_new1,"filtered_data_dub_180320_02egel.csv")

# Attention, there are some special cases: talk over with Bruno

# ecxlude double or tripple transactions_id
df_new2 <- df_new1[!(duplicated(df_new1$transaction_id)),]
b <- df_new1[duplicated(df_new1$transaction_id),]
write.csv2(b,"duplicate transaction_id 180320 egel.csv")#interim result
# write_rds(df_new2,"filtered_data_180320_02egel.Rda") 
# write_csv(df_new2, "filtered_data_180320_02egel.csv") # comma seperated

# single card numbers
single=df_new1[!duplicated(df_new1$card_num),] # one NA is still in the data
single = single[na.omit(single$card_num),] # delete this case

describe(single$member) # member distribution
describe(single$Geschlecht) # gender distribution
CrossTable(single$Geschlecht,single$member)

# mean age => problems of na and cases over 118 years old

mean_age=single[single$age < 100,]
psych::describe(mean_age$age)



# Chi-Square
setwd("C:/Users/egel/switchdrive/ZHAW/01_NOVANIMAL/01_meetings/180415 sga tagung")
df_tot <- read_csv2("all data over year 180405 egel.csv")

df_tot$value <- as.integer(df_tot$value)
df_tot$X1 <- NULL

# read this http://seananderson.ca/2013/10/19/reshape/
test <- melt(df_tot, value ~ year)



chisq.test(df_7$Geschlecht,df_7$label_content)
chisq.test(df_7$condit,df_7$label_content)
