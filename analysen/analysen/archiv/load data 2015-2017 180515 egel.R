#### load and merge data from 2015 to 2017
### status: 14.06.18 // egel


# see script load data 2017------
# prepare data from 2017, with locals and selling time from 11 a.m. until 2 p.m.
# assumption that locals from 2015 and 2016 are included in the till data (maybe ask michael krauer for that)
menu_tot7 <- filter(df_17, (hour(trans_date) >= 11 & hour(trans_date) <= 13) & total_amount < 1000)

# filter payment -------
# transactions with Gutschein are double transactions
# => delete all gutschein payers
menu_tot7 <- filter(menu_tot7, !grepl("Gutschein",menu_tot7$pay_description))

# add semester week to dataframe
menu_tot7$semwk <-
    ifelse(menu_tot7$week == 40,3,ifelse(menu_tot7$week == 41, 4, ifelse(
        menu_tot7$week == 42,5, ifelse(menu_tot7$week == 43,6,ifelse(
            menu_tot7$week == 44,7,ifelse(menu_tot7$week == 45, 8, ifelse(
                menu_tot7$week == 46,9, ifelse(menu_tot7$week == 47,10,ifelse(
                    menu_tot7$week == 48,11,ifelse(menu_tot7$week == 49,12,ifelse(
                        menu_tot7$week == 50,13,14)
                    ))
                ))
            ))
        ))
    ))

# rename labels of content -------------
menu_tot7$label_content <-  str_replace(menu_tot7$label_content,"Fleisch","Meat")
menu_tot7$label_content <-  str_replace(menu_tot7$label_content,"Vegetarisch","Vegetarian")
menu_tot7$label_content <-  str_replace(menu_tot7$label_content,"Pflanzlich","Vegan")

# filter variables
menu_tot7 <- group_by(menu_tot7, label_content, week, semwk, year, shop_description) %>% summarise(value=n()) # group it again, for merge with other data, no condit variable
menu_tot7 <- rename(menu_tot7, variable = label_content)
menu_tot7 <- select(menu_tot7, variable,value,year,week,semwk,shop_description)


#load data from 2015 and 2016----------------- (version 21.2.18)------
dat_hs <- read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_testdaten_analysen/explorativ/data/verkaufsdaten täglich HS 15-16 180221.csv",
                     delim = ';',
                     locale = locale(encoding = "LATIN1"),
                     col_types = cols(date = col_date(format="%d.%m.%Y"))) # see script: datensätze bereinigen 170307 HS  02egel.R
dat_hs$X1 <- NULL
df_old <-  filter(dat_hs, name %in% c('KitchenTotal','Green','FavoriteTotal','BuffetTotal'))
df_old <- droplevels(df_old)
df_old$week <- strftime(df_old$date, format= "%V")

names(df_old)[names(df_old) == "name"] <- "article_description"
names(df_old)[names(df_old) == "Verkaufte.Stücke"] <- "tot_sold"
names(df_old)[names(df_old) == "ort"] <- "shop_description"

#load data from 2015 and 2016------------ (version 15.05.18)-------
#same dataset as in earlier version
dat_hs_tot <- read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/verkaufsdaten täglich HS 15-16 180518.csv", 
                         delim = ';',
                         col_types = cols(date = col_date(format="%d.%m.%Y"))) %>% # load data
    dplyr::rename(tot_sold=Verkaufte_Stücke, shop_description = ort) %>% # rename variables
    filter(article_description %in% c('KitchenTotal','Green','FavoriteTotal','BuffetTotal')) %>% # filter data for the four main meals
    mutate(week=strftime(date, format= "%V")) # might get an error
    
df_old <- droplevels(dat_hs_tot)

# select only important variables--------------
df_tot <- select(df_old, article_description, date, week, year, shop_description, tot_sold)

# change article_description variables
df_tot$article_description <- str_replace(df_tot$article_description,"BuffetTotal","Hot and Cold")
df_tot$article_description <-  str_replace(df_tot$article_description,"KitchenTotal", "Kitchen")
df_tot$article_description <-  str_replace(df_tot$article_description,"FavoriteTotal", "Favorite")

# change shop_description variables
df_tot$shop_description <- str_replace(df_tot$shop_description,"grüental","Grüental Mensa")
df_tot$shop_description <- str_replace(df_tot$shop_description,"vista","Vista Mensa")

### 2015: calculation of the relative meal content-----
df <- as.data.frame(group_by(df_tot, year, article_description, week) %>% summarise(tot= sum(tot_sold)))
dt <- filter(df, year == 2015) 
dt <- droplevels(dt)

Fleisch_Fisch_Vogel <- dt[dt[,"article_description"]=="Favorite",]$tot*0.8 + dt[dt[,"article_description"]=="Kitchen",]$tot # 80% of Favorite meals and 100% of Kitchen meals contain meat
Vegetarisch <- dt[dt[,"article_description"]=="Green",]$tot + dt[dt[,"article_description"]=="Favorite",]$tot*.2 # 100% of Green meals and 20% of Favorite meals are vegetarian

menu_tot5 <- melt(data.frame(dt[dt[,"article_description"]=="Hot and Cold",]$tot,Fleisch_Fisch_Vogel, Vegetarisch)) # create data.frame with variable hot and cold seperatly
levels(menu_tot5$variable)[1] <- "Hot and Cold"
levels(menu_tot5$variable)[2] <- "Meat"
levels(menu_tot5$variable)[3] <- "Vegetarian"
menu_tot5$year <- 2015 # add variable year
menu_tot5$week <- rep(36:52,3) # add week numbers
menu_tot5$semwk <- rep(-1:15,3)# add semester week numbers

### 2016: calculation of the relative meal content----
df <- as.data.frame(group_by(df_tot, year, article_description, week) %>% summarise(tot= sum(tot_sold)))
dt <- filter(df, year == 2016) 
dt <- droplevels(dt)

Fleisch_Fisch_Vogel <- dt[dt[,"article_description"]=="Favorite",]$tot*0.8 + dt[dt[,"article_description"]=="Kitchen",]$tot # 80% of Favorite meals and 100% of Kitchen meals contain meat
Vegetarisch <- dt[dt[,"article_description"]=="Green",]$tot + dt[dt[,"article_description"]=="Favorite",]$tot*.2 # 100% of Green meals and 20% of Favorite meals are vegetarian

menu_tot6 <- melt(data.frame(dt[dt[,"article_description"]=="Hot and Cold",]$tot,Fleisch_Fisch_Vogel, Vegetarisch)) # create data.frame with variable hot and cold seperatly
levels(menu_tot6$variable)[1] <- "Hot and Cold"
levels(menu_tot6$variable)[2] <- "Meat"
levels(menu_tot6$variable)[3] <- "Vegetarian" 
menu_tot6$year <- 2016
menu_tot6$week <- rep(35:51,3) # add week numbers
menu_tot6$semwk <- rep(-2:14,3)# add semester week numbers

# Concatinate data frames 2015 until 2017---

menu_tot5 <- as_tibble(filter(menu_tot5, week >=40 & week <=51)) # filter weeks
menu_tot6 <- as_tibble(filter(menu_tot6, week >=40 & week <=51)) # filter weeks
menu_tot <- bind_rows(menu_tot7,menu_tot6,menu_tot5) # coerc factors into characters

# remove some data from working space
rm(list=c("dat_hs","dat_hs_tot","df_old", "df_tot","df","dt","menu_tot5","menu_tot6","menu_tot7"))

# save dataset
write_delim(menu_tot,"S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse/all data over year 180424 03egel.csv",delim = ';') # for ist-soll analysis (between 11 and 14 oclock and without locals) see folder
