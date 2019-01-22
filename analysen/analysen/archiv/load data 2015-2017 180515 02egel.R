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

# group data for calculation
df <- as.data.frame(group_by(df_tot, year, article_description, week, shop_description) %>% summarise(tot= sum(tot_sold)))

### 2015: calculation of the relative meal content-----
# define meal content: version 1----
dt <- filter(df, year == 2015) 

Fleisch_Fisch_Vogel_g <- dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental Mensa",]$tot*0.8 + dt[dt["article_description"]=="Kitchen"& dt["shop_description"]=="Grüental Mensa",]$tot # 80% of Favorite meals and 100% of Kitchen meals contain meat
Vegetarisch_g <- dt[dt["article_description"]=="Green" & dt["shop_description"]=="Grüental Mensa",]$tot + dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental Mensa",]$tot*.2 # 100% of Green meals and 20% of Favorite meals are vegetarian

Fleisch_Fisch_Vogel_v <- dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista Mensa",]$tot*0.8 + dt[dt["article_description"]=="Kitchen" & dt["shop_description"]=="Vista Mensa",]$tot # 80% of Favorite meals and 100% of Kitchen meals contain meat
Vegetarisch_v <- dt[dt["article_description"]=="Green" & dt["shop_description"]=="Vista Mensa",]$tot + dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista Mensa",]$tot*.2 # 100% of Green meals and 20% of Favorite meals are vegetarian

menu_tot5_g <- melt(data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental Mensa",]$tot,Fleisch_Fisch_Vogel_g, Vegetarisch_g)) # create data.frame with variable hot and cold seperatly
menu_tot5_g$shop_description <- "Grüental Mensa"
menu_tot5_v <- melt(data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Vista Mensa",]$tot,Fleisch_Fisch_Vogel_v, Vegetarisch_v)) # create data.frame with variable hot and cold seperatly
menu_tot5_v$shop_description <- "Vista Mensa"

#rbind information from both canteens
menu_tot5 <- rbind(menu_tot5_v,menu_tot5_g)

# # define meal content: version 2----
# 
# test <- df %>%
#     filter(year == 2015, shop_description == "Grüental Mensa") %>%
#     mutate(Fleisch_Fisch_Vogel = df[df["article_description"]=="Favorite" & df["shop_description"]=="Grüental Mensa",]$tot*0.8 + df[df["article_description"]=="Kitchen"& df["shop_description"]=="Grüental Mensa",]$tot)
# 
# 
# 
# 
# # version 3----
# fleisch="definelist"
# for(string in df$article_description){
#     if(df[df[text]== "Favorite",]){
#         fleisch[text]=df$tot*.8
#     }else if(df[df[text]== "Kitchen",]){
#         fleisch[text]=df$tot*.2
#     }
# }


# rename levels
levels(menu_tot5$variable)[1] <- "Hot and Cold"
levels(menu_tot5$variable)[2] <- "Meat"
levels(menu_tot5$variable)[3] <- "Vegetarian"
levels(menu_tot5$variable)[4] <- "Meat"
levels(menu_tot5$variable)[4] <- "Vegetarian"
menu_tot5$year <- 2015 # add variable year
menu_tot5$week <- rep(36:52,3) # add week numbers
menu_tot5$semwk <- rep(-1:15,3)# add semester week numbers

### 2016: calculation of the relative meal content----

# define meal content: version 1----
dt <- filter(df, year == 2016) 

Fleisch_Fisch_Vogel_g <- dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental Mensa",]$tot*0.8 + dt[dt["article_description"]=="Kitchen"& dt["shop_description"]=="Grüental Mensa",]$tot # 80% of Favorite meals and 100% of Kitchen meals contain meat
Vegetarisch_g <- dt[dt["article_description"]=="Green" & dt["shop_description"]=="Grüental Mensa",]$tot + dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental Mensa",]$tot*.2 # 100% of Green meals and 20% of Favorite meals are vegetarian

Fleisch_Fisch_Vogel_v <- dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista Mensa",]$tot*0.8 + dt[dt["article_description"]=="Kitchen" & dt["shop_description"]=="Vista Mensa",]$tot # 80% of Favorite meals and 100% of Kitchen meals contain meat
Vegetarisch_v <- dt[dt["article_description"]=="Green" & dt["shop_description"]=="Vista Mensa",]$tot + dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista Mensa",]$tot*.2 # 100% of Green meals and 20% of Favorite meals are vegetarian

menu_tot6_g <- melt(data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental Mensa",]$tot,Fleisch_Fisch_Vogel_g, Vegetarisch_g)) # create data.frame with variable hot and cold seperatly
menu_tot6_g$shop_description <- "Grüental Mensa"
menu_tot6_v <- melt(data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Vista Mensa",]$tot,Fleisch_Fisch_Vogel_v, Vegetarisch_v)) # create data.frame with variable hot and cold seperatly
menu_tot6_v$shop_description <- "Vista Mensa"

#rbind information from both canteens
menu_tot6 <- rbind(menu_tot6_v,menu_tot6_g)

# change some variables
levels(menu_tot6$variable)[1] <- "Hot and Cold"
levels(menu_tot6$variable)[2] <- "Meat"
levels(menu_tot6$variable)[3] <- "Vegetarian" 
levels(menu_tot6$variable)[4] <- "Meat"
levels(menu_tot6$variable)[4] <- "Vegetarian"
menu_tot6$year <- 2016
menu_tot6$week <- rep(35:51,3) # add week numbers
menu_tot6$semwk <- rep(-2:14,3)# add semester week numbers

# Concatinate data frames 2015 until 2017---

menu_tot5 <- as_tibble(filter(menu_tot5, week >=40 & week <=51)) # filter weeks
menu_tot6 <- as_tibble(filter(menu_tot6, week >=40 & week <=51)) # filter weeks
menu_tot <- bind_rows(menu_tot7,menu_tot6,menu_tot5) # coerc factors into characters

# remove some data from working space
rm(list=c("Fleisch_Fisch_Vogel_v","Fleisch_Fisch_Vogel_g","Vegetarisch_v","Vegetarisch_g","dat_hs","dat_hs_tot","df_old", "df_tot","df","dt","menu_tot5","menu_tot5_v","menu_tot5_g","menu_tot6","menu_tot6_g","menu_tot6_v","menu_tot7"))

# save dataset
write_delim(menu_tot,"S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse/all data over year 180424 04egel.csv",delim = ';') # for ist-soll analysis (between 11 and 14 oclock and without locals) see folder
