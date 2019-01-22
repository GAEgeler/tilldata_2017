#### load and merge daily data from 2015 to 2017
### status: 20.06.18 // egel


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
menu_tot7_ <- menu_tot7 %>%
    group_by(label_content,date, week, semwk, year, shop_description) %>% 
    summarise(value=n()) %>% # group it again, for merge with other data, no condit variable
    rename(variable = label_content) %>%
    ungroup() %>%
    select(variable,value,date,year,week,semwk,shop_description)

#load data from 2015 and 2016------------ (version 15.05.18)-------
#same dataset as in earlier version
dat_hs_tot <- read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_kassendaten 2015 & 2016/verkaufsdaten täglich HS 15-16 180518.csv", 
                         delim = ';',
                         col_types = cols(date = col_date(format="%d.%m.%Y"))) %>% # load data
    dplyr::rename(tot_sold=Verkaufte_Stücke, shop_description = ort) %>% # rename variables
    filter(article_description %in% c('KitchenTotal','Green','FavoriteTotal','BuffetTotal')) %>% # filter data for the four main meals
    mutate(week=strftime(date, format= "%V")) %>% # might get an error
    select(article_description, date, week, year, shop_description, tot_sold)

df_tot <- droplevels(dat_hs_tot)

# change article_description variables
df_tot$article_description <- str_replace(df_tot$article_description,"BuffetTotal","Hot and Cold")
df_tot$article_description <-  str_replace(df_tot$article_description,"KitchenTotal", "Kitchen")
df_tot$article_description <-  str_replace(df_tot$article_description,"FavoriteTotal", "Favorite")

# change shop_description variables
df_tot$shop_description <- str_replace(df_tot$shop_description,"grüental","Grüental Mensa")
df_tot$shop_description <- str_replace(df_tot$shop_description,"vista","Vista Mensa")


### 2015: calculation of the relative meal content-----
# define meal content: version 1----
dt <- filter(df_tot, year == 2015) %>%
    rename(tot=tot_sold)

Fleisch_Fisch_Vogel_g <- dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental Mensa",]$tot*0.8 + dt[dt["article_description"]=="Kitchen"& dt["shop_description"]=="Grüental Mensa",]$tot # 80% of Favorite meals and 100% of Kitchen meals contain meat
Vegetarisch_g <- dt[dt["article_description"]=="Green" & dt["shop_description"]=="Grüental Mensa",]$tot + dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental Mensa",]$tot*.2 # 100% of Green meals and 20% of Favorite meals are vegetarian

Fleisch_Fisch_Vogel_v <- dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista Mensa",]$tot*0.8 + dt[dt["article_description"]=="Kitchen" & dt["shop_description"]=="Vista Mensa",]$tot # 80% of Favorite meals and 100% of Kitchen meals contain meat
Vegetarisch_v <- dt[dt["article_description"]=="Green" & dt["shop_description"]=="Vista Mensa",]$tot + dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista Mensa",]$tot*.2 # 100% of Green meals and 20% of Favorite meals are vegetarian

menu_tot5_g <- melt(data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental Mensa",]$tot,dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental Mensa",]$week,Fleisch_Fisch_Vogel_g, Vegetarisch_g)) # create data.frame with variable hot and cold seperatly
menu_tot5_g$shop_description <- "Grüental Mensa"
menu_tot5_v <- melt(data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Vista Mensa",]$tot,Fleisch_Fisch_Vogel_v, Vegetarisch_v)) # create data.frame with variable hot and cold seperatly
menu_tot5_v$shop_description <- "Vista Mensa"

#rbind information from both canteens
menu_tot5 <- rbind(menu_tot5_v,menu_tot5_g)

# rename levels
levels(menu_tot5$variable)[1] <- "Hot and Cold"
levels(menu_tot5$variable)[2] <- "Meat"
levels(menu_tot5$variable)[3] <- "Vegetarian"
levels(menu_tot5$variable)[4] <- "Meat"
levels(menu_tot5$variable)[4] <- "Vegetarian"
menu_tot5$year <- 2015 # add variable year
# need a better way, however seems to be right
menu_tot5$date <- ifelse(menu_tot5[menu_tot5["variable"]=="Hot and Cold" & menu_tot5["shop_description"]=="Grüental Mensa",]$value == dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental Mensa",]$tot, dt$date,NA)
menu_tot5$date <- as_date(menu_tot5$date) # add date
menu_tot5$week <- week(menu_tot5$date) # add week numbers
menu_tot5$semwk <- ifelse(menu_tot5$week == 35,-2,NA) # exclude cases with week 35
menu_tot5_ <- filter(menu_tot5, week > 35) # exclude all cases with week 35
menu_tot5_$semwk <- rep(-1:14,each=5) # add semester week numbers

# test <- menu_tot5 %>% 
#     left_join(menu_tot5_, by = c("variable", "value", "shop_description", "year", "date", "week")) %>%
#     rename(semwk = semwk.x, semwk = semwk.y) %>%
#     arrange(semwk)
menu_tot5 <- unique(rbindlist(list(menu_tot5_, menu_tot5)),by = c("variable", "value", "shop_description", "year", "date", "week")) # update and arrange both dataframes

### 2016: calculation of the relative meal content----

# define meal content: version 1----
dt <- filter(df_tot, year == 2016) %>%
    rename(tot=tot_sold)

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
# need a better way, however seems to be right
menu_tot6$date <- ifelse(menu_tot6[menu_tot6["variable"]=="Hot and Cold" & menu_tot6["shop_description"]=="Grüental Mensa",]$value == dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental Mensa",]$tot, dt$date,NA)
menu_tot6$date <- as_date(menu_tot6$date) # change to date format
menu_tot6$week <- week(menu_tot6$date) # add week numbers
menu_tot6$semwk <- ifelse(menu_tot6$week == 52,15,NA) # for all weeks 52 give the value 15
menu_tot6_ <- filter(menu_tot6, week < 52) # exclude all cases with week 35
menu_tot6_$semwk <- rep(-1:14,each=5) # add semester week numbers
menu_tot6 <- unique(rbindlist(list(menu_tot6_, menu_tot6)),by = c("variable", "value", "shop_description", "year", "date", "week")) # update and arrange both dataframes

# Concatinate data frames 2015 until 2017---

menu_tot5 <- as_tibble(filter(menu_tot5, week >=40)) %>% # filter weeks
    mutate(date=as.POSIXct(.$date, format="%Y-%m-%d"))
menu_tot6 <- as_tibble(filter(menu_tot6, week >=40)) # filter weeks
menu_tot <- bind_rows(menu_tot7,menu_tot6,menu_tot5) # coerc factors into characters

# remove some data from working space
rm(list=c("Fleisch_Fisch_Vogel_v","Fleisch_Fisch_Vogel_g","Vegetarisch_v","Vegetarisch_g","dat_hs","dat_hs_tot","df_old", "df_tot","df","dt","menu_tot5","menu_tot5_v","menu_tot5_g","menu_tot6","menu_tot6_g","menu_tot6_v","menu_tot7"))

# save dataset
write_delim(menu_tot,"S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse/all data over year 180424 04egel.csv",delim = ';') # for ist-soll analysis (between 11 and 14 oclock and without locals) see folder
