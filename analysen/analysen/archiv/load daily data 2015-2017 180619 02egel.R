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
    summarise(tot_sold=n()) %>% # group it again, for merge with other data, no condit variable
    ungroup() %>%
    select(label_content,tot_sold,date,year,week,semwk,shop_description)

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

# meal content for mensa grüental
#meat
Fleisch_g <- cbind.data.frame(dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental Mensa",]$tot*0.8 +
                              dt[dt["article_description"]=="Kitchen"& dt["shop_description"]=="Grüental Mensa",]$tot, # 80% of Favorite meals and 100% of Kitchen meals contain meat
                              dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental Mensa",]$date)

names(Fleisch_g) <- c("tot_sold","date") # rename
Fleisch_g$label_content <- "Meat" # add label content
Fleisch_g$shop_description <- "Grüental Mensa" # add shop_description

#vegetarian
Vegetarisch_g <- cbind.data.frame(dt[dt["article_description"]=="Green" & dt["shop_description"]=="Grüental Mensa",]$tot +
                                  dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental Mensa",]$tot*.2, # 100% of Green meals and 20% of Favorite meals are vegetarian
                                  dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental Mensa",]$date)

names(Vegetarisch_g) <- c("tot_sold","date")
Vegetarisch_g$label_content <- "Vegetarian"
Vegetarisch_g$shop_description <- "Grüental Mensa"

#buffet
Hot_Cold_g <- cbind.data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental Mensa",]$tot,
                             dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental Mensa",]$date)

names(Hot_Cold_g) <- c("tot_sold","date")
Hot_Cold_g$label_content <- "Hot and Cold"
Hot_Cold_g$shop_description <- "Grüental Mensa"


# meal content for mensa vista
#meat
Fleisch_v <- cbind.data.frame(dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista Mensa",]$tot*0.8 +
                              dt[dt["article_description"]=="Kitchen" & dt["shop_description"]=="Vista Mensa",]$tot, # 80% of Favorite meals and 100% of Kitchen meals contain meat
                              dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista Mensa",]$date)

names(Fleisch_v) <- c("tot_sold","date") 
Fleisch_v$label_content <- "Meat"
Fleisch_v$shop_description <- "Vista Mensa"

#vegetarian
Vegetarisch_v <- cbind.data.frame(dt[dt["article_description"]=="Green" & dt["shop_description"]=="Vista Mensa",]$tot + 
                                  dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista Mensa",]$tot*.2, # 100% of Green meals and 20% of Favorite meals are vegetarian
                                  dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista Mensa",]$date)

names(Vegetarisch_v) <- c("tot_sold","date")
Vegetarisch_v$label_content <- "Vegetarian"
Vegetarisch_v$shop_description <- "Vista Mensa"

#buffet
Hot_Cold_v <- cbind.data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Vista Mensa",]$tot,
                             dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Vista Mensa",]$date)

names(Hot_Cold_v) <- c("tot_sold","date")
Hot_Cold_v$label_content <- "Hot and Cold"
Hot_Cold_v$shop_description <- "Vista Mensa"

menu_tot5 <- rbind(Fleisch_g, Fleisch_v, Vegetarisch_g, Vegetarisch_v, Hot_Cold_g, Hot_Cold_v) # concate all data frames

#add some variables
menu_tot5$year <- year(menu_tot5$date) # add variable year
menu_tot5$week <- isoweek(menu_tot5$date) # add week nr -- ATTENTION week() returns wrong week number

### 2016: calculation of the relative meal content----

# define meal content: version 1----
dt <- filter(df_tot, year == 2016) %>%
    rename(tot=tot_sold)

# meal content for mensa grüental
#meat
Fleisch_g <- cbind.data.frame(dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental Mensa",]$tot*0.8 +
                              dt[dt["article_description"]=="Kitchen"& dt["shop_description"]=="Grüental Mensa",]$tot, # 80% of Favorite meals and 100% of Kitchen meals contain meat
                              dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental Mensa",]$date)

names(Fleisch_g) <- c("tot_sold","date") # rename 
Fleisch_g$label_content <- "Meat" # add label_content
Fleisch_g$shop_description <- "Grüental Mensa" # add shop_description

#vegetarian
Vegetarisch_g <- cbind.data.frame(dt[dt["article_description"]=="Green" & dt["shop_description"]=="Grüental Mensa",]$tot +
                                  dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental Mensa",]$tot*.2, # 100% of Green meals and 20% of Favorite meals are vegetarian
                                  dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental Mensa",]$date)

names(Vegetarisch_g) <- c("tot_sold","date")
Vegetarisch_g$label_content <- "Vegetarian"
Vegetarisch_g$shop_description <- "Grüental Mensa"

#buffet
Hot_Cold_g <- cbind.data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental Mensa",]$tot,
                               dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental Mensa",]$date)

names(Hot_Cold_g) <- c("tot_sold","date")
Hot_Cold_g$label_content <- "Hot and Cold"
Hot_Cold_g$shop_description <- "Grüental Mensa"


# meal content for mensa vista
#meat
Fleisch_v <- cbind.data.frame(dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista Mensa",]$tot*0.8 +
                                  dt[dt["article_description"]=="Kitchen" & dt["shop_description"]=="Vista Mensa",]$tot, # 80% of Favorite meals and 100% of Kitchen meals contain meat
                              dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista Mensa",]$date)


names(Fleisch_v) <- c("tot_sold","date")
Fleisch_v$label_content <- "Meat"
Fleisch_v$shop_description <- "Vista Mensa"

#vegetarian
Vegetarisch_v <- cbind.data.frame(dt[dt["article_description"]=="Green" & dt["shop_description"]=="Vista Mensa",]$tot + 
                                      dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista Mensa",]$tot*.2, # 100% of Green meals and 20% of Favorite meals are vegetarian
                                  dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista Mensa",]$date)

names(Vegetarisch_v) <- c("tot_sold","date")
Vegetarisch_v$label_content <- "Vegetarian"
Vegetarisch_v$shop_description <- "Vista Mensa"

#buffet
Hot_Cold_v <- cbind.data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Vista Mensa",]$tot,
                               dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Vista Mensa",]$date)

names(Hot_Cold_v) <- c("tot_sold","date")
Hot_Cold_v$label_content <- "Hot and Cold"
Hot_Cold_v$shop_description <- "Vista Mensa"

menu_tot6 <- rbind(Fleisch_g, Fleisch_v, Vegetarisch_g, Vegetarisch_v, Hot_Cold_g, Hot_Cold_v) # concate all data frames

#add some variables
menu_tot6$year <- year(menu_tot6$date) # add variable year
menu_tot6$week <- isoweek(menu_tot6$date) # add week nr -- ATTENTION week() returns wrong week number


# Concatinate data frames 2015 until 2017---
menu_tot5 <- as_tibble(filter(menu_tot5, week >=40))  # filter weeks
menu_tot5$semwk <- ifelse(menu_tot5$week == 52, 15, NA) # year 2015 has more weeks than the year 2016,17, thus to calculate semweek we need to split up the data frame
menu_5 <- filter(menu_tot5, week < 52) # subset data all exept week 52 (only 3 occurencies per label_content => thus impossible to use rep() function)
menu_5$semwk <- rep(3:14,each=30) # add semester week
menu_tot5$semwk <- ifelse(is.na(menu_tot5$semwk),menu_5$semwk,menu_tot5$semwk) # update both data frames

menu_tot6 <- as_tibble(filter(menu_tot6, week >=40)) # filter weeks
menu_tot6$semwk <- rep(3:14, each=30) # add semester week

#problems with bind_rows because date differs between menu_tot5,6 and 7
menu_tot <- rbind(menu_tot7_,menu_tot6,menu_tot5) # coerc factors into characters

# remove some data from working space
rm(list=c("Hot_Cold_v","Hot_Cold_g","Fleisch_v","Fleisch_g","Vegetarisch_v","Vegetarisch_g","menu_5","dat_hs","dat_hs_tot","df_old", "df_tot","df","dt","menu_tot5","menu_tot5_v","menu_tot5_g","menu_tot6","menu_tot6_g","menu_tot6_v","menu_tot7","menu_tot7_"))

# save dataset
write_delim(menu_tot,"S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse/all data daily 15_17 180620 egel.csv",delim = ';') # for ist-soll analysis (between 11 and 14 oclock and without locals) see folder
