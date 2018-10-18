## load data -----

# status 17.10.18 // egel

# required packages
pack <- c("dplyr", "lubridate", "readr", "stringr", "readxl", "tidyr")
lapply(pack, function(x){do.call("library", list(x))})


####
# data from 2017-------- for individual analysis
# problem with encoding, dont know how to handle => especially while file savings
df_17 <- read_delim("augmented data/data_edit_180929_egel.csv", delim = ";", locale = locale(encoding = "LATIN1")) %>%
    mutate(date = as.Date(date)) 


# data from 2017---------- for aggregated analysis
df_agg <- read_delim("augmented data/data_edit_180802_egel.csv", delim = ";", locale = locale(encoding = "LATIN1")) %>%
    mutate(date = as.Date(date)) 


# merge documentation info with environmental data-----
# see 05_load_add_data for further information
source("05_load_add_data_181001_egel.R")

envir <- filter(envir_tot, !duplicated(envir_tot[c("article_description", "label_content", "date", "cycle", "week", "meal_name", "tot_ubp", "tot_gwp")])) %>% # select only those with tot_ubp and tot_gwp and drop all other
    select(article_description, label_content, date, cycle, week, meal_name, tot_ubp, tot_gwp)
info_compl <- left_join(info_orig, envir, by=c("meal_name", "article_description","date", "cycle", "week", "label_content")) %>% # left join
    select(-`Kein Protein`,-Kommentar) # diselect all variables of non use

nutri <- filter(nutri_wide_, !duplicated(nutri_wide_[c("article_description", "label_content", "date", "cycle", "week", "meal_name.y","ebp_points", "ebp_label", "teller_points", "teller_label")])) %>%
    select(c(article_description, label_content, date, cycle, week, meal_name.y, ebp_points, ebp_label, teller_points, teller_label))
info_compl <- left_join(info_compl, nutri, by=c("meal_name" = "meal_name.y", "article_description","date", "cycle", "week", "label_content")) 
   
# documentation with buffet data
info_compl <- left_join(info_compl, buffet, by=c("date","article_description","shop_description"))

# merge documentation with data 2017
info <- select(info_orig, date, article_description, label_content, cycle, meal_name, shop_description) # subset of info_orig
info_ <- select(info_compl, date,article_description,label_content,cycle,meal_name,shop_description, tot_ubp, tot_gwp, ebp_points, ebp_label, teller_points, teller_label, buffet_animal_comp) # subset of info_compl
df_7 <- left_join(df_17, info, by = c("shop_description","date","article_description","cycle"))
df_7_ <- left_join(df_17,info_, by = c("shop_description","date","article_description","cycle"))

# dataset without double entries: check script yy_plausibility
df_2017 <- filter(df_7, !(duplicated(df_7$ccrs) & duplicated(df_7$transaction_id) & duplicated(df_7$trans_date) & total_amount == prop_price))

# load aggregated sv data
df_agg <- left_join(df_agg, info, by = c("shop_description","date","article_description","cycle"))

# delete some datasets
rm(list = c("pack", "buffet", "envir", "envir_tot", "info", "info_", "info_compl", "info_orig", "nutri", "nutri_wide_"))


######
# data from 2015 & 2016 calculate sellings per label_content----
#same dataset as in earlier version
dat_hs_tot <- read_delim("raw data/verkaufsdaten täglich HS 15-16 180731.csv", 
                         delim = ';',
                         col_types = cols(date = col_date(format = "%d.%m.%Y"))) %>% # load data
    dplyr::rename(tot_sold=Verkaufte_Stücke, shop_description = ort) %>% # rename variables
    filter(article_description %in% c('KitchenTotal','Green','FavoriteTotal','BuffetTotal')) %>% # filter data for the four main meals
    mutate(week=strftime(date, format= "%V")) %>% # might get an error
    select(article_description, date, week, year, shop_description, tot_sold)

df_tot <- droplevels(dat_hs_tot) # not really necessary

# change article_description variables
df_tot$article_description <- str_replace(df_tot$article_description,"BuffetTotal","Hot and Cold")
df_tot$article_description <-  str_replace(df_tot$article_description,"KitchenTotal", "Kitchen")
df_tot$article_description <-  str_replace(df_tot$article_description,"FavoriteTotal", "Favorite")

# change first letter of shop_description
firstup <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
}
df_tot$shop_description <- firstup(df_tot$shop_description)

### 2015: calculation of the relative meal content-----
# define meal content: version 1
dt <- filter(df_tot, year == 2015) %>%
    rename(tot=tot_sold)

# meal content for mensa grüental
#meat
Fleisch_g <- cbind.data.frame(dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental",]$tot*0.8 +
                                  dt[dt["article_description"]=="Kitchen"& dt["shop_description"]=="Grüental",]$tot, # 80% of Favorite meals and 100% of Kitchen meals contain meat
                              dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental",]$date)

names(Fleisch_g) <- c("tot_sold","date") # rename
Fleisch_g$label_content <- "Meat" # add label content
Fleisch_g$shop_description <- "Grüental" # add shop_description

#vegetarian
Vegetarisch_g <- cbind.data.frame(dt[dt["article_description"]=="Green" & dt["shop_description"]=="Grüental",]$tot +
                                      dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental",]$tot*.2, # 100% of Green meals and 20% of Favorite meals are vegetarian
                                  dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental",]$date)

names(Vegetarisch_g) <- c("tot_sold","date")
Vegetarisch_g$label_content <- "Vegetarian"
Vegetarisch_g$shop_description <- "Grüental"

#buffet
Hot_Cold_g <- cbind.data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental",]$tot,
                               dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental",]$date)

names(Hot_Cold_g) <- c("tot_sold","date")
Hot_Cold_g$label_content <- "Hot and Cold"
Hot_Cold_g$shop_description <- "Grüental  "


# meal content for mensa vista
#meat
Fleisch_v <- cbind.data.frame(dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista",]$tot*0.8 +
                                  dt[dt["article_description"]=="Kitchen" & dt["shop_description"]=="Vista",]$tot, # 80% of Favorite meals and 100% of Kitchen meals contain meat
                              dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista",]$date)

names(Fleisch_v) <- c("tot_sold","date") 
Fleisch_v$label_content <- "Meat"
Fleisch_v$shop_description <- "Vista"

#vegetarian
Vegetarisch_v <- cbind.data.frame(dt[dt["article_description"]=="Green" & dt["shop_description"]=="Vista",]$tot + 
                                      dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista",]$tot*.2, # 100% of Green meals and 20% of Favorite meals are vegetarian
                                  dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista",]$date)

names(Vegetarisch_v) <- c("tot_sold","date")
Vegetarisch_v$label_content <- "Vegetarian"
Vegetarisch_v$shop_description <- "Vista"

#buffet
Hot_Cold_v <- cbind.data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Vista",]$tot,
                               dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Vista",]$date)

names(Hot_Cold_v) <- c("tot_sold","date")
Hot_Cold_v$label_content <- "Hot and Cold"
Hot_Cold_v$shop_description <- "Vista"

menu_tot5 <- rbind(Fleisch_g, Fleisch_v, Vegetarisch_g, Vegetarisch_v, Hot_Cold_g, Hot_Cold_v) # concate all data frames

#add some variables
menu_tot5$year <- year(menu_tot5$date) # add variable year
menu_tot5$week <- isoweek(menu_tot5$date) # add week nr -- ATTENTION week() returns wrong week number

### 2016: calculation of the relative meal content----
# define meal content: version 1
dt <- filter(df_tot, year == 2016) %>%
    rename(tot=tot_sold)

# meal content for mensa grüental
#meat
Fleisch_g <- cbind.data.frame(dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental",]$tot*0.8 +
                                  dt[dt["article_description"]=="Kitchen"& dt["shop_description"]=="Grüental",]$tot, # 80% of Favorite meals and 100% of Kitchen meals contain meat
                              dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental",]$date)

names(Fleisch_g) <- c("tot_sold","date") # rename 
Fleisch_g$label_content <- "Meat" # add label_content
Fleisch_g$shop_description <- "Grüental" # add shop_description

#vegetarian
Vegetarisch_g <- cbind.data.frame(dt[dt["article_description"]=="Green" & dt["shop_description"]=="Grüental",]$tot +
                                      dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental",]$tot*.2, # 100% of Green meals and 20% of Favorite meals are vegetarian
                                  dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Grüental",]$date)

names(Vegetarisch_g) <- c("tot_sold","date")
Vegetarisch_g$label_content <- "Vegetarian"
Vegetarisch_g$shop_description <- "Grüental"

#buffet
Hot_Cold_g <- cbind.data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental",]$tot,
                               dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Grüental",]$date)

names(Hot_Cold_g) <- c("tot_sold","date")
Hot_Cold_g$label_content <- "Hot and Cold"
Hot_Cold_g$shop_description <- "Grüental"


# meal content for mensa vista
#meat
Fleisch_v <- cbind.data.frame(dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista",]$tot*0.8 +
                                  dt[dt["article_description"]=="Kitchen" & dt["shop_description"]=="Vista",]$tot, # 80% of Favorite meals and 100% of Kitchen meals contain meat
                              dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista",]$date)


names(Fleisch_v) <- c("tot_sold","date")
Fleisch_v$label_content <- "Meat"
Fleisch_v$shop_description <- "Vista"

#vegetarian
Vegetarisch_v <- cbind.data.frame(dt[dt["article_description"]=="Green" & dt["shop_description"]=="Vista",]$tot + 
                                      dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista",]$tot*.2, # 100% of Green meals and 20% of Favorite meals are vegetarian
                                  dt[dt["article_description"]=="Favorite" & dt["shop_description"]=="Vista",]$date)

names(Vegetarisch_v) <- c("tot_sold","date")
Vegetarisch_v$label_content <- "Vegetarian"
Vegetarisch_v$shop_description <- "Vista"

#buffet
Hot_Cold_v <- cbind.data.frame(dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Vista",]$tot,
                               dt[dt["article_description"]=="Hot and Cold" & dt["shop_description"]=="Vista",]$date)

names(Hot_Cold_v) <- c("tot_sold","date")
Hot_Cold_v$label_content <- "Hot and Cold"
Hot_Cold_v$shop_description <- "Vista"

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


#### 2017: group meal content----
menu_tot7 <- df_agg %>%
    group_by(date, label_content, shop_description, year, week, semwk) %>%
    summarise(tot_sold = n())


#combine all three data frames
menu_tot <- bind_rows(menu_tot5, menu_tot6, menu_tot7)

# remove some data from working space
rm(list=c("Hot_Cold_v","Hot_Cold_g","Fleisch_v","Fleisch_g","Vegetarisch_v","Vegetarisch_g","menu_5","dat_hs","dat_hs_tot","df_old", "df_tot","df","dt","menu_tot5","menu_tot5_v","menu_tot5_g","menu_tot6","menu_tot6_g","menu_tot6_v","menu_tot7","menu_tot7_"))




######
# data from 2015 & 2016 calculate sellings per meal_line------
dat_hs_tot <- read_delim("raw data/verkaufsdaten täglich HS 15-16 180731.csv", 
                         delim = ';',
                         col_types = cols(date = col_date(format = "%d.%m.%Y"))) %>% # load data
    rename(tot_sold=Verkaufte_Stücke, shop_description = ort) %>% # rename variables
    filter(article_description %in% c('KitchenTotal','Green','FavoriteTotal','BuffetTotal')) %>% # filter data for the four main meals
    mutate(week=strftime(date, format= "%V")) %>% # might get an error
    filter(week >= 40 & week <= 51) %>% # select only between week 40 and 51
    mutate(date = parse_date(date)) %>%
    select(date, article_description, year, week, shop_description, tot_sold)

dat_hs_tot$week <- as.numeric(dat_hs_tot$week) # change character to numeric
dat_hs_tot$cycle <- ifelse(dat_hs_tot$week >=40 & dat_hs_tot$week <= 45, 1, 2) 
dat_hs_tot$condit <- ifelse(dat_hs_tot$week %%2 == 0 & dat_hs_tot$cycle == 1,"Basis",ifelse(dat_hs_tot$week %%2 == 1 & dat_hs_tot$cycle == 2,"Basis","Intervention")) 

df_tot <- droplevels(dat_hs_tot)

# change article_description variables
df_tot$article_description <- str_replace(df_tot$article_description,"BuffetTotal","Hot and Cold")
df_tot$article_description <-  str_replace(df_tot$article_description,"KitchenTotal", "Kitchen")
df_tot$article_description <-  str_replace(df_tot$article_description,"FavoriteTotal", "Favorite")

# change first letter of shop_description
df_tot$shop_description <- firstup(df_tot$shop_description) # see function above

# group data 2017 according meal line----
menu_tot7 <- df_agg %>%
    group_by(article_description, date, week, year, cycle, shop_description, condit) %>% 
    summarise(tot_sold=n()) %>% # group it again, for merge with other data, no condit variable
    ungroup() %>%
    select(date, article_description, year, week, cycle, shop_description, condit, tot_sold)

# merge with data 2017
menu_tot <- bind_rows(df_tot, menu_tot7)

rm(list = c("dat_hs_tot", "menu_tot7", "df_tot"))
