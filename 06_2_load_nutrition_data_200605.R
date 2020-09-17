# load nutritional data

###
# state: june 2020
# author: gian-Andrea egeler
###

# requried packages
library(tidyverse)
library(readxl)
library(stringr)
library(here)

#load & edit data--------
nutri <- read_xlsx(here::here("raw data/Zusammenfassung_EBP_Tellermodell_V7.xlsx"), range = "Tabelle1!A6:H99", trim_ws = T, col_names = F, .name_repair = legacy_repair) %>%
    rename(week = X__1, meal_name = X__2, ebp_points = X__5, ebp_label = X__6, teller_points = X__7, teller_label = X__8) %>%
    mutate(info = sub(".*_","",.$meal_name)) %>% # search string for _ and grep all after
    mutate(week = as.numeric(str_sub(.$info,start = 1, end = 2))) %>% # grep fist to letters of string as week
    mutate(date = paste0(str_sub(.$info, start=4),".17")) %>% # grep and paste to date
    mutate(meal_line = str_sub(.$info, start=3, end=3)) %>%
    mutate(article_description = ifelse(.$meal_line == "F","Favorite", ifelse(.$meal_line == "K","Kitchen", ifelse(.$meal_line == "G","World","no_info")))) %>%
    mutate(date = sub("\\.\\.",".",.$date)) %>% # replace double dot
    mutate(date = parse_date(.$date, format = "%d.%m.%y")) %>% # convert to date format for merging with documentation, PAY ATTENTION to date format for merging, trying with CET and not UTC
    mutate(cycle = 1) %>%
    dplyr::select(meal_name, article_description, date, week, cycle, ebp_points, ebp_label, teller_points, teller_label)

#special edits------
#seelachsnuggets, vesuvio, seitan, hot pumkin, quornragout, moussaka with meat
nutri[grep("Seelachsn", nutri$meal_name),]$cycle <- 2  #change week of seelachsnuggets to week nr 
nutri[grep("Vesuvio_46",nutri$meal_name),]$cycle <- 2 #change week nr of vesuvio, because without egg
nutri[grep("Seitangeschn",nutri$meal_name),]$cycle <- 2 # change week nr of seitangeschnetzeltes

nutri[(nutri$article_description == "Favorite") & (grepl("Quornragout",nutri$meal_name)),]$date <- strptime(as.character("2017-11-10"), "%Y-%m-%d") # changes date in CET
nutri[grep("Hot", nutri$meal_name),]$date <-  strptime(as.character("2017-11-09"), "%Y-%m-%d")

nutri <- nutri[-grep("85_Auberginen-Moussaka_45K9.11",nutri$meal_name),] # delete moussaka with meat 
nutri[grep("85_Auberginen-Moussaka",nutri$meal_name),]$date <- strptime(as.character("2017-11-09"), "%Y-%m-%d") # change date to correct date in cycle 1


# add date to the second cycle------
# problems of six dublicates => we need to delete them manually!
t <- bind_rows(nutri, nutri[, -5]) %>% # drop cycle
    mutate(cycle = ifelse(is.na(cycle), 2, cycle))  # add second cycle, however now there are six meals to much (those 3 form the second cycle and their duplicated from the first cycle)


# delete six cases (attention in comparison to the ubp dataset, there ist only meal_name_comp, thus the search strings need to be changed)
# filtering with duplicated() is not doing the job, thus delete manually
# (multiple conditioning is somehow not working)
t <- t[-grep("Seitangesch", t$meal_name)[1],] # delete one (due to duplicate)
t <- t[-grep("Seelachs", t$meal_name)[1],] # delete one (due to duplicate)
t <- t[-grep("20_Vesuvio_46F1", t$meal_name)[1],] # delete one (due to duplicate) 
t <- t[-grep("20_Vesuvio_mit Eiernudeln_41F10.10", t$meal_name)[2], ] # delete the one in the second cycle 
t <- t[-grep("16_QuornragoutStroganoff_41K9.10", t$meal_name)[2], ] # delete the one on the kitchen and with ebly
t <- t[-grep("Lachsbur", t$meal_name)[2], ] # delete the one in the second cycle

group_by(t, cycle) %>% count # check, both cycles shoud be 90

# add new dates to the date of the first cycle
# add date only for the second cycle!
first_part <- t %>% 
    filter(cycle == 2 & week < 46) %>% # only 87 meals (because 3 meals have already the correct date - were added manually, see above)
    mutate(date = ifelse(.$week%%2 ==0, .$date %m+% weeks(7), .$date %m+% weeks(5))) %>% # add weeks: for even weeks 7 weeks to the date and for the odd weeks 5
    mutate(date = as_date(date)) %>% # changes dates back to origin format 
    mutate(week = isoweek(date))

# then add the first cycle
second_part <- first_part %>% 
    bind_rows(nutri) # joining went bad, thus an alternative, seems to be right


# merge info of documentation with the nutrition info----------
# for merge to till data
nutri_ <- info_orig %>%
    filter(!grepl("Local ", .$article_description) & article_description != "Hot and Cold" & shop_description == "Grüental") %>%
    dplyr::select(article_description, week, cycle, date, label_content, meal_name) %>%
    left_join(second_part, .,by = c("article_description", "cycle", "date", "week")) %>%
    select(-meal_name.x) %>% 
    rename(meal_name = meal_name.y)



#delete other datasets
rm(list=c("t", "first_part", "second_part"
          ))
