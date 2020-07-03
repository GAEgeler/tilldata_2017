## load environmental data

###
# state: june 2020
# author: gian-Andrea egeler
###

# requried packages
library(tidyverse)
library(readxl)
library(stringr)
library(here)


# attention readxl did some updates with new features, especially with giving names of unnames rows (old: X__1, new: ...1)
# there is a way to omit that (reference here: https://github.com/tidyverse/readxl/issues/546)
legacy_repair <- function(nms, prefix = "X", sep = "__") {
  if (length(nms) == 0) return(character())
  blank <- nms == ""
  nms[!blank] <- make.unique(nms[!blank], sep = sep)
  new_nms <- setdiff(paste(prefix, seq_along(nms), sep = sep), nms)
  nms[blank] <- new_nms[seq_len(sum(blank))]
  nms
}


## load ubp------------
# meal_name_muir: author of the calculations of ubp
ubp <- read_xlsx(here::here("raw data/Novanimal_Umweltbewertung_Menus_final.xlsx"), range="Rezepte!A3:T96", trim_ws=TRUE, col_names = T, .name_repair = legacy_repair) %>% # load specific space in excel sheet
  rename(label_content = Menuart, meal_name_muir = `Name Menu`, meal_name_comp = X__2, article_description = X__3, week = X__4,  gemuse_fruchte = `Gemüse & Früchte`, ol_fett_nuss = `Öle, Fette & Nüsse`, suss_salz_alk = `Süsses/Salziges/Alkoholisches`, foodwaste = `Foodwaste, ohne Tellerrest`, zubereitung = `Zubereitung Mensa`) %>% # rename variables
  mutate(cycle = 1, 
         method = "ubp", 
         X__5 = str_sub(X__5, end = 5)) %>% # create new variables: cycle, calculation method, date
  mutate(date=paste(str_sub(X__5, end = -3), str_sub(X__5, start = 4), 17, sep = ".")) %>% # paste date together, however need some changes
  dplyr::select(meal_name_muir, meal_name_comp, article_description, date, week, cycle, method, Kohlenhydrate, Protein, gemuse_fruchte, ol_fett_nuss, suss_salz_alk, foodwaste, zubereitung) %>% ## select variables
  # sum all variables => only for sorting in plot
  mutate(tot_ubp = Kohlenhydrate + Protein + gemuse_fruchte + ol_fett_nuss + suss_salz_alk + foodwaste + zubereitung) # add all variables together


# do some changes to the data, due to changes in meal offer, see egeler (2019): cycle, date, label_content
ubp[grep("Seitangesch", ubp$meal_name_muir),]$article_description <- "Kitchen"
ubp[grep("Seitangesch", ubp$meal_name_muir),]$week <- 46
ubp[grep("Seitangesch", ubp$meal_name_muir),]$cycle <-  2

ubp[grep("Seelachs", ubp$meal_name_muir),]$article_description <- "Kitchen"
ubp[grep("Seelachs", ubp$meal_name_muir),]$week <- 50
ubp[grep("Seelachs", ubp$meal_name_muir),]$cycle <- 2

ubp[grep("Nudeln ohne", ubp$meal_name_muir),]$date <- "Nov.14.17"
ubp[grep("Nudeln ohne", ubp$meal_name_muir),]$week <- 46
ubp[grep("Nudeln ohne", ubp$meal_name_muir),]$cycle <- 2

ubp[grep("Monte", ubp$meal_name_muir),]$week <- 44

ubp[grep("Hot", ubp$meal_name_muir), ]$date <- "Nov.9.17"
ubp[grep("Quornragout", ubp$meal_name_muir), ]$date[1] <- "Nov.10.17"

# edit and create date
ubp_ <- ubp %>%
  mutate(date = str_replace(.$date,"Dez", "Dec")) %>% 
  mutate(date = str_replace(.$date, "Okt", "Oct")) %>%  # replace month name, due to format in as.Date
  mutate(date = as.Date(.$date, format="%b.%d.%y")) # important change date to date format (not posixct)

# problem UBP information of first cycle only, 
# try do duplicate the first
# problems of six dublicates => we need to delete them manually!
t <- bind_rows(ubp_, ubp_[ ,-c(6)]) %>% 
  mutate(cycle = ifelse(is.na(cycle), 2, cycle)) # add second cycle, however now there are six meals to much (those 3 form the second cycle and their duplicated from the first cycle)

# delete six cases
t <- t[-grep("Seitangesch", t$meal_name_muir)[1],]
t <- t[-grep("Seelachs", t$meal_name_muir)[1],]
t <- t[-grep("Nudeln ohne", t$meal_name_muir)[1],]
t <- t[-grep("Eiernudeln", t$meal_name_muir)[2], ] # delete the one in the second cycle (multiple conditioning is somehow not working)
t <- t[-grep("Quornragout", t$meal_name_muir)[4], ] # delete the one on the kitchen and with ebly
t <- t[-grep("Lachsbur", t$meal_name_muir)[2], ]

group_by(t, cycle) %>% count # check, both cycles shoud sum up at 90

# add new dates to the date of the first cycle
library(lubridate)

# define date of the 2nd cycle
first_part <- t %>% 
  filter(cycle == 2 & week < 46) %>% # only 87 meals (because 3 meals have already the correct date - were added manually, see above)
  mutate(date = ifelse(.$week%%2 ==0, .$date %m+% weeks(7), .$date %m+% weeks(5))) %>% # adds for even weeks 7 weeks to the date and for the odd week 5
  mutate(date = as_date(date)) %>% # changes dates back to origin format 
  mutate(week = isoweek(date))

second_part <- first_part %>% 
  bind_rows(ubp_) # joining went bad, thus an alternative, seems to be right

#remove data frames to omit problems
rm(list = "first_part", "t")

# join with documentation to achive
library(statar) # has some features which dyplr do not have (check and _merge)

# load info_orig
source("07_change_documentary_190128_egel.R", encoding = "Latin1")

# it looks like, that the merging with info_orig has too many duplicates
# solution (not a nice one): filter info_orig only for one canteen and drop the rest (= 180 meals)
ubp_1 <- info_orig %>%
  filter(!grepl("Local ", .$article_description) & article_description != "Hot and Cold" & shop_description == "Grüental") %>% 
  dplyr::select(article_description, week, cycle, date, label_content, meal_name) %>%
  # filter(!duplicated(meal_name)) %>% # only duplicates (because of shop_description) => not a good
  join(second_part, ., on = c("article_description", "date", "cycle", "week"), kind = "full", check = m~1)  # gen = "_merge" to check if rows matched


## load gwp-----------
gwp <- read_xlsx(here::here("raw data/Novanimal_Umweltbewertung_Menus_final.xlsx"), range="Rezepte!AC3:AN96", trim_ws=TRUE, col_names = T, .name_repair = legacy_repair) %>% # load specific space in excel sheet
  rename(label_content = Menuart, meal_name_comp = X__2, gemuse_fruchte = `Gemüse & Früchte`, ol_fett_nuss = `Öle, Fette & Nüsse`, suss_salz_alk = `Süsses/Salziges/Alkoholisches`, foodwaste = `Foodwaste, ohne Tellerrest`, zubereitung = `Zubereitung Mensa`) %>% # rename variables
  mutate(method = "gwp") %>% # create new variables: calculation method
  dplyr::select(meal_name_comp, method, Kohlenhydrate, Protein, gemuse_fruchte, ol_fett_nuss, suss_salz_alk, foodwaste, zubereitung) %>%
  mutate(tot_gwp = Kohlenhydrate + Protein + gemuse_fruchte + ol_fett_nuss + suss_salz_alk + foodwaste + zubereitung)

# date, week and cycle is missing => merge with ubp
gwp_ <- left_join(gwp[ , -2], ubp_[ , c("meal_name_comp", "article_description", "date", "cycle", "week")], by = c("meal_name_comp"))


# problems of six dublicates => we need to delete them manually!
t <- bind_rows(gwp_, gwp_[ , -12]) %>% # drop cycle
  mutate(cycle = ifelse(is.na(cycle), 2, cycle))  # add second cycle, however now there are six meals to much (those 3 form the second cycle and their duplicated from the first cycle)
  

# delete six cases (attention in comparison to the ubp dataset, there ist only meal_name_comp, thus the search strings need to be changed)
# filtering with duplicated() is not doing the job, thus delete manually
# (multiple conditioning is somehow not working)
t <- t[-grep("Seitanragout", t$meal_name_comp)[1],] # delete one (due to duplicate)
t <- t[-grep("Seelachs", t$meal_name_comp)[1],] # delete one (due to duplicate)
t <- t[-grep("Nudeln ohne", t$meal_name_comp)[1],] # delete one (due to duplicate) 
t <- t[-grep("\\(Nudeln mit", t$meal_name_comp)[2], ] # delete the one in the second cycle 
t <- t[-grep("Quornragout", t$meal_name_comp)[4], ] # delete the one on the kitchen and with ebly
t <- t[-grep("Lachsbur", t$meal_name_comp)[2], ] # delete the one in the second cycle

group_by(t, cycle) %>% count # check, both cycles shoud be 90

# add new dates to the date of the first cycle
# add date only for the second cycle!
first_part <- t %>% 
  filter(cycle == 2 & week < 46) %>% # only 87 meals (because 3 meals have already the correct date - were added manually, see above)
  mutate(date = ifelse(.$week%%2 ==0, .$date %m+% weeks(7), .$date %m+% weeks(5))) %>% # adds for even weeks 7 weeks to the date and for the odd week 5
  mutate(date = as_date(date)) %>% # changes dates back to origin format 
  mutate(week = isoweek(date))

# then add the first cycle
second_part <- first_part %>% 
    bind_rows(gwp_) # joining went bad, thus an alternative, seems to be right


# join with documentation to achive
# it looks like, that the merging with info_orig has too many duplicates
# solution (not a nice one): filter info_orig only for one canteen and drop the rest (= 180 meals)
# attention with meal_name and label_content: not identical between this two files (label_content.y out of info_orig is important)
gwp_1 <- info_orig %>%
  filter(!grepl("Local ", .$article_description) & article_description != "Hot and Cold" & shop_description == "Grüental") %>% 
  dplyr::select(article_description, week, cycle, date, label_content, meal_name) %>%
  # filter(!duplicated(meal_name)) %>% # only duplicates (because of shop_description) => not a good
  join(second_part, ., on = c("article_description", "date", "cycle", "week"), kind = "full", check = m~1)  # gen = "_merge" to check if rows matched


# merge dataframes of environmental data
# to merge with df_agg or df_2017 this file should it do
# contains only tot_ubp and tot_gwp => for more information see envir_tot
envir <- left_join(ubp_1[ , c("meal_name_comp", "meal_name_muir", "article_description", "label_content", "date", "week", "cycle", "tot_ubp")], 
                   gwp_1[ , c("meal_name_comp", "article_description", "label_content", "date", "week", "cycle", "tot_gwp")], 
                   by = c("meal_name_comp", "article_description", "label_content", "date", "week", "cycle"))



# # do we need that one?
# # 1260 obs / 14 returns us 90 meals (for each meal 7 criteria for ubp and gwp e.g. foodwaste)
# envir_tot <- inner_join(ubp_long, gwp_long, by = c("meal_name.y" = "meal_name", "article_description", "label_content.y" = "label_content", "date", "content", "cycle", "meal_name_comp")) %>%
#   rename("meal_name" = "meal_name.y", "label_content" = "label_content.y") %>%
#   mutate(week = isoweek(.$date))


#delete other datasets
rm(list=c("gwp", "gwp_1", "ubp", "ubp_1",
          "t", "first_part", "second_part"))
