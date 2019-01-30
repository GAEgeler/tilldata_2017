## load additional data-----

###
# state: january 2019
# author: gian-Andrea egeler
###

# load packages
pck <- c("dplyr", "stringr", "readr", "readxl", "reshape2", "lubridate")
lapply(pck, function(x){do.call("library", list(x))})

# load documentation ----------- (last update janaury 2019)
# of with info about fish
source("07_change_documentary_190128_egel.R") # some changes in label_content


# load hot and cold buffet information-----
buffet <- read_xlsx("augmented data/buffet_animal_180425_03egel.xlsx") %>%
    mutate(article_description = "Hot and Cold") %>%
    mutate(date = parse_date(.$date)) %>% # to get same date format as other data frames
    mutate(shop_description = str_replace(.$shop_description, " .*", ""))


# load environmental data-----------
## load ubp
ubp <- read_xlsx("S:/pools/n/N-IUNR-nova-data/09_bewertung_umwelt/Auswertung/Novanimal_Umweltbewertung_Menüs_final.xlsx", range="Rezepte!A3:T96", trim_ws=TRUE, col_names = T) %>% # load specific space in excel sheet
    rename(label_content = Menuart, meal_name = `Name Menu`, meal_name_comp = X__2, article_description = X__3, week = X__4,  gemuse_fruchte = `Gemüse & Früchte`, ol_fett_nuss = `Öle, Fette & Nüsse`, suss_salz_alk = `Süsses/Salziges/Alkoholisches`, foodwaste = `Foodwaste, ohne Tellerrest`, zubereitung = `Zubereitung Mensa`) %>% # rename variables
    mutate(cycle = 1, method = "ubp", X__5 = str_sub(X__5, end = 5)) %>% # create new variables: cycle, calculation method, date
    mutate(date=paste(str_sub(X__5, end = -3), str_sub(X__5, start = 4), 17, sep = ".")) %>% # paste date together, however need some changes
    dplyr::select(meal_name, meal_name_comp, article_description, label_content, date, week, cycle, method, Kohlenhydrate, Protein, gemuse_fruchte, ol_fett_nuss, suss_salz_alk, foodwaste, zubereitung) %>% ## select variables
    # sum all variables => only for sorting in plot
    mutate(tot_ubp = Kohlenhydrate + Protein + gemuse_fruchte + ol_fett_nuss + suss_salz_alk + foodwaste + zubereitung) # add all variables together


# do some changes to the data: cycle, date, label_content
ubp[grep("Seitangesch", ubp$meal_name),]$article_description <- "Kitchen"
ubp[grep("Seitangesch", ubp$meal_name),]$week <- 46
ubp[grep("Seitangesch", ubp$meal_name),]$cycle <-  2

ubp[grep("Seelachs", ubp$meal_name),]$article_description <- "Kitchen"
ubp[grep("Seelachs", ubp$meal_name),]$week <- 50
ubp[grep("Seelachs", ubp$meal_name),]$cycle <- 2

ubp[grep("Nudeln ohne", ubp$meal_name),]$date <- "Nov.14.17"
ubp[grep("Nudeln ohne", ubp$meal_name),]$week <- 46
ubp[grep("Nudeln ohne", ubp$meal_name),]$cycle <- 2

ubp[grep("Monte", ubp$meal_name),]$week <- 44

ubp[grep("Hot", ubp$meal_name), ]$date <- "Nov.9.17"
ubp[grep("Quornragout", ubp$meal_name), ]$date[1] <- "Nov.10.17"

# edit and create date
ubp <- ubp %>%
    mutate(date=as.Date(ubp$date,format="%b.%d.%y")) # important change date to date format (not posixct)

# problem UBP information of first cycle only, 
# try do duplicate the first
# problems of six dublicates => we need to delete them manually!
t <- bind_rows(ubp, ubp[ ,-c(7)]) %>% 
    mutate(cycle = ifelse(is.na(cycle), 2, cycle)) # add second cycle, however now there are six meals to much (those 3 form the second cycle and their duplicated from the first cycle)

# delete six cases
t <- t[-grep("Seitangesch", t$meal_name)[1],]
t <- t[-grep("Seelachs", t$meal_name)[1],]
t <- t[-grep("Nudeln ohne", t$meal_name)[1],]
t <- t[-grep("Eiernudeln", t$meal_name)[2], ] # delete the one in the second cycle (multiple conditioning is somehow not working)
t <- t[-grep("Quornragout", t$meal_name)[4], ] # delete the one on the kitchen and with ebly
t <- t[-grep("Lachsbur", t$meal_name)[2], ]

group_by(t, cycle) %>% count # check, both cycles shoud be 90

# add new dates to the date of the first cycle
library(lubridate)
library(anytime)

first_part <- t %>% 
    filter(cycle == 2 & week < 46) %>% 
    mutate(date = ifelse(.$week%%2 ==0, .$date %m+% weeks(7), .$date %m+% weeks(5))) %>% # adds for even weeks 7 weeks to the date and for the odd week 5
    mutate(date = anydate(date)) %>% # changes dates back to origin format 
    mutate(week = isoweek(date))

ubp <- t %>% 
    filter(cycle == 1 | week > 45) %>% 
    bind_rows(first_part) # joining went bad, thus an alternative, seems to be right


# join with documentation to achive
library(statar) # has some features which dyplr do not have (check and _merge)

# it looks like, that the merging with info_orig has too many duplicates
# solution (not a nice one): filter info_orig only for one canteen and drop the rest (= 180 meals)
ubp_ <- info_orig %>%
    filter(!grepl("Local ", .$article_description) & article_description != "Hot and Cold" & shop_description == "Grüental") %>% 
    dplyr::select(article_description, week, cycle, date, label_content, meal_name) %>%
    # filter(!duplicated(meal_name)) %>% # only duplicates (because of shop_description) => not a good
    join(ubp, ., on = c("article_description", "date", "cycle", "week"), kind = "full", check = m~1)  # gen = "_merge" to check if rows matched

# melt into long format
# meal_name.x = names from the original file
# meal_name_comp = names form the original file
# label_content.x = names/labels from the original file
# meal_name.y = names from info_orig
ubp_long <- melt(ubp_, id.vars = c("meal_name.y", "meal_name_comp", "date", "cycle", "article_description", "label_content.y", "tot_ubp"), measure.vars = c("Kohlenhydrate", "Protein", "gemuse_fruchte", "ol_fett_nuss", "suss_salz_alk", "foodwaste", "zubereitung"), variable.name = "content", value.name = "ubp")


## load gwp
gwp <- read_xlsx("S:/pools/n/N-IUNR-nova-data/09_bewertung_umwelt/Auswertung/Novanimal_Umweltbewertung_Menüs_final.xlsx", range="Rezepte!AB3:AM96", trim_ws=TRUE, col_names = T) %>% # load specific space in excel sheet
    rename(label_content = Menuart, meal_name_comp = X__2, gemuse_fruchte = `Gemüse & Früchte`, ol_fett_nuss = `Öle, Fette & Nüsse`, suss_salz_alk = `Süsses/Salziges/Alkoholisches`, foodwaste = `Foodwaste, ohne Tellerrest`, zubereitung = `Zubereitung Mensa`) %>% # rename variables
    mutate(method = "gwp") %>% # create new variables: calculation method
    dplyr::select(meal_name_comp, label_content, method, Kohlenhydrate, Protein, gemuse_fruchte, ol_fett_nuss, suss_salz_alk, foodwaste, zubereitung) %>%
    mutate(tot_gwp = Kohlenhydrate + Protein + gemuse_fruchte + ol_fett_nuss + suss_salz_alk + foodwaste + zubereitung)

# change label content for merge
gwp$label_content <- str_replace(gwp$label_content, "Vegetarisch", "ovo-lakto-vegetarisch")
gwp[grep("Seitanragout", gwp$meal_name_comp), ]$label_content <- "vegan (Fleischersatz)"

# date, week and cycle is missing => merge with ubp
gwp_ <- left_join(gwp, ubp[c("meal_name_comp", "label_content", "article_description", "date", "week", "cycle")], by = c("meal_name_comp", "label_content"))

# merge with documentation
gwp_1 <- info_orig %>%
    filter(!grepl("Local ", .$article_description) & article_description != "Hot and Cold" & shop_description == "Grüental") %>% 
    dplyr::select(article_description, week, cycle, date, label_content, meal_name) %>%
    # filter(!duplicated(meal_name)) %>% # only duplicates (because of shop_description) => not a good
    join(gwp_, ., on = c("article_description", "date", "cycle", "week"), kind = "full", check = m~1)  


# melt into long format
# meal_name_comp = names form the original file
# label_content.x = names/labels from the original file
# meal_name = names from info_orig (corresponds to meal_name.y in ubp data set)
gwp_long <- melt(gwp_1, id.vars = c("meal_name", "meal_name_comp", "date", "cycle", "article_description", "label_content.y", "tot_gwp"), measure.vars = c("Kohlenhydrate", "Protein", "gemuse_fruchte", "ol_fett_nuss", "suss_salz_alk", "foodwaste", "zubereitung"), variable.name = "content", value.name = "gwp")

# merge dataframes of environmental data
# to merge with df_agg or df_2017 this file should it do
# contains only tot_ubp and tot_gwp => for more information see envir_tot
envir <- left_join(ubp_[ , c("meal_name_comp", "meal_name.y", "article_description", "label_content.y", "date", "week", "cycle", "tot_ubp")], 
                    gwp_1[ , c("meal_name_comp", "meal_name", "article_description", "label_content.y", "date", "week", "cycle", "tot_gwp")], 
                    by = c("meal_name.y" = "meal_name", "meal_name_comp", "article_description", "label_content.y", "date", "week", "cycle")) %>%
    rename("meal_name" = "meal_name.y", "label_content" = "label_content.y")


# do we need that one?
# 1260 obs / 14 returns us 90 meals (for each meal 7 criteria for ubp and gwp e.g. foodwaste)
envir_tot <- inner_join(ubp_long, gwp_long, by = c("meal_name.y" = "meal_name", "article_description", "label_content.y", "date", "content", "cycle", "meal_name_comp")) %>%
    rename("meal_name" = "meal_name.y", "label_content" = "label_content.y") %>%
    mutate(week = isoweek(.$date))


# load nutritional data-----------
nutri_wide <- read_xlsx("S:/pools/n/N-IUNR-nova-data/08_bewertung_ausgewogenheit/Zusammenfassung_EBP_Tellermodell_V7.xlsx", range = "Tabelle1!A6:H99", trim_ws = T, col_names = F) %>%
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

#special edits: seelachsnuggets, vesuvio, seitan, hot pumkin, quornragout, moussaka with meat
nutri_wide[grep("Seelachsn", nutri_wide$meal_name),]$cycle <- 2  #change week of seelachsnuggets to week nr 
nutri_wide[grep("Vesuvio_46",nutri_wide$meal_name),]$cycle <- 2 #change week nr of vesuvio, because without egg
nutri_wide[grep("Seitangeschn",nutri_wide$meal_name),]$cycle <- 2 # change week nr of seitangeschnetzeltes

nutri_wide[(nutri_wide$article_description == "Favorite") & (grepl("Quornragout",nutri_wide$meal_name)),]$date <- "2017-11-10" # changes date in CET
nutri_wide[grep("Hot", nutri_wide$meal_name),]$date <-  strptime(as.character("2017-11-09"), "%Y-%m-%d")

nutri_wide <- nutri_wide[-grep("85_Auberginen-Moussaka_45K9.11",nutri_wide$meal_name),] # delete moussaka with meat 
nutri_wide[grep("85_Auberginen-Moussaka",nutri_wide$meal_name),]$date <- "2017-11-09" # change date to correct date in cycle 1

# nutri_wide_ <- info_orig %>%
#     filter(here comes filter from above!)
#     dplyr::select(article_description, week, cycle, date, label_content, meal_name) %>%
#     left_join(nutri_wide, .,by = c("article_description", "cycle", "date", "week")) %>%
#     filter(!duplicated(meal_name.y)) %>%
    


# attention there is only data for the first cycle => in case we need that infomation see code from above!

# # nutritionoal data into long format 
# nutri_long <- melt(nutri_wide_, id.vars = c("meal_name.y", "date", "cycle", "article_description", "label_content"), measure.vars = c("ebp_points", "ebp_label", "teller_points", "teller_label"))


#delete other datasets
rm(list=c("gwp", "gwp_", "nutri_wide_", "nutri_wide",
          "pck",  "ubp" , "t", "first_part",   
          "ubp_long", "gwp_long", "nutri_long"))
