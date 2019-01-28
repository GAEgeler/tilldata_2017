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

# join with documentation to achive the best names matches
ubp_ <- info_orig %>%
    dplyr::select(article_description, week, cycle, date, label_content, meal_name) %>%
    filter(duplicated(meal_name)) %>% # only duplicates (because of shop_description)
    left_join(ubp, .,by = c("article_description", "cycle", "date", "week")) %>%
    filter(!duplicated(meal_name.y))

# melt into long format
ubp_long <- melt(ubp_, id.vars = c("meal_name.y", "meal_name_comp", "date", "cycle", "article_description", "label_content.y", "label_content.x", "tot_ubp"), measure.vars = c("Kohlenhydrate", "Protein", "gemuse_fruchte", "ol_fett_nuss", "suss_salz_alk", "foodwaste", "zubereitung"), variable.name = "content", value.name = "ubp")


## load gwp
gwp <- read_xlsx("S:/pools/n/N-IUNR-nova-data/09_bewertung_umwelt/Auswertung/Novanimal_Umweltbewertung_Menüs_final.xlsx", range="Rezepte!AB3:AM96", trim_ws=TRUE, col_names = T) %>% # load specific space in excel sheet
    rename(label_content = Menuart, meal_name_comp = X__2, gemuse_fruchte = `Gemüse & Früchte`, ol_fett_nuss = `Öle, Fette & Nüsse`, suss_salz_alk = `Süsses/Salziges/Alkoholisches`, foodwaste = `Foodwaste, ohne Tellerrest`, zubereitung = `Zubereitung Mensa`) %>% # rename variables
    mutate(method = "gwp") %>% # create new variables: calculation method
    dplyr::select(meal_name_comp, label_content, method, Kohlenhydrate, Protein, gemuse_fruchte, ol_fett_nuss, suss_salz_alk, foodwaste, zubereitung) %>%
    mutate(tot_gwp = Kohlenhydrate + Protein + gemuse_fruchte + ol_fett_nuss + suss_salz_alk + foodwaste + zubereitung)

# change label content for merge
gwp$label_content <- str_replace(gwp$label_content, "Vegetarisch", "ovo-lakto-vegetarisch")
gwp[grep("Seitanragout", gwp$meal_name_comp), ]$label_content <- "vegan (Fleischersatz)"

# date is missing => merge with ubp
gwp_ <- left_join(gwp, ubp[c("meal_name_comp", "label_content", "article_description", "date", "week", "cycle")], by = c("meal_name_comp", "label_content"))

# merge with documentation
gwp_1 <- info_orig %>%
    dplyr::select(article_description, week, cycle, date, label_content, meal_name) %>%
    filter(duplicated(meal_name)) %>% # only duplicates (because of shop_description)
    left_join(gwp_, .,by = c("article_description", "cycle", "date", "week")) %>%
    filter(!duplicated(meal_name))

# melt into long format
gwp_long <- melt(gwp_1, id.vars = c("meal_name", "meal_name_comp", "date", "cycle", "article_description", "label_content.y", "label_content.x", "tot_gwp"), measure.vars = c("Kohlenhydrate", "Protein", "gemuse_fruchte", "ol_fett_nuss", "suss_salz_alk", "foodwaste", "zubereitung"), variable.name = "content", value.name = "gwp")

# merge dataframes of environmental data
# generates 7 duplicates of each date => dont know why
envir_tot <- inner_join(ubp_long[-7], gwp_long[-c(2,4,7)], by = c("meal_name.y" = "meal_name", "article_description", "label_content.y", "date", "content")) %>%
    rename("meal_name" = "meal_name.y", "label_content" = "label_content.y") %>%
    mutate(week = isoweek(.$date)) %>% 
    filter(!duplicated(meal_name))


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

nutri_wide_ <- info_orig %>%
    dplyr::select(article_description, week, cycle, date, label_content, meal_name) %>%
    left_join(nutri_wide, .,by = c("article_description", "cycle", "date", "week")) %>%
    filter(!duplicated(meal_name.y)) %>%
    dplyr::select(-meal_name.x)


# nutritionoal data into long format 
nutri_long <- melt(nutri_wide_, id.vars = c("meal_name.y", "date", "cycle", "article_description", "label_content"), measure.vars = c("ebp_points", "ebp_label", "teller_points", "teller_label"))


#delete other datasets
rm(list=c("gwp", "gwp_", "nutri_wide",  "pck",  "ubp" ,        
           "ubp_long", "gwp_long", "nutri_long"))
