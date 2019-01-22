## load data -----

# status 11.09.18 // egel

# required packages
pack <- c("dplyr", "lubridate", "readr", "stringr", "readxl", "here")
lapply(pack, function(x){do.call("library", list(x))})


# set dir for calling source code
# it needs the full directory to call the script from another script
# its not working with the here package

# load documentation -----------
#here("augmented data/menu_inhalt_protein_180420_egel07_final.csv" # seems not to work, dont know why
info_orig <- read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/02_tilldata_2017/augmented data/menu_inhalt_protein_180420_egel07_final.csv", delim =';', locale = locale(encoding = 'LATIN1'),
                         col_types = cols(date = col_date(format = "%d.%m.%Y"))) %>% # pay attention to the date format (now is it Date format not POSIXct)
    mutate(date = as.Date(.$date)) %>%
    mutate(week = isoweek(.$date)) %>%
    mutate(condit = ifelse(.$cycle ==1 & .$week %%2 == 0,"Basis",ifelse(.$cycle == 2 & .$week %%2 == 1,"Basis","Intervention")))

# load nutrition data---------
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
    select(meal_name, article_description, date, week, cycle, ebp_points, ebp_label, teller_points, teller_label)

#special edits: seelachsnuggets, vesuvio, seitan, hot pumkin, quornragout, moussaka with meat
nutri_wide[grep("Seelachsn", nutri_wide$meal_name),]$cycle <- 2  #change week of seelachsnuggets to week nr 
nutri_wide[grep("Vesuvio_46",nutri_wide$meal_name),]$cycle <- 2 #change week nr of vesuvio, because without egg
nutri_wide[grep("Seitangeschn",nutri_wide$meal_name),]$cycle <- 2 # change week nr of seitangeschnetzeltes

nutri_wide[(nutri_wide$article_description == "Favorite") & (grepl("Quornragout",nutri_wide$meal_name)),]$date <- "2017-11-10" # changes date in CET
nutri_wide[grep("Hot", nutri_wide$meal_name),]$date <-  strptime(as.character("2017-11-09"), "%Y-%m-%d")

nutri_wide <- nutri_wide[-grep("85_Auberginen-Moussaka_45K9.11",nutri_wide$meal_name),] # delete moussaka with meat 
nutri_wide[grep("85_Auberginen-Moussaka",nutri_wide$meal_name),]$date <- "2017-11-09" # change date to correct date in cycle 1

#load environmental data----------------

ubp <- read_xlsx("S:/pools/n/N-IUNR-nova-data/09_bewertung_umwelt/Auswertung/Novanimal_Umweltbewertung_Menüs_final.xlsx", range="Rezepte!A3:T96", trim_ws=TRUE, col_names = T) %>% # load specific space in excel sheet
    rename(label_content = Menuart, meal_name = `Name Menu`, meal_name_comp = X__2, article_description = X__3, week = X__4,  gemuse_fruchte = `Gemüse & Früchte`, ol_fett_nuss = `Öle, Fette & Nüsse`, suss_salz_alk = `Süsses/Salziges/Alkoholisches`, foodwaste = `Foodwaste, ohne Tellerrest`, zubereitung = `Zubereitung Mensa`) %>% # rename variables
    mutate(cycle = 1, method = "ubp", X__5 = str_sub(X__5, end = 5)) %>% # create new variables: cycle, calculation method, date
    mutate(date=paste(str_sub(X__5, end = -3), str_sub(X__5, start = 4), 17, sep = ".")) %>% # paste date together, however need some changes
    select(meal_name, meal_name_comp, article_description, label_content, date, week, cycle, method, Kohlenhydrate, Protein, gemuse_fruchte, ol_fett_nuss, suss_salz_alk, foodwaste, zubereitung) %>% ## select variables
    # sum all variables => only for sorting in plot
    mutate(tot_ubp = Kohlenhydrate + Protein + gemuse_fruchte + ol_fett_nuss + suss_salz_alk + foodwaste + zubereitung)

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
    mutate(date=as.Date(ubp$date,format="%b.%d.%y"))


# load hot and cold buffet information-----
buffet <- read_xlsx("S:/pools/n/N-IUNR-nova-data/02_kassendaten/02_tilldata_2017/augmented data/buffet_animal_180425_03egel.xlsx") %>%
    mutate(article_description = "Hot and Cold") %>%
    mutate(date = parse_date(.$date)) # to get same date format as other data frames


# data from 2017--------
df_17 <- read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/02_tilldata_2017/augmented data/data_edit_180802_egel.csv", delim = ";", locale = locale(encoding = "LATIN1")) %>%
    mutate(date = as.Date(date)) 


# merge information with data from 2017-----
# documentation with data of nutrition 
info_compl <- left_join(info_orig, nutri_wide, by=c("article_description","date", "cycle", "week")) %>% # attention only first cycle is represented
    select(-meal_name.y,-`Kein Protein`,-Kommentar, -content) %>%
    rename(meal_name = meal_name.x)

# documentation with data of environment
info_compl <- left_join(info_compl, ubp, by=c("article_description","date", "cycle", "week")) %>% # attention only first cycle is represented
    select(-meal_name.y,-label_content.y) %>%
    rename(meal_name = meal_name.x, label_content = label_content.x)

# documentation with buffet data
info_compl <- left_join(info_compl, buffet, by=c("date","article_description","shop_description"))

# merge documentation with data 2017
info <- select(info_orig, date, article_description, label_content, cycle, meal_name, shop_description) # subset of info_orig
info_ <- select(info_compl, date, article_description,label_content, cycle, meal_name, shop_description, tot_ubp, ebp_points, ebp_label, teller_points, teller_label, buffet_animal_comp) # subset of info_compl
df_7 <- left_join(df_17,info, by = c("shop_description","date","article_description","cycle")) # join with info_orig (less variables)
df_7_ <- left_join(df_17,info_, by = c("shop_description","date","article_description","cycle")) # full join with info_complete

#save data for Research Methods
write_delim(df_7_, "C:/Users/egel/switchdrive/ZHAW/03_Lehre/research methods MSc/übungen_reme_180911_egel/novanimal_data_180911_egel.csv", delim = ";")

# data from 2015 & 2016----

