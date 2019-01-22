####
## Umwelt und Gesundheitsdaten
####

# status: 12.6.18//egel

# check all again

#load data ubp and edit some special cases: LONG FORMAT-----------------
ubp <- read_xlsx("S:/pools/n/N-IUNR-nova-data/09_bewertung_umwelt/Auswertung/Umweltbewertung_Menusv0.21.xlsx", range="Rezepte!D4:T96", trim_ws=TRUE, col_names = F) %>% # load specific space in excel sheet
    rename(meal_name = X__1, article_description = X__2, week = X__3, tot_ingred = X__9,  carbs = X__10,  protein = X__11,   veg_fruit = X__12, oil_fat_nut = X__13,  sweet_salt = X__14,  beverag = X__15, foodwaste = X__16, kitchen = X__17, total = X__18) %>% # rename variables
    mutate(cycle = 1, method = "ubp", X__4=str_sub(X__4, end = -2)) %>% # create some new variables, like cycle, calculation method, date
    mutate(date=paste(str_sub(X__4, end=-3), str_sub(X__4, start=4),17, sep = ".")) %>% # paste date together, however need some changes
    select(meal_name, article_description, date, week, cycle, method, carbs,  protein,   veg_fruit, oil_fat_nut,  sweet_salt,  beverag,  kitchen, total) ## select variables

# edit and create date
ubp <- ubp %>%
    mutate(date=as.POSIXct(ubp$date,format="%b.%d.%y"))

#load data gwp and edit some special cases
gwp <- read_xlsx("S:/pools/n/N-IUNR-nova-data/09_bewertung_umwelt/Auswertung/Umweltbewertung_Menusv0.21.xlsx", range="Rezepte!AD4:AL96", trim_ws=TRUE, col_names = F) %>%
    rename(meal_name = X__1, tot_ingred = X__2,  carbs = X__3,  protein = X__4,   veg_fruit = X__5, oil_fat_nut = X__6,  sweet_salt = X__7,  beverag = X__8, foodwaste = X__9, kitchen = X__10, total = X__11) %>%
    mutate(cycle = ubp$cycle, article_description = ubp$article_description, week = ubp$week, method="gwp", date=ubp$date) %>%
    select(meal_name, article_description, date,week, cycle, method,  carbs,  protein,   veg_fruit, oil_fat_nut,  sweet_salt,  beverag, kitchen,total) 

#concate ubp and gwp
envir <- bind_rows(ubp, gwp)

#special edits: insert manually some information about meals: seitanragoout,lachsnuggets, vesuvio
envir[grep("Seitanragout",envir$meal_name ),]$date <- "2017-11-13" # insert date 
envir[grep("Seitanragout",envir$meal_name ),]$article_description <- "Kitchen" # insert meal line
envir[grep("Seitanragout",envir$meal_name ),]$week <- 46 # insert week nr
envir[grep("Seitanragout",envir$meal_name ),]$cycle <- 2 # change cycle number

envir[grep("Seelachs",envir$meal_name ),]$date <- "2017-12-12" # insert date
envir[grep("Seelachs",envir$meal_name ),]$article_description <- "Kitchen" # insert meal line
envir[grep("Seelachs",envir$meal_name ),]$week <- 50 # insert week nr
envir[grep("Seelachs",envir$meal_name ),]$cycle <- 2# change cycle number

envir[grep("ohne Ei",envir$meal_name ),]$date <- "2017-11-14" # insert date
envir[grep("ohne Ei",envir$meal_name ),]$week <- 46 # insert week nr
envir[grep("ohne Ei",envir$meal_name ),]$cycle <- 2 # change cycle nr
envir[grep("mit Ei",envir$meal_name ),]$date <- "2017-10-10" # insert correct date

envir[grep("Hot",envir$meal_name),]$date <- "2017-11-09" # change date of hot pumpkin and quorn stroganoff
envir[grepl("Quornragout",envir$meal_name) & envir$week == 45,]$date <- "2017-11-10"

envir[grep("Monte Christo",envir$meal_name ),]$week <- 44 # change week nr to 44

# test <- filter(envir, grepl("Quornragout",envir$meal_name) & article_description == "Favorite") %>% 
#     mutate(date = "2017-11-10")

# save data
write_delim(envir,"S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files/envir_imp_180612_egel.csv", delim = ';')

#load data nutrition and edit some cases: LONG FORMAT-----------
ebp <- read_xlsx("S:/pools/n/N-IUNR-nova-data/08_bewertung_gesundheit/Zusammenfassung_EBP_Tellermodell_V6.xlsx", range = "Tabelle1!A6:F99", trim_ws = T, col_names = F) %>%
    rename(week = X__1, meal_name = X__2, points = X__5, label = X__6) %>%
    mutate(method = "ebp") %>%
    mutate(info = sub(".*_","",.$meal_name)) %>% # search string for _ and grep all after
    mutate(week = as.numeric(str_sub(.$info,start = 1, end = 2))) %>% # grep fist to letters of string as week
    mutate(date = paste0(str_sub(.$info, start=4),".17")) %>% # grep and paste to date
    mutate(meal_line = str_sub(.$info, start=3, end=3)) %>%
    mutate(article_description = ifelse(.$meal_line == "F","Favorite", ifelse(.$meal_line == "K","Kitchen", ifelse(.$meal_line == "G","World","no_info")))) %>%
    mutate(date = sub("\\.\\.",".",.$date)) %>% # replace double dot
    mutate(date = as.POSIXct(.$date, format = "%d.%m.%y")) %>% # convert to date format for merging with documentation
    mutate(cycle = 1) %>%
    select(meal_name, article_description, date, week, cycle,method, points, label)

teller <- read_xlsx("S:/pools/n/N-IUNR-nova-data/08_bewertung_gesundheit/Zusammenfassung_EBP_Tellermodell_V6.xlsx", range = "Tabelle1!G6:H99", trim_ws = T, col_names = F) %>%
    rename(points =  X__1, label =  X__2) %>%
    mutate(meal_name = ebp$meal_name,article_description = ebp$article_description, week = ebp$week, date = ebp$date, cycle = ebp$cycle, method = "teller_mod") %>%
    select(meal_name, article_description, date, week, cycle, method, points, label)

# bind rows of both data frames
nutri <- bind_rows(ebp, teller)

#special edits: seelachsnuggets, vesuvio, seitan, hot pumkin, quornragout, moussaka with meat
nutri[grep("Seelachsn", nutri$meal_name),]$cycle <- 2  #change week of seelachsnuggets to week nr 
nutri[grep("Vesuvio_46",nutri$meal_name),]$cycle <- 2 #change week nr of vesuvio, because without egg
nutri[grep("Seitangeschn",nutri$meal_name),]$cycle <- 2 # change week nr of seitangeschnetzeltes

nutri[(nutri$article_description == "Favorite") & (grepl("Quornragout",nutri$meal_name)),]$date <- "2017-11-10" # change date to 
nutri[grep("Hot", nutri$meal_name),]$date <- "2017-11-09"

nutri <- nutri[-grep("85_Auberginen-Moussaka_45K9.11",nutri$meal_name),] # delete moussaka with meat 
nutri[grep("85_Auberginen-Moussaka",nutri$meal_name),]$date <- "2017-11-09" # change date to correct date in cycle 1

write_delim(nutri,"S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files/nutri_profil_180612_egel.csv", delim = ';')


## wide format------------
# all info
# envir

envir_wide <- read_xlsx("S:/pools/n/N-IUNR-nova-data/09_bewertung_umwelt/Auswertung/Umweltbewertung_Menusv0.21.xlsx", range="Rezepte!D4:AN96", trim_ws=TRUE, col_names = F) %>% # load specific space in excel sheet
    rename(meal_name = X__1, article_description = X__2, week = X__3, ubp_ingred = X__9,  ubp_carbs = X__10,  ubp_protein = X__11,   ubp_veg_fruit = X__12, ubp_oil_fat_nut = X__13,  ubp_sweet_salt = X__14,  ubp_beverag = X__15, ubp_foodwaste = X__16,  ubp_kitchen = X__17, ubp_tot = X__18, gwp_ingred = X__28,  gwp_carbs = X__29,  gwp_protein = X__30, ubp_veg_fruit = X__31, gwp_oil_fat_nut = X__32,  ubp_sweet_salt = X__33,  gwp_beverag = X__34, gwp_foodwaste = X__35,  gwp_kitchen = X__36, gwp_tot = X__37) %>% # rename variables
    mutate(cycle = 1, method = "ubp", X__4=str_sub(X__4, end = -2)) %>% # create some new variables, like cycle, calculation method, date
    mutate(date=paste(str_sub(X__4, end=-3), str_sub(X__4, start=4),17, sep = ".")) %>% # paste date together, however need some changes
    select(meal_name, article_description, date, week, cycle, ubp_ingred,  ubp_carbs,  ubp_protein,   ubp_veg_fruit, ubp_oil_fat_nut,  ubp_sweet_salt,  ubp_beverag, ubp_foodwaste,  ubp_kitchen, ubp_tot, gwp_ingred,  gwp_carbs,  gwp_protein, ubp_veg_fruit, gwp_oil_fat_nut,  ubp_sweet_salt,  gwp_beverag, gwp_foodwaste,  gwp_kitchen, gwp_tot)) ## select variables

# edit and create date
ubp <- ubp %>%
    mutate(date=as.POSIXct(ubp$date,format="%b.%d.%y"))

# nutri



# only total of both methods
envir_2_wide <- dcast(envir,  meal_name+article_description+date+week+cycle ~ method)
# only total of both methods
nutri_2_wide <- dcast(nutri, meal_name+article_description+date+week+cycle ~ method)
## merge datasets from above--------
envir_nutri <- inner_join(envir_2_wide, nutri_2_wide[,-1], by=c("article_description","date","week","cycle")) # best way for calculations

##do some calculations---------
# correlation between methods
cor.test(envir_nutri$gwp, envir_nutri$ubp, method = "spearman") # very high correlation

#plot
coef(lm(envir_nutri$ubp~envir_nutri$gwp)) # for plotting the correlation line
ggplot(envir_nutri,aes(gwp,ubp))+
    geom_point()+
    geom_abline(intercept = 646.2251, slope = 1864.0516) +
    ylab("Umweltbelastungspunkte")+
    xlab("Treibhauspotential")+
    mytheme

# cross tables between methods nutri
CrossTable(envir_nutri$ebp, envir_nutri$teller_mod)

cor.test(nutri_wide$ebp, nutri_wide$teller_mod, method = "spearman")
