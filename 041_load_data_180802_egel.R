## load data -----

# status 2.8.18 // egel

# required packages
pack <- c("dplyr", "lubridate", "readr", "stringr", "readxl", "here")
lapply(pack, function(x){do.call("library", list(x))})


# set dir for calling source code
# it needs the full directory to call the script from another script
# its not working with the here package

# load documentation -----------
#here("augmented data/menu_inhalt_protein_180420_egel07_final.csv" # seems not to work, dont know why
info_orig <- read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_tilldata_2017/augmented data/menu_inhalt_protein_180420_egel07_final.csv", delim =';', locale = locale(encoding = 'LATIN1'),
                         col_types = cols(date = col_date(format = "%d.%m.%Y"))) %>% # pay attention to the date format (now is it Date format not POSIXct)
    mutate(date = as.Date(.$date)) %>%
    mutate(week = isoweek(.$date)) %>%
    mutate(condit = ifelse(.$cycle ==1 & .$week %%2 == 0,"Basis",ifelse(.$cycle == 2 & .$week %%2 == 1,"Basis","Intervention")))

# load environment and nutrition data
# beacause of saving as csv (write.csv) there are some changes in structure!
## ATTENTION: there are still some mistakes in the data!!
## data ist not updated
envir_nutri <- read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_tilldata_2017/augmented data/envir_nutri_tot_wide_180613_egel.csv", delim = ';',locale = locale(encoding = "LATIN1"),
                          col_types = cols(
                              gwp_ingred = col_number(),
                              gwp_carbs = col_number(),
                              gwp_protein = col_number(),
                              gwp_veg_fruit = col_number(),
                              gwp_oil_fat_nut = col_number(),
                              gwp_sweet_salt = col_number(),
                              gwp_foodwaste = col_number(),
                              gwp_kitchen = col_number(),
                              gwp_tot = col_number())) %>%
    mutate(date=parse_date(date, format = "%Y-%m-%d")) %>%
    select(-X1)

# load hot and cold buffet information-----
buffet <- read_xlsx("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_tilldata_2017/augmented data/buffet_animal_180425_03egel.xlsx") %>%
    mutate(article_description = "Hot and Cold") %>%
    mutate(date = parse_date(.$date)) # to get same date format as other data frames

# data from 2017--------
df_17 <- read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_tilldata_2017/augmented data/data_edit_180802_egel.csv", delim = ";", locale = locale(encoding = "LATIN1")) %>%
    mutate(date = as.Date(date)) 

# merge information with data from 2017-----
# documentation with data to nutrition 
info_compl <- left_join(info_orig, envir_nutri, by=c("article_description","date", "cycle", "week")) %>%
    select(-meal_name.y,-`Kein Protein`,-Kommentar, -content, -week) %>%
    rename(meal_name = meal_name.x)

# documentation with buffet data
info_compl <- left_join(info_compl, buffet, by=c("date","article_description","shop_description"))

# merge documentation with data 2017
info <- select(info_orig, date, article_description, label_content, cycle, meal_name, shop_description) # subset of info_orig
info_ <- select(info_compl, date,article_description,label_content,cycle,meal_name,shop_description, gwp_tot, ubp_tot, ebp_points, ebp_label, teller_points, teller_label, buffet_animal_comp) # subset of info_compl
df_7 <- left_join(df_17,info, by = c("shop_description","date","article_description","cycle")) # join with info_orig (less variables)
df_7_ <- left_join(df_17,info_, by = c("shop_description","date","article_description","cycle")) # full join with info_complete

# data from 2015 & 2016----

