## load additional data-----

###
# state: may 2019
# author: gian-Andrea egeler
###

# load packages
# attention reshape2 and data.table have conflicts with melt()
pck <- c("dplyr", "stringr", "readr", "readxl", "lubridate", "here")
lapply(pck, function(x){do.call("library", list(x))})

# load your conig file
#config::get(file = "config.yml")


#load info about meal:nutrition and environment
source("06_1_load_envir_data_200605.R", encoding = "Latin1")
source("06_2_load_nutrition_data_200605.R", encoding = "Latin1")


# merge nutrition and environment
envir_nutri <- left_join(envir[ , -2], nutri_) # drop variable meal_name_muir

#load hot and cold buffet information-----
buffet <- read_xlsx(here::here("./augmented data/buffet_animal_180425.xlsx")) %>%
    mutate(article_description = "Hot and Cold") %>%
    mutate(date = as_date(.$date)) %>% # to get same date format as other data frames, somehow parse_date not working
    mutate(shop_description = str_replace(.$shop_description, " .*", ""))


# prepare data for merge with the till data  
# merge it with the info about the environmet and nutrition
info_compl <- left_join(info_orig, envir_nutri, by=c("meal_name", "article_description","date", "cycle", "week", "label_content")) # left join

# merge the file with the documentation of the hot and cold buffet
info_compl <- left_join(info_compl, buffet, by=c("date","article_description","shop_description"))


# export info for the webpage
if (file.exists(here::here("./augmented data/2017_ZHAW_add_variables_200605.csv"))) {
    print("Files were already saved") 
}else{
    write_delim(info_compl, here::here("./augmented data/2017_ZHAW_add_variables_200605.csv"), delim = ";")
    print("Attention: your files were overwritten")
}


#delete other data sets
rm(list=c("nutri_", "nutri", "envir_nutri", "envir",
          "ubp_", "gwp_", "buffet", 
          "pck", "pats"))

