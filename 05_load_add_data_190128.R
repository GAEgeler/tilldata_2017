## load additional data-----

###
# state: may 2019
# author: gian-Andrea egeler
###

# load packages
# attention reshape2 and data.table have conflicts with melt()
pck <- c("dplyr", "stringr", "readr", "readxl", "lubridate", "here")
lapply(pck, function(x){do.call("library", list(x))})


#load feldtagebuch, meal nutrition (ebp) and environmental impact of the meal
##---only for producibility----------
# source("06_1_load_envir_data_200605.R", encoding = "Latin1")
# source("06_2_load_nutrition_data_200605.R", encoding = "Latin1")


#UBP & GWP
envir <- data.table::fread("https://novanimal.ch/wp-content/uploads/2020/09/2017_ZHAW_gwp_ubp_NOVANIMAL.csv", encoding = "unknown", sep = ";" ) %>% 
    mutate(date = as_date(date)) %>%
    as_tibble()


#EBP
nutri_ <- data.table::fread("https://novanimal.ch/wp-content/uploads/2020/09/2017_ZHAW_ebp_teller_NOVANIMAL.csv", encoding = "unknown", sep = ";") %>% 
    mutate(date = as_date(date)) %>% 
    as_tibble()

#feldtagebuch
info_orig <- data.table::fread("https://novanimal.ch/wp-content/uploads/2020/09/2017_ZHAW_feldtagebuch_dokumentation_NOVANIMAL.csv", encoding = "unknown", sep = ";") %>% 
    mutate(date = as_date(date)) %>% 
    as_tibble()



# merge nutrition and environment
envir_nutri <- left_join(envir, nutri_, by = c("date", "article_description", "cycle", "week", "meal_name", "label_content")) %>%
    mutate(meal_name_comp = stringr::str_replace_all(.$meal_name_comp, '[[:punct:]]', ''))  # drop variable meal_name_muir

#load information about the meat content of the hot and cold buffet-----
# only for producibility
# buffet <- read_xlsx(here::here("./augmented data/buffet_animal_180425.xlsx")) %>%
#     mutate(article_description = "Hot and Cold") %>%
#     mutate(date = as_date(.$date)) %>% # to get same date format as other data frames, somehow parse_date not working
#     mutate(shop_description = str_replace(.$shop_description, " .*", ""))

# infos from here: https://github.com/tidyverse/readxl/issues/278
tmp <- tempfile(fileext = ".xlsx")
buffet <- curl::curl_download(url= "https://novanimal.ch/wp-content/uploads/2020/09/2017_ZHAW_buffet_content_NOVANIMAL.xlsx", tmp) %>% 
    read_excel() %>% 
    mutate(date = as_date(date)) # causes an error: Error in the HTTP2 framing layer
file.remove(tmp)


# prepare data for merge with the till data  
# merge it with the info about the environmet and nutrition
info_compl <- left_join(info_orig, envir_nutri, by=c("meal_name", "article_description","date", "cycle", "week", "label_content")) # left join

# merge the file with the documentation of the hot and cold buffet
info_compl <- left_join(info_compl, buffet, by=c("date", "shop_description"))


#delete other data sets
rm(list=c("nutri_", "envir_nutri", "envir",
          "buffet","tmp", "pck", "info_orig"))

