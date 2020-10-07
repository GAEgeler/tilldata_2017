## load till data 2017-----

###
# state: may 2019
# author: gian-Andrea egeler
###

# required packages
pack <- c("dplyr", "lubridate", "readr", "stringr", "readxl", "tidyr", "here", "magrittr")
lapply(pack, function(x){do.call("library", list(x))})


####load data from zenodo
# data from 2017 --- meal buying with campuscard
df_17 <- data.table::fread("URL", encoding = "Latin1", sep = ";") %>% 
    mutate(date = as.Date(date)) %>% 
    as_tibble()
    
#data from 2017 --- for aggregated analysis
df_ <- data.table::fread("URL", encoding = "Latin1", sep = ";") %>% 
    mutate(date = as.Date(date)) %>% 
    as_tibble()

# preprocess data (only if worked with the augmented data)
# source("04_preprocessing_HS17_190128.R", encoding = "Latin1")

#merge documentation info with environmental data (no nutritional data (code is here, however not finished))-----
#information can be downloaded form the website ...
source("05_load_add_data_190128.R", encoding = "Latin1") # while the first sourcing
    
# merge documentation with data 2017
info_ <- select(info_compl, meal_name_comp, meal_name, article_description, label_content, date, cycle, shop_description, tot_ubp, tot_gwp, buffet_animal_comp, outside, sun, clouds, rainfall) # subset of info_compl

# merge aggregated sv data
df_2017 <- left_join(df_17, info_, by = c("shop_description","date","article_description","cycle")) # attention with cylce as key variable => all information for second cycle is not included!!
df_agg <- left_join(df_, info_, by = c("shop_description","date","article_description", "cycle"))

# delete some datasets
rm(list = c("pack", "info_", "df_17", "df_"))
