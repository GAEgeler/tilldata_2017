## load data -----

###
# state: january 2019
# author: gian-Andrea egeler
###

# required packages
pack <- c("dplyr", "lubridate", "readr", "stringr", "readxl", "tidyr")
lapply(pack, function(x){do.call("library", list(x))})


####
# data from 2017-------- for individual analysis
# problem with encoding, dont know how to handle => especially while file savings
df_17 <- read_delim("augmented data/data_edit_180929_egel.csv", delim = ";", locale = locale(encoding = "LATIN1")) %>%
    mutate(date = as.Date(date)) 


# data from 2017---------- for aggregated analysis
df_agg <- read_delim("augmented data/data_edit_180802_egel.csv", delim = ";", locale = locale(encoding = "LATIN1"), trim_ws = T) %>%
    mutate(date = as.Date(date)) 


# merge documentation info with environmental data (no nutritional data (code is here, however not finished))-----
source("05_1_load_add_data_190128_egel.R") # with the difference between fish and meat

# merge  
info_compl <- left_join(info_orig, envir, by=c("meal_name", "article_description","date", "cycle", "week", "label_content")) # left join
    

# documentation with buffet data
info_compl <- left_join(info_compl, buffet, by=c("date","article_description","shop_description"))

# merge documentation with data 2017
info_ <- select(info_compl, meal_name_comp, meal_name, article_description, label_content, date, cycle, shop_description, tot_ubp, tot_gwp, buffet_animal_comp, outside, sun, clouds, rainfall) # subset of info_compl
df_7_ <- left_join(df_17, info_, by = c("shop_description","date","article_description","cycle")) # attention with cylce as key variable => all information for second cycle is not included!!

# dataset without double entries: check script yy_plausibility for more information
df_7 <- filter(df_7, !qty_weight > 1) # delete 75 entries, which payed more than one meal (qty bigger than 1)
df_2017 <- group_by(df_7, ccrs, date) %>% 
    summarize(multi_date = n()) %>% 
    filter(multi_date > 1) %>% # check how many transactions per person were made per day
    ungroup() %>% 
    anti_join(df_7,., by = c("ccrs", "date")) # in total 2027 transactions were deleted

# merge aggregated sv data
df_agg <- left_join(df_agg, info_, by = c("shop_description","date","article_description", "cycle"))
    

# delete some datasets
rm(list = c("pack", "envir", "buffet", "df_17", "df_7_", "envir_tot", "info_", "info_compl", "info_orig"))
