## load data -----

###
# state: may 2019
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
df_ <- read_delim("augmented data/data_edit_180802_egel.csv", delim = ";", locale = locale(encoding = "LATIN1"), trim_ws = T) %>%
    mutate(date = as.Date(date)) 

# peeple who payed more than one meal at once, are beeing replicated
df_agg <- filter(df_, qty_weight > 1) %>%
    uncount(qty_weight) %>%  # replicates rows according quantity of buyings
    left_join(df_, .) # merge it back to agg dataframe


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

# first: check for special entries due to occurencies of the same dates per person (usually one person etas once per day for him/herself)
# 2050 (from them 435 duplicates)
df_dat <- df_7_ %>% 
    group_by(ccrs, date) %>% # take all that transactions with double dates
    add_tally() %>% # counts occurencies
    filter(n > 1) %>%
    ungroup() %>% 
    select(-n)

#second: exlude them all
df_ <- anti_join(df_7_, df_dat)

# third: some cases of df_dat can be kept (200): some transactions where even due to same date identical thus keep one of them => see script yy_plausibility
df_keep <- group_by(distinct(df_dat), ccrs, date, article_description) %>% 
    add_tally(.) %>% # mutate + summarize with n()
    filter(n > 1) %>% # filter all with double entries around 400
    distinct(ccrs, date, article_description, .keep_all = T) # select only one of them (200)
    
# fourth: row_bind add that 200 cases
df_2017 <- bind_rows(df_, df_keep)

# merge aggregated sv data
df_agg <- left_join(df_agg, info_, by = c("shop_description","date","article_description", "cycle"))
    

# delete some datasets
rm(list = c("pack", "envir", "buffet", "df_", "df_7_", "df_17", "envir_tot", "info_", "info_compl", "info_orig"))
