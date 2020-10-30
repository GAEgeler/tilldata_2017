## preprocess till data 2017-----

###
# state: october 2020
# author: gian-Andrea egeler
###

# load packages
library(tidyverse)


# data from 2017 ---------- 
# individual data set
df_17 <- read_delim(here::here("augmented data/", 
                               "2017_ZHAW_individual_menu_sales_NOVANIMAL.csv"),
                    delim = ";", locale = locale(encoding = "LATIN1")) %>%
    mutate(date = as.Date(date)) 

# dataset without double entries: check script yy_plausibility for more information

# first: check for special entries due to occurencies of the same dates per person (usually one person etas once per day for him/herself)
# 2050 (from them 435 duplicates)
df_double <- df_17 %>% 
    group_by(ccrs, date) %>% # take all that transactions with double dates
    add_tally() %>% # counts occurencies
    filter(n > 1) %>%
    ungroup() %>% 
    select(-n)

#second: exlude them all
df_keep <- anti_join(df_17, df_double)

# third: some cases of df_dat can be kept (200): some transactions where even due to same date identical thus keep one of them => see script yy_plausibility
df_keep2 <- group_by(distinct(df_double), ccrs, date, 
                     article_description) %>% 
    add_tally(.) %>% # mutate + summarize with n()
    filter(n > 1) %>% # filter all with double entries around 400
    distinct(ccrs, date, article_description, .keep_all = T) %>%  # select only one of them (200)
    select(-n)
    
# fourth: row_bind add that 200 cases
df_17 <- bind_rows(df_keep, df_keep2)

# data from 2017---------- 
# aggregated data set
df_ <- read_delim(here::here("augmented data/", 
                             "2017_ZHAW_aggregated_menu_sales_NOVANIMAL.csv"), 
                  delim = ";", locale = locale(encoding = "LATIN1"), trim_ws = T) %>%
    mutate(date = as.Date(date)) 

# peeple who payed more than one meal at once, are beeing replicated
df_ %>% 
    filter(qty_weight > 1) %>%
    uncount(qty_weight) %>%  # replicates rows according quantity of buyings
    left_join(df_, .) -> df_agg # merge it back to agg dataframe


#merge documentation info with environmental data (no nutritional data (code is here, however not finished))-----
#information can be downloaded form the website ...
source("05_load_add_data_190128.R", encoding = "Latin1") # while the first sourcing

# merge documentation with data 2017
info_ <- select(info_compl, meal_name_comp, meal_name, article_description, label_content, date, cycle, shop_description, tot_ubp, tot_gwp, buffet_animal_comp, outside, sun, clouds, rainfall) # subset of info_compl

# merge aggregated sv data
df_2017 <- left_join(df_17, info_, by = c("shop_description","date","article_description","cycle")) # attention with cylce as key variable => all information for second cycle is not included!!
df_agg <- left_join(df_agg, info_, by = c("shop_description","date","article_description", "cycle"))


# delete some datasets
rm(list = c("df_" ,"df_17", "df_double", "df_keep", "df_keep2", "info_compl"))

