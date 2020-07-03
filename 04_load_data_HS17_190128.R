## load till data 2017-----

###
# state: may 2019
# author: gian-Andrea egeler
###

# required packages
pack <- c("dplyr", "lubridate", "readr", "stringr", "readxl", "tidyr", "here", "magrittr")
lapply(pack, function(x){do.call("library", list(x))})


####load data

if(file.exists("augmented data/data_edit_180929_egel.csv")){
    # data from 2017-------- meal buying with campuscard
    df_17 <- read_delim("augmented data/data_edit_180929_egel.csv", delim = ";", locale = locale(encoding = "LATIN1")) %>%
    mutate(date = as.Date(date))  # Data of 12 Personen where excluded from the individual dataset (1564-1552)


    # data from 2017---------- for aggregated analysis
    df_ <- read_delim("augmented data/data_edit_agg_180802_egel.csv", delim = ";", locale = locale(encoding = "LATIN1"), trim_ws = T) %>%
    mutate(date = as.Date(date)) 
    
    print("---You load your data from an local server---")
    
}else{
    # data from 2017 --- meal buying with campuscard
    df_17 <- fread("URL", encoding = "Latin1") %>% 
        mutate(date = as.Date(date)) %>% 
        as_tibble()
    
    #data from 2017 --- for aggregated analysis
    df_ <- fread("URL", encoding = "Latin1") %>% 
        mutate(date = as.Date(date)) %>% 
        as_tibble()
    
    print("---You load your data from ...(Webpage)---")
}



# peeple who payed more than one meal at once, are beeing replicated
df_agg <- filter(df_, qty_weight > 1) %>%
    uncount(qty_weight) %>%  # replicates rows according quantity of buyings
    left_join(df_, .) # merge it back to agg dataframe


#merge documentation info with environmental data (no nutritional data (code is here, however not finished))-----
#information can be downloaded form the website ...
if(file.exists("05_load_add_data_190128.R")){
    # load data
    source("05_load_add_data_190128.R", encoding = "Latin1") # with the difference between fish and meat
    
    print("---You load your data from an local server---")
}else{
    # load data form webpage
    info_compl <- fread("URL", )
    
    print("---You load your data from ...(Webpage)---")
}
# merge documentation with data 2017
info_ <- select(info_compl, meal_name_comp, meal_name, article_description, label_content, date, cycle, shop_description, tot_ubp, tot_gwp, buffet_animal_comp, outside, sun, clouds, rainfall) # subset of info_compl
df_7_ <- left_join(df_17, info_, by = c("shop_description","date","article_description","cycle")) # attention with cylce as key variable => all information for second cycle is not included!!

# dataset without double entries: check script yy_plausibility for more information

# first: check for special entries due to occurencies of the same dates per person (usually one person etas once per day for him/herself)
# 2050 (from them 435 duplicates)
df_double <- df_7_ %>% 
    group_by(ccrs, date) %>% # take all that transactions with double dates
    add_tally() %>% # counts occurencies
    filter(n > 1) %>%
    ungroup() %>% 
    select(-n)

#second: exlude them all
df_keep <- anti_join(df_7_, df_double)

# third: some cases of df_dat can be kept (200): some transactions where even due to same date identical thus keep one of them => see script yy_plausibility
df_keep2 <- group_by(distinct(df_double), ccrs, date, article_description) %>% 
    add_tally(.) %>% # mutate + summarize with n()
    filter(n > 1) %>% # filter all with double entries around 400
    distinct(ccrs, date, article_description, .keep_all = T) # select only one of them (200)
    
# fourth: row_bind add that 200 cases
df_2017 <- bind_rows(df_keep, df_keep2)

# merge aggregated sv data
df_agg <- left_join(df_agg, info_, by = c("shop_description","date","article_description", "cycle"))

# delete some datasets
rm(list = c("pack","df_" ,"df_17", "df_7_", "info_", "df_double", "df_keep", "df_keep2", "info_compl"))
