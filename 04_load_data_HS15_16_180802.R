## load data 2015-2016-----

###
# state: december 2019
# author: gian-Andrea egeler
###

# snapshot of packages
# checkpoint::checkpoint("2019-12-09")

# required packages
pack <- c("dplyr", "lubridate", "readr", "stringr", "readxl", "tidyr", "here")
lapply(pack, function(x){do.call("library", list(x))})


#### 2015-2016: label content-----
# using here and source from another project is causing major problems
# this helps: https://stackoverflow.com/questions/42815889/r-source-and-path-to-source-files
# check if path exists, if so load from pool otherwise from online-server
if (dir.exists("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_tilldata_2015_2016/")) {
    source("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_tilldata_2015_2016/03_edit_data_label_191206_egel.R", chdir = T, encoding = "Latin1")
} else{
    source("./scripts from another project/03_edit_data_label_191206_egel.R", chdir = TRUE, encoding = "ISO8859-1")
    
}


#### 2017: group meal content----
source("04_load_data_HS17_190128.R", chdir = TRUE, encoding = "Latin1")
rm(list = c("df_2017")) # only need aggregated data

# edit aggregated data 2017
menu_tot7 <- df_agg %>%
    group_by(date, label_content, shop_description, year, week, semwk) %>%
    summarise(tot_sold = n())


#combine all three data frames----------
menu_tot_label <- bind_rows(menu_tot5, menu_tot6, menu_tot7)

# remove some data from working space
rm(list = c("menu_tot5", "menu_tot6", "menu_tot7"))

#### 2015-2016: meal line
if (dir.exists("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_tilldata_2015_2016/")) {
    source("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_tilldata_2015_2016/03_edit_data_line_191206_egel.R", chdir = T)
} else{
    source("scripts from another project/03_edit_data_line_191206_egel.R", chdir = TRUE, encoding = "ISO8859-1") # there are some weird transaction concerning the kitchen meal line e.g. 2016-12-20 0 kitchen sellings (however if i check the original data, no kitchen entry)
    
}

# group data 2017 according meal line----
menu_tot7 <- df_agg %>%
    group_by(article_description, date, week, year, cycle, shop_description, condit, price_article) %>% 
    summarise(tot_sold=n()) %>% # group it again, for merge with other data, no condit variable
    ungroup() %>%
    select(date, article_description, year, week, cycle, shop_description, condit, tot_sold, price_article)

# merge with data 2017
menu_tot_line <- bind_rows(df_tot, menu_tot7)

# print message
print("Data: 'menu_tot_line' (with all info about the mealline) and 'menu_tot_label' (all info about the meal content) loaded successfully!")    

# delete unused data sets
rm(list = c("dat_hs_tot", "df_tot", "menu_tot7", "df_tot"))