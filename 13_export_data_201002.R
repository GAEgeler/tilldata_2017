# export data for zenodo --------

###
# state: october 2020
# author: gian-Andrea egeler
###

# in case data is not load
# source("04_preprocessing_HS17_190128.R")

# export aggregated data
df_agg  %>% 
    select(-card_num) %>% 
    rename(total_amount = total_amount.x) %>% 
    write_delim(here::here("export_zenodo/","2017_ZHAW_aggregated_menu_sales_NOVANIMAL.csv"),
                delim = ";")
    
# export individual data
df_2017 %>% 
    mutate(age_group = cut(age, breaks = c(-Inf, 25, 34, 49, 65, Inf),
                           labels=c("15 bis 25-jährig","26 bis 34-jährig",
                                    "35 bis 49-jährig","50 bis 64-jährig",
                                    "keine Angaben"))) %>% 
    select(-Dob, -age) %>% 
    write_delim(here::here("export_zenodo/","2017_ZHAW_individual_menu_sales_NOVANIMAL.csv"),
                delim = ";")

