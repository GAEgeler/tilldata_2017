## load till data 2017-----

###
# state: october 2020
# author: gian-Andrea egeler
###

# required packages
library(c("data.table", "tidyverse", "lubridate"))

####if data is loaded from augmented data (for reproducibility)
# source("04_preprocessing_HS17_190128.R")


####load data from zenodo
# data from 2017 --- meal buying with campuscard
df_17 <- read_delim("https://zenodo.org/record/3890949/files/2017_ZHAW_individual_menu_sales_NOVANIMAL.csv?download=1", locale = locale(encoding = "latin1"), delim = ";") %>% 
    mutate(date = as_date(date))
    
#data from 2017 --- for aggregated analysis
df_ <- read_delim("https://zenodo.org/record/3890931/files/2017_ZHAW_aggregated_menu_sales_NOVANIMAL.csv?download=1", locale = locale(encoding = "latin1"), delim = ";") %>% 
    mutate(date = as_date(date))


# no you should be (hopefully) able to knit the 2020_egeler_script_wp_purchase_behavior_NOVANIMAL.Rmd file

