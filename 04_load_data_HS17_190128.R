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
df_17 <- data.table::fread("URL", encoding = "Latin1", sep = ";") %>% 
    mutate(date = as_date(date)) %>% 
    as_tibble()
    
#data from 2017 --- for aggregated analysis
df_ <- data.table::fread("URL", encoding = "Latin1", sep = ";") %>% 
    mutate(date = as_date(date)) %>% 
    as_tibble()

