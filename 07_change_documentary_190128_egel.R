# specify meal content fleisch to fisch and fleisch

# state: january 2019

# author: gian-andrea egeler

# load documentation ----------- (last update sept 2018)
library(tidyverse)
library(lubridate)
library(here)

info_orig <- read_delim(here::here("augmented data/","2017_ZHAW_feldtagebuch_dokumentation_NOVANIMAL.csv"), trim_ws = T, delim =';', locale = locale(encoding = 'LATIN1'),
                        col_types = cols(date = col_date(format = "%d.%m.%Y"))) %>% # pay attention to the date format (now is it Date format not POSIXct)
    mutate(date = as.Date(.$date)) %>%
    mutate(week = isoweek(.$date)) %>%
    mutate(condit = ifelse(.$cycle == 1 & .$week %%2 == 0,"Basis",ifelse(.$cycle == 2 & .$week %%2 == 1,"Basis","Intervention"))) %>%
    mutate(shop_description = str_replace(.$shop_description, " .*", "")) # take only first word of shop_description (better for merge)

# in total there where 6 meals containint fish
# words like: ASC, MSC, Dorsch, Nordica, lachs

pats <- c("ASC|MSC|Dorsch|Nordica|lachs") # pattern to search after fisch meals (| indicates OR)
info_orig$label_content <- ifelse(grepl(pats, info_orig$meal_name) & info_orig$label_content == "Fleisch", 
                                   "Fisch", info_orig$label_content)


# in total there where 5 meals containint chicken
# words like: ASC, MSC, Dorsch, Nordica, lachs

pats <- c("Poulet|Hawaii|Chickeria|Indian|Massaman|Nasi") # pattern to search after fisch meals (| indicates OR)
info_orig$label_content <- ifelse(grepl(pats, info_orig$meal_name) & info_orig$label_content == "Fleisch", 
                                  "Geflügel", info_orig$label_content)


# save it
# write_delim(info_orig, "augmented data/menu_inhalt_protein_180420_09egel.csv", delim = ";")

