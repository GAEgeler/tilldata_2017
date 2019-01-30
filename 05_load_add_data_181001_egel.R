## load additional data-----

###
# state: january 2019
# author: gian-Andrea egeler
###

# load packages
pck <- c("dplyr", "stringr", "readr", "readxl", "reshape2", "lubridate")
lapply(pck, function(x){do.call("library", list(x))})

# load documentation ----------- (last update sept 2018)
info_orig <- read_delim("augmented data/menu_inhalt_protein_180420_matu08.csv", trim_ws = T, delim =';', locale = locale(encoding = 'LATIN1'),
                        col_types = cols(date = col_date(format = "%d.%m.%Y"))) %>% # pay attention to the date format (now is it Date format not POSIXct)
    mutate(date = as.Date(.$date)) %>%
    mutate(week = isoweek(.$date)) %>%
    mutate(condit = ifelse(.$cycle == 1 & .$week %%2 == 0,"Basis",ifelse(.$cycle == 2 & .$week %%2 == 1,"Basis","Intervention"))) %>%
    mutate(shop_description = str_replace(.$shop_description, " .*", "")) # take only first word of shop_description (better for merge)

# of with disctingction between meat and fish check out "07_change_documentary_190128_egel.R"

# load hot and cold buffet information-----
buffet <- read_xlsx("augmented data/buffet_animal_180425_03egel.xlsx") %>%
    mutate(article_description = "Hot and Cold") %>%
    mutate(date = parse_date(.$date)) %>% # to get same date format as other data frames
    mutate(shop_description = str_replace(.$shop_description, " .*", ""))


# for loading nutrition and environmental data see second load data file "05_1_load_add_data_190128.R"
