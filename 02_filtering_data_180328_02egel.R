########
### Filtering Data
########

# required packages
library(stringr)
library(readr)
library(dplyr)
library(eeptools)

## Status: 2.5.18 // egel

# load data hs17 -------------
dat <- read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files/data_180320_05egel.csv",delim = ';')

# subset data: Favorite, Garden (= Hot n Cold), Kitchen (0,1,2,3,4), LocalFavorite, LocalKitchen, LocalWorld, World => Local Kitchen is missing?
articles <- c(
    "A10001" ,# Favorite
    "A10010", # Kitchen 0
    "A10011", # Kitchen 1
    "A10040" ,# Garden 
    "A10012" ,# Kitchen 2
    "A10013" ,# Kitchen 3
    "A10014" ,# Kitchen 4
    "A10025" ,# Local Favorite
    "A10026" ,# Local World
    "A10027" ,# Local Tössfeld = Local Kitchen
    "A10015" ,# World
    "A10020" ,# Local 0
    "A10021" ,#	Local 1
    "A10022" ,#	Local 2
    "A10023" ,#	Local 3
    "A10024"  # Local 4
)

# as control, subset data with article names => same result as above
articl_name <- c(
    "Favorite",
    "Kitchen 0",
    "Kitchen 1",
    "Garden" ,
    "Kitchen 2",
    "Kitchen 3",
    "Kitchen 4",
    "Local Favorite",
    "Local World",
    "Local Tössfeld",
    "World")


df_new_ <- filter(dat, article_description %in% articl_name)
df_new <- filter(dat, art_code %in% articles) # no time filter

# save data
write_delim(df_new,"S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files/filtered_r_180502_egel.csv",delim = ';')




