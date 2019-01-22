######
# Load data
######

# status: 2.5.18 // egel

# required packages ---------------------
lapply(c("Hmisc","lubridate","car","readxl","psych","reshape2","gtools","dplyr","readr","stringr","factoextra","FactoMineR","gmodels","tidyr"), FUN = function(X) {
    do.call("require", list(X)) 
})


# load data --------------------- 
# attention to working directory

df_new1 <- read_delim(
    'S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files/filtered_r_final_180502_egel.csv', delim=';',locale = locale(encoding='LATIN1'),
        col_types = cols(
        transaction_id = col_double(),
        trans_date = col_datetime(format = ""),
        date = col_datetime(format = ""),
        article_description = col_character(),
        art_code = col_character(),
        qty_weight = col_double(),
        card_num = col_double(),
        Geschlecht = col_character(),
        Geburtsjahr2 = col_date(format = ""),
        member = col_character(),
        rab_descript = col_character(),
        total_amount = col_double(),
        price_article = col_double(),
        price_payment = col_double(),
        prop_price = col_double(),
        pay_description = col_character(),
        shop_description = col_character(),
        week = col_integer(),
        year = col_integer(),
        age = col_double(),
        cycle = col_integer(),
        condit = col_character()
    )
)


# load and merge documentation with dataset -----------
info <- read_delim("S:/pools/n/N-IUNR-nova-data/06_add_var/01_dokumentation/final/menu_inhalt_protein_180420_egel02_final.csv", delim=';',locale = locale(encoding = 'LATIN1'), col_types = cols(date=col_datetime(format = "%d.%m.%Y"))) # should be the final version :) 
info <- select(info,date,article_description,label_content,cycle,meal_name,shop_description)
df_17 <- left_join(df_new1,info, by = c("shop_description","date","article_description","cycle"))

# filter time -----------
df_7 <- filter(df_new1, (hour(trans_date) >= 9) & (hour(trans_date) <= 14)) # 9.00 is included but not 15.00 is not included, dont know how to handle itdf_7 <- left_join(df_17,info, by = c("shop_description","date","article_description","cycle")) # differences to df_17 are transactions on the same time => see "analyse_180320_03egel"

# add semester week to dataframe => there is surely a better way --------------- 

df_7$semwk <-
    ifelse(df_7$week == 40,3,ifelse(df_7$week == 41, 4, ifelse(
        df_7$week == 42,5, ifelse(df_7$week == 43,6,ifelse(
            df_7$week == 44,7,ifelse(df_7$week == 45, 8, ifelse(
                df_7$week == 46,9, ifelse(df_7$week == 47,10,ifelse(
                    df_7$week == 48,11,ifelse(df_7$week == 49,12,ifelse(
                        df_7$week == 50,13,14)
                    ))
                ))
            ))
        ))
    ))

# rename labels of content -------------
df_7$label_content <-  str_replace(df_7$label_content,"Fleisch","Meat")
df_7$label_content <-  str_replace(df_7$label_content,"Vegetarisch","Vegetarian")
df_7$label_content <-  str_replace(df_7$label_content,"Pflanzlich","Vegan")


