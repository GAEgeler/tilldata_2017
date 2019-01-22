######
# Load data
######

# status: 3.5.18 // egel

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
        Geburtsjahr2 = col_double(),
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
info1 <- read_delim("S:/pools/n/N-IUNR-nova-data/06_add_var/01_dokumentation/final/menu_inhalt_protein_180420_egel06_final.csv", delim=';',locale = locale(encoding = 'LATIN1'), col_types = cols(date=col_datetime(format = "%d.%m.%Y"))) # should be the final version :) 
info <- select(info1,date,article_description,label_content,cycle,meal_name,shop_description)
df_17 <- left_join(df_new1,info, by = c("shop_description","date","article_description","cycle"))

# load and merge documentation with nutritional profiles and environmental impact
envir_nutri <- read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files/envir_nutri_tot_wide_180613_egel.csv", delim = ';',locale = locale(encoding = "LATIN1")) %>%
    select(meal_name, article_description, date, week, cycle, ubp_ingred, ubp_foodwaste, ubp_tot,gwp_ingred, gwp_foodwaste, gwp_tot, ebp_points, ebp_label, teller_points, teller_label, -X1) 
    # %>% mutate(date=as.POSIXct(date, format="%Y-%m-%d"))
    
    
info_compl <- 


# filter time -----------
# between 8 and 9 oclock are weiered transactions (total amount over 1000). it seems that michael krauer then feed in missing sold meals of the evenings
# filter all meals between 9 and 10 to see how many are affected 
# => in total 3 transactions are strange, set filter which starts at 9.15 and ends with 15:00 (not possible till now)

df_7 <- filter(df_17, (hour(trans_date) >= 9 & hour(trans_date) <= 14) & total_amount < 1000) # starts at 9:00 and end at 14:59, and excluds cases (only one) which the total_amount is higher than 1000

# filter payment -------
# transactions with Gutschein are double transactions
# => delete all gutschein payers

# gutschein <- filter(df_7, grepl("Gutschein",df_7$pay_description))
df_7 <- filter(df_7, !grepl("Gutschein",df_7$pay_description))


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

# save dataset
# write_delim(df_7, "S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files/r_final_180503_egel.csv", delim=';')
