# Clean and edit data --------

###
# state: december 2019
# author: gian-Andrea egeler
###

# required packages -----------
pack <- c("stringr", "readr", "dplyr", "lubridate", "here")
lapply(pack, function(x){do.call("library", list(x))})


# individual data set-----------
df_ind <- read_delim(here("raw data/data_trans_ind_180929_egel_check.csv"), delim=';', col_types = cols(qty_weight = col_double(),
                                                                                      trans_date = col_datetime()))  # in comparison to the old data set the actual set counts around 53'000 less transactions
# filter for grüental and vista (agg data set 50'026, individual data set 41'585) -----------
df_dat <- df_ind %>%
    filter(shop_description == "Grüental" | shop_description== "Vista") %>%
    select(ccrs, transaction_id, trans_date, date, art_description, art_code, qty_weight, 
                 card_num, gender, Dob, member, price_descript,
                 total_amount, price_article, single_price, pay_description, shop_description) %>%
    rename(article_description = art_description, rab_descript = price_descript, prop_price = single_price) # total_amount_trans = total_amount.x, total_amount_pay = total_amount.y


# change names of kitchen and hot and cold ----------- 
# only one kitchen 1 in data set => check in agg dataset if true => YES
df_dat$article_description <-  str_replace(df_dat$article_description, "Garden", "Hot and Cold")

# change names of gender ----------
df_dat$gender <- toupper(df_dat$gender) # better than female and male

# add new and some convenience variables: week, year, age, semwk, cycle, condit ------------
df_dat$week <- isoweek(df_dat$date)

df_dat$year <- year(df_dat$date)

df_dat$age <- 2017-df_dat$Dob # age calculation (very simple 2017 - birthdate) => attention max age is 118

df_dat$semwk <-
    ifelse(df_dat$week == 40,3,ifelse(df_dat$week == 41, 4, ifelse(
        df_dat$week == 42,5, ifelse(df_dat$week == 43,6,ifelse(
            df_dat$week == 44,7,ifelse(df_dat$week == 45, 8, ifelse(
                df_dat$week == 46,9, ifelse(df_dat$week == 47,10,ifelse(
                    df_dat$week == 48,11,ifelse(df_dat$week == 49,12,ifelse(
                        df_dat$week == 50,13,14)
                    ))
                ))
            ))
        ))
    ))

df_dat$cycle <- ifelse(df_dat$week >= 40 & df_dat$week <= 45,1,2)

df_dat$condit <- ifelse(df_dat$week %%2 == 0 & df_dat$cycle == 1, "Basis", ifelse(df_dat$week %%2 == 1 & df_dat$cycle == 2, "Basis", "Intervention"))

# save individual data ----------
write_delim(df_dat,"clean data/data_clean_ind_180929_egel_check.csv", delim = ';')

###############################
###############################------------------------------------------------------
###############################


# aggregated data set (rename from ZHAW_transactions_180802_egel)-----
df_agg <- read_delim(here("raw data/data_trans_agg_180802_egel_check.csv"),  delim=';', col_types = cols(qty_weight = col_double()))


# filter for grüental and vista (agg data set 50'026, individual data set 41'585) -----------
# attention changes in december (card_num is not any longer in df_)
df_dat <- df_agg %>%
    filter(shop_description == "Grüental" | shop_description== "Vista") %>%
    select(transaction_id, trans_date, date, art_description, art_code, qty_weight, 
           price_descript,
           total_amount, price_article, single_price, pay_description, shop_description) %>%
    rename(article_description = art_description, rab_descript = price_descript, prop_price = single_price) # total_amount_trans = total_amount.x, total_amount_pay = total_amount.y


# change names of kitchen and hot and cold ----------- 
# only one kitchen 1 in data set => check in agg dataset if true => YES
df_dat$article_description <-  str_replace(df_dat$article_description, "Garden", "Hot and Cold")

# add new and some convenience variables: week, year, semwk, cycle, condit ------------
df_dat$week <- isoweek(df_dat$date)

df_dat$year <- year(df_dat$date)

df_dat$semwk <-
    ifelse(df_dat$week == 40,3,ifelse(df_dat$week == 41, 4, ifelse(
        df_dat$week == 42,5, ifelse(df_dat$week == 43,6,ifelse(
            df_dat$week == 44,7,ifelse(df_dat$week == 45, 8, ifelse(
                df_dat$week == 46,9, ifelse(df_dat$week == 47,10,ifelse(
                    df_dat$week == 48,11,ifelse(df_dat$week == 49,12,ifelse(
                        df_dat$week == 50,13,14)
                    ))
                ))
            ))
        ))
    ))

df_dat$cycle <- ifelse(df_dat$week >= 40 & df_dat$week <= 45,1,2)

df_dat$condit <- ifelse(df_dat$week %%2 == 0 & df_dat$cycle == 1, "Basis", ifelse(df_dat$week %%2 == 1 & df_dat$cycle == 2, "Basis", "Intervention"))

# save agg data -------
write_delim(df_dat,"clean data/data_clean_agg_180802_egel_check.csv", delim = ';')