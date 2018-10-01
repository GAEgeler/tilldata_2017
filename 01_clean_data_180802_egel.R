# Clean and edit data --------

## Status: 29.9.18 // egel

# required packages -----------
pack <- c("stringr", "readr", "dplyr", "lubridate", "here")
lapply(pack, function(x){do.call("library", list(x))})


# load data ---------
# new data set
df_n <- read_delim("raw data/data_trans_180929_egel.csv", delim=';', col_types = cols(qty_weight = col_double(),
                                                                                      trans_date = col_datetime()))  # in comparison to the old data set the actual set counts around 53'000 less transactions

# old data set (rename from ZHAW_transactions_180802_egel)
df_n1 <- read_delim("raw data/data_trans_180802_egel.csv",  delim=';', col_types = cols(qty_weight = col_double()))
# filter for grüental and vista (old data set 50'026, new data set 41'585) -----------
# after first comparisons of the two data sets, my fist conclusion, they seem to be completely different
df_dat <- df_n %>%
    filter(shop_description == "Grüental" | shop_description== "Vista") %>%
    select(transaction_id, trans_date, date, art_description, art_code, qty_weight, 
                 card_num, gender, Dob, member, price_descript,
                 total_amount, price_article, single_price, pay_description, shop_description) %>%
    rename(article_description = art_description, rab_descript = price_descript, prop_price = single_price) # total_amount_trans = total_amount.x, total_amount_pay = total_amount.y


# set #NV in variable member to lernende (information from Pädi Buenter => dont exist anymore) 
# df_dat$member[df_dat$member == "#NV"] <- "Lernende"

# change names of kitchen and hot and cold ----------- only one kitchen 1 in data set => check in old dataset if true => YES
df_dat$article_description <-  str_replace(df_dat$article_description, "Garden", "Hot and Cold")

# change name of grüntal to grüental ------------
# df_dat$shop_description <-  str_replace_all(df_dat$shop_description, 'Grüntal Mensa', 'Grüental Mensa')

# change names of gender ----------
df_dat$gender <- toupper(df_dat$gender) # better than female and male

# add new variables: week, year, age, semwk, cycle, condit ------------
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

# save data ----------
write_delim(df_dat,"clean data/data_clean_180929_egel.csv", delim = ';')
