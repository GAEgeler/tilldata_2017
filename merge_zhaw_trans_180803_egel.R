## merge transactions from patrick buenter (e-mail from 24.7.18)

# status: 3.8.18 // egel


#load data and prepare data
zhaw_pers <- read_xlsx("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/Kaufverhalten_2018-01-19.xlsx", sheet = 1)

zhaw_trans <- read_xlsx("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/Kaufverhalten_2018-01-19.xlsx", sheet = 2) %>%
    mutate(TRANSACTION_DATE_TIME = as.POSIXct(.$TRANSACTION_DATE_TIME ,format = "%Y-%m-%d %H:%M:%S")) %>%
    #mutate(SERIAL_NUMBER = as.numeric(.$SERIAL_NUMBER)) %>%
    rename(terminal_id = CASH_STATION)


# insert shop information from the sv group
# load data from zhaw
info <- read_xlsx("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/Kaufverhalten_2018-01-19.xlsx", sheet = 3) %>%
    select(terminal_id, campus)

# load data from sv
shop <- read_xlsx("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_tilldata_2017/raw data/ZHAW_shops.xlsx")

info$sv_shop_id <- NA # new variable
info$sv_shop_code <- NA # new variable

info[grep("Grüental", info$campus),]$sv_shop_code <- shop[grep("Grüntal*", shop$description),]$code 
info[grep("Grüental", info$campus),]$sv_shop_id <- shop[grep("Grüntal*", shop$description),]$id 

info[grep("Technikum", info$campus),]$sv_shop_code <- shop[grep("Technikum*", shop$description),]$code 
info[grep("Technikum", info$campus),]$sv_shop_id <- shop[grep("Technikum*", shop$description),]$id 

info[grep("St. Georgen", info$campus),]$sv_shop_code <- shop[grep("St. Georgenplatz*", shop$description),]$code 
info[grep("St. Georgen", info$campus),]$sv_shop_id <- shop[grep("St. Georgenplatz*", shop$description),]$id 

info[grep("Reidbach", info$campus),]$sv_shop_code <- shop[grep("Vista*", shop$description),]$code 
info[grep("Reidbach", info$campus),]$sv_shop_id <- shop[grep("Vista*", shop$description),]$id 

#merge info to zhaw_trans
df <- left_join(zhaw_trans, info, by = "terminal_id")
# seems to work better only 5 missings in Kategorie ( = university membership)
df1 <- df %>%
    rename(mseriennr = SERIAL_NUMBER) %>%
    left_join(zhaw_pers, by = c("mseriennr")) %>%
    mutate(date = date(TRANSACTION_DATE_TIME))

write_delim(df1, "S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_tilldata_2017/augmented data/zhaw_transactions_buent_180803_egel.csv", delim = ';')


# seems to cause more missings than above (e.g. 81276 missings in membership)
df1 <-  df %>%
    rename(chipseriennummer = SERIAL_NUMBER) %>%
    left_join(zhaw_pers, by = c("chipseriennummer"))


### attention: there are almost 20'000 cases with turnover 0, with article number 65535 => reasons?

