## merge transactions from patrick buenter (e-mail from 24.7.18)

# status: 11.8.18 // egel

#load packages
#load data and prepare data
zhaw_pers <- read_xlsx(here("raw data/", "Kaufverhalten_2018-01-19.xlsx"), sheet = 1) # there are some mseriennr (19) with underlines => why?
zhaw_pers$mseriennr <- str_replace(zhaw_pers$mseriennr, "_", "") # 19 cases with _ in mseriennr

zhaw_trans <- read_xlsx(here("raw data/", "Kaufverhalten_2018-01-19.xlsx"), sheet = 2) %>% # SERIAL_NUMBER was saved as test in excel, changed it into number
    mutate(TRANSACTION_DATE_TIME = as.POSIXct(.$TRANSACTION_DATE_TIME ,format = "%Y-%m-%d %H:%M:%S")) %>%
    #mutate(SERIAL_NUMBER = as.integer64(.$SERIAL_NUMBER)) %>% # problems to convert number into integer
    rename(terminal_id = CASH_STATION)


# join zhaw_trans and zhaw_pers
df <- left_join(zhaw_trans, zhaw_pers, by = c("SERIAL_NUMBER" = "mseriennr"))

# filter first all data (only for grüental (terminal_id: 185, 186) and reidbach (terminal_id: 181, 187))
zhaw_wead <- filter(df, terminal_id == 181 | terminal_id == 187 | terminal_id == 185 | terminal_id == 186)
zhaw_waed$SERIAL_NUMBER <- as.integer64(zhaw_waed$SERIAL_NUMBER)


# load data from sv
till <- read_xlsx(here("raw data/ZHAW_Tills.xlsx")) %>%
    select(id, code, shop_id, description)

# select till number from description (terminal_id: 185, 186) and reidbach (terminal_id: 181, 187))
till$terminal_id <- str_sub(till$description, start = -3)
till$terminal_id <- parse_integer(till$terminal_id) # change format of terminal_id (from character to integer)

# join info from terminal_id with zhaw_waedi
df2 <- left_join(df, till[c("shop_id", "description", "terminal_id")], by = "terminal_id")
df2$date <- lubridate::date(df2$TRANSACTION_DATE_TIME) # add date as variable // not working??

# save data
write_delim(zhaw_wead, here("augmented data", "zhaw_transactions_buent_180926_egel.csv"), delim = ';')

### attention: there are almost 3'000 cases with turnover 0, with article number 65535 => reasons?
#counts <- group_by(zhaw_wead, TURNOVER) %>% summarise(counts = n())


#####################################################################------------------------------

# load data zhaw (patrick buenter)
zhaw_waed <- read_delim(here("augmented data", "zhaw_transactions_buent_180808_egel.csv"), delim = ';', col_types = cols(SERIAL_NUMBER = col_double())) # cnnr variable gives 4 paring failures, due to missing data
zhaw_waed$SERIAL_NUMBER <- as.integer64(zhaw_waed$SERIAL_NUMBER)

# try to select only last 7 digits form mseriennr
zhaw_waed_ <-  zhaw_waed %>% 
    mutate(card_num = parse_character(SERIAL_NUMBER)) %>%
    mutate(card_num = str_sub(.$SERIAL_NUMBER, start = -7)) %>%
    mutate(card_num = parse_integer(.$card_num))

n_distinct(zhaw_waed_$chipseriennummer) # count single chipnumbers: 1762

# load data sv (bruno petrino) cant only acces via sv-group in dubendorf
sv_dat <- read_delim("E:/ZHAW_Transactions_original.csv", delim = ';', trim_ws = T, na = "NULL") %>%
    select(id, till_id, shop_id, trans_num, split_num, trans_date, total_amount, bookkeeping_date, pricelevel_id, card_num) %>%
    mutate(date = parse_date(sv_dat$bookkeeping_date)) %>% # change date format
    mutate(card_num = parse_number(.$card_num)) # change card_num format

# filter data for grüental and reidbach (grüental: shop_id == 6, vista: shop_id == 7)
sv_waed <- filter(sv_dat, shop_id == 6 | shop_id == 7)
n_distinct(sv_waed$card_num) # count single card_numbers: 1900


# try to merge via card_num (short version of SERIAL_NUMBER)
# check if shop_id (from zhaw_waed_) matches shop_id (from sv_waed)
# dont use till_id for merge
test <- left_join(sv_waed, zhaw_waed_, by=c("shop_id","date", "card_num")) # needs lots of RAM




############################################################################-------------------------
# some testing
# try to search card_num from sv data in SERIAL_NUMBER(mseriennr) from zhaw data
# problem card_num is shorter as in SERIAL_NUMBER (most of all only for one digit)
# use sub_extract to solve this problem 
same_sv <- str_extract(zhaw$mseriennr, paste(sv$card_num, collapse = "|")) # returns number of match form sv dataset
n_distinct(as.numeric(same_sv))

df <- filter(sv, card_num %in% same_sv) # check how much transactions are same
zhaw$card_num <- as.numeric(same_sv) # add numbers of match from sv dataset to df (with data from zhaw) for merge

#df_ <- inner_join(zhaw, sv, by = c("date","card_num", "shop_id")) # problem with ram


############ create some test data set------
########## test sample sv
test_vec <- c(352872, # match
              937796,
              352753, # match
              132,
              904485, # match
              160324,
              499853, # match
              571908,
              122859, # match
              992452,
              636356,
              676285, # match
              641092,
              721662, # match
              238189, # match
              530052, 
              349478, # match
              349646, # match
              352462, # match
              309828,
              22532 # match
)

test_vec_only_match <- c(352872, # match
                         352753, # match
                         904485, # match
                         499853, # match
                         122859, # match
                         676285, # match
                         721662, # match
                         238189, # match
                         349478, # match
                         349646, # match
                         352462 # match
)

test_vec_not_match <- setdiff(test_vec, test_vec_only_match)

# test_vec_not_match2 <- c(937796,
#                         160324,
#                         571908,
#                         992452,
#                         636356,
#                         641092,
#                         530052, 
#                         309828,
#                         22532
# )

test_sv <- filter(sv_dat, card_num %in% test_vec) %>%
    write_delim(here("augmented data", "test_data_sv.csv"), delim = ';')

# never show this code, thus illegal :) 
test_sv <- read_delim(here("augmented data/test_data_sv.csv"), delim = ';') %>%
    mutate(card_num = as.character(.$card_num))

# try to merge with zhaw_waed_
# however not working => why
t <- left_join(test_sv, df2[c("SERIAL_NUMBER","shop_id", "date", "TURNOVER", "Geschlecht", "Geburtsjahr2", "Kategorisierung")], by = c("date", "shop_id", "card_num" = "SERIAL_NUMBER"))


# find matchings in test data sv and zhaw_waed
mtch <- grepl(paste(test_sv$card_num, collapse = "|"), zhaw_waed_$SERIAL_NUMBER) # dont find matches
mtch2 <- str_extract(parse_integer(zhaw_waed_$SERIAL_NUMBER), paste(test_sv$card_num, collapse = "|")) #dont work either

########### test sample zhaw
set.seed(46)
test_zhaw <- sample_n(zhaw_dat, size = 1000, replace = T)

bool <- grepl(paste(test_sv$card_num, collapse = "|"), test_zhaw$mseriennr)
bool2 <- grep(paste(test_sv$card_num, collapse = "|"), test_zhaw$mseriennr, value = T, fixed = F)
t <- na.omit(str_match(test_zhaw$mseriennr, paste(test_sv$card_num, collapse = "|")))
t <- str_extract(test_zhaw$mseriennr, paste(test_sv$card_num, collapse = "|"))
t2 <- str_match(test_zhaw$chipseriennummer, paste(test_sv$card_num, collapse = "|"))
bool3 <- grep(paste(test_zhaw$mseriennr, collapse = "|"), test_sv$card_num, value = T, fixed = F, invert = T)   # not working     

test4 <- filter(test_zhaw, bool) # seems to work, however how do i merge now, need to bring back information of matching 
test5 <- filter(test_sv, card_num %in% bool2)

test4$card_num <- as.numeric(t[ ,1])

df_ <- left_join(test_sv, test4, by = c("date","card_num", "shop_id"))



