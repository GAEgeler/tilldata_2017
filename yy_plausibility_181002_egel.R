# check for plausibility

# state: april 18 
# author: gian-andrea egeler

# load data see script 04_load_data

# short notice: ----
# after discussion with baur 26.10.18:-----
# - delete all cases with multiple entries (e.g. those which payed for another one)
# in total 987 cases deleted (attention do not take the sum of the signle steps, due to duplicates in the signle steps)

# 0. 435 cases are double in datafreame df_17, however if we look closer there are some other weird transactions! => see A - D
test_0 <- df_17[duplicated(df_17),] 
test_01 <- df_17[duplicated(df_17$trans_date),]


# A. 67 persons with 75 transactions
test_a <- filter(df_17, qty_weight > 1)

# B. 490 persons with double dates (2038 cases)
test_b <- group_by(df_17, ccrs, date) %>% 
    summarize(multi_date = n()) %>% 
    filter(multi_date > 1) %>% # attention the data includes some of the cases from A
    ungroup()

test_b1 <- inner_join(df_17, test_b, by =c("ccrs", "date")) %>% filter(qty_weight <= 1) # something is weird => somehow the total transactions differ form here to the total in 04_load_data


# C. 431 cases with duplicates 
# duplicates in ccrs, transaction_id and total_amount > prop_price: means that the person payed more than one meal
test_c <- filter(df_17, duplicated(ccrs) & duplicated(transaction_id) & total_amount > prop_price & qty_weight <= 1) # avoid entries from test_a

anti_join(test_c, test_b1) #  check if test_c is in test_b: YES

# D. 265 cases, all the same
# duplicated trans_date with total_amount == prop_price (some cases are the same as above: 87)
test_d <- filter(df_17, duplicated(ccrs) & duplicated(transaction_id) & duplicated(trans_date) & total_amount == prop_price & qty_weight <= 1) # attention some entries f come rom test_b => see diff_c2
diff_d1 <- anti_join(test_c, test_b, by = "ccrs") # 178 cases differ between test_c and test_b (that means 87 entries in comon => see test_c2)

anti_join(test_d, test_b1) # check if test_d is in test_b: YES


# E. three people with more than 60 transaction in 60 days (resp. meal purchases) 252 cases
#ccrs != 1000564422 & ccrs !=1000584092 & ccrs !=1000610019
test_e <- df_17 %>% group_by(ccrs) %>% summarise(tot = n()) %>% filter(tot>60) # because of the lenght of the fieldexperiment
test_e1 <- filter(df_17, ccrs %in% test_d$ccrs)

anti_join(test_e, test_b1) # check if these three persons where in test_b: YES



###################
# generate sample for prior looking (some of the old code) ----
set.seed(17)

df_list <- df_7 %>%
    sample_n(., size = 15) %>%
    select(ccrs)

df <- filter(df_7, ccrs %in% df_list$ccrs)

write_delim(df, "augmented data/plausibility_180310_egel.csv", delim = ";") # some problems with the encoding => co worker checked for misterious transactions (Julia Matyas, in October 2018)

# check if rab_description is same as member ----
df_7$rab_descript2 <- str_replace(df_7$rab_descript, "Mitarbeiter", "Mitarbeitende")

t <- filter(df_7, rab_descript == "Externer Preis") # 24 obs => is it realistic, that only 24
t_ <- filter(df_agg, rab_descript == "Externer Preis") # in comparison 403 obs in data frame from sv, however only 25 were payed with campuscard
t2 <- filter(df_7, member  == "Spezialkarten")
t3 <- df_7[!df_7$member == df_7$rab_descript2,] # two observations are missing somehow

# compare unique card numbers and their member and check it with padis information----
df <- filter(df_7, ccrs %in% unique(df_7$ccrs)) # what is that for code??

library(gmodels)
df <- group_by(df_7, ccrs, member, gender) %>% summarise(test = n())
CrossTable(df$member,df$gender)

# differs the sample from the population 
sam_gruen <- c(452,
               575,
               236,
               244,
               57
)

pop_zhaw <- c(678,
              791,
              333,
              336,
              358
)

d = pop_zhaw/sum(pop_zhaw) # expected probability of occuracy in population

chisq.test(sam_gruen, pop_zhaw, correct = T) # seems not to differ
chisq.test(sam_gruen, p = d) # seems to differ signicifantly (p = expected probability in population (according to patrik buenter))

# without spezialkarten
sam_gruen <- c(452,
               575,
               236,
               244
)

pop_zhaw <- c(678,
              791,
              333,
              336
              
)

d1 = pop_zhaw/sum(pop_zhaw)
chisq.test(sam_gruen, p = d1) # seems not to differ anymore


# try it on hole sample---------
# fist check if hot and cold has double transactions
# only cases, that someone helped another one out
hot <- filter(df_7, label_content == "Hot and Cold" & duplicated(df_7$transaction_id) & duplicated(df_7$trans_date))

# single case transaction_id: 2381978 (ccrs: 1000620827 )
t <- filter(df_7, transaction_id == 2381978)

# exclude hot and cold to see if same results after
test0 <- filter(df_7,label_content != "Hot and Cold") # doesnt matter if you exclude or include hot and cold

# 265 transactions has same transaction_id, same transaction date
test1 <- filter(df_7, duplicated(df_7$ccrs) & duplicated(df_7$transaction_id) & duplicated(df_7$trans_date) & total_amount == prop_price)
test1_ <- df_7[duplicated(df_7),] # 435 cases are double in df_7, however also those with multiple transactions

# double check with another method: 231 (difference of 34)
i1 <- filter(df_7, duplicated(df_7[c('transaction_id', 'trans_date', 'ccrs')]) & total_amount == prop_price) 
i2 <- filter(df_7, duplicated(cbind(df_7$transaction_id, df_7$trans_date, df_7$ccrs)))

# count cases where different card holders, but same transaction_id
# check difference of both tests form above
diff_ <- anti_join(test1, test1_) # 25 cases differ
diff_2 <- anti_join(test1, i1) # 34 differences => most of the strange transactions, where same id but two card holders behind*
diff_3 <- anti_join(i2, test1) # 514 cases where one person has multiple transactions => not bad :)

# take a look closer: seems like these 25 cases have same transaction_id but are two different card holders
t <- filter(trans_dat, transaction_id == 2347691)
t2 <- filter(df_7, transaction_id == 2347691) # is same as above

t <- filter(trans_dat, transaction_id == 2423516)
t2 <- filter(df_7, transaction_id == 2423516) # is same as above

# 70 strange transactions*
# they come always in pairs e.g. 1000630381 and 1000610019 has exaclty same transaction (on same time) however are two different persons
t3 <- filter(df_7, transaction_id %in% diff_$transaction_id) # in total 13 unique ccrs numbers (and one ccrs (1000584132) number has same transaction but different meals)

# counter check again
t <- filter(trans_dat, transaction_id == 2342280) # same transaction_id
t <- filter(trans_dat, transaction_id == 2372291) # same transaction_id for two different card holders, thus take ccrs nummer also into account
t <- filter(trans_dat, transaction_id == 2523637) # same transaction id, however different dob
t <- filter(trans_dat, transaction_id == 2525577) # same transaction id, however two different card holders

