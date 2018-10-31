# check for plausibility

# status: 17.10.18 // egel

# load data see script 04_load_data
# generate sample for further looking ----
set.seed(17)

df_list <- df_7 %>%
    sample_n(., size = 15) %>%
    select(ccrs)

df <- filter(df_7, ccrs %in% df_list$ccrs)

write_delim(df, "augmented data/plausibility_180310_egel.csv", delim = ";") # some problems with the encoding

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

# exclude hot and cold to see if same reults after
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


# dataset without double entries seems to be right-----
df_2017 <- filter(df_7, !(duplicated(df_7$ccrs) & duplicated(df_7$transaction_id) & duplicated(df_7$trans_date) & total_amount == prop_price))


# short notice: ----
# 1. there are some cases (in total 75 cases), where the quantity is more than 1: means that the person payed more than one meal: question here, was it the same meal e.g. 2327593
# 2. there are some cases (around 900 cases ), where the same card holder payed more than one meal, but with different transactions_id
# => filter(df_2017, total_amount > prop_price & duplicated(transaction_id))
# 3. there are some cases (in total 70 cases), wehere the same transaction (id, time, sometimes meal is the same): hoewever there are two different meals on the same transaction 
# 4. there are some cases (in total 265), where the hole entrie is double (same id, time, meal, ect.): dont now why

# after discussion with baur 26.10.18:-----
# - delete all cases with multiple entries (e.g. those which payed for another one) (see points form above 1,2,4)

# 1. 75 cases => let them in the dataset, because only one transaction
test <- filter(df_17, qty_weight > 1)

# 2. 436 cases => dont know, how i came up with 900, probabiliy 436 * 2
# delete these cases
test <- filter(df_17, total_amount > prop_price & duplicated(transaction_id))
test1_ <- df_17[duplicated(df_17),] # one case different

# 3. 70 cases, all the same, however two different meals => take the first out of the data
test1 <- filter(df_17, duplicated(df_17$ccrs) & duplicated(df_17$transaction_id) & duplicated(df_17$trans_date) & total_amount == prop_price) # 265 cases
test1_ <- df_17[duplicated(df_17),] # 435 cases are double in df_7, however also those with multiple transactions
diff_ <- anti_join(test1, test1_)

test2 <- filter(df_7, transaction_id %in% diff_$transaction_id) # in total 13 unique ccrs numbers (and one ccrs (1000584132) number has same transaction but different meals)


# 4. 265 cases => delete these cases
test_ <- filter(df_7, !(duplicated(df_7$ccrs) & duplicated(df_7$transaction_id) & duplicated(df_7$trans_date) & total_amount == prop_price))



# some code left----
# only two cases left seems to have same transaction_id (however two different persons)
test2 <- filter(test0, duplicated(test0$transaction_id) & total_amount == prop_price)

## there are some problems with some of the transactions 
# load trans_dat, trans_art, trans_pay to check the transaction => see script 00_merge
trans_art2 <- left_join(trans_art,  art_info, by = "article_id")
trans_pay2 <- left_join(trans_pay, pay_method, by = "payment_id")

# single cases are from the dataframe called plausibility_egel
# ccrs nr 1000598155
c <- filter(trans_dat, ccrs == 1000598155) # in original data set
c1 <- filter(df_7, ccrs == 1000598155) # in merged data set
t <- filter(trans_dat, transaction_id == 2377305) # double entry in, which is not making sense
a <- filter(trans_art2, transaction_id == 2377305) # only one entry
p <- filter(trans_pay2, transaction_id == 2377305) # only one entry


t <- filter(trans_dat, transaction_id == 2377327) # double entry in, which is not making sense


a <- filter(trans_art2, transaction_id == 2354099)

# check for double or more same entries according time stamp and transaction id
d_ <- filter(trans_dat, duplicated(trans_dat$trans_date) & duplicated(trans_dat$transaction_id)) 
d <- filter(d_, ccrs == 1000598155)

# another try via trans_num (KassenbonNr)
d_2 <- filter(trans_dat, duplicated(trans_dat$trans_num)) # is resulting in many more duplicates


# antijoin between dubplicates and trans_dat
# continue to check the data again
trans_dat2 <- anti_join(trans_dat, d_, by = c("transaction_id", "trans_date"))

t <- filter(trans_dat2, transaction_id == 2433098)


# try to find a way to delete douplicates, without loosing double payers (e.g. payed for a friend)
# in that case exclude hot and cold, however check if there are same problems with double transaction_id and same tot_amount as price
test0 <- filter(df,label_content != "Hot and Cold" & ccrs == 1000598155) # test case

# check first duplicats in trans_dat and transaction_id and second: is prop_price same as tot_amount
# seems to work
test1 <- filter(df_, duplicated(df_$transaction_id) & duplicated(df_$trans_date) & df_$total_amount == df_$prop_price) 

# now create data set wich excludes "test" from above: i expect dataset with 51-8 rows
# seems to work 
test2 <- filter(df_, !(duplicated(df_$transaction_id) & duplicated(df_$trans_date) & df_$total_amount == df_$prop_price)) 

# seems not to work: my guess is, it excludes both transaction not only one: 51-16
test3 <- anti_join(test0, test1)

# fourth way: is not working as above excludes both transactions
test4 <- filter(test0, !(transaction_id %in% test1$transaction_id))

