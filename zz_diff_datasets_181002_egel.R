###Difference between new and old data set------

# state: april 2019
# author: gian-Andrea egeler

# new data set # 23'683 obs
df_17 <- read_delim("augmented data/data_edit_180929_egel.csv", delim = ";", locale = locale(encoding = "LATIN1")) %>%
    mutate(date = as.Date(date)) 

# old data set # 23'503 obs
df_17_test <- read_delim("augmented data/data_edit_180802_egel.csv", delim = ";", locale = locale(encoding = "LATIN1")) %>%
    mutate(date = as.Date(date)) %>%
    filter(., !pay_description == "Bargeld CHF") %>% # exclude bargeld
    filter(., !pay_description == "Subvention Internat") # exclude subvention

df_7 <- left_join(df_17, info, by = c("shop_description","date","article_description","cycle"))
df_7_test <- left_join(df_17_test, info, by = c("shop_description","date","article_description","cycle"))


# 23683-23503 = 180
#group data
g1 <- df_7 %>%
    group_by(week, label_content, condit) %>%
    summarise(tot = n())
g2 <- df_7_test %>%
    group_by(week, label_content, condit) %>%
    summarise(tot = n())

# describe datasets
Hmisc::describe(df_7$label_content)
Hmisc::describe(df_7_test$label_content)

Hmisc::describe(df_7$rab_descript)
Hmisc::describe(df_7_test$rab_descript)


# conclusion: data sets differ only marginally


###difference between individual and aggregated dataset------
# individual data set # 23'683 obs
df_17 <- read_delim("augmented data/data_edit_180929_egel.csv", delim = ";", locale = locale(encoding = "LATIN1")) %>%
    mutate(date = as.Date(date))

# aggregated data set # 26234 obs
df_agg <- read_delim("augmented data/data_edit_180802_egel.csv", delim = ";", locale = locale(encoding = "LATIN1"), trim_ws = T) %>%
    mutate(date = as.Date(date)) 

diff <- setdiff(df_agg$transaction_id, df_17$transaction_id) # package lubridate

diff_data <- filter(df_agg, transaction_id %in% diff)

group_by(diff_data, pay_description) %>% count()

# one open question:
filter(diff_data, pay_description == "Badge (Debit)") %>% select(card_num) %>% n_distinct()

# conclusion: main difference between individual and aggregated data set are cash payers and pupils from the internat
# one question remains: why are 49 persons payed with campuscard not in the individual data set? => one guess, join between personal data (ZHAW) and data for the articles (SV) caused some drop outs: cannot show evidence
