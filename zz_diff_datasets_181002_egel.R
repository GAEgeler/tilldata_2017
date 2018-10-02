#Difference between new and old data set

# status: 2.10.18 // egel

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