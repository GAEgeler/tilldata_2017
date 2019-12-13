
#### try to replicate data set from (180929)
# there are several problems: 
#1. random.seed() not a good solution for replication, most of the difference due to that
#2. some differences due to switch between python and R. not sure why? (evtl. digest are different, however from eye i did not see an difference, not difference in structure)


###3 individual dataset

d_ind <- read_delim('clean data/data_filtered_ind_180929_egel_check.csv', delim = ";", locale = locale(encoding = "utf-8"))
e <- read_delim("clean data/data_filtered_180929_egel.csv", delim = ";")
identical(d, e) # TRUE => good


d_ck <- read_delim("augmented data/data_edit_180929_egel.csv", delim = ";", locale = locale(encoding = "latin1")) # original => many publications with it              
d_py_ind <- read_delim("augmented data/data_edit_ind_180929_egel_check.csv", delim = ";") # 1try to replicate (did some stuff in python => especially editing)
d_py_y <- read_delim("data_edit_ind_180929_check.csv", delim = ";") # 1try to replicate (did some stuff in python => especially editing)

d_ck_19 <- read_delim("augmented data/data_edit_ind_180929_egel_check19.csv", delim = ";") # 2try to replicate, exept joining (all editing was made in R)
identical(d_ck, d_ck_19) # False not good
why <- anti_join(d_ck, d_py) # now 20 cases differ => all differences are due to that 23. october 2019 (what should i do?)
why <- anti_join(d_ck, d_ck_19) # 905 cases differ => reading in python and saving unchanged file occurs 900 difference => at the moment testing where the difference is (so far nothing found, thus im taking them to be equal)


fil_18 <- filter(d_ck_18, date == "2017-10-23" & article_description == "Favorite" & shop_description == "Vista")
fil_19 <- filter(d_ck_19, date == "2017-10-23" & article_description == "Favorite" & shop_description == "Vista")
anti_join(fil_18, fil_19) # 9 cases differ => ok??


### aggregated dataset

d_agg <- read_delim("augmented data/data_edit_agg_180802_egel.csv", delim = ";", locale = locale(encoding = "LATIN1"), trim_ws = T) %>%
    mutate(date = as.Date(date)) # original data set
d_py_agg <- read_delim("augmented data/data_edit_agg_180802_egel_check.csv", delim = ";") # try to replicate (12.12.2019)
d_py2 <- read_delim("augmented data/data_edit_agg_180802_egel_check19.csv", delim = ";") # try to replicate (12.12.2019)
identical(d_py_agg, d_py2) # np.random.seed seems to work
anti_join(d_py, d_py2)


anti_join(d_agg, d_py_agg) # 31 cases differ: 2 cases due to wrong edit (did not change the content), 28 because of the wrong seed function, 1 case i dont know why (2017-12-13)


# 65 cases the same from aggregated and individual data set 2019: otherwise delete that
t <- d_py_ind %>% filter(date == "2017-10-23" & shop_description == "Vista") # 214
s <- d_py_agg %>% filter(date == "2017-10-23" & shop_description == "Vista") # 230

View(anti_join(t, s)) #24 cases differ (because of the sample method from python!)

# 65 cases the same form agg and individual data set 2018
t <- d_ind %>% filter(date == "2017-10-23" & shop_description == "Vista") # 214
s <- d_agg %>% filter(date == "2017-10-23" & shop_description == "Vista") # 230

# not working: reasons?
diff <- anti_join(t, s) # always that much as the lenght of the first merge data set
