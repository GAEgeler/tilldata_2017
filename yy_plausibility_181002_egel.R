# check for plausibility

# status: 2.10.18 // egel

# load data see script 04_load_data
set.seed(17)

df_list <- df_7 %>%
    sample_n(., size = 15) %>%
    select(ccrs)

df <- filter(df_7, ccrs %in% df_list$ccrs)

write_delim(df, "augmented data/plausibility_180310_egel.csv", delim = ";") # some problems with the encoding

# check if rab_description is same as member
df_7$rab_descript2 <- str_replace(df_7$rab_descript, "Mitarbeiter", "Mitarbeitende")

t <- filter(df_7, rab_descript == "Externer Preis") # 24 obs => is it realistic, that only 24
t_ <- filter(df_agg, rab_descript == "Externer Preis") # in comparison 403 obs in data frame from sv
t2 <- filter(df_7, member  == "Spezialkarten")
t3 <- df_7[!df_7$member == df_7$rab_descript2,] # two observations are missing somehow

# compare unique card numbers and their member and check it with padis information

df <- filter(df_7, ccrs %in% unique(df_7$ccrs)) # not working

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

