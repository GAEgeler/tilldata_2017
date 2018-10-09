## analyse data -----

# status 4.10.18 // egel

# required packages
pack <- c("dplyr", "lubridate", "readr", "stringr", "readxl", "gmodels", "car")
lapply(pack, function(x){do.call("library", list(x))})


# load data => see script 04_load_data

# test it again!! last testing long time ago

### statistical differences between semwk and label content-------------
CrossTable(df_agg$semwk,df_agg$label_content, chisq = T)
chisq.test(df_agg$semwk, df_agg$label_content)$stdres # show residuals only

adjp <- 0.05/(length(unique(df_agg$semwk))*length(unique(df_agg$label_content)))# adjusted p-value
qnorm(adjp, lower.tail = F) # critical z-value from websit:. +- 3.39 


### statistical differences in selling data between the semester weeks-----------
# selling differences between days?
sell_dat <- df_agg %>%
    mutate(day=wday(date, label = T)) %>% 
    group_by(day, semwk) %>%
    summarise(tot_sold=n())

leveneTest(sell_dat$tot_sold, as.factor(sell_dat$semwk))# are varianzes homogene
ao=(aov(sell_dat$tot_sold~as.factor(sell_dat$semwk))) # is this the right test?
summary(ao)
TukeyHSD(ao) # no selling differences between days


# selling differences between weeks?
sell_dat <- df_agg %>%
    mutate(day=wday(date, label = T)) %>% 
    group_by(day, semwk, condit) %>%
    summarise(tot_sold=n())

leveneTest(sell_dat$tot_sold, sell_dat$condit)# are varianzes homogene
ao=(aov(sell_dat$tot_sold ~ sell_dat$condit : sell_dat$day)) # is this the right test?
summary(ao)
TukeyHSD(ao) # no selling differences between same weekdays and condition (e.g. Monday Basis vs. Monday Intervention etc.)


### statistical differences in selling bewteen intervention and basis (dependend of weeks and label content)
# fist method: anova
sell_dat <- df_agg %>%
    mutate(day=wday(date, label = T)) %>% # to get the daily varianze in data, however makes it not comparable (24.7.18, egel)
    group_by(day, semwk, condit) %>%
    summarise(tot_sold=n())

sell_dat <- df_agg %>%
    group_by(week, condit) %>%
    summarise(tot_sold=n())


ao <- (aov(sell_dat$tot_sold ~ sell_dat$condit)) # is this the right test?
summary.lm(ao)
TukeyHSD(ao)

chisq.test(sell_dat$tot_sold, sell_dat$condit)

### statistical analyses: differ label_content between weeks?
df_ <- df_agg  
df_[grepl("Pflanzlich$",df_$label_content),]$label_content <- "Vegetarisch" # $ sign matches the end of the string, find only vegan (without the +)
df_[grepl("Pflanzlich\\+",df_$label_content),]$label_content <- "Vegetarisch" # after \\ matches even with special sign


sell_dat2 <- df_ %>%    
    group_by(condit ,week, label_content)%>%
    summarise(tot_sold=n())

leveneTest(sell_dat2$tot_sold ~ sell_dat2$label_content) # not significant

ao1 <- aov(sell_dat2$tot_sold ~ interaction(sell_dat$condit, sell_dat$label_content))
summary.lm(ao1)
TukeyHSD(ao1)

lm2 <- lm(df_2$tot_sold ~ df_2$label_content*df_2$condit)
summary(lm2)