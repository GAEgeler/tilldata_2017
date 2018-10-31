## analyse data -----

# status 4.10.18 // egel

# required packages
pack <- c("dplyr", "lubridate", "readr", "stringr", "readxl", "gmodels", "car")
lapply(pack, function(x){do.call("library", list(x))})


# test it again!! last testing long time ago ()

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


### statistical differences in selling bewteen intervention and basis (dependend of weeks and label content)----
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

### statistical analyses: differ label_content between weeks?----
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


### test if population differs from sample----

canteen <- df_2017[!(duplicated(df_2017$ccrs)),]# check only men, women, students and co-workers
canteen <- filter(canteen, canteen$member != "Spezialkarten")

cant <- canteen %>% group_by(gender, member) %>% summarise(tot = n()) %>% ungroup() %>% mutate(canteen_member = c("Mitarbeiterinnen", "Studentinnen", "Mitarbeiter", "Studenten"))
test_ <- tibble(member = cant$canteen_member, # take observations from dataset above
                canteen = cant$tot, 
                pop = c(333, 678, 336, 791), # observations from padtik buenter
                cant_pct = cant$tot/sum(cant$tot), # percentage of member of the sample
                pop_pct = pop/sum(pop)) # percentage of member of population

# test chi_square/fishers_test
chisq.test(test_$canteen, p = test_$pop_pct) # its another test? see https://mgimond.github.io/Stats-in-R/ChiSquare_test.html 
chisq.test(test_[ ,2:3]) # why so different than above?
fisher.test(test_[ ,2:3]) # almost same as chi_square, use one or the other!!


### test for difference between gender, member and meal sellings
# same result as above
CrossTable(canteen$gender, canteen$member, chisq = T)

# check influence of age => makes no sense
summary.lm(aov(age ~ gender + member, data = canteen))

# another way => build age groups
canteen2 <- canteen %>% 
    filter(age != 117) %>%
    mutate(age_group = cut(age,breaks=c(-Inf, 25, 35, 50, 65, Inf), # menuCH age groups
                       labels=c("16 bis 25-jährig","26 bis 34-jährig","35 bis 49-jährig","50 bis 64-jährig","keine Angaben")))
CrossTable(canteen2$age_group, canteen2$member)    

# best way, visualize data 
# age and gender
df <- group_by(canteen2, age, gender) %>%
    summarise(tot = n())

ggplot(df, aes(x = age,y = tot, color = gender)) + 
    geom_point(stat="identity", size = 2) + 
    geom_smooth(method = "loess", se = F, size = 1.3) + 
    scale_color_manual(breaks = c("F","M"),
                       values = c("F" = "#99f200","M" = "#008099"),
                       labels = c("Frauen", "Männer")) +
    guides(color = guide_legend(title = "Geschlecht")) +
    labs(x = "Alter in Jahren", y = "Häufigkeit") + #, caption = "Daten: Kassendaten SV und ZHAW"
    mytheme

# save for presentation agrifoodsystems
ggsave("C:/Users/egel/switchdrive/ZHAW/03_Lehre/agrofoodsystems/age_gender_181031_egel.pdf",
       height = 10,
       width = 18,
       dpi = 200,
       device = cairo_pdf)

#plot member and age
df <- group_by(canteen2, age, member) %>%
    summarise(tot = n())

ggplot(df, aes(x = age,y = tot, color = member)) + 
           geom_point(stat="identity", size = 2) + 
           geom_smooth(method = "loess", se = F, size = 1.3) + 
           scale_color_manual(breaks = c("Mitarbeitende","Studierende"),
                              values = c("Mitarbeitende" = "#6619e6","Studierende" = "#80ccff")) +
           guides(color = guide_legend(title = "Hochschulzugehörigkeit")) +
           labs(x = "Alter in Jahren", y = "Häufigkeit") + #, caption = "Daten: Kassendaten SV und ZHAW"
           mytheme

# save for presentation agrifoodsystems
ggsave("C:/Users/egel/switchdrive/ZHAW/03_Lehre/agrofoodsystems/age_member_181031_egel.pdf",
       height = 10,
       width = 24,
       dpi = 200,
       device = cairo_pdf)


# visiter frequency --------
canteen <- df_2017 %>%
    group_by(ccrs, shop_description) %>%
    summarise(visit = n())

canteen2 <- canteen %>% 
    mutate(category=cut(visit, breaks = c(-Inf,2,12,24,36,48,60,Inf), labels=c("einmaliger Besuch", "max. 1x\n pro Woche","max. 2x\n pro Woche","max. 3x\n pro Woche","max. 4x\n pro Woche", "max. 5x\n pro Woche","mehr als 5x\n pro Woche"))) %>%
    group_by(shop_description, category) %>% 
    summarise(visit_counts=n()) %>% # count how hoften a visit occurs, e.g. oneday visitors occur 200 times 
    mutate(pct=visit_counts/sum(visit_counts))

# test for differences
df <- tibble(gruen = canteen2[canteen2$shop_description == "Grüental",]$visit_counts,
             vista = canteen2[canteen2$shop_description == "Vista",]$visit_counts)

fisher.test(df, simulate.p.value = T, B = 1000000) # seems to have differences
