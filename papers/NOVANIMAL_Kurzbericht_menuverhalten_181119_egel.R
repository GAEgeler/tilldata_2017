# R-Skript for NOVANIMAL_Kurzbericht_menuverhalten_181119_egel
# Status: 23.11.2018

# to load data (analyses will be made with df_2017)
source(file = "04_load_data_180802_egel.R")
# to load themes
source(file = "08_theme_plots_180419_egel.R")
# warnings can be ignored => due to rm() command

# check for NA's----
library(Amelia)
missmap(df_2017) # especially gender (all specialcards) and some missings in label_content (locals)

# compare sample with population------
# summary canteen card holders according to gender and member
canteen <- df_2017 %>%
    filter(!duplicated(df_2017$ccrs)) %>% 
    group_by(gender, member) %>% 
    summarise(tot_canteen = n()) %>% 
    ungroup() %>%
    mutate(canteen_member = c("Mitarbeiterinnen", "Studentinnen", "Mitarbeiter", "Studenten", "Spezialkarten"),
           canteen_pct = round((tot_canteen/sum(tot_canteen))*100,1)) %>% 
    select(canteen_member, tot_canteen, canteen_pct)

pop_w <- c(333, 678, 336, 791, 358) # card holders according to gender and member of wädenswil (status: märz 2018)

canteen$tot_pop <- pop_w
canteen$pop_pct <- round((canteen$tot_pop/sum(canteen$tot_pop))*100,1)

# dont differ statistically
fisher.test(canteen[c(1:4), c(2,4)], simulate.p.value = T, B = 10000) # exclude spezialcards

# mean of age and age imputation------

canteen <- df_2017 %>%
    filter(!duplicated(df_2017$ccrs))

summary(canteen[canteen$age != 117,]$age)
library(psych)
describe(canteen[canteen$age != 117,]$age)
which(canteen$age == 117) # 58 cases has age 117 => impute or not?


# visiter frequency--------
visiter <- df_2017 %>%
    group_by(ccrs, shop_description) %>%
    summarise(visit = n())

check_cases <- filter(visiter, visit > 60) %>% select(ccrs)
check_cases <- filter(df_2017, ccrs %in% check_cases$ccrs)
    

mean(visiter$visit)

visiter_freq <- visiter %>% 
    mutate(category=cut(visit, breaks = c(-Inf,2,12,24,36,48,60,Inf), labels=c("einmaliger Besuch", "max. 1x\n pro Woche","max. 2x\n pro Woche","max. 3x\n pro Woche","max. 4x\n pro Woche", "max. 5x\n pro Woche","mehr als 5x\n pro Woche"))) %>%
    group_by(shop_description, category) %>% 
    summarise(visit_counts=n()) %>% # count how hoften a visit occurs, e.g. oneday visitors occur 200 times 
    mutate(pct=visit_counts/sum(visit_counts))

# test for differences
df <- tibble(gruen = visiter_freq[visiter_freq$shop_description == "Grüental",]$visit_counts,
             vista = visiter_freq[visiter_freq$shop_description == "Vista",]$visit_counts)

fisher.test(df, simulate.p.value = T, B = 100000) # seems to have differences

# plot frequency of visitors between canteens

visiter_freq %>% filter(visit_counts > 2) %>%
    ggplot(aes(x=category,y=pct, fill=shop_description)) +
    geom_bar(stat="identity",colour=NA, position = position_dodge(width = NULL), width = .6)+
    scale_fill_manual(values =  c("#fad60d","#c5b87c"))+
    scale_y_continuous(labels=scales::percent) +
    # scale_alpha_discrete(range = c(0.6, .8), guide=F)+
    # scale_alpha_manual(values=c(0.5, 1), guide=F)+
    xlab("\nDurchschnittliche Mensabesuche pro Woche") +
    ylab("Anteil Mensabesucher")+
    guides(fill= guide_legend(title = "Mensa-Standort"))+
    geom_text(aes(label = scales::percent(round(pct,2))), colour = "#000000", position = position_dodge(width = .6), size= 8) +
    theme_bw()+ # see skript 08_theme_plots
    theme(legend.key = element_rect(color = NA, fill = NA),
          legend.key.size = unit(1.5, "cm"), 
          legend.text = element_text(size = 30), 
          legend.title = element_text(size = 30),
          plot.title = element_text(size = 20, face = "bold"),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20, face = "plain"),
          axis.title.y = element_text(size = 25, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(size = 25,  margin = margin(t = 20, r = 0, b = 0, l = 0)),
          plot.subtitle=element_text(margin=margin(b=15),size = 20),
          plot.caption=element_text(margin=margin(t=15), face="italic", size=20))


# save for presentation agrifoodsystems
ggsave("plots/visit_freq_181128_egel.pdf",
       height = 10,
       width = 24,
       dpi = 200,
       device = cairo_pdf)


# turnover analysis-------
# turnover: payed meal price

# group data
df_ <- group_by(df_2017, condit , week, price_article) %>%
    summarise(tot_sold = n()) %>% 
    mutate(tot_turn = tot_sold * price_article) %>% 
    ungroup() %>% 
    group_by(condit, week) %>% 
    summarise(tot_sold = sum(tot_sold),
              tot_turn = sum(tot_turn)) %>% 
    mutate(pct_turn = tot_turn/sum(tot_turn))

df_hnc <- df_2017 %>%
    filter(article_description == "Hot and Cold") %>%
    group_by(condit ,week, label_content, price_article)%>%
    summarise(total = sum(price_article)) %>% 
    mutate(article_description = "Hot and Cold") %>% 
    ungroup() %>% 
    group_by(week, condit, article_description) %>% 
    summarise(total = sum(total))

df_fav <- df_2017 %>% 
    filter(grepl("+Favorite", df_2017$article_description)) %>%
    group_by(condit ,week, label_content, price_article) %>% 
    summarise(total = sum(price_article)) %>% 
    mutate(article_description = "Favorite") %>% 
    ungroup() %>% 
    group_by(week, condit, article_description) %>% 
    summarise(total = sum(total))

df_kit <- df_2017 %>% 
    filter(grepl("+Kitchen", df_2017$article_description)) %>%
    group_by(condit ,week, label_content, price_article) %>% 
    summarise(total = sum(price_article)) %>% 
    mutate(article_description = "Kitchen") %>%
    ungroup() %>% 
    group_by(week, condit, article_description) %>% 
    summarise(total = sum(total))

df_world <- df_2017 %>% 
    filter(grepl("+World", df_2017$article_description)) %>%
    group_by(condit ,week, label_content, price_article) %>% 
    summarise(total = sum(price_article)) %>% 
    mutate(article_description = "World") %>% 
    ungroup() %>% 
    group_by(week, condit, article_description) %>% 
    summarise(total = sum(total))

df_2 <- bind_rows(df_fav, df_hnc, df_kit, df_world) %>% 
    group_by(week, article_description) %>% 
    summarise(tot_turn = sum(total)) %>% 
    mutate(year = 2017)

# add data from 2015 and 2016
df_3 <- df_tot %>% # see script 04_load_data from lines 244:266
    group_by(article_description, year, week, Bruttobetrag) %>% 
    summarise(tot_sold = sum(tot_sold)) %>% 
    mutate(tot_turn = sum(Bruttobetrag)) %>% 
    ungroup() %>% 
    group_by(year, week, article_description) %>% 
    summarise(tot_turn = sum(tot_turn))

df_ <- bind_rows(df_2, df_3) # order of variable seems not to play a role ;)
                                                                                      
# plot => check sumbers of 2015 and 2016 again!!
pl <- df_ %>% 
    mutate(xlab = paste(week, condit, sep = "\n"))

ggplot(df_, aes(x = factor(week), y = tot_turn, fill = article_description)) + 
    geom_bar(stat = "identity", width = .6) +
    facet_wrap(~year)
    geom_text(aes(label = scales::percent(round(pl$pct_turn,3))), position = position_dodge(width = .9), size = 8, color = "black")

# compare with last two years??





# cluster analysis of repeated measures--------
df_ <- df_2017 %>% 
    select(ccrs, label_content, gender, member, age) %>% # select variable of interest
    dcast(formula = ccrs + gender + age + member ~ label_content, value.var="label_content", fun.aggregate= length) %>% # reshape into wide format and aggregate after occurencies of label content
    rename(Unknown = 'NA') %>% # rename NA to unknown
    mutate(tot_buy = rowSums(.[,-c(1:4)])) %>% # exclude ccrs and information of person for sum of rows
    mutate(meaty =.$Fleisch/.$tot_buy,
           hnc = .$`Hot and Cold` / .$tot_buy)
           #  , 
           # meatless = (.$Pflanzlich +.$`Pflanzlich+` + .$Vegetarisch) / .$tot_buy,
           # veg = .$Vegetarisch / .$tot_buy,
           # vegan = (.$Pflanzlich + .$`Pflanzlich+`) / .$tot_buy,
           # buffet = .$`Hot and Cold`/.$tot_buy,
           # cont_unkn= .$Unknown /.$tot_buy) # calculate meat, Vegetarisch, buffet and unknown proportions
           # 
 
# pre defined groups (one timers, some timers, buffetarians, canteeners (with three subgroups according to meat consumption))
# one timers: people went only once to the canteen 
# have some problems to override cluster with existing content
one <-  df_ %>%  
    filter(tot_buy == 1) %>% 
    mutate(cluster = "one")
 
# some timers: people went only 5 times in 60 days to the canteen 
some <-  df_ %>%  
    filter(tot_buy > 1 & tot_buy <=5) %>% 
    mutate(cluster = "some") 
 
# hot and cold: people went to the hot and cold buffet 
buffet <-  df_ %>%  
    filter(tot_buy > 5 & hnc == 1) %>% 
    mutate(cluster = "buffet") 

# meat avoiders: people only eat veg/vegan 
meat_avoiders <- df_ %>%
    filter(tot_buy > 5 & hnc < 1 & meaty == 0 & hnc == 0) %>% 
    mutate(cluster = "avoiders")

# meat lovers
meat_lovers <- df_ %>% 
    filter(tot_buy > 5 & hnc < 1 & meaty == 1 & hnc == 0) %>% 
    mutate(cluster = "lovers")

# canteen goers: people went at least once per week to the canteen
canteen <-  df_ %>%  
    filter(tot_buy > 5 & hnc < 1) %>% # min once per week
    anti_join(., meat_avoiders) %>% # to avoid conflicts with meat_avoiders
    anti_join(.,meat_lovers) # to avoid conflicts with meat_lovers

# subgroup vegi flexies
flexi_vegi <- canteen %>% 
    filter(meaty >= 0 & meaty <= 1/3 & hnc >= 0) %>% 
    mutate(cluster = "vegi flexies")

# subgroup flexi flexies
flexi_flex <- canteen %>%
    filter(meaty > 1/3 & meaty <= 2/3 & hnc >= 0) %>% 
    mutate(cluster = "flexi flexies")

# subgroup meat flexies
flexi_meat <- canteen %>%
    filter(meaty > 2/3 & meaty < 1 & hnc >= 0) %>% 
    mutate(cluster = "meat flexies")

# concat all clusters to one
df_2 <- bind_rows(one, some, buffet, meat_avoiders, meat_lovers, flexi_vegi, flexi_flex, flexi_meat)

# plot all groups: mosaik plot-------
# search for the right plot

dat <- df_2 %>% 
    group_by(cluster) %>% 
    summarise(count = n()) %>% 
    mutate(cluster.count = sum(count),
           pct = count/sum(count)) %>% 
    ungroup()

# why not stacked, because fill and x are same variable
ggplot(dat, aes(x = cluster, y = pct, fill = cluster)) +
    geom_bar(stat = "identity", position = "stack", width = .6) +
    scale_y_continuous(labels = scales::percent)+
    geom_text(aes(label = scales::percent(round(pct,2))), position = position_stack(vjust = 0.5))  # if labels are desired
    
   

 
# first group: one timers: 183----
one <- filter(df_2, cluster == "one")

# describe this group
Hmisc::describe(one$gender) # gender distribution
psych::describe(one[one$age<100,]$age) # mean of age
Hmisc::describe(one$member) # member distribution
mean(one$tot_buy) # mean of visit days


# prepare for plot => eating behavior
dat <- filter(df_2017, ccrs %in% one$ccrs) # to get other variables out of the data

pl <- dat %>%  
    group_by(gender, label_content) %>% 
    summarise(tot = n()) %>% 
    ungroup() %>% 
    mutate(gender = ifelse(is.na(gender), "NA", gender),
           label_content = ifelse(is.na(label_content), "Unbekannt",label_content)) %>% 
    mutate(gender = recode(.$gender, "F" = "Frauen", "M" = "Männer", "NA" = "Spezialkarten")) %>% 
    group_by(gender)%>% 
    mutate(pct = tot/sum(tot)) 

# add annotation
pl <- group_by(pl, gender) %>% 
    summarise(tot = sum(tot)) %>% 
    mutate(xlab0 = paste("(", tot, ")"),
           xlab = paste(gender, xlab0, sep = "\n")) %>% 
    ungroup() %>% 
    inner_join(., pl, by = "gender")

# detects dark color: for labelling the bars
isDark <- function(color) {
    (sum(grDevices::col2rgb(color) *c(299, 587,114))/1000 < 123)
}

pl$label_color <- as.factor(sapply(unlist(ColsPerCat)[pl$label_content], # takes every label and their belonged color
                                   function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"


# function to increase vertical spacing between legend keys
# source: https://github.com/tidyverse/ggplot2/issues/2844
    draw_key_polygon3 <- function(data, params, size) {
        lwd <- min(data$size, min(size) / 4)
        
        grid::rectGrob(
            width = grid::unit(0.6, "npc"),
            height = grid::unit(0.6, "npc"),
            gp = grid::gpar(
                col = data$colour,
                fill = alpha(data$fill, data$alpha),
                lty = data$linetype,
                lwd = lwd * .pt,
                linejoin = "mitre"
            ))
    }
    
    # register new key drawing function, 
    # the effect is global & persistent throughout the R session
    GeomBar$draw_key = draw_key_polygon3
    

# plot member and gender
p <- ggplot(pl, aes(y = pct,x = xlab, fill = factor(label_content, c("Unbekannt", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    # xlab(cat('"winter semester weeks (Basis: "','meat','" week, Intervention: "','vegetarian','" week"'))+
    ylab("\nVerkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot and Cold"))+
    scale_color_manual(values = levels(pl$label_color))+
    geom_text(aes(label=ifelse(pct*100>2,paste0(round(pct*100, digits=0),"%"),"")),size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    theme_bw()+ # see skript 08_theme_plots
    theme(legend.key = element_rect(color = NA, fill = NA),
          legend.key.size = unit(1.5, "cm"), 
          legend.text = element_text(size = 30), 
          legend.title = element_text(size = 30),
          plot.title = element_text(size = 20, face = "bold"),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20, face = "plain"),
          axis.title.y = element_text(size = 25, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(size = 25,  margin = margin(t = 20, r = 0, b = 0, l = 0)),
          plot.subtitle=element_text(margin=margin(b=15),size = 20),
          plot.caption=element_text(margin=margin(t=15), face="italic", size=20))


p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")

ggsave("plots/eating_one_timers_181129_egel.pdf",p,
       width = 12,
       height = 10,
       dpi = 600,
       device = cairo_pdf)

# second group: some timers: 333-----
some <- filter(df_2, cluster == "some")

# describe this group
Hmisc::describe(some$gender) # gender distribution
psych::describe(some[some$age<100,]$age) # mean of age
Hmisc::describe(some$member) # member distribution
mean(some$tot_buy)



# how do they eat -> plot eating behavior
dat <- filter(df_2017, ccrs %in% some$ccrs) # check what dos 333 person consumed during the experiement
pl <- dat %>% 
    group_by(label_content, gender) %>% 
    summarise(tot_sold = n()) %>% 
    ungroup() %>% 
    group_by(gender) %>%
    mutate(pct = tot_sold/sum(tot_sold)) %>% 
    ungroup() %>% 
    mutate(label_content = ifelse(is.na(label_content),"Unbekannt",label_content),
           gender = recode(.$gender, "F" = "Frauen", "M" = "Männer"), # attention the order of those 2 code lines matter
           gender = ifelse(is.na(.$gender),"Spezialkarten", gender))

# add annotation
text <- group_by(dat, gender) %>% summarise(tot = n()) %>%
    mutate(label = "italic(n)") %>%
    mutate(label2 = paste(label, tot, sep="=="))

# add xlab
pl <- some %>% group_by(gender) %>% 
    summarise(tot_member = n()) %>% 
    mutate(gender = recode(.$gender, "F" = "Frauen", "M" = "Männer")) %>% 
    mutate(gender = ifelse(is.na(gender),"Spezialkarten",gender)) %>% 
    inner_join(.,pl, by = "gender") %>% 
    ungroup() %>% 
    mutate(xlab0 = paste("(",tot_member,")"),
           xlab = paste(gender, xlab0, sep = "\n"))


## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unbekannt" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
pl$label_color <- as.factor(sapply(unlist(ColsPerCat)[pl$label_content], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"


#plot
p <- ggplot(pl, aes(y = pct,x = xlab, fill = factor(label_content, c("Unbekannt", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    # xlab(cat('"winter semester weeks (Basis: "','meat','" week, Intervention: "','vegetarian','" week"'))+
    ylab("\nVerkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot and Cold"))+
    scale_color_manual(values = levels(pl$label_color))+
    geom_text(aes(label=ifelse(pct*100>2,paste0(round(pct*100, digits=0),"%"),"")),size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:3, y = 1.03, label = text$label2,parse=T, size=9) + # why so big differences to the first version
    theme_bw()+ # see skript 08_theme_plots
    theme(legend.key = element_rect(color = NA, fill = NA),
          legend.key.size = unit(1.5, "cm"), 
          legend.text = element_text(size = 30), 
          legend.title = element_text(size = 30),
          plot.title = element_text(size = 20, face = "bold"),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20, face = "plain"),
          axis.title.y = element_text(size = 25, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(size = 25,  margin = margin(t = 20, r = 0, b = 0, l = 0)),
          plot.subtitle=element_text(margin=margin(b=15),size = 20),
          plot.caption=element_text(margin=margin(t=15), face="italic", size=20))


p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")

ggsave("plots/eating_some_timers_181129_egel.pdf",p,
       width = 12,
       height = 10,
       dpi = 600,
       device = cairo_pdf)


# third group: canteen people----
# cluster hot and colders 18 --------
buffetarians <- filter(df_2, cluster == "buffet")

# describe group
Hmisc::describe(buffetarians$gender)
psych::describe(buffetarians[buffetarians$age<100,]$age)
Hmisc::describe(buffetarians$member)
mean(buffetarians$tot_buy)

# plot eventually gender and member
dat <- filter(df_2017, ccrs %in% buffetarians$ccrs) # check what dos 333 person consumed during the experiement
pl <- dat %>% 
    group_by(label_content, gender) %>% 
    summarise(tot_sold = n()) %>% 
    ungroup() %>% 
    group_by(gender) %>%
    mutate(pct = tot_sold/sum(tot_sold)) %>% 
    ungroup() %>% 
    mutate(label_content = ifelse(is.na(label_content),"Unbekannt",label_content),
           gender = recode(.$gender, "F" = "Frauen", "M" = "Männer"), # attention the order of those 2 code lines matter
           gender = ifelse(is.na(.$gender),"Spezialkarten", gender))



# cluster meat avoiders: 21, no hot and cold----
meat_avoiders <- filter(df_2, cluster == "avoiders")

## describe this cluster
Hmisc::describe(meat_avoiders$gender)
psych::describe(meat_avoiders[meat_avoiders$age<100,]$age)
Hmisc::describe(meat_avoiders$member)
mean(meat_avoiders$tot_buy)

# plot eventually gender and member
dat <- filter(df_2017, ccrs %in% meat_avoiders$ccrs) # check what dos 333 person consumed during the experiement
pl <- dat %>% 
    group_by(label_content, gender) %>% 
    summarise(tot_sold = n()) %>% 
    ungroup() %>% 
    group_by(gender) %>%
    mutate(pct = tot_sold/sum(tot_sold)) %>% 
    ungroup() %>% 
    mutate(label_content = ifelse(is.na(label_content),"Unbekannt",label_content),
           gender = recode(.$gender, "F" = "Frauen", "M" = "Männer"), # attention the order of those 2 code lines matter
           gender = ifelse(is.na(.$gender),"Spezialkarten", gender))

# cluster meat lovers: 16 persons => realistic??----
meat_lovers <- filter(df_2, cluster == "lovers")


## describe this cluster
Hmisc::describe(meat_lovers$gender) # gender distribution
psych::describe(meat_lovers[meat_lovers$age<100,]$age) # mean of age
Hmisc::describe(meat_lovers$member) # member distribution
mean(meat_lovers$tot_buy) # mean of visit days


#plot maybe gender and member
dat <- filter(df_2017, ccrs %in% meat_lovers$ccrs) # check what dos 333 person consumed during the experiement
pl <- dat %>% 
    group_by(label_content, gender) %>% 
    summarise(tot_sold = n()) %>% 
    ungroup() %>% 
    group_by(gender) %>%
    mutate(pct = tot_sold/sum(tot_sold)) %>% 
    ungroup() %>% 
    mutate(label_content = ifelse(is.na(label_content),"Unbekannt",label_content),
           gender = recode(.$gender, "F" = "Frauen", "M" = "Männer"), # attention the order of those 2 code lines matter
           gender = ifelse(is.na(.$gender),"Spezialkarten", gender))

# cluster vegi flexitarians: 1-33% meat consumtion (330)----------
flexi_vegi <- filter(df_2, cluster == "vegi flexies")

# describe this cluster
Hmisc::describe(flexi_vegi$gender) # gender distribution
psych::describe(flexi_vegi[flexi_vegi$age<100,]$age) # mean of age
Hmisc::describe(flexi_vegi$member) # member distribution
mean(flexi_vegi$tot_buy) # mean of visit days

# plot difference in eating behavior an gender
dat <- filter(df_2017, ccrs %in% flexi_vegi$ccrs)
pl <- dat %>% 
    group_by(label_content, gender) %>% 
    summarise(tot_sold = n()) %>% 
    ungroup() %>% 
    group_by(gender) %>%
    mutate(pct = tot_sold/sum(tot_sold)) %>% 
    ungroup() %>% 
    mutate(label_content = ifelse(is.na(label_content),"Unbekannt",label_content),
           gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "Männer"))

# add annotation => is wrong
text <- group_by(dat, gender) %>% summarise(tot = n()) %>%
    mutate(label = "italic(n)") %>%
    mutate(label2 = paste(label, tot, sep="=="))

# add xlab
pl <- flexi_vegi %>% group_by(gender) %>% 
    summarise(tot = n()) %>% 
    mutate(gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "Männer")) %>% 
    inner_join(.,pl, by = "gender") %>% 
    ungroup() %>% 
    mutate(gender = recode(.$gender, "F" = "Frauen", "M" = "Männer")) %>% 
    mutate(xlab0 = paste("(",tot,")"),
           xlab = paste(gender, xlab0, sep = "\n"))



## check if the background color is dark or not
# my colors for the plot
ColsPerCat=c("Unbekannt" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
pl$label_color <- as.factor(sapply(unlist(ColsPerCat)[pl$label_content], # takes every label and their belonged color
                                   function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"


#plot
p <- ggplot(pl, aes(y = pct,x = xlab, fill = factor(label_content, c("Unbekannt", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    # xlab(cat('"winter semester weeks (Basis: "','meat','" week, Intervention: "','vegetarian','" week"'))+
    ylab("\nVerkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot and Cold"))+
    scale_color_manual(values = levels(pl$label_color))+
    geom_text(aes(label=ifelse(pct*100>2,paste0(round(pct*100, digits=0),"%"),"")),size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:3, y = 1.03, label = text$label2,parse=T, size=9) + # why so big differences to the first version
    theme_bw()+ # see skript 08_theme_plots
    theme(legend.key = element_rect(color = NA, fill = NA),
          legend.key.size = unit(1.5, "cm"), 
          legend.text = element_text(size = 30), 
          legend.title = element_text(size = 30),
          plot.title = element_text(size = 20, face = "bold"),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20, face = "plain"),
          axis.title.y = element_text(size = 25, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(size = 25,  margin = margin(t = 20, r = 0, b = 0, l = 0)),
          plot.subtitle=element_text(margin=margin(b=15),size = 20),
          plot.caption=element_text(margin=margin(t=15), face="italic", size=20))


p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")

ggsave("plots/eating_flexi_veg_181129_egel.pdf",p,
       width = 12,
       height = 10,
       dpi = 600,
       device = cairo_pdf)

# cluster flexi flexitarians: 34-66% meat consumption (352)-------
flexi_flex <- filter(df_2, cluster == "flexi flexies")

# describe this cluster
Hmisc::describe(flexi_flex$gender)
psych::describe(flexi_flex[flexi_flex$age<100,]$age)
Hmisc::describe(flexi_flex$member)
mean(flexi_flex$tot_buy)

# plot difference in eating behavior an gender
dat <- filter(df_2017, ccrs %in% flexi_flex$ccrs)
pl <- dat %>% 
    group_by(label_content, gender) %>% 
    summarise(tot_sold = n()) %>% 
    ungroup() %>% 
    group_by(gender) %>%
    mutate(pct = tot_sold/sum(tot_sold)) %>% 
    ungroup() %>% 
    mutate(label_content = ifelse(is.na(label_content),"Unbekannt",label_content),
           gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "Männer"))

# add annotation => is wrong
text <- group_by(dat, gender) %>% summarise(tot = n()) %>%
    mutate(label = "italic(n)") %>%
    mutate(label2 = paste(label, tot, sep="=="))

# add xlab
pl <- flexi_flex %>% group_by(gender) %>% 
    summarise(tot = n()) %>% 
    mutate(gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "Männer")) %>% 
    inner_join(.,pl, by = "gender") %>% 
    ungroup() %>% 
    mutate(gender = recode(.$gender, "F" = "Frauen", "M" = "Männer")) %>% 
    mutate(xlab0 = paste("(",tot,")"),
           xlab = paste(gender, xlab0, sep = "\n"))



## check if the background color is dark or not
# my colors for the plot
ColsPerCat=c("Unbekannt" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
pl$label_color <- as.factor(sapply(unlist(ColsPerCat)[pl$label_content], # takes every label and their belonged color
                                   function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"


#plot
p <- ggplot(pl, aes(y = pct,x = xlab, fill = factor(label_content, c("Unbekannt", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    # xlab(cat('"winter semester weeks (Basis: "','meat','" week, Intervention: "','vegetarian','" week"'))+
    ylab("\nVerkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot and Cold"))+
    scale_color_manual(values = levels(pl$label_color))+
    geom_text(aes(label=ifelse(pct*100>2,paste0(round(pct*100, digits=0),"%"),"")),size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:3, y = 1.03, label = text$label2,parse=T, size=9) + # why so big differences to the first version
    theme_bw()+ # see skript 08_theme_plots
    theme(legend.key = element_rect(color = NA, fill = NA),
          legend.key.size = unit(1.5, "cm"), 
          legend.text = element_text(size = 30), 
          legend.title = element_text(size = 30),
          plot.title = element_text(size = 20, face = "bold"),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20, face = "plain"),
          axis.title.y = element_text(size = 25, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(size = 25,  margin = margin(t = 20, r = 0, b = 0, l = 0)),
          plot.subtitle=element_text(margin=margin(b=15),size = 20),
          plot.caption=element_text(margin=margin(t=15), face="italic", size=20))


p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")

ggsave("plots/eating_flexi_flex_181129_egel.pdf",p,
       width = 12,
       height = 10,
       dpi = 600,
       device = cairo_pdf)

# cluster meat flexitarians: over 67% meat consumption (311)--------
flexi_meat <- filter(df_2, cluster == "meat flexies")

# describe this cluster
Hmisc::describe(flexi_meat$gender)
psych::describe(flexi_meat[flexi_meat$age<100,]$age)
Hmisc::describe(flexi_meat$member)
mean(flexi_meat$tot_buy)

# plot difference in eating behavior an gender
dat <- filter(df_2017, ccrs %in% flexi_meat$ccrs)
pl <- dat %>% 
    group_by(label_content, gender) %>% 
    summarise(tot_sold = n()) %>% 
    ungroup() %>% 
    group_by(gender) %>%
    mutate(pct = tot_sold/sum(tot_sold)) %>% 
    ungroup() %>% 
    mutate(label_content = ifelse(is.na(label_content),"Unbekannt",label_content),
           gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "Männer"))

# add annotation => is wrong
text <- group_by(dat, gender) %>% summarise(tot = n()) %>%
    mutate(label = "italic(n)") %>%
    mutate(label2 = paste(label, tot, sep="=="))

# add xlab
pl <- flexi_meat %>% group_by(gender) %>% 
    summarise(tot = n()) %>% 
    mutate(gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "Männer")) %>% 
    inner_join(.,pl, by = "gender") %>% 
    ungroup() %>% 
    mutate(gender = recode(.$gender, "F" = "Frauen", "M" = "Männer")) %>% 
    mutate(xlab0 = paste("(",.$tot,")"),
           xlab = paste(gender, xlab0, sep = "\n"))



## check if the background color is dark or not
# my colors for the plot
ColsPerCat=c("Unbekannt" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
pl$label_color <- as.factor(sapply(unlist(ColsPerCat)[pl$label_content], # takes every label and their belonged color
                                   function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"


#plot
p <- ggplot(pl, aes(y = pct,x = xlab, fill = factor(label_content, c("Unbekannt", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    # xlab(cat('"winter semester weeks (Basis: "','meat','" week, Intervention: "','vegetarian','" week"'))+
    ylab("\nVerkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot and Cold"))+
    scale_color_manual(values = levels(pl$label_color))+
    geom_text(aes(label=ifelse(pct*100>2,paste0(round(pct*100, digits=0),"%"),"")),size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:3, y = 1.03, label = text$label2,parse=T, size=9) + # why so big differences to the first version
    theme_bw()+ # see skript 08_theme_plots
    theme(legend.key = element_rect(color = NA, fill = NA),
          legend.key.size = unit(1.5, "cm"), 
          legend.text = element_text(size = 30), 
          legend.title = element_text(size = 30),
          plot.title = element_text(size = 20, face = "bold"),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20, face = "plain"),
          axis.title.y = element_text(size = 25, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(size = 25,  margin = margin(t = 20, r = 0, b = 0, l = 0)),
          plot.subtitle=element_text(margin=margin(b=15),size = 20),
          plot.caption=element_text(margin=margin(t=15), face="italic", size=20))


p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")

ggsave("plots/eating_flexi_meat_181129_egel.pdf",p,
       width = 12,
       height = 10,
       dpi = 600,
       device = cairo_pdf)

# for further analyses exclude some Na's---------
# check for NA's
library(Amelia)
missmap(df_2017) # missings in meal_content, gender, label_content

# drop column, meal name
df_2017 <- select(df_2017, -meal_name)

# drop missing in label_content
df_2017 <- drop_na(df_2017, label_content) #116 cases

# 836 cases missing gender
# drop them?
df_2017 <- drop_na(df_2017, gender)

# check for age => 22 cases are 117 years old
# impute them
summary(df_2017$age) 
df_2017$age <- ifelse(df_2017$age == 117, NA, df_2017$age)

# see https://www.linkedin.com/pulse/amelia-packager-missing-data-imputation-ramprakash-veluchamy
# seems not to work
test <- amelia(df_2017, idvars=c("ccrs","transaction_id","date","article_description", "art_code","gender","member","rab_descript","pay_descript","shop_description","condit","label_content"), p2s = 0)


# fist of all check again, how are the choices distributed for more than 3 choices
set.seed(17)
t <- group_by(df_2017, ccrs) %>% summarise(tot = n()) %>% filter(tot >= 6) %>% sample_frac(.1) 
t2 <- filter(df_2017, ccrs %in% t$ccrs) # 1203 card holders with more than 2 transactions
ggplot(t2, aes(y=label_content, x = date, color = label_content)) + geom_point() + facet_wrap(~ccrs)

# change variable to

# logit regression with random intercept (ccrs and condit)-----
library(lme4)
mod <- glmer(meat ~ family = "binomial")

# multinom regression (eventually with glmer or brms possible) => https://stats.stackexchange.com/questions/319427/mixed-model-with-categorical-response-variable
library(brms)
mod0 <- brms::brm()

brms tutorials https://bayesat.github.io/lund2018/slides/andrey_anikin_slides.pdf
