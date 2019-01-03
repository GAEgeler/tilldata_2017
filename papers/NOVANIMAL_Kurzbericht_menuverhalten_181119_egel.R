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

pop_w <- c(342, 671, 335, 797, 358) # card holders according to gender and member of wädenswil (status: dezember 2017)

canteen$tot_pop <- pop_w
canteen$pop_pct <- round((canteen$tot_pop/sum(canteen$tot_pop))*100,1)

# dont differ statistically
# depends on spezialkarten
fisher.test(canteen[c(1:4), c(2,4)], simulate.p.value = T, B = 10000) # exclude spezialcards


# mean of age and age imputation------

canteen <- df_2017 %>%
    filter(!duplicated(df_2017$ccrs))

summary(canteen[canteen$age != 117,]$age)
library(psych)
describe(canteen[canteen$age != 117,]$age)
which(canteen$age == 117) # 58 cases has age 117 => impute or not?


# visiter frequency by shop description--------
visiter <- df_2017 %>%
    group_by(ccrs, shop_description) %>%
    summarise(visit = n())

aggregate(visiter$visit ~ visiter$shop_description, FUN = mean)

visiter_freq <- visiter %>% 
    mutate(category=cut(visit, breaks = c(-Inf,2,12,24,36,48,60,Inf), labels=c("einmaliger Besuch", "max. 1x\n pro Woche","max. 2x\n pro Woche","max. 3x\n pro Woche","max. 4x\n pro Woche", "max. 5x\n pro Woche","mehr als 5x\n pro Woche"))) %>%
    group_by(shop_description, category) %>% 
    summarise(visit_counts=n()) %>% # count how hoften a visit occurs, e.g. oneday visitors occur 200 times 
    mutate(pct=visit_counts/sum(visit_counts))

# test for differences
df <- tibble(gruen = visiter_freq[visiter_freq$shop_description == "Grüental", ]$visit_counts,
             vista = visiter_freq[visiter_freq$shop_description == "Vista", ]$visit_counts,
             pct_gruen = visiter_freq[visiter_freq$shop_description == "Grüental", ]$visit_counts / sum(visiter_freq[visiter_freq$shop_description == "Grüental",]$visit_counts),
             pct_vista = visiter_freq[visiter_freq$shop_description == "Vista", ]$visit_counts / sum(visiter_freq[visiter_freq$shop_description == "Vista",]$visit_counts))

fisher.test(df[ ,1:2], simulate.p.value = T, B = 100000) # seems to have differences
chisq.test(df)

# plot frequency of visitors between canteens

ggplot(visiter_freq,aes(x=category,y=pct, fill=shop_description)) +
    geom_bar(stat="identity",colour=NA, position = position_dodge(width = NULL), width = .6)+
    scale_fill_manual(values =  c("#fad60d","#c5b87c"))+
    scale_y_continuous(labels=scales::percent) +
    # scale_alpha_discrete(range = c(0.6, .8), guide=F)+
    # scale_alpha_manual(values=c(0.5, 1), guide=F)+
    xlab("\nDurchschnittliche Mensabesuche pro Woche") +
    ylab("Anteil Mensabesucher")+
    guides(fill= guide_legend(title = "Mensa-Standort"))+
    geom_text(aes(label = scales::percent(round(pct,2))), colour = "#000000", position = position_dodge(width = .6),  vjust=-0.25, size= 9) +
    mytheme # see skript 08_theme_plots
    
# save for presentation agrifoodsystems
ggsave("plots/visit_freq_181128_egel.pdf",
       height = 10,
       width = 24,
       dpi = 200,
       device = cairo_pdf)


# visitier frequency by gender --------
visiter <- df_2017 %>%
    group_by(ccrs, shop_description, gender) %>%
    summarise(visit = n()) %>% 
    ungroup() %>% 
    mutate(gender = ifelse(is.na(.$gender), "Spezialkarten", .$gender))

# mean of visits for gender
aggregate(visiter$visit ~ visiter$gender, FUN = mean)

# prepare data
visiter_freq <- visiter %>% 
    mutate(category=cut(visit, breaks = c(-Inf,2,12,24,36,48,60,Inf), labels=c("einmaliger Besuch", "max. 1x\n pro Woche","max. 2x\n pro Woche","max. 3x\n pro Woche","max. 4x\n pro Woche", "max. 5x\n pro Woche","mehr als 5x\n pro Woche"))) %>%
    group_by(gender, category) %>% 
    summarise(visit_counts=n()) %>% # count how hoften a visit occurs, e.g. oneday visitors occur 200 times 
    mutate(pct=visit_counts/sum(visit_counts))

# plot
ggplot(visiter_freq, aes(x = category, y = pct, fill = gender)) +
    geom_bar(stat="identity",colour=NA, position = position_dodge(width = NULL), width = .6)+
    scale_fill_manual(values =  c("#fad60d","#c5b87c", "grey60"),
                      labels = c("F" = "Frauen", "M" = "Männer"))+
    scale_y_continuous(labels=scales::percent) +
    # scale_alpha_discrete(range = c(0.6, .8), guide=F)+
    # scale_alpha_manual(values=c(0.5, 1), guide=F)+
    xlab("\nDurchschnittliche Mensabesuche pro Woche") +
    ylab("Anteil Mensabesucher")+
    guides(fill= guide_legend(title = "Geschlecht"))+
    geom_text(aes(label = scales::percent(round(pct,2))), colour = "#000000", position = position_dodge(width = .6),  vjust=-0.25, size= 9) +
    mytheme


# save
ggsave("plots/visit_freq_gender_181128_egel.pdf",
       height = 10,
       width = 24,
       dpi = 200,
       device = cairo_pdf)

# visitier frequency by member--------
visiter <- df_2017 %>%
    group_by(ccrs, shop_description, member) %>%
    summarise(visit = n()) %>% 
    ungroup() 

# mean of visits for gender
aggregate(visiter$visit ~ visiter$member, FUN = mean)

# prepare data
visiter_freq <- visiter %>% 
    mutate(category=cut(visit, breaks = c(-Inf,2,12,24,36,48,60,Inf), labels=c("einmaliger Besuch", "max. 1x\n pro Woche","max. 2x\n pro Woche","max. 3x\n pro Woche","max. 4x\n pro Woche", "max. 5x\n pro Woche","mehr als 5x\n pro Woche"))) %>%
    group_by(member, category) %>% 
    summarise(visit_counts=n()) %>% # count how hoften a visit occurs, e.g. oneday visitors occur 200 times 
    mutate(pct=visit_counts/sum(visit_counts))

# plot
ggplot(visiter_freq, aes(x = category, y = pct, fill = factor(member, levels = c("Studierende", "Mitarbeitende", "Spezialkarten")))) +
    geom_bar(stat="identity",colour=NA, position = position_dodge(width = NULL), width = .6)+
    scale_fill_manual(values =  c("#fad60d","#c5b87c", "grey60"))+
    scale_y_continuous(labels=scales::percent) +
    # scale_alpha_discrete(range = c(0.6, .8), guide=F)+
    # scale_alpha_manual(values=c(0.5, 1), guide=F)+
    xlab("\nDurchschnittliche Mensabesuche pro Woche") +
    ylab("Anteil Mensabesucher")+
    guides(fill= guide_legend(title = "Geschelcht"))+
    geom_text(aes(label = scales::percent(round(pct,2))), colour = "#000000", position = position_dodge(width = .6),  vjust=-0.25, size= 9) +
    mytheme

# save
ggsave("plots/visit_freq_member_181128_egel.pdf",
       height = 10,
       width = 24,
       dpi = 200,
       device = cairo_pdf)


# visitier frequency by condition--------
# make that sense?
visiter <- df_2017 %>%
    group_by(ccrs, condit) %>%
    summarise(visit = n()) %>% 
    ungroup() 

# mean of visits for gender
aggregate(visiter$visit ~ visiter$condit, FUN = mean)

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
# 18 (1%)
buffet <-  df_ %>%  
    filter(tot_buy > 5 & hnc == 1) %>% 
    mutate(cluster = "buffet") 

# build groups of canteen visitors (1043-18) beacause of their meat consumption per week (see dagevous) 
# minus buffet eaters
canteen_visitors <- filter(df_, tot_buy > 5) %>% 
    anti_join(., buffet) %>% # eclude buffet eaters
    mutate(meat_week = Fleisch/12) # divide meat buys through 12 weeks to get meat consumption per week

# meat avoiders
# 78 (5%)
meat_avoiders <- canteen_visitors %>%
    filter(meat_week == 0) %>% 
    mutate(cluster = "meat avoiders")

# conscious flexitarians: less than one third of all buyings contain meat
# 190 (190/1560)
conc_flex <- canteen_visitors %>% 
    filter(meat_week > 0 & meaty <= 1/4) %>% 
    mutate(cluster = "conscious flexitarians")

# unconscious flexitarians: less than the halt of all buyings contain meat
# 224 (224/1560)
unconc_flex <- canteen_visitors %>% 
    filter(meat_week > 0 & meaty > 1/4 & meaty <= 1/2) %>% 
    mutate(cluster = "unconscious flexitarians")# to avoid conflicts with meat_lovers

# extravert flexitarians: less than the halt of all buyings contain meat
# 328 (328/1560)
extravert_flex <- canteen_visitors %>% 
    filter(meat_week > 0 & meaty > 1/2 & meaty <= 3/4) %>% 
    mutate(cluster = "extravert flexitarians")# to avoid conflicts with meat_lovers

# meat lovers
# 205 (205/1560)
meat_lovers <- canteen_visitors %>% 
    filter(meat_week > 0 & meaty > 3/4) %>% 
    mutate(cluster = "meat lovers")# to avoid conflicts with meat_lovers



# concat all clusters to one
df_2 <- bind_rows(one, some, buffet, meat_avoiders, conc_flex, unconc_flex, extravert_flex, meat_lovers)


# plot all groups: treemap-------
# prepare data
library(treemap)
library(RColorBrewer) # color palettes

set.seed(17)
pal <- brewer.pal(n = 8, name = "Set2")

dat <- df_2 %>% 
    group_by(cluster) %>% 
    summarise(count = n()) %>% 
    mutate(pct = count/sum(count)) %>% 
    ungroup() %>% 
    mutate(cluster = recode(cluster, "one" = "one timers", "some" = "some timers", 
                            "conscious flexitarians" = " conscious flexitarians\n\u2264 1/4 Fleisch", 
                            "unconscious flexitarians" = "unconscious flexitarians\n> 1/4 bis \u2264 1/2 Fleisch",
                            "extravert flexitarians" = "extravert flexitarians \n> 1/2 bis \u2264 3/4 Fleisch", 
                            "buffet" = "buffetarians",
                            "meat lovers" = "meat lovers\n > 3/4 Fleisch")) %>% # rename clusters
    mutate(label = paste(cluster, paste(round(pct*100, 1), "%", sep = " "), sep = "\n"))


# get order in treeplot
# did not work
dat$test <- vector()
dat[grepl("one", dat$cluster), ]$test <- 2
dat[grepl("some", dat$cluster), ]$test <- 1
dat$test <- sort(dat[dat$cluster != "one",]$pct)

# open pdf device
# how to order the rectangles => sortID, however not working
cairo_pdf("plots/treemap_cluster_181221_egel.pdf", width = 20, height = 20)

treemap(dat, #Your data frame object
        index="label",  #A list of your categorical variables
        vSize = "count",  #This is your quantitative variable
        # type = "index",#Type sets the organization and color scheme of your treemap
        palette = pal,  #Select your color palette from the RColorBrewer presets or make your own.
        fontsize.title = 20, #Change the font size of the title
        title = "", # Change the name of the plot
        fontsize.labels = 30,
        algorithm = "pivotSize",
        sortID = "test",
        # aspRatio = 50/50, # Change font of the labels
        fontfamily.labels = "sans")

dev.off() # close device



# plot all groups according to their visit frequency-----

# prepare data
visiter <- df_2 %>% 
    mutate(category=cut(tot_buy, breaks = c(-Inf,2,12,24,36,48,60,Inf), labels=c("einmaliger Besuch", "max. 1x\n pro Woche","max. 2x\n pro Woche","max. 3x\n pro Woche","max. 4x\n pro Woche", "max. 5x\n pro Woche","mehr als 5x\n pro Woche"))) %>%
    group_by(cluster, category) %>% 
    summarise(visit_counts=n()) %>% # count how hoften a visit occurs, e.g. oneday visitors occur 200 times 
    mutate(pct=visit_counts/sum(visit_counts)) %>% 
    filter(cluster != "one" & cluster != "some")
 

# plot
ggplot(visiter, aes(x = category, y = pct, fill = factor(cluster, levels = c("buffet", "meat avoiders", "conscious flexitarians", "unconscious flexitarians", "extravert flexitarians", "meat lovers")))) +
    geom_bar(stat="identity",colour=NA, position = position_dodge(width = NULL), width = .6)+
    scale_fill_manual(values =  c("buffet" = "#262626","meat avoiders" = "#c5b87c", "conscious flexitarians" = "#fad60d","unconscious flexitarians" = "#e64d00", "extravert flexitarians" = "#6619e6","meat lovers" = "#99f200"))+
    scale_y_continuous(labels=scales::percent) +
    xlab("\nDurchschnittliche Mensabesuche pro Woche") +
    ylab("Anteil Mensabesucher")+
    guides(fill= guide_legend(title = "Ernährungsmuster"))+
    geom_text(aes(label = scales::percent(round(pct,2))), colour = "#000000", position = position_dodge(width = .6),  vjust=-0.25, size= 9) +
    mytheme


# save
ggsave("plots/visit_freq_cluster_181227_egel.pdf",
       height = 10,
       width = 24,
       dpi = 200,
       device = cairo_pdf)



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
    group_by(gender, label_content, condit) %>% 
    summarise(tot = n()) %>% 
    ungroup() %>% 
    mutate(gender = ifelse(is.na(gender), "NA", gender),
           label_content = ifelse(is.na(label_content), "Unbekannt",label_content)) %>% 
    mutate(gender = recode(.$gender, "F" = "Frauen", "M" = "Männer", "NA" = "Spezialkarten")) %>% 
    group_by(gender, condit)%>% 
    mutate(pct = tot/sum(tot)) %>% 
    mutate(xlab = paste(gender,condit, sep = "\n"))

# add annotation
txt <- group_by(pl, gender, condit) %>% 
    summarise(tot = sum(tot))

# detects dark color: for labelling the bars
isDark <- function(color) {
    (sum(grDevices::col2rgb(color) *c(299, 587,114))/1000 < 123)
}

ColsPerCat=c("Unbekannt" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

pl$label_color <- as.factor(sapply(unlist(ColsPerCat)[pl$label_content], # takes every label and their belonged color
                                   function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"


# plot member and gender
p <- ggplot(pl, aes(y = pct, x = xlab, fill = factor(label_content, c("Unbekannt", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    # xlab(cat('"winter semester weeks (Basis: "','meat','" week, Intervention: "','vegetarian','" week"'))+
    ylab("\nVerkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot & Cold (Buffet)"))+
    scale_color_manual(values = levels(pl$label_color))+
    geom_text(aes(label = ifelse(pl$pct<.021,"", scales::percent(round(pct,2)))),size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate("text", x = 1:6, y = 1.03, label = txt$tot, size = 8) +
    mytheme # see skript 08_theme_plots
    

p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")

ggsave("plots/eating_one_timers_181129_egel.pdf",p,
       width = 18,
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
    group_by(label_content, gender, condit) %>% 
    summarise(tot_sold = n()) %>% 
    ungroup() %>% 
    group_by(gender, condit) %>%
    mutate(pct = tot_sold/sum(tot_sold)) %>% 
    ungroup() %>% 
    mutate(label_content = ifelse(is.na(label_content),"Unbekannt",label_content),
           gender = recode(.$gender, "F" = "Frauen", "M" = "Männer"), # attention the order of those 2 code lines matter
           gender = ifelse(is.na(.$gender),"Spezialkarten", gender))




# add annotation
text <- group_by(dat, gender, condit) %>% summarise(tot = n()) %>%
    mutate(label = "italic(n)") %>%
    mutate(label2 = paste(label, tot, sep="=="))

# add xlab
pl <- dat %>% 
    filter(!duplicated(ccrs)) %>% 
    group_by(gender, condit) %>% 
    summarise(tot_member = n()) %>% 
    ungroup() %>% 
    mutate(gender = recode(.$gender, "F" = "Frauen", "M" = "Männer")) %>% 
    mutate(gender = ifelse(is.na(gender),"Spezialkarten",gender)) %>% 
    inner_join(.,pl, by = c("gender","condit")) %>% 
    ungroup() %>% 
    mutate(xlab0 = paste("(",tot_member,")"),
           xlab = paste(gender, xlab0, sep = "\n"),
           xlab = paste(xlab, condit, sep = "\n"))


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
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot & Cold (Buffet)"))+
    scale_color_manual(values = levels(pl$label_color))+
    geom_text(aes(label=ifelse(pct*100>2,paste0(round(pct*100, digits=0),"%"),"")),size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:6, y = 1.03, label = text$label2,parse=T, size=9) + # why so big differences to the first version
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")

ggsave("plots/eating_some_timers_181129_egel.pdf",p,
       width = 18,
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
    group_by(label_content, gender, condit) %>% 
    summarise(tot_sold = n()) %>% 
    ungroup() %>% 
    group_by(gender, condit) %>%
    mutate(pct = tot_sold/sum(tot_sold)) %>% 
    ungroup() %>% 
    mutate(label_content = ifelse(is.na(label_content),"Unbekannt",label_content),
           gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "Männer"))

# check mean of sellings over label content
pl %>% filter(label_content == "Fleisch") %>%
    group_by(condit) %>% 
    summarise(m = mean(pct)) %>%
    mutate(diff = diff(m))

# anovas with interaction => however what for interactions should be taken into account?
aov1 <- aov(log10(pl$tot_sold) ~ pl$gender*pl$label_content) # spezialkarten have very low variances, thus log transformation
autoplot(aov1)
summary.lm(aov1)

TukeyHSD(aov1, ordered = T) # no differences between condition and label_content => makes however not really sense does it?

# add annotation => is wrong
text <- group_by(dat, gender, condit) %>% summarise(tot = n()) %>%
    mutate(label = "italic(n)") %>%
    mutate(label2 = paste(label, tot, sep="=="))

# add xlab
pl <- dat %>%
    filter(!duplicated(ccrs)) %>% 
    group_by(gender, condit) %>% 
    summarise(tot_member = n()) %>% 
    ungroup() %>%
    mutate(gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "Männer")) %>% 
    inner_join(.,pl, by = c("gender", "condit")) %>% 
    ungroup() %>% 
    mutate(gender = recode(.$gender, "F" = "Frauen", "M" = "Männer")) %>% 
    mutate(xlab0 = paste("(",tot_member,")"),
           xlab = paste(gender, xlab0, sep = "\n"),
           xlab1 = paste(xlab, condit, sep = "\n")) 
  
## check if the background color is dark or not
# my colors for the plot
ColsPerCat=c("Unbekannt" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
pl$label_color <- as.factor(sapply(unlist(ColsPerCat)[pl$label_content], # takes every label and their belonged color
                                   function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"



# plot
# order is somehow not right => why?
p <- ggplot(pl, aes(y = pct,x = factor(xlab1, levels = c("Frauen\n( 110 )\nBasis", "Frauen\n( 77 )\nIntervention" ,"Männer\n( 69 )\nBasis",  "Männer\n( 41 )\nIntervention", "Spezialkarten\n( 4 )\nBasis", "Spezialkarten\n( 5 )\nIntervention")), fill = factor(label_content, c("Unbekannt", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    # xlab(cat('"winter semester weeks (Basis: "','meat','" week, Intervention: "','vegetarian','" week"'))+
    ylab("\nVerkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot & Cold (Buffet)"))+
    scale_color_manual(values = levels(pl$label_color))+
    # scale_x_discrete(label = c("Frauen\n( 110 )\nBasis", "Frauen\n( 77 )\nIntervention" ,"Männer\n( 69 )\nBasis",  "Männer\n( 41 )\nIntervention", "Spezialkarten\n( 4 )\nBasis", "Spezialkarten\n( 5 )\nIntervention"))+
    geom_text(aes(label=ifelse(pct*100>2,paste0(round(pct*100, digits=0),"%"),"")),size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:6, y = 1.03, label = text$label2, parse=T, size=9) + # why so big differences to the first version
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")

ggsave("plots/eating_flexi_veg_181129_egel.pdf",p,
       width = 18,
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
    group_by(label_content, gender, condit) %>% 
    summarise(tot_sold = n()) %>% 
    ungroup() %>% 
    group_by(gender, condit) %>%
    mutate(pct = tot_sold/sum(tot_sold)) %>% 
    ungroup() %>% 
    mutate(label_content = ifelse(is.na(label_content),"Unbekannt",label_content),
           gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "Männer"))

# check mean of sellings over label content
pl %>% filter(label_content == "Fleisch") %>%
    group_by(condit) %>% 
    summarise(m = mean(pct)) %>%
    mutate(diff = diff(m))

# add annotation => is wrong
text <- group_by(dat, gender, condit) %>% summarise(tot = n()) %>%
    mutate(label = "italic(n)") %>%
    mutate(label2 = paste(label, tot, sep="=="))

# add xlab
pl <- dat %>% 
    filter(!duplicated(ccrs)) %>%
    group_by(gender, condit) %>% 
    summarise(tot_member = n()) %>% 
    ungroup() %>% 
    mutate(gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "Männer")) %>% 
    inner_join(.,pl, by = c("gender", "condit")) %>% 
    ungroup() %>% 
    mutate(gender = recode(.$gender, "F" = "Frauen", "M" = "Männer")) %>% 
    mutate(xlab0 = paste("(",tot_member,")"),
           xlab = paste(gender, xlab0, sep = "\n"),
           xlab = paste(xlab, condit, sep = "\n"))



## check if the background color is dark or not
# my colors for the plot
ColsPerCat=c("Unbekannt" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
pl$label_color <- as.factor(sapply(unlist(ColsPerCat)[pl$label_content], # takes every label and their belonged color
                                   function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"


#plot
p <- ggplot(pl, aes(y = pct,x = factor(xlab, levels = c("Frauen\n( 73 )\nBasis", "Frauen\n( 76 )\nIntervention" ,"Männer\n( 127 )\nBasis",  "Männer\n( 78 )\nIntervention", "Spezialkarten\n( 9 )\nBasis", "Spezialkarten\n( 8 )\nIntervention")), fill = factor(label_content, c("Unbekannt", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    # xlab(cat('"winter semester weeks (Basis: "','meat','" week, Intervention: "','vegetarian','" week"'))+
    ylab("\nVerkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot & Cold (Buffet)"))+
    scale_color_manual(values = levels(pl$label_color))+
    geom_text(aes(label=ifelse(pct*100>2,paste0(round(pct*100, digits=0),"%"),"")),size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:6, y = 1.03, label = text$label2,parse=T, size=9) + # why so big differences to the first version
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")

ggsave("plots/eating_flexi_flex_181129_egel.pdf",p,
       width = 18,
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
    group_by(label_content, gender, condit) %>% 
    summarise(tot_sold = n()) %>% 
    ungroup() %>% 
    group_by(gender, condit) %>%
    mutate(pct = tot_sold/sum(tot_sold)) %>% 
    ungroup() %>% 
    mutate(label_content = ifelse(is.na(label_content),"Unbekannt",label_content),
           gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "Männer"))

# check mean of sellings over label content
pl %>% filter(label_content == "Fleisch") %>%
    group_by(condit) %>% 
    summarise(m = mean(pct)) %>%
    mutate(diff = diff(m))

# add annotation => is wrong
text <- group_by(dat, gender, condit) %>% summarise(tot = n()) %>%
    mutate(label = "italic(n)") %>%
    mutate(label2 = paste(label, tot, sep="=="))

# add xlab
pl <- dat %>% 
    filter(!duplicated(ccrs)) %>% 
    group_by(gender, condit) %>% 
    summarise(tot_member = n()) %>% 
    ungroup() %>% 
    mutate(gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "Männer")) %>% 
    inner_join(.,pl, by = c("gender", "condit")) %>% 
    ungroup() %>% 
    mutate(gender = recode(.$gender, "F" = "Frauen", "M" = "Männer")) %>% 
    mutate(xlab0 = paste("(",.$tot_member,")"),
           xlab = paste(gender, xlab0, sep = "\n"),
           xlab1 = paste(xlab, condit, sep = "\n")) 

## check if the background color is dark or not
# my colors for the plot
ColsPerCat=c("Unbekannt" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
pl$label_color <- as.factor(sapply(unlist(ColsPerCat)[pl$label_content], # takes every label and their belonged color
                                   function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"


#plot
p <- ggplot(pl, aes(y = pct, x = factor(xlab1, levels = c("Frauen\n( 36 )\nBasis", "Frauen\n( 15 )\nIntervention" ,"Männer\n( 141 )\nBasis",  "Männer\n( 111 )\nIntervention", "Spezialkarten\n( 5 )\nBasis", "Spezialkarten\n( 2 )\nIntervention")), fill = factor(label_content, c("Unbekannt", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    ylab("\nVerkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot & Cold (Buffet)"))+
    scale_color_manual(values = levels(pl$label_color))+
    geom_text(aes(label=ifelse(pct*100>2,paste0(round(pct*100, digits=0),"%"),"")),size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:6, y = 1.03, label = text$label2,parse=T, size=9) + # why so big differences to the first version
    mytheme


p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")

ggsave("plots/eating_flexi_meat_181129_egel.pdf",p,
       width = 18,
       height = 10,
       dpi = 600,
       device = cairo_pdf)

# cluster analyses for only people in both conditions-------
# cluster analysis of repeated measures--------
# exclude first people who eat less than 5 times in the canteen
df_ <- df_2017 %>% 
    select(ccrs, label_content, gender, member, age) %>% # select variable of interest
    dcast(formula = ccrs + gender + age + member ~ label_content, value.var="label_content", fun.aggregate= length) %>% # reshape into wide format and aggregate after occurencies of label content
    rename(Unknown = 'NA') %>% # rename NA to unknown
    mutate(tot_buy = rowSums(.[,-c(1:4)])) %>% # exclude ccrs and information of person for sum of rows
    mutate(meaty =.$Fleisch/.$tot_buy,
           hnc = .$`Hot and Cold` / .$tot_buy) %>% 
    filter(tot_buy > 5) # all canteen visitors => 1048

# check in dataset for condition
df_2 <- df_2017 %>%
    filter(ccrs %in% df_$ccrs) %>%
    select(ccrs, label_content, gender, member, age, condit) %>% # select variable of interest
    dcast(formula = ccrs + gender + age + member + condit ~ label_content, value.var="label_content", fun.aggregate= length) %>% # reshape into wide format and aggregate after occurencies of label content
    rename(Unknown = 'NA') %>% # rename NA to unknown
    mutate(tot_buy = rowSums(.[,-c(1:5)])) %>%  # exclude ccrs and information of person for sum of rows
    group_by(ccrs) %>% 
    mutate(tot_visit = sum(tot_buy), # sum all visits
           pct_visit = tot_buy / tot_visit) # calculate pct

# group people into three categories => dont make sense in my eyes
# 1. basis (people which bought their meals mostly in basis weeks (2/3))
# 2. intervention (people which bought their meals mostly in intervention weeks (2/3))
# 3. equals (people which bought their same amount in basis as in intervention(50:50))
gr1 <- filter(df_2, condit == "Basis" & pct_visit == 1/2) 
gr1 <- filter(df_2017, ccrs %in% gr1$ccrs)


# plot 
group_by(gr1, condit, week, label_content) %>%
    summarise(tot = n()) %>% 
    mutate(pct = tot/sum(tot)) %>% 
    ggplot(aes(y = pct, x = paste(factor(week), condit, sep = "\n"), fill = factor(label_content, levels = c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")))) + geom_bar(stat = "identity")



# it seems that only three people were once in one condition
t <- anti_join(df_, filter(df_2, duplicated(df_2$ccrs)), by = "ccrs")
t2 <- df_2017 %>% # check in dataset for condition
    filter(ccrs %in% t$ccrs) # went only to basis weeks, exclude them

# exclude 3 ccrs numbers
# unique cards 1040
df_3 <- anti_join(df_2, t, by = "ccrs")

# some poeple have only one observation in one week => set minimum of twice per week 
# unique cards 740
unique_cards <-  df_3 %>% 
    group_by(ccrs, condit) %>%
    summarise(tot = sum(tot_buy)) %>%
    mutate(tot_card = sum(tot)) %>% # sum both conditions
    mutate(pct_visit = tot/tot_card) %>%  # percend of visit to total visit
    filter(tot_card > 11) %>% 
    ungroup()

test2 <- df_3 %>% 
    arrange(ccrs) %>% 
    group_by(ccrs) %>%
    mutate(t = tot_buy - lag(tot_buy, default = tot_buy[1])) # check the differences in buyings between the two conditions

# check cases where both went same often into both conditions
# 143 persons
test2 <- df_3 %>% 
    arrange(ccrs) %>% 
    group_by(ccrs) %>%
    filter(tot_buy - lag(tot_buy, default = tot_buy[1]) == 0) %>%  # check the differences in buyings between the two conditions
    filter(duplicated(ccrs)) # to select all ccrs which have same buyings in both onditions (lag function causes more 0 than usable)

#can be used to double check the results
test2_ <- df_2017 %>% 
    filter(ccrs %in% test2$ccrs)
    

# plot sellings over the twelve weeks
# check for all transaktion of those 740 card numbers
pl <- df_2017 %>% 
    filter(ccrs %in% unique_cards$ccrs) %>% 
    group_by(week, condit, label_content) %>% # pay attention to the order of grouping variables!!
    summarise(tot_buy = n()) %>% 
    mutate(pct = tot_buy/sum(tot_buy)) %>% 
    ungroup() %>% 
    mutate(xlab = paste(week, condit, sep = "\n")) %>% 
    mutate(label_content = ifelse(is.na(.$label_content), "Unbekannt", .$label_content))


# text sellings per week
txt <- df_2017 %>% 
    filter(ccrs %in% unique_cards$ccrs) %>% 
    group_by(week, condit) %>% 
    summarise(week_sell = n()) %>%
    mutate(label = paste("italic(n)",week_sell, sep = "=="))

   

# add text (card numbers per week) per week => not that easy!!
text <- df_2017 %>% 
    filter(ccrs %in% unique_cards$ccrs) %>%
    group_by(condit, week) %>% 
    summarise(week_sell = n()) 

## check if the background color is dark or not
# my colors for the plot
ColsPerCat=c("Unbekannt" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
pl$label_color <- as.factor(sapply(unlist(ColsPerCat)[pl$label_content], # takes every label and their belonged color
                                   function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"


# plot
# order is somehow not right => why?
p <- ggplot(pl, aes(y = pct,x = factor(xlab), fill = factor(label_content, c("Unbekannt", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    # xlab(cat('"winter semester weeks (Basis: "','meat','" week, Intervention: "','vegetarian','" week"'))+
    ylab("\nVerkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot & Cold (Buffet)"))+
    scale_color_manual(values = levels(pl$label_color))+
    # scale_x_discrete(label = c("Frauen\n( 110 )\nBasis", "Frauen\n( 77 )\nIntervention" ,"Männer\n( 69 )\nBasis",  "Männer\n( 41 )\nIntervention", "Spezialkarten\n( 4 )\nBasis", "Spezialkarten\n( 5 )\nIntervention"))+
    geom_text(aes(label=ifelse(pct*100>2,paste0(round(pct*100, digits=0),"%"),"")),size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:12, y = 1.03, label = txt$label, parse=T, size=9) + # why so big differences to the first version
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")

# save plot
ggsave("plots/181219.pdf",p)

# for further analyses exclude some Na's---------
# check for NA's
library(Amelia)
missmap(df_2017) # missings in meal_content, gender, label_content

# drop column, meal name
df_17 <- df_2017
df_2017 <- select(df_2017, -meal_name)

# drop missing in label_content
df_2017 <- drop_na(df_2017, label_content) #116 cases

# 843 cases missing gender
sum(is.na(df_17$gender))

# drop or impute them?
df_2017 <- drop_na(df_2017, gender)

# check for age => 843 (from above)+ 22 cases are 117 years old
df_17[df_17$age == 117,]
# impute them
summary(df_2017$age) 
df_17$age <- ifelse(df_17$age == 117, NA, df_17$age)

# check if missings are MAR
# https://stats.stackexchange.com/questions/11991/are-misses-in-my-data-distributed-completely-at-random

# prepare data for imputation
# gender as factor
df_17$gender <- factor(df_17$gender, levels = c("M","F"))
df_17$label_content <- factor(df_17$label_content)

# drop variables not for imputation
test <- select(df_17, date, gender, age, price_article, label_content)

# see https://www.linkedin.com/pulse/amelia-packager-missing-data-imputation-ramprakash-veluchamy
# seems not to work
test <- amelia(test, m = 3, ts = "date", noms = "gender", idvars=c("label_content","price_article"))
test <- amelia(df_2017, m = 3, ts = c("date","year"), noms = "gender", 
               idvars=c("ccrs","transaction_id","article_description", "art_code","member","rab_descript","pay_descript","shop_description","condit","label_content"), p2s = 0)


# fist of all check again, how are the choices distributed for more than 3 choices
set.seed(17)
t <- group_by(df_2017, ccrs) %>% summarise(tot = n()) %>% filter(tot >= 6) %>% sample_frac(.1) 
t2 <- filter(df_2017, ccrs %in% t$ccrs) # 1203 card holders with more than 2 transactions
ggplot(t2, aes(y=label_content, x = date, color = label_content)) + geom_point() + facet_wrap(~ccrs)

# change variable to meat variable (hot and cold => no meat)
# Hot and cold are declared as vegetarian meals!
# attention age ist still with 117 coded => drop them (22)
# drop label_content (116) and gender (843) with NA
dat_17 <- df_2017 %>% 
    filter(age != 117) %>% 
    drop_na(label_content, gender) %>%  # drop NA's meal_content due to locals (116) and gender due to spezialkarten (843)
    mutate(member = factor(member, levels = c("Studierende", "Mitarbeitende")),
           gender = factor(gender, levels = c("F","M")))

dat_17$meat <- ifelse(dat_17$label_content == "Fleisch", 1, 0)
dat_17$ccrs <- parse_integer(dat_17$ccrs)


#show table
table(dat_17$meat)

# logit regression with random intercept (ccrs and condit)-----
library(lme4)
# interaction seems not to converge
# age and gender no interaction effect
# condition causes some problems
mod <- glmer(meat ~ gender + member + age + condit + (1|ccrs), data = dat_17, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
mod1 <- glmer(meat ~ gender + member + age + (1|shop_description) + (1|ccrs) + (1|condit), data = dat_17, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(mod)


library(GLMMadaptive)
mod2 <- mixed_model(fixed = meat ~ gender + age + member + condit, random = ~1 | ccrs, data = dat_17, family = binomial())
summary(mod2)

library(brms)
#take first sample 
test <- dat_17 %>% sample_frac(.1)

# takes around 10-20min!
# many problems! see http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup 
mod3 <- brms::brm(meat ~ gender + age + member + (1|ccrs) + (1|condit), data = test, family = "bernoulli") # evtl bernoulli
summary(mod3)
stancode(mod3)

#predict model
predicted <- predict(mod1, dat_17, type = "response")

# erzeugt eine Tabelle mit den beobachteten Fleischesser/Nichtleischesser und den Vorhersagen des Modells
km <- table(dat_17$meat, predicted > 0.5)
dimnames(km) <- list(c("kein Fleisch", "Fleisch"), c("Modell kein Fleisch", "Modell Fleisch"))
km

# kalkuliert die Missklassifizierungsrate 
mf <- 1-sum(diag(km)/sum(km)) # ist mit knapp 23% eher hoch ???
mf

# Pseudo R^2
library(MuMIn)
r.squaredGLMM(mod1) # 44% of the variance is explained through the model
# das marginale R^2 gibt uns die erklärte Varianz der fixen Effekte
# das conditionale R^2 gibt uns die erklärte Varianz für das ganze Modell (mit fixen und variablen Effekten)
# für weitere Informationen: https://rdrr.io/cran/MuMIn/man/r.squaredGLMM.html 

# zusätzliche Informationen, welche für die Interpretation gut sein kann
exp(confint(mod1)) # sollte gleiche infos ergeben wie die fixef() funktion


# fixef: fixed effects from the model were obtained
# ranef: ranfom effects from the model were obtained
# coef:  returns the subject-specific coefficients, i.e., the sum of the fixed and random effects coefficients

fixef(mod1)
ranef(mod1)
coef(mod1)
marginal_coefs(mod1) # only with GLLMadaptive possible

# calculated standard error 
# square root of covariance matrix
se <- sqrt(diag(vcov(mod1)))

tab1 <- cbind(Est = fixef(mod1), LL = fixef(mod1) - 1.96 * se, UL = fixef(mod1) + 1.96 *
                  se)

# erzeugt die Odds Ratios
exp(tab1)

# probabilites of logits (see function above)
logit2prob(tab1[ ,1])

# same should result using predict
# however not working
predict(mod1, data.frame(gender = 1, member = 1, age = 20), type = "response")

# write formula
# meat consumption for men
prob(meat) = b0 + b1+1*genderM + b2*memberStudent + b3*age
?? = .40 + .79 + .46


# logits to probabilites
logit2prob <- function(logit){
    odds <- exp(logit)
    prob <- odds / (1 + odds)
    return(prob)
}


# distribution of logit and probabilities
logit_seq <- seq(-10, 10, by = .1)

prob_seq <- logit2prob(logit_seq)


df <- data.frame(Logit = logit_seq,
                 Probability = prob_seq)

ggplot(df) +
    aes(x = logit_seq, y = prob_seq) +
    geom_point(size = 2, alpha = .3) +
    labs(x = "logit", y = "probability of success")+
    theme_bw()







ggplot(dat_17, aes(y = meat, x = predicted)) + 
    geom_point() + 
    geom_smooth(family="binomial", se=F)






# multinom regression (eventually with glmer or brms possible) => https://stats.stackexchange.com/questions/319427/mixed-model-with-categorical-response-variable
library(brms)
mod0 <- brms::brm()

brms tutorials https://bayesat.github.io/lund2018/slides/andrey_anikin_slides.pdf
