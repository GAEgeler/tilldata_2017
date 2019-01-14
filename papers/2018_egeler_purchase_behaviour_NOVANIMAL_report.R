# R-Skript for brief report of selected results on purchase behaviour
# Status: 10.01.2019

# to load data (analyses will be made with df_2017)
source(file = "04_load_data_180802_egel.R")
# to load themes
source(file = "08_theme_plots_180419_egel.R")
# warnings can be ignored => due to rm() command

# chapter 3.1. compare sample with population----------
# summary canteen card holders according to gender and member
canteen <- df_2017 %>%
    filter(!duplicated(df_2017$ccrs)) %>% 
    group_by(gender, member) %>% 
    summarise(tot_canteen = n()) %>% 
    ungroup() %>%
    mutate(canteen_member = c("Mitarbeiterinnen", "Studentinnen", "Mitarbeiter", "Studenten", "Spezialkarten"),
           canteen_pct = round((tot_canteen/sum(tot_canteen))*100,1)) %>% 
    select(canteen_member, tot_canteen, canteen_pct)

# see excel_file in folder 00_grundgesamtheit dep N
pop_w <- c(342, 671, 335, 797, 358) # card holders according to gender and member of w?denswil (status: dezember 2017)

canteen$tot_pop <- pop_w
canteen$pop_pct <- round((canteen$tot_pop/sum(canteen$tot_pop))*100,1)

# dont differ statistically
# depends on spezialkarten
fisher.test(canteen[c(1:4), c(2,4)], simulate.p.value = T, B = 10000) # exclude spezialcards


# mean of age and age imputation
canteen <- df_2017 %>%
    filter(!duplicated(df_2017$ccrs))

summary(canteen[canteen$age != 117,]$age)
library(psych)
describe(canteen[canteen$age != 117,]$age)
which(canteen$age == 117) # 58 cases has age 117 => impute or not?

# chapter 3.2: visiter frequency by shop description--------
visiter <- df_2017 %>%
    group_by(ccrs, shop_description) %>%
    summarise(visit = n())

aggregate(visiter$visit ~ visiter$shop_description, FUN = mean)

visiter_freq <- visiter %>% 
    mutate(category=cut(visit, breaks = c(-Inf,2,5,12,24,36,48,Inf), 
                        labels=c("einmaliger\n Besuch", "weniger als 1x\n pro Woche\n (2 bis 5-mal)", "max. 1x\n pro Woche \n (6 bis 12-mal)",
                                 "max. 2x\n pro Woche \n (13 bis 24-mal)","max. 3x\n pro Woche \n (25 bis 36-mal)","max. 4x\n pro Woche \n (37 bis 48-mal)",
                                 "max. 5x\n pro Woche \n (48 bis 60-mal)"))) %>%
    group_by(shop_description, category) %>% 
    summarise(visit_counts=n()) %>% # count how hoften a visit occurs, e.g. oneday visitors occur 200 times 
    mutate(pct=visit_counts/sum(visit_counts))

# test for differences
df <- tibble(gruen = visiter_freq[visiter_freq$shop_description == "Gr?ental", ]$visit_counts,
             vista = visiter_freq[visiter_freq$shop_description == "Vista", ]$visit_counts,
             pct_gruen = visiter_freq[visiter_freq$shop_description == "Gr?ental", ]$visit_counts / sum(visiter_freq[visiter_freq$shop_description == "Gr?ental",]$visit_counts),
             pct_vista = visiter_freq[visiter_freq$shop_description == "Vista", ]$visit_counts / sum(visiter_freq[visiter_freq$shop_description == "Vista",]$visit_counts))

fisher.test(df[ ,1:2], simulate.p.value = T, B = 100000) # seems to have differences
chisq.test(df)

# plot frequency of visitors between canteens
ggplot(visiter_freq,aes(x=category,y=pct, fill=shop_description)) +
    geom_bar(stat="identity",colour=NA, position = position_dodge(width = NULL), width = .6)+
    scale_fill_manual(values =  c("#fad60d","#c5b87c"))+
    scale_y_continuous(limits = c(0, .30), breaks = seq(0,.30,.10), labels=scales::percent) +
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


# chapter 3.2: visitier frequency by gender -----
visiter <- df_2017 %>%
    group_by(ccrs, shop_description, gender) %>%
    summarise(visit = n()) %>% 
    ungroup() %>% 
    mutate(gender = ifelse(is.na(.$gender), "Spezialkarten", .$gender)) %>% 
    filter(!grepl("Spezialkarten", .$gender)) # exclude spezialkarten

# mean of visits for gender
aggregate(visiter$visit ~ visiter$gender, FUN = mean)

# prepare data
visiter_freq <- visiter %>% # exclude spezialkarten
    mutate(category=cut(visit, breaks = c(-Inf,2,5,12,24,36,48,Inf), 
                        labels=c("einmaliger\n Besuch", "weniger als 1x\n pro Woche\n (2 bis 5-mal)", "max. 1x\n pro Woche \n (6 bis 12-mal)",
                                 "max. 2x\n pro Woche \n (13 bis 24-mal)","max. 3x\n pro Woche \n (25 bis 36-mal)","max. 4x\n pro Woche \n (37 bis 48-mal)",
                                 "max. 5x\n pro Woche \n (48 bis 60-mal)"))) %>%
    group_by(gender, category) %>% 
    summarise(visit_counts=n()) %>%
    mutate(pct=visit_counts/sum(visit_counts))

# plot
ggplot(visiter_freq, aes(x = category, y = pct, fill = gender)) +
    geom_bar(stat="identity",colour=NA, position = position_dodge(width = NULL), width = .6)+
    scale_fill_manual(values =  c("#fad60d","#c5b87c", "grey60"),
                      labels = c("F" = "Frauen", "M" = "M?nner"))+
    scale_y_continuous(limits = c(0, .35), breaks = seq(0,.35,.10), labels=scales::percent) +
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

# chapter 3.2: visitier frequency by member------
visiter <- df_2017 %>%
    group_by(ccrs, shop_description, member) %>%
    summarise(visit = n()) %>% 
    ungroup() 

# mean of visits for member
aggregate(visiter$visit ~ visiter$member, FUN = mean)

# prepare data
visiter_freq <- visiter %>% 
    filter(!grepl("Spezialkarten", .$member)) %>% # exclude spezialkarten
    mutate(category=cut(visit, breaks = c(-Inf,2,5,12,24,36,48,Inf), 
                        labels=c("einmaliger\n Besuch", "weniger als 1x\n pro Woche\n (2 bis 5-mal)", "max. 1x\n pro Woche \n (6 bis 12-mal)",
                                 "max. 2x\n pro Woche \n (13 bis 24-mal)","max. 3x\n pro Woche \n (25 bis 36-mal)","max. 4x\n pro Woche \n (37 bis 48-mal)",
                                 "max. 5x\n pro Woche \n (48 bis 60-mal)"))) %>%
    group_by(member, category) %>% 
    summarise(visit_counts=n()) %>% 
    mutate(pct=visit_counts/sum(visit_counts))


# plot
ggplot(visiter_freq, aes(x = category, y = pct, fill = factor(member, levels = c("Studierende", "Mitarbeitende", "Spezialkarten")))) +
    geom_bar(stat="identity",colour=NA, position = position_dodge(width = NULL), width = .6)+
    scale_fill_manual(values =  c("#fad60d","#c5b87c", "grey60"))+
    scale_y_continuous(limits = c(0, .30), breaks = seq(0,.30,.10), labels=scales::percent) +
    xlab("\nDurchschnittliche Mensabesuche pro Woche") +
    ylab("Anteil Mensabesucher")+
    guides(fill= guide_legend(title = "Hochschulangeh?rigkeit"))+
    geom_text(aes(label = scales::percent(round(pct,2))), colour = "#000000", position = position_dodge(width = .6),  vjust=-0.25, size= 9) +
    mytheme

# save
ggsave("plots/visit_freq_member_181128_egel.pdf",
       height = 11,
       width = 24,
       dpi = 200,
       device = cairo_pdf)


# chapter 3.3: cluster analysis for nutritional patterns-----
df_ <- df_2017 %>% 
    select(ccrs, label_content, gender, member, age) %>% # select variable of interest
    dcast(formula = ccrs + gender + age + member ~ label_content, value.var="label_content", fun.aggregate= length) %>% # reshape into wide format and aggregate after occurencies of label content
    rename(Unknown = 'NA') %>% # rename NA to unknown
    mutate(tot_buy = rowSums(.[,-c(1:4)])) %>% # exclude ccrs and information of person for sum of rows
    mutate(meaty =.$Fleisch/.$tot_buy,
           hnc = .$`Hot and Cold` / .$tot_buy)


# pre defined groups (one timers, some timers, buffetarians, canteen eaters (with three subgroups according to meat consumption)
# one timers: people went only once to the canteen 
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
    anti_join(., buffet) # exclude buffet eaters
    

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
    mutate(cluster = "unconscious flexitarians")

# extravert flexitarians: less than the halt of all buyings contain meat
# 328 (328/1560)
disenag_meat <- canteen_visitors %>% 
    filter(meat_week > 0 & meaty > 1/2 & meaty <= 3/4) %>% 
    mutate(cluster = "disengaged meat-eaters")

# meat lovers
# 205 (205/1560)
meat_lovers <- canteen_visitors %>% 
    filter(meat_week > 0 & meaty > 3/4) %>% 
    mutate(cluster = "meat lovers")


# concatenate all clusters to one
df_2 <- bind_rows(one, some, buffet, meat_avoiders, conc_flex, unconc_flex, disenag_meat, meat_lovers)


# plot all groups: treemap
# prepare data
library(treemap)
library(RColorBrewer) # color palettes

set.seed(18)
pal <- brewer.pal(n = 8, name = "Set1") # first version

pal2 <- c("#262626", "#fad60d", "#c5b87c", "#e64d00", "#6619e6", "#99f200", "#008099", "#80ccff") # novanimal colors

dat <- df_2 %>% 
    group_by(cluster) %>% 
    summarise(count = n()) %>% 
    mutate(pct = count/sum(count)) %>% 
    ungroup() %>% 
    mutate(cluster = recode(cluster, "one" = "one timers", "some" = "some timers", 
                            "conscious flexitarians" = "conscious flexitarians\n\u2264 1/4 Fleisch", 
                            "unconscious flexitarians" = "unconscious flexitarians\n> 1/4 bis \u2264 1/2 Fleisch",
                            "disengaged meat-eaters" = "disengaged meat-eaters \n> 1/2 bis \u2264 3/4 Fleisch", 
                            "buffet" = "buffetarians",
                            "meat lovers" = "meat lovers\n > 3/4 Fleisch")) %>% # rename clusters
    mutate(label = paste(cluster, paste(round(pct*100, 0), "%", sep = " "), sep = "\n"))


# get order in treeplot
dat$test <- factor(dat$cluster, levels = c("one timers", "some timers", "buffetarians", "meat avoiders", 
                                           "conscious flexitarians\n\u2264 1/4 Fleisch",
                                           "unconscious flexitarians\n> 1/4 bis \u2264 1/2 Fleisch", 
                                           "disengaged meat-eaters \n> 1/2 bis \u2264 3/4 Fleisch", 
                                           "meat lovers\n > 3/4 Fleisch"))
dat$test2 <- as.numeric(dat$test) # get number of factor

# open pdf device

cairo_pdf("plots/treemap_cluster_181221_egel.pdf", width = 20, height = 20)

treemap(dat, #Your data frame object
        index="label",  #A list of your categorical variables
        vSize = "count",  #This is your quantitative variable
        palette = pal, #Select your color palette from the RColorBrewer presets or make your own.
        fontsize.title = 20, #Change the font size of the title
        title = "", # Change the name of the plot
        fontsize.labels = 30,
        algorithm = "pivotSize",
        sortID = "test2",
        fontfamily.labels = "sans")

dev.off() # close device

# dont now yet how to put an invidivual color to the treemap
c("one timers\n12 %" = "#99f200",
  "some timers" = "#80ccff",
  "buffet" = "#262626",
  "meat avoiders" = "#c5b87c", 
  "conscious flexitarians" = "#fad60d",
  "unconscious flexitarians" = "#e64d00", 
  "extravert flexitarians" = "#6619e6",
  "meat lovers" = "#008099")


# plot all groups according to their visit frequency
# prepare data
visiter <- df_2 %>% 
    mutate(category=cut(tot_buy, breaks = c(-Inf,2,12,24,36,48,60,Inf), labels=c("einmaliger Besuch", "max. 1x\n pro Woche","max. 2x\n pro Woche","max. 3x\n pro Woche","max. 4x\n pro Woche", "max. 5x\n pro Woche","mehr als 5x\n pro Woche"))) %>%
    group_by(cluster, category) %>% 
    summarise(visit_counts=n()) %>%  
    filter(cluster != "one" & cluster != "some")

# calculate percentage of the visiter frequency - how many clusters went once to the canteen etc.
visiter2 <- visiter %>% 
    group_by(category, cluster) %>% 
    summarise(visit_counts = sum(visit_counts)) %>% 
    mutate(pct=visit_counts/sum(visit_counts))

# add annotation
text <- group_by(visiter, category) %>% 
    summarise(tot = sum(visit_counts)) %>% 
    mutate(label = paste("italic(n)", tot, sep = "=="))

# detects dark color: for labelling the bars
isDark <- function(color) {
    (sum(grDevices::col2rgb(color) *c(299, 587,114))/1000 < 123)
}


# add colors
ColsPerCat=c("buffet" = "#262626","meat avoiders" = "#c5b87c", "conscious flexitarians" = "#fad60d","unconscious flexitarians" = "#e64d00", "disengaged meat-eaters" = "#6619e6","meat lovers" = "#008099")

visiter2$label_color <- as.factor(sapply(unlist(ColsPerCat)[visiter2$cluster], # takes every label and their belonged color
                                         function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

# plot
ggplot(visiter2, aes(x = category, y = pct, fill = factor(cluster, levels = c("buffet", "meat avoiders", "conscious flexitarians", "unconscious flexitarians", "disengaged meat-eaters", "meat lovers")), color = label_color)) +
    geom_bar(stat = "identity", colour = NA, position = position_stack(), width = .6) +
    scale_fill_manual(values =  ColsPerCat,
                      labels = c("buffetarians", "meat avoiders", "conscious flexitarians", "unconscious flexitarians", "disengaged meat-eaters", "meat lovers")) +
    scale_color_manual(values = levels(visiter2$label_color)) +
    guides(fill = guide_legend("Ern?hrungsmuster\n"),
           color = F) +
    scale_y_continuous(labels=scales::percent) +
    xlab("\nDurchschnittliche Mensabesuche pro Woche") +
    ylab("Anteil Mensabesucher pro Ern?hrungsmuster") +
    geom_text(aes(label = ifelse(visiter2$pct<.02,"", scales::percent(round(pct,2)))), position = position_stack(vjust = .5), size= 5) +
    annotate("text", x = 1:5, y = 1.05, label = text$label, parse = T, size = 7) +
    mytheme


# save
ggsave("plots/visit_freq_cluster_181227_egel.pdf",
       height = 10,
       width = 24,
       dpi = 200,
       device = cairo_pdf)


# chapter 3.3: cluster one timers: 183----
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
    mutate(gender = recode(.$gender, "F" = "Frauen", "M" = "M?nner", "NA" = "Spezialkarten")) %>% 
    group_by(gender, condit)%>% 
    mutate(pct = tot/sum(tot)) %>% 
    mutate(xlab = paste(gender,condit, sep = "\n"))

# summarise purchase behavior
aggregate(pl$tot ~ label_content, FUN = mean)
aggregate(pl$pct ~ label_content, FUN = mean) # is that a proper way? mean of pct?

# chapter 3.3: cluster some timers: 333-----
some <- filter(df_2, cluster == "some")

# describe this group
Hmisc::describe(some$gender) # gender distribution
psych::describe(some[some$age<100,]$age) # mean of age
Hmisc::describe(some$member) # member distribution
mean(some$tot_buy)

# how do they eat -> plot eating behavior
dat <- filter(df_2017, ccrs %in% some$ccrs) 
pl <- dat %>% 
    group_by(label_content, gender, condit) %>% 
    summarise(tot_sold = n()) %>% 
    ungroup() %>% 
    group_by(gender, condit) %>%
    mutate(pct = tot_sold/sum(tot_sold)) %>% 
    ungroup() %>% 
    mutate(label_content = ifelse(is.na(label_content),"Unbekannt",label_content),
           gender = recode(.$gender, "F" = "Frauen", "M" = "M?nner"), # attention the order of those 2 code lines matter
           gender = ifelse(is.na(.$gender),"Spezialkarten", gender))

# summarise purchase behavior
aggregate(pl$tot ~ label_content, FUN = mean)
aggregate(pl$pct ~ label_content, FUN = mean) # is that a proper way? mean of pct?


# chapter 3.3: cluster canteen people----
## describe all canteen visitors 
canteen <- filter(df_2, cluster != "one" & cluster!= "some")
Hmisc::describe(canteen$gender) # gender distribution
psych::describe(canteen[canteen$age<100,]$age) # mean of age
Hmisc::describe(canteen$member) # member distribution
mean(canteen$tot_buy)

# cluster hot and colders 18 --------
buffetarians <- filter(df_2, cluster == "buffet")

# describe group
Hmisc::describe(buffetarians$gender)
psych::describe(buffetarians[buffetarians$age<100,]$age)
Hmisc::describe(buffetarians$member)
mean(buffetarians$tot_buy)


# chapter 3.3: cluster meat avoiders: 78, with hot and cold----
meat_avoiders <- filter(df_2, cluster == "meat avoiders")

## describe this cluster
Hmisc::describe(meat_avoiders$gender)
psych::describe(meat_avoiders[meat_avoiders$age<100,]$age)
Hmisc::describe(meat_avoiders$member)
mean(meat_avoiders$tot_buy)

# gender and condit
dat <- filter(df_2017, ccrs %in% meat_avoiders$ccrs) 
pl <- dat %>% 
    group_by(label_content, gender, condit) %>% 
    summarise(tot_sold = n()) %>% 
    ungroup() %>% 
    group_by(gender, condit) %>%
    mutate(pct = tot_sold/sum(tot_sold)) %>% 
    ungroup() %>% 
    mutate(label_content = ifelse(is.na(label_content),"Unbekannt",label_content),
           gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "M?nner"))

# add xlab
pl <- dat %>% 
    filter(!duplicated(ccrs)) %>%
    group_by(gender, condit) %>% 
    summarise(tot_member = n()) %>% 
    ungroup() %>% 
    mutate(gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "M?nner")) %>% 
    inner_join(.,pl, by = c("gender", "condit")) %>% 
    ungroup() %>% 
    mutate(gender = recode(.$gender, "F" = "Frauen", "M" = "M?nner")) %>% 
    mutate(xlab0 = paste("(",tot_member,")"),
           xlab = paste(gender, xlab0, sep = "\n"),
           xlab = paste(xlab, condit, sep = "\n"))

# add annotation
text <- group_by(dat, gender, condit) %>% summarise(tot = n()) %>%
    mutate(label = "italic(n)") %>%
    mutate(label2 = paste(label, tot, sep="=="))


## check if the background color is dark or not
# my colors for the plot
ColsPerCat=c("Unbekannt" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
pl$label_color <- as.factor(sapply(unlist(ColsPerCat)[pl$label_content], 
                                   function(color) { if (isDark(color)) 'white' else 'black' })) 


#plot
p <- ggplot(pl, aes(y = pct,x = xlab, fill = factor(label_content, c("Unbekannt", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    ylab("\nVerkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("Men?-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot & Cold (Buffet)"))+
    scale_color_manual(values = levels(pl$label_color))+
    geom_text(aes(label=ifelse(pct<0.02,"",scales::percent(round(pct,2)))), size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:6, y = 1.03, label = text$label2,parse=T, size=9) + 
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")

#save
ggsave("plots/eating_meat_avoider_181227_egel.pdf",p,
       width = 18,
       height = 10,
       dpi = 600,
       device = cairo_pdf)


# chapter 3.3: cluster conscoius flexitarians: 1-26% meat consumtion (330)----------
cons_flex <- filter(df_2, cluster == "conscious flexitarians")

# describe this cluster
Hmisc::describe(cons_flex$gender) # gender distribution
psych::describe(cons_flex[cons_flex$age<100,]$age) # mean of age
Hmisc::describe(cons_flex$member) # member distribution
mean(cons_flex$tot_buy) # mean of visit days

# plot difference in eating behavior an gender
dat <- filter(df_2017, ccrs %in% cons_flex$ccrs)
pl <- dat %>% 
    group_by(label_content, gender, condit) %>% 
    summarise(tot_sold = n()) %>% 
    ungroup() %>% 
    group_by(gender, condit) %>%
    mutate(pct = tot_sold/sum(tot_sold)) %>% 
    ungroup() %>% 
    mutate(label_content = ifelse(is.na(label_content),"Unbekannt",label_content),
           gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "M?nner"))

# add xlab
pl <- dat %>%
    filter(!duplicated(ccrs)) %>% 
    group_by(gender, condit) %>% 
    summarise(tot_member = n()) %>% 
    ungroup() %>%
    mutate(gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "M?nner")) %>% 
    inner_join(.,pl, by = c("gender", "condit")) %>% 
    ungroup() %>% 
    mutate(gender = recode(.$gender, "F" = "Frauen", "M" = "M?nner")) %>% 
    mutate(xlab0 = paste("(",tot_member,")"),
           xlab = paste(gender, xlab0, sep = "\n"),
           xlab = paste(xlab, condit, sep = "\n")) 

# add annotation
text <- group_by(dat, gender, condit) %>% summarise(tot = n()) %>%
    mutate(label = "italic(n)") %>%
    mutate(label2 = paste(label, tot, sep="=="))

## check if the background color is dark or not
# my colors for the plot
ColsPerCat=c("Unbekannt" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
pl$label_color <- as.factor(sapply(unlist(ColsPerCat)[pl$label_content], 
                                   function(color) { if (isDark(color)) 'white' else 'black' })) 



# plot
# order is somehow not right => because of ggplot takes factor out of it, however not that order i want!
# find a easy way to reorder factors, except with levels => parse_factors(sorts per defaut alphabetically)
# check out forcats => good to change order of factors!

p <- ggplot(pl, aes(y = pct, x = parse_factor(pl$xlab, levels = c()), fill = factor(label_content, c("Unbekannt", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    ylab("\nVerkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("Men?-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot & Cold (Buffet)"))+
    scale_color_manual(values = levels(pl$label_color))+
    geom_text(aes(label=ifelse(pct*100>2,paste0(round(pct*100, digits=0),"%"),"")),size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:6, y = 1.03, label = text$label2, parse=T, size=9) + # why so big differences to the first version
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")

ggsave("plots/eating_cons_flex_181227_egel.pdf",p,
       width = 18,
       height = 10,
       dpi = 600,
       device = cairo_pdf)

# chapter 3.3: cluster unconscious  flexitarians: 34-66% meat consumption (352)-------
uncons_flex <- filter(df_2, cluster == "unconscious flexitarians")

# describe this cluster
Hmisc::describe(uncons_flex$gender)
psych::describe(uncons_flex[uncons_flex$age<100,]$age)
Hmisc::describe(uncons_flex$member)
mean(uncons_flex$tot_buy)

# plot difference in eating behavior an gender
dat <- filter(df_2017, ccrs %in% uncons_flex$ccrs)
pl <- dat %>% 
    group_by(label_content, gender, condit) %>% 
    summarise(tot_sold = n()) %>% 
    ungroup() %>% 
    group_by(gender, condit) %>%
    mutate(pct = tot_sold/sum(tot_sold)) %>% 
    ungroup() %>% 
    mutate(label_content = ifelse(is.na(label_content),"Unbekannt",label_content),
           gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "M?nner"))


# add xlab
pl <- dat %>% 
    filter(!duplicated(ccrs)) %>%
    group_by(gender, condit) %>% 
    summarise(tot_member = n()) %>% 
    ungroup() %>% 
    mutate(gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "M?nner")) %>% 
    inner_join(.,pl, by = c("gender", "condit")) %>% 
    ungroup() %>% 
    mutate(gender = recode(.$gender, "F" = "Frauen", "M" = "M?nner")) %>% 
    mutate(xlab0 = paste("(",tot_member,")"),
           xlab = paste(gender, xlab0, sep = "\n"),
           xlab = paste(xlab, condit, sep = "\n"))


# add annotation
text <- group_by(dat, gender, condit) %>% summarise(tot = n()) %>%
    mutate(label = "italic(n)") %>%
    mutate(label2 = paste(label, tot, sep="=="))

## check if the background color is dark or not
# my colors for the plot
ColsPerCat=c("Unbekannt" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
pl$label_color <- as.factor(sapply(unlist(ColsPerCat)[pl$label_content], 
                                   function(color) { if (isDark(color)) 'white' else 'black' })) 


#plot
p <- ggplot(pl, aes(y = pct, x = parse_factor(xlab, levels = c()), fill = factor(label_content, c("Unbekannt", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + 
    xlab("") +
    ylab("\nVerkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("Men?-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot & Cold (Buffet)"))+
    scale_color_manual(values = levels(pl$label_color))+
    geom_text(aes(label=ifelse(pct*100>2,paste0(round(pct*100, digits=0),"%"),"")),size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:6, y = 1.03, label = text$label2,parse=T, size=9) + 
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")

ggsave("plots/eating_uncons_flex_181129_egel.pdf",p,
       width = 18,
       height = 10,
       dpi = 600,
       device = cairo_pdf)

# chapter 3.3: cluster disengaged meat-eaters: over 67% meat consumption (311)--------
diseng_meat <- filter(df_2, cluster == "disengaged meat-eaters")

# describe this cluster
Hmisc::describe(diseng_meat$gender)
psych::describe(diseng_meat[diseng_meat$age<100,]$age)
Hmisc::describe(diseng_meat$member)
mean(diseng_meat$tot_buy)

# plot difference in eating behavior an gender
dat <- filter(df_2017, ccrs %in% diseng_meat$ccrs)
pl <- dat %>% 
    group_by(label_content, gender, condit) %>% 
    summarise(tot_sold = n()) %>% 
    ungroup() %>% 
    group_by(gender, condit) %>%
    mutate(pct = tot_sold/sum(tot_sold)) %>% 
    ungroup() %>% 
    mutate(label_content = ifelse(is.na(label_content),"Unbekannt",label_content),
           gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "M?nner"))


# add xlab
pl <- dat %>% 
    filter(!duplicated(ccrs)) %>% 
    group_by(gender, condit) %>% 
    summarise(tot_member = n()) %>% 
    ungroup() %>% 
    mutate(gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "M?nner")) %>% 
    inner_join(.,pl, by = c("gender", "condit")) %>% 
    ungroup() %>% 
    mutate(gender = recode(.$gender, "F" = "Frauen", "M" = "M?nner")) %>% 
    mutate(xlab0 = paste("(",.$tot_member,")"),
           xlab = paste(gender, xlab0, sep = "\n"),
           xlab = paste(xlab, condit, sep = "\n")) 

# add annotation
text <- group_by(dat, gender, condit) %>% summarise(tot = n()) %>%
    mutate(label = "italic(n)") %>%
    mutate(label2 = paste(label, tot, sep="=="))


## check if the background color is dark or not
# my colors for the plot
ColsPerCat=c("Unbekannt" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
pl$label_color <- as.factor(sapply(unlist(ColsPerCat)[pl$label_content], 
                                   function(color) { if (isDark(color)) 'white' else 'black' })) 


#plot
p <- ggplot(pl, aes(y = pct, x = parse_factor(xlab, levels = c()), fill = factor(label_content, c("Unbekannt", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    ylab("\nVerkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("Men?-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot & Cold (Buffet)"))+
    scale_color_manual(values = levels(pl$label_color))+
    geom_text(aes(label=ifelse(pct*100>2,paste0(round(pct*100, digits=0),"%"),"")),size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:6, y = 1.03, label = text$label2,parse=T, size=9) + 
    mytheme


p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")

ggsave("plots/eating_extra_flexi_181228_egel.pdf",p,
       width = 18,
       height = 10,
       dpi = 600,
       device = cairo_pdf)



# chapter 3.3: cluster meat lovers: 205 persons over 75% of buyings meat----
meat_lovers <- filter(df_2, cluster == "meat lovers")


## describe this cluster
Hmisc::describe(meat_lovers$gender) # gender distribution
psych::describe(meat_lovers[meat_lovers$age<100,]$age) # mean of age
Hmisc::describe(meat_lovers$member) # member distribution
mean(meat_lovers$tot_buy) # mean of visit days


#plot 
dat <- filter(df_2017, ccrs %in% meat_lovers$ccrs) 
pl <- dat %>% 
    group_by(label_content, gender, condit) %>% 
    summarise(tot_sold = n()) %>% 
    ungroup() %>% 
    group_by(gender, condit) %>%
    mutate(pct = tot_sold/sum(tot_sold)) %>% 
    ungroup() %>% 
    mutate(label_content = ifelse(is.na(label_content),"Unbekannt",label_content),
           gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "M?nner"))


# add xlab
pl <- dat %>% 
    filter(!duplicated(ccrs)) %>%
    group_by(gender, condit) %>% 
    summarise(tot_member = n()) %>% 
    ungroup() %>% 
    mutate(gender = ifelse(is.na(gender),"Spezialkarten",gender),
           gender = recode(gender, "F" = "Frauen", "M" = "M?nner")) %>% 
    complete(gender, condit, fill = list(tot_member = 4)) %>% # spezial case, all 4 persons of the special card, where in both conditions
    inner_join(.,pl, by = c("gender", "condit")) %>% 
    ungroup() %>% 
    mutate(gender = recode(.$gender, "F" = "Frauen", "M" = "M?nner")) %>% 
    mutate(xlab0 = paste("(",.$tot_member,")"),
           xlab = paste(gender, xlab0, sep = "\n"),
           xlab = paste(xlab, condit, sep = "\n")) 

# add annotation
text <- group_by(dat, gender, condit) %>% summarise(tot = n()) %>%
    mutate(label = "italic(n)") %>%
    mutate(label2 = paste(label, tot, sep="=="))


## check if the background color is dark or not
# my colors for the plot
ColsPerCat=c("Unbekannt" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
pl$label_color <- as.factor(sapply(unlist(ColsPerCat)[pl$label_content], 
                                   function(color) { if (isDark(color)) 'white' else 'black' })) 


#plot
p <- ggplot(pl, aes(y = pct, x = parse_factor(xlab, levels = c()), fill = factor(label_content, c("Unbekannt", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    ylab("\nVerkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("Men?-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot & Cold (Buffet)"))+
    scale_color_manual(values = levels(pl$label_color))+
    geom_text(aes(label=ifelse(pct*100>2,paste0(round(pct*100, digits=0),"%"),"")),size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:6, y = 1.03, label = text$label2,parse=T, size=9) + 
    mytheme


p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")

#save plot
ggsave("plots/eating_meat_lovers_181228_egel.pdf",p,
       width = 18,
       height = 10,
       dpi = 600,
       device = cairo_pdf)

# chapter 3.4: cluster analyses only for people with same buyings in both conditions--------
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

# group people into three categories (first two make no sense)
# 1. basis only people which ate in basisweeks => only 3 persons
# 2. intervention only people which ate in interventionweeks => 0 persons
# 3. equals (people which bought their same amount in basis as in intervention(50:50)) => 148 persons

# 1.
gr1 <- filter(df_2, condit == "Basis" & pct_visit == 1) 
gr1 <- filter(df_2017, ccrs %in% gr1$ccrs) # only 21 transaktions

# 2.
gr2 <- filter(df_2, condit == "Basis" & pct_visit == 0) 

# 3.
gr3 <- filter(df_2, condit == "Basis" & pct_visit == 1/2) 

# descriptives for third group
Hmisc::describe(gr3$gender)
psych::describe(gr3[gr3$age<100,]$age)
Hmisc::describe(gr3$member)
mean(gr3$tot_buy)

# prepare data for plot
pl <- df_2017 %>% 
    filter(ccrs %in% gr3$ccrs) %>% 
    group_by(week, condit, label_content) %>% # pay attention to the order of grouping variables!!
    summarise(tot_buy = n()) %>% 
    mutate(pct = tot_buy/sum(tot_buy))


# add card numbers per week (shortcut to run the code wont work, why?)
pl1 <- df_2017 %>% 
    filter(ccrs %in% gr3$ccrs) %>%
    group_by(week) %>% 
    summarise(card_week = n_distinct(ccrs)) %>%  # or n()
    left_join(., pl) %>% 
    ungroup() %>% 
    mutate(xlab0 = paste("(", card_week, ")"),
           xlab1 = paste(week, condit ,sep = "\n"),
           xlab = paste(xlab1, xlab0 , sep = "\n")) %>% 
    mutate(label_content = ifelse(is.na(.$label_content), "Unbekannt", .$label_content))


# sellings per week
txt <- df_2017 %>% 
    filter(ccrs %in% gr3$ccrs) %>% 
    group_by(week) %>% 
    summarise(week_sell = n()) %>%
    mutate(label = paste("italic(n)",week_sell, sep = "=="))


## check if the background color is dark or not
# my colors for the plot
ColsPerCat=c("Unbekannt" = "black", "Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
pl1$label_color <- as.factor(sapply(unlist(ColsPerCat)[pl1$label_content], 
                                    function(color) { if (isDark(color)) 'white' else 'black' })) 


# plot
p <- ggplot(pl1, aes(y = pct,x = factor(xlab), fill = factor(label_content, c("Unbekannt", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    ylab("\nVerkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("Men?-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot & Cold (Buffet)"))+
    scale_color_manual(values = levels(pl1$label_color))+
    geom_text(aes(label=ifelse(pct*100>2,paste0(round(pct*100, digits=0),"%"),"")),size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:12, y = 1.03, label = txt$label, parse=T, size=9) + # why so big differences to the first version
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")


#save plot
ggsave("plots/decision_eating_190104_egel.pdf",p,
       width = 26,
       height = 15,
       dpi = 600,
       device = cairo_pdf)


# chapter 3.5. regression analysis------
# check for NA's
library(Amelia)
missmap(df_2017) # missings in meal_content, gender (all specialcards), label_content

# check if missings are MAR
# https://stats.stackexchange.com/questions/11991/are-misses-in-my-data-distributed-completely-at-random
# prepare data for imputation
# gender as factor
# with amelia package it didnt work (exclude )


# change variable to meat variable
# Hot and cold are declared as vegetarian meals!
# attention age ist still with 117 coded => drop them (22)
# drop label_content (116) and gender (843) with NA
# in total 973 cases where droped
dat_17 <- df_2017 %>% 
    filter(age != 117) %>% 
    drop_na(label_content, gender) %>%  # drop NA's meal_content due to locals (116) and gender due to spezialkarten (843)
    mutate(member = factor(member, levels = c("Studierende", "Mitarbeitende")), # studierende as reference group
           gender = factor(gender, levels = c("F","M"))) # female as reference group

# add meat variable
dat_17$meat <- ifelse(dat_17$label_content == "Fleisch", 1, 0)
dat_17$ccrs <- parse_integer(dat_17$ccrs)

# add meat and veg offers per day (choice sets)
source("05_load_add_data_181001_egel.R") # to load info_orig

dat_17 <- info_orig %>% 
    filter(label_content == "Fleisch") %>% 
    group_by(date, shop_description, label_content) %>% 
    summarize(day_meat_offer = n()) %>% 
    ungroup() %>% 
    select(-label_content) %>% # to avoid second variable label_content while joining
    left_join(dat_17, ., by = c("date", "shop_description")) 

# add age groups

dat_17$age_groups <- cut(dat_17$age,breaks=c(-Inf, 25, 35, 50, 65, Inf), 
                         labels=c("16 bis 25-j?hrig","26 bis 34-j?hrig","35 bis 49-j?hrig","50 bis 64-j?hrig","keine Angaben"))

dat_17$age_groups <- relevel(dat_17$age_groups, ref = "26 bis 34-j?hrig") # 26 bis 34-j?hrig as reference group

# add cluster
dat_17 <- left_join(dat_17, df_2[ ,c("ccrs", "cluster")], by = "ccrs")


#show table
table(dat_17$meat)

# explore predictors
library(GGally)
ggpairs(dat_17[, c("gender", "age", "member", "condit")])


# regression models
library(lme4)
# with age (as numeric) the model seems not to converge => possible reasons?
# full modell with all interactions
full.model <- glmer(meat ~ gender*age_groups + member*age_groups + condit*gender + (1|ccrs), data = dat_17, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(full.model)

# check for best model
# takes around 20min
options(na.action = "na.fail") #set na action => na.fail returns error if there is an NA in the data set
dredge(full.model) 

# member seems have no influence
# gender*age_group have no influence
# member*age_group have no influence
# take one of the better models according dredge function
mod3 <- glmer(meat ~ gender + age_groups + condit + gender*condit + day_meat_offer + (1|ccrs), data = dat_17, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(mod3)

# cluster dont converge if in modell integrated => why?
# however results if other variables where droped dont make sense => e.g. meat avoiders are not signifikant negative
mod4 <- glmer(meat ~ gender + age_groups + cluster + (1|ccrs), data = dat_17, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(mod4)

# Pseudo R^2
library(MuMIn)
r.squaredGLMM(mod3) # 44% of the variance is explained through the model
# for more info: https://rdrr.io/cran/MuMIn/man/r.squaredGLMM.html 


# fixef: fixed effects from the model were obtained
# ranef: ranfom effects from the model were obtained
# coef:  returns the subject-specific coefficients, i.e., the sum of the fixed and random effects coefficients
fixef(mod2)
head(ranef(mod2))
coef(mod2)

# calculate the profile likelihood confidence intervals => with boot method takes a while
confint(mod2, method="boot", nsim=1000, parm=1:3)
confint(mod3, method="Wald", nsim=1000)

# calculated standard error 
# source: https://stats.idre.ucla.edu/r/dae/mixed-effects-logistic-regression/
# square root of covariance matrix
# are random effect taken into account?
se <- sqrt(diag(vcov(mod3)))

tab1 <- cbind(Est = fixef(mod3), LL = fixef(mod3) - 1.96 * se, UL = fixef(mod3) + 1.96 *
                  se)

# erzeugt die Odds Ratios
or <- exp(tab1)

# logits to probabilites
logit2prob <- function(logit){
    odds <- exp(logit)
    prob <- odds / (1 + odds)
    return(prob)
}

# probabilites of logits (see function above)
logit2prob(fixef(mod3))


# distribution of logit and probabilities for better understandig
logit_seq <- seq(-10, 10, by = .1)

prob_seq <- logit2prob(logit_seq)


df <- data.frame(Logit = logit_seq,
                 Probability = prob_seq)

ggplot(df) +
    aes(x = logit_seq, y = prob_seq) +
    geom_point(size = 2, alpha = .3) +
    labs(x = "logit", y = "probability of success")+
    theme_bw()


# extract model to word
library(stargazer)
stargazer(mod3, type = "html", dep.var.labels.include = T, out = "plots/table_regression_meat_190105_egel.html", digits = 2)
stargazer(exp(tab1), type = "html", dep.var.labels.include = T, out = "plots/table_regression_OR_meat_190105_egel.html", digits = 2)

# chapter 3.5: plot meat consumption between gender and condit-----
sell_dat <- dat_17 %>% 
    filter(meat == 1) %>% 
    group_by(week, label_content, gender, condit) %>% 
    summarise(tot_sold = n())

m_sell <- sell_dat %>% group_by(condit,label_content, gender) %>% summarise(val = mean(tot_sold)) # calculate means (sellings per week and gender) 

p <- ggplot(sell_dat, aes(x = condit, y = tot_sold, linetype = gender, shape = gender)) +
    geom_point(data = m_sell, aes(y = val), size = 4) +
    geom_line(data = m_sell, aes(y = val, group = gender), size = 2) + 
    labs(y = "Durchschnittlich verkaufte fleischhaltige Gerichte\n pro Woche und Bedingung\n", x = "Experimentbedingungen") + 
    scale_y_continuous(breaks = seq(0,1000,200), limits = c(0, 1000)) +
    scale_x_discrete(label = c("Basiswochen", "Interventionswochen"))+
    scale_shape_manual(values = c("F" = 15, "M" = 21), # change order of shape labels
                       breaks = c("M","F"),
                       labels = c("M?nner", "Frauen"))+
    guides(shape = guide_legend(title = "Geschlecht\n"), linetype = F)+
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz (2017)")

# save
ggsave("plots/gender_condition_HS17.pdf",
       height = 10,
       width = 14,
       dpi = 600,
       device = cairo_pdf)
