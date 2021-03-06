# R-Skript for brief report of selected results on purchase behaviour
# State: may 2019
# author: gian-andrea egeler

####ATTENTION: some encoding problems with umlaute!!

# to load data (analyses will be made with df_2017)
source(file = "04_load_data_180802_egel.R")
source(file = "04_1_load_data_190128_egel.R")
# to load themes
source(file = "08_theme_plots_180419_egel.R")
# warnings can be ignored => due to rm() command

# load function isDark
source("09_function_is_dark_190114_egel.R")

# chapter 3.1. compare sample with population----------
# summary canteen card holders according to gender and member
canteen <- df_2017 %>%
    filter(!duplicated(df_2017$ccrs)) %>% # to get single card holders
    group_by(gender, member) %>% 
    summarise(tot_canteen = n()) %>% 
    ungroup() %>%
    mutate(canteen_member = c("Mitarbeiterin", "Studentin", "Mitarbeiter", "Student", "Spezialkarten"),
           canteen_pct = round((tot_canteen/sum(tot_canteen))*100,1)) %>% 
    select(-gender)

# see excel_file in folder 00_grundgesamtheit dep N: population w�denswil
pop_w <- read_excel("S:/pools/n/N-IUNR-nova-data/00_grundgesamtheit dep N/campus_cards_181220_egel.xlsx", range = "F3:G10", sheet = 2) %>%
    rename(pop_member = X__1, tot_pop = Frequenz) %>% 
    drop_na() %>% 
    mutate(pop_pct = round(.$tot_pop / sum(.$tot_pop)*100,1)) # card holders according to gender and member of w�denswil (status: dezember 2017)

canteen <- left_join(canteen, pop_w, by = c("canteen_member" = "pop_member"))

# dont differ statistically
# depends on spezialkarten
fisher.test(canteen[c(1:4), c(2,5)], simulate.p.value = T, B = 10000) # exclude spezialcards


# mean of age and age imputation
canteen <- df_2017 %>%
    filter(!duplicated(df_2017$ccrs))

summary(canteen[canteen$age != 117,]$age)
psych::describe(canteen[canteen$age != 117,]$age)
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
df <- tibble(gruen = visiter_freq[visiter_freq$shop_description == "Gr�ental", ]$visit_counts,
             vista = visiter_freq[visiter_freq$shop_description == "Vista", ]$visit_counts,
             pct_gruen = visiter_freq[visiter_freq$shop_description == "Gr�ental", ]$visit_counts / sum(visiter_freq[visiter_freq$shop_description == "Gr�ental",]$visit_counts),
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
                      labels = c("F" = "Frauen", "M" = "M�nner"))+
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
    guides(fill= guide_legend(title = "Hochschulangeh�rigkeit"))+
    geom_text(aes(label = scales::percent(round(pct,2))), colour = "#000000", position = position_dodge(width = .6),  vjust=-0.25, size= 9) +
    mytheme

# save
ggsave("plots/visit_freq_member_181128_egel.pdf",
       height = 11,
       width = 24,
       dpi = 200,
       device = cairo_pdf)


# chapter 3.3: cluster analysis for nutritional patterns-----
df_ <- df_2017
df_$label_content <- str_replace(df_$label_content, "Fisch|Gefl�gel", "Fleisch")
df_ <- df_ %>% # watch with what for dataset you work!
    select(ccrs, label_content, gender, member, age) %>% # select variable of interest
    filter(member != "Spezialkarten") %>% # exclude spezialkarten
    dcast(formula = ccrs + gender + age + member ~ label_content, value.var="label_content", fun.aggregate= length) %>% # reshape into wide format and aggregate after occurencies of label content
    rename(Unknown = 'NA') %>% # rename NA to unknown
    mutate(tot_buy = rowSums(.[ ,-c(1:4)])) %>% # exclude ccrs and information of person for sum of rows
    mutate(meaty =(.$Fleisch)/.$tot_buy,
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
# nrow(buffet)
buffet <-  df_ %>%  
    filter(tot_buy > 5 & hnc == 1) %>% 
    mutate(cluster = "buffet") 


# never meat
# nrow(meat_avoiders)
no_meat <- df_ %>%
    filter(tot_buy > 5 & meaty == 0 & hnc == 0) %>% 
    mutate(cluster = "never meat")

# always meat
# nrow(alwy_meat)
alwy_meat <- df_ %>% 
    filter(tot_buy > 5 & meaty  == 1 & hnc == 0) %>% 
    mutate(cluster = "always meat")

# build groups of canteen visitors beacause of their meat consumption per week
# minus buffet eaters
# minus meat_avoiders
# minus alway_meat (to avoid dublicates)
canteen_visitors <- filter(df_, tot_buy > 5) %>% 
    anti_join(., buffet) %>%   # exclude buffet eaters
    anti_join(., no_meat) %>%  # exclude meat avoiders
    anti_join(., alwy_meat) # exclude always meat


# veg-flexitarians: less than one third of all buyings contain meat
# 246 nrow(meat_avoiders)/nrow(canteen_visitors) => 22.2%
veg_flex <- canteen_visitors %>% 
    filter(meaty <= 1/4) %>% 
    mutate(cluster = "veg-flexitarians")

# meat-flexitarians: less than the halt of all buyings contain meat
#  nrow(meat_flex)/nrow(canteen_visitors) => 22.7%
meat_flex <- canteen_visitors %>% 
    filter(meaty > 1/4 & meaty <= 1/2) %>% 
    mutate(cluster = "meat-flexitarians")

# meat-eaters: less than the halt of all buyings contain meat
#  nrow(meat_eat)/nrow(canteen_visitors) => 33.2%
meat_eat <- canteen_visitors %>% 
    filter(meaty > 1/2 & meaty <= 3/4) %>% 
    mutate(cluster = "meat-eaters")

# meat lovers
#  nrow(meat_lovers)/nrow(canteen_visitors) => 19.1%
meat_lovers <- canteen_visitors %>% 
    filter(meaty > 3/4) %>% 
    mutate(cluster = "meat lovers")


# concatenate all canteen visitors
canteen_visitors <- bind_rows(buffet, no_meat, veg_flex, meat_flex, meat_eat, meat_lovers, alwy_meat)

# concatenate all clusters to one
df_2 <- bind_rows(one, some, buffet, no_meat, veg_flex, meat_flex, meat_eat, meat_lovers, alwy_meat)


# plot all groups: treemap
# prepare data
library(treemap)
library(RColorBrewer) # color palettes

dat <- canteen_visitors %>% 
    group_by(cluster) %>% 
    summarise(count = n()) %>% 
    mutate(pct = count/sum(count)) %>% 
    ungroup() %>% 
    mutate(cluster2 = recode(cluster, 
                             # "one" = "one timers", "some" = "some timers",
                             "buffet" = "buffetarians",
                             "never meat" = "never meat", #if i want to change the label name, then the letters need to match the letters of the string in df_2$cluster otherwiese takes the original label
                             "veg-flexitarians" = "veg-flexitarians\n\u2264 1/4 Fleisch oder Fisch", 
                             "meat-flexitarians" = "meat-flexitarians\n> 1/4 bis \u2264 1/2 Fleisch oder Fisch",
                             "meat-eaters" = "meat-eaters\n> 1/2 bis \u2264 3/4 Fleisch oder Fisch", 
                             "meat lovers" = "meat lovers\n > 3/4 Fleisch oder Fisch",
                             "always meat" = "always meat")) %>% # rename clusters
    mutate(label = paste(cluster2, paste(round(pct*100, 0), "%", sep = " "), sep = "\n")) %>% 
    mutate(color = recode(cluster,
                          "buffet" = "#e64d00",
                          "never meat" = "#99f200",
                          "veg-flexitarians" = "#c5b87c", 
                          "meat-flexitarians" = "#fad60d",
                          "meat-eaters" = "#80ccff" , 
                          "meat lovers" = "#6619e6",
                          "always meat" = "#008099")) # add color


# get order in treeplot
dat$test <- parse_factor(dat$cluster2, levels = c("buffetarians", # "one timers", "some timers", 
                                                  "never meat", 
                                                  "veg-flexitarians\n\u2264 1/4 Fleisch oder Fisch",
                                                  "meat-flexitarians\n> 1/4 bis \u2264 1/2 Fleisch oder Fisch", 
                                                  "meat-eaters\n> 1/2 bis \u2264 3/4 Fleisch oder Fisch", 
                                                  "meat lovers\n > 3/4 Fleisch oder Fisch",
                                                  "always meat"))
dat$test2 <- as.numeric(dat$test) # get number of factor

# add two groups for different font sizes
# not working
dat$group <- ifelse(dat$test2 > 2 & dat$test2 < 7, "group", "subgroup")

# open pdf device for saving plot

cairo_pdf("Plots/treemap_cluster_190822_egel.pdf", width = 20, height = 20)

treemap(dat, #Your data frame object
        index="label",  #A list of your categorical variables
        vSize = "count",  #This is your quantitative variable
        # palette = pal2, #Select your color palette from the RColorBrewer presets or make your own.
        vColor = "color", # with color, the order isnt taken into acount anymore
        type="color",
        fontsize.title = 30, #Change the font size of the title
        title = "", # Change the name of the plot
        fontsize.labels = 32,
        algorithm = "pivotSize",
        sortID = "test2",
        fontfamily.labels = "sans")

dev.off() # close device

# plot all groups according to their purchase behavior
purchase <- right_join(df_2017, canteen_visitors[,c("ccrs", "cluster")], by = "ccrs") %>% 
    # filter(cluster != "one" & cluster!= "some") %>%
    group_by(., cluster) %>% 
    summarise(tot_sell = n()) %>% 
    mutate(pct_sellings = tot_sell / sum(tot_sell))

# information about clusters
cluster_dat <- canteen_visitors %>% 
    # filter(cluster != "one" & cluster!= "some") %>%
    group_by(cluster) %>% 
    summarise(count_cluster = n()) %>% 
    mutate(pct_cluster = count_cluster/sum(count_cluster)) %>% 
    ungroup()

#prepare data for plot
pl <- left_join(purchase, cluster_dat, by = "cluster") %>% 
    select(- count_cluster, -tot_sell) %>% 
    melt() # long format

# plot
p <- ggplot(pl, aes(y = value, x = parse_factor(cluster, levels = c("buffet","never meat", "veg-flexitarians", "meat-flexitarians", "meat-eaters", "meat lovers", "always meat")), 
               fill = parse_factor(variable, levels = c("pct_cluster","pct_sellings")))) + 
    geom_bar(stat = "identity", position = position_dodge(), width = .6) +
    scale_y_continuous(label = scales::percent) +
    scale_fill_manual(values = c("pct_cluster" = "#fad60d","pct_sellings" = "#c5b87c"),
                      breaks = c("pct_cluster", "pct_sellings"),
                      labels = c("Anteil Personen im Ern�hrungsmuster", "Anteil Men�-K�ufe")) +
    scale_x_discrete(labels = c("buffetarians","never meat", "veg-flexitarians", "meat-flexitarians", "meat-eaters", "meat lovers", "always meat")) +
    geom_text(aes(label = scales::percent(round(pl$value,2))), position = position_dodge(width = .4), vjust = -.25, size = 9) + #check_overlap = TRUE
    guides(fill = guide_legend("")) +
    xlab("Ern�hrungsmuster") +
    ylab("Anteil in Prozent") +
    mytheme

p <- labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")

# save
ggsave("plots/cluster_purchase_190131_egel.pdf",
       height = 13,
       width = 25,
       dpi = 600,
       device = cairo_pdf)

# plot all groups according to their visit frequency
# prepare data
visiter <- canteen_visitors %>% 
    mutate(category=cut(tot_buy, breaks = c(-Inf,2,12,24,36,48,60,Inf), labels=c("einmaliger Besuch", "max. 1x\n pro Woche","max. 2x\n pro Woche","max. 3x\n pro Woche","max. 4x\n pro Woche", "max. 5x\n pro Woche","mehr als 5x\n pro Woche"))) %>%
    group_by(cluster, category) %>% 
    summarise(visit_counts=n()) %>% 
    ungroup()#%>%  
    # filter(cluster != "one" & cluster != "some")

# calculate percentage of the visiter frequency - how many clusters went once to the canteen etc.
visiter2 <- visiter %>% 
    group_by(cluster, category) %>% 
    summarise(visit_counts = sum(visit_counts)) %>% 
    mutate(pct=visit_counts/sum(visit_counts)) %>% 
    ungroup()

# add annotation
text <- visiter %>% 
    mutate(cluster = parse_factor(cluster, levels = c("buffet", "never meat", 
                                                      "veg-flexitarians", "meat-flexitarians", 
                                                      "meat-eaters", "meat lovers", "always meat"))) %>% 
    group_by(cluster) %>% 
    summarise(tot = sum(visit_counts)) %>% 
    mutate(label = paste("italic(n)", tot, sep = "=="))

# add colors
# ColsPerCat2=c("buffet" = "#000000","meat avoiders" = "#c5b87c", "veg-flexitarians" = "#fad60d","meat-flexitarians" = "#e64d00", "meat-eaters" = "#6619e6","meat lovers" = "#008099", "always meat" = "#262626")
# ColsPerCat = c("buffet" = "#e64d00",
# "meat avoiders" = "#99f200",
# "veg-flexitarians" = "#c5b87c", 
# "meat-flexitarians" = "#fad60d",
# "meat-eaters" = "#80ccff" , 
# "meat lovers" = "#6619e6",
# "always meat" = "#008099")

ColsPerCat = c("max. 1x\n pro Woche" = "grey90",
"max. 2x\n pro Woche" = "#99f200",
"max. 3x\n pro Woche" = "#c5b87c",
"max. 4x\n pro Woche" = "#fad60d",
"max. 5x\n pro Woche" = "#80ccff")

visiter2$label_color <- as.factor(sapply(unlist(ColsPerCat)[visiter2$cluster], # takes every label and their belonged color
                                         function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

# plot
ggplot(visiter2, aes(x = parse_factor(cluster, levels = c("buffet", "never meat", "veg-flexitarians", "meat-flexitarians", "meat-eaters", "meat lovers", "always meat")), 
                     y = pct, fill = category, color = label_color)) +
    geom_bar(stat = "identity", colour = NA, position = position_stack(), width = .6) +
    scale_fill_manual(values =  ColsPerCat) + #,
    #                   labels = c("buffetarians", "never meat", "veg-flexitarians", "meat-flexitarians", "meat-eaters", "meat lovers", "always meat")) +
    scale_color_manual(values = levels(visiter2$label_color)) +
    guides(fill = guide_legend("Durchschnittliche Mensabesuche pro Woche\n"),
           color = F) +
    scale_y_continuous(labels=scales::percent) +
    xlab("\nErn�hrungsmuster") +
    ylab("Anteil Mensabesucher") +
    geom_text(aes(label = ifelse(visiter2$pct<.02,"", scales::percent(round(pct,2)))), position = position_stack(vjust = .5), size= 5) +
    annotate("text", x = 1:7, y = 1.05, label = text$label, parse = T, size = 7) +
    mytheme


# save
ggsave("plots/visit_freq_cluster_190525_egel.pdf",
       height = 10,
       width = 24,
       dpi = 200,
       device = cairo_pdf)

# chapter 3.3: describe clusters --------
# create loop with separate outputs (if time)

dfList <- list(one, some, buffet, no_meat, veg_flex, meat_flex, meat_eat, meat_lovers, alwy_meat)

for (i in 1:length(dfList)) {
    description <- filter(df_2, cluster == dfList[[i]]$cluster[1]) # filter first datasets
    a <- Hmisc::describe(description$gender)
    b <- psych::describe(description[description$age<100,]$age)
    c <- Hmisc::describe(description$member)
    d <- mean(description$tot_buy)
    e <- dfList[[i]]$cluster[1]
    f <- paste(nrow(description), "count of person in that cluster")
    g <- paste(nrow(description)/nrow(df_2) * 100, "percent of that cluster")
    h <- paste(nrow(filter(df_2017, ccrs %in% dfList[[i]]$ccrs))/nrow(df_2017)*100, "percent of all sellings")
    print(list(a,b,c,d,e,f,g,h))
}


# chapter 3.3: plot purchase behavior of the clusters-------

# create a list with all dataframes (alwy_meat makes no sense to plot)
dfList <- list(no_meat, veg_flex, meat_flex, meat_eat, meat_lovers, alwy_meat)

# loop though that list
for (i in 1:length(dfList)) {
    dat <- filter(df_2017, ccrs %in% dfList[[i]]$ccrs) # filter first datasets
    
    # new variable to check in which dataset we are
    dat$check <- paste(dfList[[i]]$cluster[1], format(Sys.Date(), "%Y_%m_%d"), "egel", sep = "_") # seach for a better name!!
    
    # prepare data
    pl <- dat %>% 
        group_by(label_content, gender, condit) %>% 
        summarise(tot_sold = n()) %>% 
        ungroup() %>% 
        group_by(gender, condit) %>%
        mutate(pct = tot_sold/sum(tot_sold)) %>% 
        ungroup() %>% 
        mutate(label_content = ifelse(is.na(label_content),"Unbekannt",label_content),
               gender = ifelse(is.na(gender),"Spezialkarten",gender),
               gender = recode(gender, "F" = "Frauen", "M" = "M�nner")) %>% 
        mutate(xlab = paste(gender, condit, sep = "\n"))
    text <- group_by(dat, gender, condit) %>% summarise(tot = n()) %>%
        mutate(label = "italic(n)") %>%
        mutate(label2 = paste(label, tot, sep="=="))
    
    # add xlab => do not take that into account (not clear enough)
    # pl <- dat %>% 
    #     # filter(!duplicated(ccrs)) %>% # not the right aproach!
    #     group_by(gender, condit, ccrs) %>% 
    #     summarise(tot_purchase = n()) %>% # summarise all purchases
    #     ungroup() %>% 
    #     group_by(gender, condit) %>% # summarise purchases accodrung gender and consit 
    #     summarise(tot_member = n()) %>% 
    #     ungroup() %>% 
    #     mutate(gender = ifelse(is.na(gender),"Spezialkarten",gender),
    #            gender = recode(gender, "F" = "Frauen", "M" = "M�nner")) %>% 
    #     inner_join(.,pl, by = c("gender", "condit")) %>% 
    #     ungroup() %>% 
    #     mutate(gender = recode(.$gender, "F" = "Frauen", "M" = "M�nner")) %>% 
    #     mutate(xlab0 = paste("(",tot_member,")"),
    #            xlab = paste(gender, xlab0, sep = "\n"),
    #            xlab = paste(xlab, condit, sep = "\n"))
    
    
    ## check if the background color is dark or not
    # my colors for the plot
    ColsPerCat=c("Unbekannt" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fisch"="#6619e6", "Fleisch" = "#fad60d", "Hot and Cold"="#4c4848")
    
    # detects dark color: for labelling the bars
    pl$label_color <- as.factor(sapply(unlist(ColsPerCat)[pl$label_content], 
                                       function(color) { if (isDark(color)) 'white' else 'black' })) 
    
    
    #plot
    p <- ggplot(pl, aes(y = pct,x = xlab, fill = factor(label_content, c("Unbekannt", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fisch","Fleisch", "Hot and Cold")), color = label_color)) + 
        geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
        xlab("") +
        ylab("\nVerkaufte Gerichte in Prozent")+
        guides(fill = guide_legend("Men�-Inhalt\n"),
               color = F)+
        scale_y_continuous(labels=scales::percent)+
        scale_fill_manual(values = ColsPerCat,
                          breaks = attributes(ColsPerCat)$name,
                          labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fisch", "Fleisch", "Hot & Cold (Buffet)"))+
        scale_color_manual(values = levels(pl$label_color))+
        geom_text(aes(label=ifelse(pct<0.02,"",scales::percent(round(pct,2)))), size = 7, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
        annotate( 
            "text",x = 1:4, y = 1.03, label = text$label2,parse=T, size=9) + 
        mytheme
    
    p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")
    
    ggsave(path = "plots/", 
           filename = paste(dat$check[1],".pdf", sep = ""),
           width = 18,
           height = 10,
           dpi = 600,
           device = cairo_pdf)
}

# chapter 3.3: some statistics--------
# create a list with all dataframes (alwy_meat makes no sense to plot)
dfList <- list(meat_avoiders, veg_flex, meat_flex, meat_eat, meat_lovers)

# loop though that list
for (i in 1:length(dfList)) {
    dat <- filter(df_2017, ccrs %in% dfList[[i]]$ccrs) # filter first datasets
    dat_ <- group_by(dat, condit, label_content) %>%
        summarise(tot_sold = n()) %>%  # summarise all sellings per meal content per condition
        mutate(pct = tot_sold / sum(tot_sold))
    dat_$check <- dfList[[i]]$cluster[1]
    dfList_[[i]] <- dat_ # append to list (do a dataframe directly => how is this possible)
    # aggregate(pl$pct ~ label_content, FUN = mean)
}

big_data <- do.call(rbind, dfList_) # concat all dataframes from list together

# calculate differences between basis and intervention in meat consumption
nameList <- unique(big_data$check)
for (i in nameList) {
    dat <- drop_na(big_data[big_data$check == i, ]) # subsetting => drop NA's otherwiese problems
    base_meat <- dat[dat$label_content == "Fisch" & dat$condit == "Basis", ]$pct +
        dat[dat$label_content == "Fleisch" & dat$condit == "Basis", ]$pct # calculate meat per basisweeks
    int_meat <- dat[dat$label_content == "Fisch" & dat$condit == "Intervention", ]$pct +
        dat[dat$label_content == "Fleisch" & dat$condit == "Intervention", ]$pct #calculate meat per intervention weeks
    diff_ <- base_meat - int_meat # difference
    print(paste(round(diff_*100,2), "percent", i))
}


# chapter 3.4: cluster analyses only for people with same buyings in both conditions--------
# exclude first people who eat less than 5 times in the canteen
df_1 <- df_2017
df_1$label_content <- str_replace(df_1$label_content, "Gefl�gel", "Fleisch")
df_ <- df_1 %>% 
    select(ccrs, label_content, gender, member, age) %>% # select variable of interest
    dcast(formula = ccrs + gender + age + member ~ label_content, value.var="label_content", fun.aggregate= length) %>% # reshape into wide format and aggregate after occurencies of label content
    rename(Unknown = 'NA') %>% # rename NA to unknown
    mutate(tot_buy = rowSums(.[,-c(1:4)])) %>% # exclude ccrs and information of person for sum of rows
    mutate(meaty =.$Fleisch/.$tot_buy,
           hnc = .$`Hot and Cold` / .$tot_buy) %>% 
    filter(tot_buy > 5) # all canteen visitors => 1048

# check in dataset for condition
df_2 <- df_1 %>%
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
pl <- df_1 %>% 
    filter(ccrs %in% gr3$ccrs) %>% 
    group_by(week, condit, label_content) %>% # pay attention to the order of grouping variables!!
    summarise(tot_buy = n()) %>% 
    mutate(pct = tot_buy/sum(tot_buy))


# add card numbers per week (shortcut to run the code wont work, why?)
pl1 <- df_1 %>% 
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
txt <- df_1 %>% 
    filter(ccrs %in% gr3$ccrs) %>% 
    group_by(week) %>% 
    summarise(week_sell = n()) %>%
    mutate(label = paste("italic(n)",week_sell, sep = "=="))


## check if the background color is dark or not
# my colors for the plot
ColsPerCat=c("Unbekannt" = "black", "Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fisch" = "#6619e6", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
pl1$label_color <- as.factor(sapply(unlist(ColsPerCat)[pl1$label_content], 
                                    function(color) { if (isDark(color)) 'white' else 'black' })) 


# plot
p <- ggplot(pl1, aes(y = pct,x = factor(xlab), fill = factor(label_content, c("Unbekannt", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    ylab("\nVerkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("Men�-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fisch", "Fleisch", "Hot & Cold (Buffet)"))+
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
                         labels=c("16 bis 25-j�hrig","26 bis 34-j�hrig","35 bis 49-j�hrig","50 bis 64-j�hrig","keine Angaben"))

dat_17$age_groups <- relevel(dat_17$age_groups, ref = "26 bis 34-j�hrig") # 26 bis 34-j�hrig as reference group

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
# are random effect taken into accountmen�
se <- sqrt(diag(vcov(mod3)))

tab1 <- cbind(Est = fixef(mod3), LL = fixef(mod3) - 1.96 * se, UL = fixef(mod3) + 1.96 *
                  se)

# erzeugt die Odds Ratios
or <- exp(tab1)


# probabilites of logits (see function above)
source("11_function_logit2probit_190114_egel.R")
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
                       labels = c("M�nner", "Frauen"))+
    guides(shape = guide_legend(title = "Geschlecht\n"), linetype = F)+
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz (2017)")

# save
ggsave("plots/gender_condition_HS17.pdf",
       height = 10,
       width = 14,
       dpi = 600,
       device = cairo_pdf)
