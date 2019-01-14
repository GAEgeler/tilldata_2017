# R-Skript for brief report of selected results on meal sellings
# Status: 10.01.2019


# required packages
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)

# to load data 
source(file = "04_load_data_180802_egel.R")
# to load themes
source(file = "08_theme_plots_180419_egel.R")
# warnings can be ignored


# chapter 3.1: plot sellings hs 2017 overall------

#prepare data for plot: aggregated data => locals are included
df_ <- group_by(df_agg, condit ,week, label_content )%>% summarise(tot_sold=n())

df_ <- df_ %>% 
    group_by(week,condit) %>% # give in variable, you want to calculate percentage
    mutate(pct=(tot_sold/sum(tot_sold)))

# ranem NA to unknown
df_$label_content <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

# annotation for selling per week
text <- group_by(df_agg, week) %>% summarise(tot = n()) %>%
    mutate(label = "italic(n)") %>%
    mutate(label2 = paste(label, tot, sep="=="))

# define x-lab for plot
df_$xlab <- paste(df_$week, df_$condit, sep = "\n")


## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unknown" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")
 
# detects dark color: for labelling the bars
isDark <- function(color) {
    (sum(grDevices::col2rgb(color) *c(299, 587,114))/1000 < 123)
}

df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"


# barplot
p <- ggplot(df_, aes(y = pct,x = as.factor(xlab), fill = factor(label_content, c("Unknown", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("Herbstsemesterwochen (Kalenderwochen 40 bis 51)") +
    # xlab(cat('"winter semester weeks (Basis: "','meat','" week, Intervention: "','vegetarian','" week"'))+
    ylab("\nVerkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("Men?-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot and Cold (Buffet)"))+
    scale_color_manual(values = levels(df_$label_color))+
    geom_text(aes(label=ifelse(pct*100>1.5,paste0(round(pct*100, digits=0),"%"),"")),size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:12, y = 1.03, label = text$label2,parse=T, size=9) + # why so big differences to the first version
    mytheme # see skript 08_theme_plots
    
p + labs(caption = "Daten: Kassendaten SV Schweiz (2017)")

ggsave("plots/selling_HS_17_egel.pdf",p,
       width = 26,
       height = 15,
       dpi = 600,
       device = cairo_pdf)


# chapter 3.1: test if week sellings statistically differ -----

sell_dat <- df_agg %>%
    mutate(day=wday(date, label = T)) %>% 
    group_by(semwk, condit) %>%
    summarise(tot_sold=n())


boxplot(sell_dat$tot_sold ~ sell_dat$condit) # check varianze homogenity
ao1 <- (aov(sell_dat$tot_sold ~ sell_dat$condit)) 
autoplot(ao1)
t.test(sell_dat$tot_sold ~ sell_dat$condit) # same as aov
summary.lm(ao1)

# test if meal content differ statistically
sell_dat <- df_agg %>%
    mutate(label_content2 = str_replace_all(.$label_content, "Pflanzlich\\+", "Vegan")) %>% # change name 
    # mutate(label_content2 = str_replace_all(.$label_content, "Pflanzlich\\+", "Vegan_plus")) %>% # to check if the sellings in the intervention week differs between vegan and vegan plus
    mutate(label_content2 = str_replace(.$label_content2, "Pflanzlich", "Vegan")) %>% # change name
    group_by(semwk, condit, label_content2) %>%
    summarise(tot_sold=n()) %>%
    na.omit()  # omit NA, another better way is drop_na()
    #filter(!label_content2 == "Hot and Cold")

boxplot(sell_dat$tot_sold ~ sell_dat$label_content2 * sell_dat$condit) # there is some variance homogenity, however anovas are very robust
ao2 <- aov(sell_dat$tot_sold ~ sell_dat$label_content2 * sell_dat$condit) # anova only with interaction between label_content and condit (tried with log10, however dont look good at al on the model diagnostics)
library(ggfortify)
autoplot(ao2) # dont look very nice, however i would accept it :)
summary.lm(ao2) # display anova
TukeyHSD(ao2) #post-hoc test
# otherwise Welch test would be appropriate as well (one.test())



# chapter 3.2: test difference in meal options ----
# show aggregated total meal offer (N = 480 or 596 without NA's)
# exclude hot and cold
t0 <- filter(info_orig, !grepl("Hot and Cold", info_orig$label_content)) %>% group_by(label_content, condit) %>% summarise(tot = n()) %>% ungroup() %>% group_by(condit) %>% mutate(pct = tot/sum(tot))  

# show aggregated offer without locals (N = 360)
p0 <- filter(info_orig, !grepl("Local ", info_orig$article_description) & !grepl("Hot and Cold", info_orig$label_content)) %>% group_by(label_content, condit) %>% summarise(tot = n()) %>% ungroup() %>% group_by(condit) %>% mutate(pct = tot/sum(tot))

# chi-square to compare if the two meals offer (total meal and planed meal offer) differs
# drop unknown meal offers for chi_square test
t <- drop_na(t0) %>% group_by(label_content) %>% summarise(tot = sum(tot)) %>% mutate(pct = tot/sum(tot)) 
p <- group_by(p0, label_content) %>% summarise(tot = sum(tot)) %>% mutate(pct = tot/sum(tot))

tp <- tibble(label = unique(t$label_content), t$tot, p$tot)

# chisquare not significant
chi_s <- chisq.test(t$tot, p = p$pct) # test against expected probability (= which is the planed meal offer)
fisher.test(tp[ ,2:3]) # exact p-value via fisher.test

# chi-square to compare if the two meals offer differ accodring condition 
# start code from the start again
tp0 <- drop_na(t0) %>% select(-pct) %>% bind_cols(., p0[c("tot")]) %>% rename( real_offer = tot, planed_offer = tot1)

# basis
tp1 <- filter(tp0, condit == "Basis") %>% mutate(pct_exp = planed_offer / sum(planed_offer), 
                                               pct_real = real_offer / sum(real_offer))

chi_s2 <- chisq.test(tp1$real_offer, p = tp1$pct_exp) # check if expected frequencies are over 5
fisher.test(tp1[ ,3:4]) # for exact p-value, it differs strongly form the p-value from chi-square => is also another test with two factors in comparison of the chi-square with only one factor!

#interventionswochen
tp1 <- filter(tp0, condit == "Intervention") %>% mutate(pct_exp = planed_offer / sum(planed_offer), pct_real = real_offer / sum(real_offer))
chi_s3 <- chisq.test(tp1$real_offer, p = tp1$pct_exp) # check if expected frequencies are over 5
fisher.test(tp1[ ,3:4]) # for exact p-value, it differs strongly form the p-value from 




# chapter 3.2: count cases, where our design was not taken into account--------
# calculate daily meal offers (in counts and pecentage)
# for basis
df_angebot_b <- info_orig %>%
    mutate(label_content = str_replace_all(.$label_content, "Pflanzlich\\+", "Vegan")) %>% # change name 
    mutate(label_content = str_replace(.$label_content, "Pflanzlich", "Vegan")) %>%
    filter(condit == "Basis" & !grepl("Hot and Cold",info_orig$label_content)) %>%
    dcast(formula = date+shop_description ~ label_content, value.var = "label_content", fun.aggregate = length) %>%
    rename(unbekannt = `NA`) %>% 
    mutate(fleisch_pct = .$Fleisch / rowSums(df_angebot_b[, c(3:6)]),
           vegetarisch_pct = .$Vegetarisch / rowSums(df_angebot_b[, c(3:6)]),
           vegan_pct = .$Vegan / rowSums(df_angebot_b[, c(3:6)]))

# for intervention
df_angebot_i <- info_orig %>%
    mutate(label_content = str_replace_all(.$label_content, "Pflanzlich\\+", "Vegan")) %>% # change name 
    mutate(label_content = str_replace(.$label_content, "Pflanzlich", "Vegan")) %>%
    filter(condit == "Intervention" & !grepl("Hot and Cold",info_orig$label_content)) %>%
    dcast(formula = date+shop_description ~ label_content, value.var = "label_content", fun.aggregate = length) %>%
    mutate(fleisch_pct = .$Fleisch / rowSums(.[, c(3:5)]),
           vegetarisch_pct = .$Vegetarisch / rowSums(.[, c(3:5)]),
           vegan_pct = .$Vegan / rowSums(.[, c(3:5)]))

# count for deviation
# basis, plan was 2/3 meat and 1/3 vegetarian
# df_angebot_b$design <- ifelse(df_angebot_b$fleisch_pct == 2/3 & rowSums(df_angebot_b[ ,8:9]) == 1/3, T, ifelse())
# table(df_angebot_b$design)
df_angebot_b$design <- ifelse(df_angebot_b$fleisch_pct == 2/3 & rowSums(df_angebot_b[ ,8:9]) == 1/3, "exp_design", 
                              ifelse(df_angebot_b$fleisch_pct == 1/2 & rowSums(df_angebot_b[ ,8:9]) == 1/2, "equal",
                                ifelse(df_angebot_b$fleisch_pct > 2/3, "more_meat","less_meat")))

# check for differences between canteens                            
table(df_angebot_b$design)
library(gmodels)
CrossTable(df_angebot_b$design, df_angebot_b$shop_description)


# intervention: plan was 1/3 meat and 1/3 vegetarian and 1/3 vegan
df_angebot_i$design <- ifelse(df_angebot_i$fleisch_pct == 1/3 & df_angebot_i$vegetarisch_pct == 1/3 & df_angebot_i$vegan_pct == 1/3, "exp_design",
                              ifelse(df_angebot_i$fleisch_pct < 1/3 & rowSums(df_angebot_i[ ,7:8]) > 2/3,"less_meat",
                                     ifelse(df_angebot_i$fleisch_pct == 1/2 & rowSums(df_angebot_i[ ,7:8]) == 1/2, "equal","more_meat")))
table(df_angebot_i$design)
CrossTable(df_angebot_i$design, df_angebot_i$shop_description)


# chapter 3.2: plot real and planed offer ----------
# prepare data
# target for 2015 and 2016 and 2017 for both canteens (per canteen 120 meals) and for both cycles: 480 melas in total
# per canten and cycle: meat:54 (30 + 30*.8), vegetarian:36, hot and cold: 30
df_plan_56 <- tibble(year = rep(2015, times=360, each=1),
                     label_content = rep(c("Fleisch","Vegetarisch"), times=c((120 + 120*.8),(120 + 120*.2))),
                     offer = "Geplant")

# plan for intervention:: per canten and cycle: meat: 45, vegetarian: 30, hot and cold: 30, vegan: 7, vegan+: 8
# plan for basis:: per canteen and cycle: meat: 60, vegetarian: 30, hot and cold: 30
df_plan_7 <- tibble(year = rep(2017, times=360),
                    label_content = rep(c("Fleisch","Vegetarisch","Pflanzlich","Pflanzlich+"), times=c(180,120,28,32)),
                    offer= "Geplant")

# Actual: Locals are included
# use documentation for that (not selling data)
# exclude hot and cold (only using filter() is deleting missing cases!)

source("05_load_add_data_181001_egel.R")
df_actual_7 <- info_orig %>% filter(!grepl("Hot and Cold", info_orig$label_content)) %>% mutate(offer = "Angeboten", year = year(date)) %>% dplyr::select(year,label_content,offer)
    
# Merge data frames
df_t= bind_rows(df_plan_56, df_plan_7, df_actual_7)

# Group data frame
df_ <- df_t %>%
    group_by(year,label_content, offer) %>%
    summarise(tot=n()) %>%
    ungroup() # otherwise dataframe is still grouped

# define some variables
df_$xlab <- paste(df_$year,df_$offer, sep = "\n")
df_$xlab <- str_replace(df_$xlab,"2015\nGeplant","2015 - 2016\nGeplant")
df_$label_content <- ifelse(is.na(df_$label_content),"Unbekannt",df_$label_content)

# define text for annotation
text <- group_by(df_, year, offer) %>%
    summarise(tot=sum(tot)) %>%
    mutate(label = "n") %>%
    mutate(label2=paste(label,tot,sep=" = "))

## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unbekannt" = "black","Pflanzlich"="grey90", "Pflanzlich+"="#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                     function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"


#plot in de
p <- ggplot(df_, aes(y=tot,x=xlab, fill=factor(label_content,levels=c("Unbekannt","Pflanzlich","Pflanzlich+","Vegetarisch","Fleisch","Hot and Cold")), color=label_color)) +
    geom_bar(stat="identity", position = "stack", color=NA, width = .5) +
    xlab("") + #"\nGeplante und angebotene Men?-Optionen w?hrend Herbstsemester (Kalenderwochen 40 bis 51)"
    ylab("Geplante und angebotene Men?-Optionen")+ # title: 
    #     ggtitle("note: locals are not included
    #          selling time between 9 a.m. until 3 p.m.
    #          locals meals in total: 2602")+
    guides(fill= guide_legend("Men?-Inhalt"),
           color=F)+
    # scale_y_continuous(labels = scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)","Vegan (authentisch)","Ovo-lakto-vegetarisch","Fleisch oder Fisch", "Hot and Cold"))+
    scale_color_manual(values = levels(df_$label_color))+
    scale_x_discrete(limits=c("2015 - 2016\nGeplant","2017\nGeplant","2017\nAngeboten"))+
    geom_text(aes(label=ifelse(tot>4,tot, "")), size = 8, position = position_stack(vjust = 0.5))+ 
    annotate("text",x=1:3,y=500,label=c(text$label2[1],text$label2[1],text$label2[2]), size=8)+
    mytheme # see skript 08_theme_plots

p + labs(caption = "Daten: ZHAW (2017)")


ggsave("plots/meal_offer_HS15_17.pdf",
       height = 10,
       width = 14,
       dpi = 600,
       device = cairo_pdf)



# chapter 3.3: test if meal content and condition has influence on sellings --------------
# calculate anova with interaction

df <- df_agg %>% group_by(condit, week, label_content) %>%
    summarise(tot = n())

aov1 <- aov(tot ~ condit*label_content, data = df)
summary(aov1)

autoplot(aov1) # dont see that bad

TukeyHSD(aov1, ordered = T)


# chapter 3.3: plot interaction condition and meal content----------
# visualize findings from above
# see line 128 for sell_dat
m_sell <- na.omit(sell_dat) %>% group_by(condit,label_content2) %>% summarise(val = mean(tot_sold)) # calculate means (per what) of the label_content per condition

p <- ggplot(sell_dat, aes(x = condit, y = tot_sold, linetype = label_content2, shape = factor(label_content2, levels = ("Fleisch", "Vegetarisch", "Vegan", "Hot and Cold")))) + 
    geom_point(data = m_sell, aes(y = val), size = 4) +
    geom_line(data = m_sell, aes(y = val, group = label_content2), size = 2) + 
    labs(y = "Durchschnittlich verkaufte Gerichte pro Woche", x = "Experimentbedingungen") + 
    scale_y_continuous(breaks = seq(0,1400,200), limits = c(0, 1400)) +
    scale_x_discrete(label = c("Basiswochen", "Interventionswochen"))+
    scale_shape_manual(values = c("Fleisch" = 15, "Hot and Cold" = 17,"Vegan" = 3, "Vegetarisch" = 19), # change order of shape labels
                        breaks = c("Fleisch", "Vegetarisch", "Vegan", "Hot and Cold"),
                        labels = c("Fleisch oder Fisch", "Ovo-lakto-vegetarisch", "Vegan", "Hot & Cold (Buffet)"))+
    guides(shape = guide_legend(title = "Men?-Inhalt\n"), linetype = F)+
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz (2017)")


ggsave("plots/content_condition_HS17.pdf",
       height = 10,
       width = 14,
       dpi = 600,
       device = cairo_pdf)





# chapter 3.4: test meal line differences between years-----------

df <- menu_tot 

# some changes in label names
df$article_description[grep("Local+",df$article_description)] <- "Local" # summarize all locals
df$article_description[grep("Green",df$article_description)] <- "Green/World"
df$article_description[grep("World",df$article_description)] <- "Green/World"

# excluce locals
df <- df[!grepl("Local", df$article_description), ]

# see variances, dont look that different
boxplot(df$tot_sold ~ as.factor(df$year) * df$article_description)

# model
aov2 <- aov(df$tot_sold ~ as.factor(df$year) * df$article_description)
autoplot(aov2) # see model diagnostics
summary.lm(aov2)
TukeyHSD(aov2, ordered = T)


# chapter 3.4: plot meal line sellings HS15 to HS17-----------
# prepare data
df <- menu_tot %>%
    group_by(article_description, year) %>%
    summarise(tot_sold = sum(tot_sold))

# change first some strings
df$article_description[grep("Local+",df$article_description)] <- "Local" # summarize all locals
df$article_description[grep("Green",df$article_description)] <- "Green/World"
df$article_description[grep("World",df$article_description)] <- "Green/World"

# aggregate again because of locale

df <- df %>% group_by(article_description, year) %>%
    summarise(tot_sold = sum(tot_sold))

df <- df %>% # calculate percentage
    group_by(year) %>% mutate(pct = tot_sold/sum(tot_sold))


## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Local" = "black", "Kitchen" = "#008099", "Green/World" = "#fad60d","Favorite"="#c5b87c","Hot and Cold"="#4c4848")

df$label_color <- as.factor(sapply(unlist(ColsPerCat)[df$article_description], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

# define annotation
text <- group_by(df, year) %>% summarise(tot=sum(tot_sold)) %>%
    mutate(tot2=format(tot, big.mark = "'", scientific = F)) %>% # add thousand seperator
    mutate(label = "n") %>%
    mutate(label2=paste(label,tot2,sep=" = "))

# plot data
p <- ggplot(df, aes(y=tot_sold,x=as.factor(year), fill=factor(article_description,levels=c("Local","Kitchen","Green/World","Favorite","Hot and Cold")), color = label_color)) +
    geom_bar(stat="identity", position = "stack", color=NA, width = .6) +
    #ggtitle("Verkaufte Men?s: 3. + 4. HSW\n") +
    xlab("Herbstsemester (Kalenderwochen 40 bis 51)") +
    ylab("Verkaufte Gerichte pro Herbstsemester")+
    guides(fill= guide_legend(title = "Men?-Linien"), 
           color=F)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$names,
                      labels = c("Local/Zusatzangebot","Kitchen","Green/World","Favorite","Hot & Cold (Buffet)"))+
    scale_color_manual(values = levels(df$label_color))+
    geom_text(aes(label=ifelse(pct*100>2,paste0(round(pct*100, digits=0),"%"),"")), size = 6, position = position_stack(vjust = 0.5))+ # omit 1% annotation
    annotate("text",x = 1:3, y = 27500, label = text$label2, size=6)+
    mytheme # see skript 08_theme_plots

p + labs(caption = "Daten: Kassendaten SV Schweiz (2017)")


ggsave("plots/meal_line_hs15_17.pdf",
       height = 10,
       width = 14, 
       dpi = 600, 
       device = cairo_pdf)


# chapter 3.4: plot meal lines sellings HS17 over weeks --------
# prepare data
meal <- df_agg
meal$article_description <- gsub("Local ","", meal$article_description)
meal <- meal %>% 
    group_by(condit, week, article_description) %>% 
    summarise(tot_sold = n())

df_ <- meal %>% 
    group_by(week,condit) %>% # give in variable, you want to calculate percentage
    mutate(pct=(tot_sold/sum(tot_sold)))

# annotation for selling per week
text <- group_by(df_agg, week) %>% summarise(tot = n()) %>%
    mutate(label = "italic(n)") %>%
    mutate(label2 = paste(label, tot, sep="=="))

# define x-lab for plot
df_$xlab <- paste(df_$week, df_$condit, sep = "\n")


## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Local/Zusatzangebot" = "black", "Kitchen" = "#008099", "World" = "#fad60d","Favorite"="#c5b87c","Hot and Cold"="#4c4848")
# detects dark color: for labelling the bars
df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$article_description], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"


# barplot
p <- ggplot(df_, aes(y = pct,x = as.factor(xlab), fill = factor(article_description, levels=c("Local/Zusatzangebot","Kitchen","World","Favorite","Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("Herbstsemesterwochen (Kalenderwochen 40 bis 51)") +
    # xlab(cat('"winter semester weeks (Basis: "','meat','" week, Intervention: "','vegetarian','" week"'))+
    ylab("\nVerkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("Men?-Linie\n"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Local/Zusatzangebot","Kitchen","World","Favorite","Hot & Cold (Buffet)"))+
    scale_color_manual(values = levels(df_$label_color))+
    geom_text(aes(label=ifelse(pct*100>1.5,paste0(round(pct*100, digits=0),"%"),"")),size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:12, y = 1.03, label = text$label2,parse=T, size=9) + # why so big differences to the first version
    mytheme # see skript 08_theme_plots

p + labs(caption = "Daten: Kassendaten SV Schweiz (2017)")

ggsave("plots/meal_line_single_HS_17_egel.pdf",p,
       width = 26,
       height = 15,
       dpi = 600,
       device = cairo_pdf)


