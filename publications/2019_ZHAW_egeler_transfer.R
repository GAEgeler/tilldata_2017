# analyses for tranfer magazine

# state: january 2019
# author: gian-andrea egeler


# load data df_agg-----
source("04_1_load_data_190128_egel.R")
source("08_theme_plots_180419_egel.R")
# detects dark color: for labelling the bars
source("09_function_is_dark_190114_egel.R")

# group data
t <- df_agg %>% 
    filter(label_content != "Hot and Cold" & !grepl("Local ", df_agg$article_description)) %>% 
    group_by(meal_name, week, article_description, condit, label_content, tot_ubp, tot_gwp) %>% 
    summarise(tot_sold = n()) %>% 
    mutate(imp_gwp = tot_sold * tot_gwp, # calculate total gwp according sellings e.g. one meal was sold 100 times (each meal contributes 1.4 gwp => total gwp is 140)
           imp_ubp = tot_sold * tot_ubp)

sum(t$tot_sold)/nrow(df_agg)*100 # percent meals with ubp points

# some information about hot and cold and local meals
#locals = 2600 (10%) => nrow(local)/nrow(df_agg)
# basis: 1222
# intervention: 1378

local <- df_agg %>% 
    filter(grepl("Local ", df_agg$article_description)) %>%  
    group_by(condit, week) %>% 
    summarise(tot = n())

summary.lm(aov(local$tot ~ local$condit))

#hot and cold = 3700 (14%) nrow(hnc)/nrow(df_agg)
# basis: 1892
# intervention: 1808
hnc <- df_agg %>% filter(label_content == "Hot and Cold") %>% 
     group_by(condit, week) %>% 
     summarise(tot = n())

summary.lm(aov(hnc$tot ~ hnc$condit))

# environmental impact for whole experiment-------
# plot gwp total----
df_ <- group_by(t, condit ,week, label_content )%>% summarise(imp_gwp=sum(imp_gwp))

df_ <- df_ %>% 
    group_by(week,condit) %>% # give in variable, you want to calculate percentage
    mutate(pct_gwp=(imp_gwp/sum(imp_gwp)))

# describe content and condition
aggregate(df_$pct_gwp ~df_$condit+df_$label_content, FUN = mean)


# annotation for selling per week
text <- group_by(t, week) %>% summarise(tot = sum(tot_sold)) %>%
    # mutate(label = "italic(CO[2]*eq)") %>%
    mutate(label2 = paste(label, round(tot,0), sep="=="))

# define x-lab for plot
df_$xlab <- paste(df_$week, df_$condit, sep = "\n")

# define date of creation
st <- strftime(today(), format = "%B %Y")


## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Fisch"="#4c4848")


df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

# barplot
p <- ggplot(df_, aes(y = imp_gwp,x = as.factor(xlab), fill = factor(label_content, c("Unknown", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Fisch")), color = label_color)) + 
    geom_bar(stat = "identity", position = "stack", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("Herbstsemesterwochen (Kalenderwochen 40 bis 51)") +
    ylab(expression(paste("\n Global warming potential GWP (kg ", "CO"[2],"eq")))+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch", "Fisch"))+
    scale_color_manual(values = levels(df_$label_color))+
    geom_text(aes(label=ifelse(pct_gwp*100>1.5,paste0(round(pct_gwp*100, digits=0),"%"),"")),size = 10, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    # annotate( 
    #     "text",x = 1:12, y = 1.03, label = text$label2,parse=T, size=6) + # why so big differences to the first version
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz (2017); Karen Muir (2017)", 
         subtitles = "Von 26'000 Transaktionen konnten für 76 Prozent (20'000) die Treibhausgase berechnet werden.\n Hot & Cold (Buffet) und Locals sind hier nicht berücksichtigt worden (Stand: Dezember 2018).")

# save plot as png and pdf
ggsave("plots/intervention_basis_gwp_181211_egel.pdf",
       width = 26, 
       height = 14, 
       dpi = 600, 
       device = cairo_pdf)

# plot ubp total----
df_ <- group_by(t, condit ,week, label_content )%>% summarise(imp_ubp=sum(imp_ubp))

df_ <- df_ %>% 
    group_by(week, condit) %>% # give in variable, you want to calculate percentage
    mutate(pct_ubp = (imp_ubp/sum(imp_ubp)))

# describe content and condition
aggregate(df_$pct_gwp ~df_$condit+df_$label_content, FUN = mean)

# annotation for selling per week
text <- group_by(t, week) %>% summarise(tot = sum(imp_ubp)) %>%
    mutate(label = "italic(UBP)") %>%
    mutate(label2 = paste(label, format(tot, digits = 2, scientific = F), sep="=="))

# define x-lab for plot
df_$xlab <- paste(df_$week, df_$condit, sep = "\n")

# define date of creation
st <- strftime(today(), format = "%B %Y")

## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fisch"="#6619e6", "Fleisch" = "#fad60d")

# detects dark color: for labelling the bars
df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

# barplot
p <- ggplot(df_, aes(y = imp_ubp,x = as.factor(xlab), fill = factor(label_content, c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fisch","Fleisch")), color = label_color)) + 
    geom_bar(stat = "identity", position = "stack", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("Herbstsemesterwochen (Kalenderwochen 40 bis 51)\n") +
    ylab("\n Umweltbelastungspunkte (UBP)")+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels = function(x){format(x,big.mark = "'", scientific = F)})+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fisch","Fleisch")) +
    scale_color_manual(values = levels(df_$label_color))+
    geom_text(aes(label=ifelse(df_$pct_ubp * 100 > 1.5, paste0(round(df_$pct_ubp * 100, digits = 0),"%"),"")),size = 10, position = position_stack(vjust = 0.5)) + # omit 0% with ifelse()
    # annotate( 
    #     "text",x = 1:12, y = 1.03, label = text$label2,parse=T, size=6) + # why so big differences to the first version
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz (2017)\nBerechnungen: Karen Muir (2017)") 
         # subtitles = "Von 26'000 Transaktionen konnten für 76 Prozent (20'000) die Umweltbelastungspunkte berechnet werden.\n Hot & Cold (Buffet) und Locals sind hier nicht berücksichtigt worden (Stand: Dezember 2018).")

# save plot as png and pdf
ggsave("plots/intervention_basis_ubp_181211_egel.pdf",
       width = 26, 
       height = 14, 
       dpi = 600, 
       device = cairo_pdf)


# plot ubp and gwp per condition according meal content----
# sort and aggregate data
df <- df_agg %>% 
    filter(label_content != "Hot and Cold" & !grepl("Local ", df_agg$article_description)) %>% 
    group_by(condit, label_content, tot_ubp, tot_gwp) %>% 
    summarise(tot_sold = n()) %>%
    mutate(total_ubp = tot_ubp * tot_sold,
           total_gwp = tot_gwp * tot_sold) %>% 
    ungroup() 

# add percent   
df_ <- df %>% 
    group_by(condit, label_content) %>% 
    summarise(total_ubp = sum(total_ubp),
              total_gwp = sum(total_gwp)) %>%
    mutate(pct_ubp = total_ubp/sum(total_ubp),
           pct_gwp = total_gwp/sum(total_gwp))

pl <- df_agg %>% 
    group_by(condit) %>% 
    summarise(mean_g = round(mean(tot_ubp, na.rm = T))) %>%  # calculate mean per meal per condition (without meal sellings!!)
    left_join(df_,., by = "condit") %>% # join it back with data from above      
    mutate(xlab_ = paste("(", "durchn. Gericht", mean_g, "UBP", ")", sep = " "), # there are still some spaces between paranteses
           xlab = paste(condit, xlab_, sep = "\n"))


# double check
b <- round(mean(df_agg[df_agg$condit == "Basis",]$tot_ubp, na.rm = T))
i <- round(mean(df[df_agg$condit == "Intervention",]$tot_ubp, na.rm = T))

# annotate
txt <- group_by(df_, condit) %>% summarise(tot_ubp = sum(total_ubp),
                                           tot_gwp = sum(total_gwp)) %>% 
    mutate(txt_ubp = paste(format(tot_ubp, scientific = T, digits = 3), "UBP"),
           txt_gwp = paste(round(tot_gwp, 0)), "CO2-eq")

# define date of creation
st <- strftime(today(), format = "%B %Y")

## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
# take ColsperCat from above

# detects dark color: for labelling the bars
pl$label_color <- as.factor(sapply(unlist(ColsPerCat)[pl$label_content], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

# barplot ubp => vertical--------
p <- ggplot(pl, aes(y = total_ubp, x = xlab, fill = factor(label_content, c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fisch","Fleisch")), color = label_color)) + 
    geom_bar(stat = "identity", position = "stack", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    ylab("\n Umweltbelastungspunkte (UBP)")+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels = function(x){format(x,big.mark = "'", scientific = F)})+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fisch", "Fleisch")) +
    scale_color_manual(values = levels(pl$label_color))+
    geom_text(aes(label=ifelse(df_$pct_ubp * 100 > 1.5, paste0(round(df_$pct_ubp * 100, digits = 0),"%"),"")),size = 10, position = position_stack(vjust = 0.5)) + # omit 0% with ifelse()
    annotate( 
         "text",x = 1:2, y = 45000000, label = txt$txt_ubp, size=9) + # attention thousand separator in text!
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz (2017)\n Berechnungen: Karen Muir (2017)")# , 
         # subtitles = paste("Ein Gericht stiess in den Basiswochen durchnittlich ", b,"UBP",
                                      # "und in den Interventionswochen ", i,"UBP aus"))

# referenzwert 20 Millionen UBP pro Jahr und Person

#save
ggsave("plots/ubp_condit_vertical_190130_egel.pdf",
       height = 12,
       width = 16,
       dpi = 300,
       device = cairo_pdf)

# barplot ubp => horizontal------
p <- ggplot(pl, aes(y = total_ubp, x = factor(xlab, levels = c("Intervention\n( durchn. Gericht 4037 UBP )","Basis\n( durchn. Gericht 4287 UBP )")), fill = factor(label_content, c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fisch","Fleisch")), color = label_color)) + 
    geom_bar(stat = "identity", position = "stack", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    ylab("\n Umweltbelastungspunkte (UBP)")+
    guides(fill = guide_legend("Kategorie:", reverse = T),
           color = F)+
    scale_y_continuous(labels = function(x){format(x,big.mark = "'", scientific = F)})+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fisch", "Fleisch")) +
    scale_color_manual(values = levels(pl$label_color))+
    geom_text(aes(label=ifelse(df_$pct_ubp * 100 > 1.5, paste0(round(df_$pct_ubp * 100, digits = 0),"%"),"")),size = 10, position = position_stack(vjust = 0.5)) + # omit 0% with ifelse()
    annotate( 
         "text", x = (1:2)+.4, y = 20000000, label = txt$txt_ubp, size=9) + # attention thousand separator in text!
    coord_flip() +
    mytheme +
    theme(legend.position = "bottom")

p + labs(caption = "Daten: Kassendaten SV Schweiz (2017)\n Berechnungen: Karen Muir (2017)")# , 
# subtitles = paste("Ein Gericht stiess in den Basiswochen durchnittlich ", b,"UBP",
# "und in den Interventionswochen ", i,"UBP aus"))

#save
# ATTENTION, font size is quite small
ggsave("plots/ubp_condit_horizontal_190130_egel.pdf",
       height = 10,
       width = 26,
       dpi = 300,
       device = cairo_pdf)

# barplot gwp----
p <- ggplot(df_, aes(y = total_gwp, x = condit, fill = factor(label_content, c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Fisch")), color = label_color)) + 
    geom_bar(stat = "identity", position = "stack", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    ylab(expression(paste("\n Global warming potential GWP (kg ", "CO"[2],"eq)")))+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels = function(x){format(x,big.mark = "'", scientific = F)})+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Fisch")) +
    scale_color_manual(values = levels(df_$label_color))+
    geom_text(aes(label=ifelse(df_$pct_gwp * 100 > 1.5, paste0(round(df_$pct_gwp * 100, digits = 0),"%"),"")),size = 6, position = position_stack(vjust = 0.5)) + # omit 0% with ifelse()
    annotate( 
        "text",x = 1:2, y = 19000, label = txt$txt_gwp, size=6) + # attention thousand separator in text!
    mytheme

# calculate mean of gwp per meal 
b <- round(mean(df[df$condit == "Basis",]$tot_gwp), 2)
i <- round(mean(df[df$condit == "Intervention",]$tot_gwp), 2)
    
p + labs(caption = "Daten: Kassendaten SV Schweiz (2017)\n Berechnungen: Karen Muir (2017)", 
         subtitles = paste("Ein Gericht stiess in den Basiswochen durchnittlich ", b,"kg CO2-eq\n",
                           "und in den Interventionswochen ", i,"kg CO2-eq aus"))

# save plot as png and pdf
ggsave("plots/gwp_condit_190130_egel.pdf",
       height = 12,
       width = 12,
       dpi = 300,
       device = cairo_pdf)

# plot mean ubp per meal for gender and condition--------
pl <- df_2017 %>% 
    filter(label_content != "Hot and Cold" & !grepl("Local ", df_2017$article_description) & !grepl("Spezial", df_2017$member)) %>%
    group_by(gender, condit) %>% 
    summarise(tot_sold = n(),
              mean_ubp = mean(tot_ubp, na.rm = T)) %>% 
    drop_na()

# do some statitics
sum(pl$tot_sold)/nrow(df_agg) #Ì how many percent of sellings do we have UBP data
dat <- df_2017 %>% 
    filter(label_content != "Hot and Cold" & !grepl("Local ", df_2017$article_description) & !grepl("Spezial", df_2017$member))

# regression
library(lme4)
lm <- lmer(dat$tot_ubp ~ dat$gender + (1| dat$ccrs)) # is signifiant difference, however r square is super small
summary(lm)

library(MuMIn)
r.squaredGLMM(lm) # marginal effect => fixed effects 

# anova
dat$gender <- parse_factor(dat$gender, levels = c())
summary(aov(tot_ubp ~ gender + Error(ccrs), data = dat))

# anova another package
library(afex)
aov_4(tot_ubp ~ gender + (1|ccrs), dat)
(a1 <- aov_car(tot_ubp ~ gender + Error(ccrs), data = dat))
lsmeans(a1, c("M","F"))


# plot
ggplot(pl, aes(y = mean_ubp, x = condit, fill = gender)) + 
    geom_bar(stat = "identity", position = position_dodge(), width = .6) +
    scale_fill_manual(values =  c("#99f200","#6619e6"),
                      labels = c("Frauen", "Männer")) + # check out other colors!
    ylab("Durchschnittliche UBP pro verkauftes Gericht") +
    xlab("Bedingungen") +
    guides(fill = guide_legend("")) +
    geom_text(aes(label = paste(expression(bar(x)), " == ", round(pl$mean_ubp))), parse = T, position = position_dodge(width = .7), vjust = -.4, size = 7) +
    mytheme 

# annotate wont work with bquote
txt <- paste(round(pl$mean_ubp), "UBP") # seems not to work either
bquote(bar(x) ~ " = " ~ round(pl$mean_ubp) ~ "UBP") # wont work
paste(expression(bar(x)), " == ", round(pl$mean_ubp)) # seems to work, however adding another text wont work, due to parse = T
paste("bar(x)", bquote(.(txt)), sep = "=") # not working too => reasons??

    
ggsave("plots/ubp_gender_condit_190226.pdf",
           height = 10,
           width = 10,
           device = cairo_pdf)


# how often do visit the people the canteen-----------
visiter <- df_2017 %>%
    group_by(ccrs, gender) %>%
    summarise(visit = n())

aggregate(visiter$visit ~ visiter$gender, FUN = mean)

visiter_freq <- visiter %>% 
    mutate(category=cut(visit, breaks = c(-Inf,2,5,Inf), 
                        labels=c("einmaliger\n Besuch", "weniger als 1x\n pro Woche\n (2 bis 5-mal)", "regelmaessig"))) %>%
    group_by(gender, category) %>% 
    summarise(visit_counts=n()) %>% # count how hoften a visit occurs, e.g. oneday visitors occur 200 times 
    drop_na() %>% 
    mutate(pct = round(visit_counts / sum(visit_counts),3))

# how often choose women hot and cold----
# 951 transactions are not possible to match with gender => df_2017 %>% filter(is.na(gender) | is.na(label_content)) 
hnc <- df_2017 %>% 
    group_by(label_content, gender) %>% 
    summarise(tot = n()) %>% 
    drop_na() %>% # NA where droped
    ungroup() %>% 
    group_by(gender) %>% 
    mutate(pct = tot / sum(tot))


# nutrition clusters and ubp-----
source("04_1_load_data_190128_egel.R")

# define clusters accoring meat purchase
df_ <- df_2017 %>% # watch with what for dataset you work!
    select(ccrs, label_content, gender, member, age) %>% # select variable of interest
    filter(member != "Spezialkarten") %>% 
    dcast(formula = ccrs + gender + age + member ~ label_content, value.var="label_content", fun.aggregate= length) %>% # reshape into wide format and aggregate after occurencies of label content
    rename(Unknown = 'NA') %>% # rename NA to unknown
    mutate(tot_buy = rowSums(.[,-c(1:4)])) %>% # exclude ccrs and information of person for sum of rows
    mutate(meaty =(.$Fleisch + .$Fisch + .$Geflügel)/.$tot_buy,
           hnc = .$`Hot and Cold` / .$tot_buy)



# hot and cold: people went to the hot and cold buffet 
# 16 nrow(buffet)/1043 => 1.7%
buffet <-  df_ %>%  
    filter(tot_buy > 5 & hnc == 1) %>% 
    mutate(cluster = "buffet") 


# meat avoiders
# 21 nrow(meat_avoiders)/1043 => 2.1%
no_meat <- df_ %>%
    filter(tot_buy > 5 & meaty == 0 & hnc == 0) %>% 
    mutate(cluster = "never meat")

# always meat
# 16 nrow(alwy_meat)/1043 => 1.5%
alwy_meat <- df_ %>% 
    filter(tot_buy > 5 & meaty  == 1 & hnc == 0) %>% 
    mutate(cluster = "always meat")

# build groups of canteen visitors (1007-(16 + 22 + 13)) beacause of their meat consumption per week
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
# 224 nrow(meat_flex)/nrow(canteen_visitors) => 22.7%
meat_flex <- canteen_visitors %>% 
    filter(meaty > 1/4 & meaty <= 1/2) %>% 
    mutate(cluster = "meat-flexitarians")

# meat-eaters: less than the halt of all buyings contain meat
# 328 nrow(meat_eat)/nrow(canteen_visitors) => 33.2%
meat_eat <- canteen_visitors %>% 
    filter(meaty > 1/2 & meaty <= 3/4) %>% 
    mutate(cluster = "meat-eaters")

# meat lovers
# 189 nrow(meat_lovers)/nrow(canteen_visitors) => 19.1%
meat_lovers <- canteen_visitors %>% 
    filter(meaty > 3/4) %>% 
    mutate(cluster = "meat lovers")


# concatenate all canteen visitors----
canteen_visitors <- bind_rows(buffet, no_meat, veg_flex, meat_flex, meat_eat, meat_lovers, alwy_meat)


# checK means of visit between 2 subgroups (less meat = flexy and more meat = meaty) 
canteen_visitors %>% 
    filter(cluster != "buffet") %>% 
    filter(ccrs != 1000581903 & ccrs != 1000587396) %>% # due to missings in ubp
    mutate(cluster3 = ifelse(.$cluster == "never meat" | .$cluster == "veg-flexitarians" | .$cluster == "meat-flexitarians", "flexi", "meaty")) %>% 
    group_by(cluster3) %>% 
    summarise(mean_visit = mean(tot_buy))


# check sellings for lenegd in plot
dat <- df_2017 %>% 
    right_join(., canteen_visitors[, c("ccrs", "cluster")], by = "ccrs") %>% 
    filter(!grepl("Hot and Cold", .$article_description)) %>% 
    filter(!grepl("Local ", .$article_description))


# count sellings
nrow(df_2017)-nrow(dat)  
# count percentage of sellings for plot
nrow(dat)/nrow(df_agg)

# two persons drop out
# 1000581903 1000587396 => reasons: they bought only locals and hot and cold
dp <- filter(df_2017, ccrs == 1000581903 | ccrs == 1000587396)
dp <- filter(canteen_visitors, ccrs == 1000581903 | ccrs == 1000587396 )

# check the chances of missings resp. how big is share for ubp to not ubp
test <- df_2017 %>% 
    right_join(., canteen_visitors[, c("ccrs", "cluster")], by = "ccrs") %>%
    filter(cluster != "buffet") %>% 
    mutate(no_envir = ifelse(is.na(.$tot_ubp) | is.na(.$tot_gwp),"no ubp","ubp calc")) %>% # is there an ubp/gwp calculation? 
    group_by(no_envir, cluster) %>% 
    summarise(tot_buy = n()) %>% 
    ungroup() %>% 
    group_by(cluster) %>% 
    mutate(pct = tot_buy / sum(tot_buy)) %>% 
    ungroup()

write_delim(test,"augmented data/ratio_ubp_noubp_calculations_190308_egel.csv", delim = ";")


# calculate ubp for intervention and basis (df_agg)
med <- df_agg %>% 
    group_by(condit) %>% 
    summarise(med_ubp = median(tot_ubp, na.rm = T),
              med_gwp = median(tot_gwp, na.rm = T))

# how many sellings where dropped?
nas <- df_agg %>% 
    filter(is.na(tot_gwp)) # 6300 Missings (Locals + Hot and Cold)

# checl again
nrow(df_agg %>% filter(!grepl("Local ", df_agg$article_description) & article_description != "Hot and Cold"))


# prepare data for plot
# merge it back to df_2017
pl <- df_2017 %>% 
    right_join(., canteen_visitors[, c("ccrs", "cluster")], by = "ccrs") %>% 
    group_by(cluster) %>% 
    summarise(med_ubp = median(tot_ubp, na.rm = T),
              med_gwp = median(tot_gwp, na.rm = T)) %>% 
    ungroup() 


# add how many people in cluster
pl2 <- canteen_visitors %>%
    filter(cluster != "buffet") %>% 
    filter(ccrs != 1000581903 & ccrs != 1000587396) %>% # two persons drop out due to meal choice
    group_by(cluster) %>% 
    summarise(tot_cluster = n()) %>%
    mutate(pct_cluster = round((tot_cluster / sum(tot_cluster) *100))) %>% 
    left_join(pl,.) %>% 
    mutate(txt_ = paste("(", tot_cluster, ")", sep = ""),
           txt = paste(cluster, txt_, sep = " " )) %>% 
    mutate(cluster2 = recode(cluster, 
                             # "one" = "one timers", "some" = "some timers",
                             "never meat" = "0 %", #if i want to change the label name, then the letters need to match the letters of the string in df_2$cluster otherwiese takes the original label
                             "veg-flexitarians" = "> 0 bis \u2264 25 %", 
                             "meat-flexitarians" = "> 25 bis \u2264 50 %",
                             "meat-eaters" = "> 50 bis \u2264 75 %", 
                             "meat lovers" = "> 75 bis < 100 %",
                             "always meat" = "100 %")) %>% # rename clusters
    mutate(color = recode(cluster,
                          "never meat" = "#99f200",
                          "veg-flexitarians" = "#c5b87c", 
                          "meat-flexitarians" = "#fad60d",
                          "meat-eaters" = "#80ccff" , 
                          "meat lovers" = "#6619e6",
                          "always meat" = "#008099")) %>% 
    drop_na() # drop buffet, some and one

# sort data
# c("never meat", "veg-flexitarians", "meat-flexitarians", "meat-eaters", "meat lovers", "always meat")

# plot data ubp-----
ggplot(pl2, aes(x = parse_factor(pl2$cluster2, levels = c(pl2$cluster2[5],  pl2$cluster2[6], pl2$cluster2[3], pl2$cluster2[2], pl2$cluster2[4], pl2$cluster2[1])), 
       y = med_ubp, fill = color)) +
    geom_bar(stat = "identity", width = .6) + # fill = "grey60"
    xlab("Anteil fleischhaltige an allen im HS 2017 gekauften Menüs") +
    ylab("Umweltbelastungspunkte (UBP) pro Menü") +
    guides(fill = F) +
    geom_text(aes(label = txt), position = position_dodge(width = 1), vjust = -.5, size = 8) +
    geom_text(aes(label = paste(round(med_ubp), " UBP")), position = position_dodge(width = 1), vjust = 1, size = 8) +
    mytheme

ggsave("plots/cluster_ubp_190306_egel.pdf",
       width = 17,
       height = 12,
       device = cairo_pdf)    

# check again (run fist two lines 484, 485)
aggregate(tot_ubp ~ as.factor(cluster),  data = pl, FUN = median)

# plot data gwp-----
ggplot(pl2, aes(x = parse_factor(pl2$cluster2, levels = c(pl2$cluster2[5],  pl2$cluster2[6], pl2$cluster2[3], pl2$cluster2[2], pl2$cluster2[4], pl2$cluster2[1])), 
                y = med_gwp, fill = color)) +
    geom_bar(stat = "identity", width = .6) +
    xlab("Anteil fleischhaltiger an allen gekauften Menüs") +
    ylab("Mittlerer Wert in GWP (Median)") +
    guides(fill = F) +
    geom_text(aes(label = txt), position = position_dodge(width = 1), vjust = -.25, size = 8) +
    geom_text(aes(label = paste(round(med_gwp,2), " UBP")), position = position_dodge(width = 1), vjust = 1, size = 6) +
    mytheme

ggsave("plots/cluster_gwp_190306_egel.pdf",
       width = 14,
       height = 10,
       device = cairo_pdf) 

# plot UBP- und GWP-Median per condition--------
# take hole data set
dat <- df_agg %>% 
    filter(label_content != "Hot and Cold" & !grepl("Local ", df_agg$article_description)) %>% 
    group_by(condit) %>% 
    summarise(tot_sold = n(),
              med_ubp = median(tot_ubp, na.rm = T),
              med_gwp = median (tot_gwp, na.rm = T))



# plot
ggplot(dat, aes(y = med_ubp, x = parse_factor(condit, levels = c("Intervention", "Basis")))) + 
    geom_bar(stat = "identity", width = .4) +
    ylab("Umweltbelastungspunkte (UBP) pro Gericht") +
    xlab("Feldexperiment im HS 2017") +
    scale_x_discrete(breaks = c("Intervention", "Basis"),
                     labels = c("vegi-lastige Wochen","fleisch-lastige Wochen")) +
    geom_text(aes(label = paste(round(med_ubp), "UBP")), position = position_dodge(width = .6), vjust = -.25, size = 8, color = "grey40") +
    mytheme +
    labs(caption = "Legende: durchschnittliche Umweltbelastung in der Ernährung 16'093 UBP pro Kopf und Tag")

ggsave("plots/med_ubp_condition_overall_190312_egel.pdf",
       width = 14,
       height = 12,
       device = cairo_pdf)


ggplot(dat, aes(y = med_gwp, x = parse_factor(condit, levels = c("Intervention", "Basis")))) + 
    geom_bar(stat = "identity", width = .4) +
    ylab(expression(paste("Treibhausgaspotential (kg ", "CO"[2],"-eq) pro Gericht"))) +
    xlab("Feldexperiment im HS 2017") +
    scale_x_discrete(breaks = c("Intervention", "Basis"),
                     labels = c("vegi-lastige Wochen","fleisch-lastige Wochen")) +
    geom_text(aes(label = paste(round(med_gwp,2),"kg CO2-eq")), position = position_dodge(width = .6), vjust = -.25, size = 8, color = "grey40") +
    scale_y_continuous(limits = c(0,2.5), breaks = seq(0,2.5,1)) +
    mytheme +
    labs(caption = expression(italic('Legende: durchschnittliche Umweltbelastung in der Ernährung 5.57 kg CO'[2]*'-eq pro Kopf und Tag'))) # it seems expression has some errors


ggsave("plots/med_gwp_condition_overall_190312_egel.pdf",
       width = 14,
       height = 16)
       # device = pdf) # expression seems to work with this device, however still causes some errors => reasons?


# check if they condit and ubp resp. gwp differ
dat <- df_agg %>% 
    filter(label_content != "Hot and Cold" & !grepl("Local ", df_agg$article_description)) %>% 
    group_by(condit, tot_gwp, tot_ubp) %>% 
    summarise(tot_sold = n())

# t-test => which dataset should i take (big one, of course signifkant => try to lmer)
t.test(tot_ubp ~ condit, data = dat)    
t.test(tot_gwp ~ condit, data = dat)
summary(aov(df_agg$tot_ubp ~ df_agg$condit + Error(df_agg$meal_name)))
library(lme4)
lmer(tot_ubp ~ condit + (1| meal_name), data = df_agg) # not working!




# flexitarians and poulet----------
pl <- df_2017 %>% 
    right_join(., canteen_visitors[, c("ccrs", "cluster")], by = "ccrs") %>%
    filter(cluster != "buffet" & cluster != "never meat" & cluster != "always meat") %>%
    group_by(cluster, label_content) %>% 
    summarise(tot_cluster = n()) %>% 
    mutate(pct_label = tot_cluster / sum(tot_cluster))
    
pl2 <- canteen_visitors %>%
    filter(cluster != "buffet" & cluster != "never meat" & cluster != "always meat") %>% 
    group_by(cluster) %>% 
    summarise(tot_member = n()) %>%
    mutate(pct_cluster = (tot_member / sum(tot_member))) %>% 
    ungroup() %>% 
    left_join(pl,.) %>% 
    mutate(txt_ = paste("(", tot_cluster, ")", sep = ""),
           txt = paste(cluster, txt_, sep = " " )) %>% 
    mutate(cluster2 = recode(cluster, 
                             # "one" = "one timers", "some" = "some timers",
                             "never meat" = "0 %", #if i want to change the label name, then the letters need to match the letters of the string in df_2$cluster otherwiese takes the original label
                             "veg-flexitarians" = "> 0 bis \u2264 25 %", 
                             "meat-flexitarians" = "> 25 bis \u2264 50 %",
                             "meat-eaters" = "> 50 bis \u2264 75 %", 
                             "meat lovers" = "> 75 bis < 100 %",
                             "always meat" = "100 %")) %>% # rename clusters
    mutate(color = recode(cluster,
                          "never meat" = "#99f200",
                          "veg-flexitarians" = "#c5b87c", 
                          "meat-flexitarians" = "#fad60d",
                          "meat-eaters" = "#80ccff" , 
                          "meat lovers" = "#6619e6",
                          "always meat" = "#008099"),
           label_content = recode(label_content, .missing = "Unbekannt")) 


# cols per cat
ColsPerCat=c("Unbekannt" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fisch"="#6619e6", "Geflügel" = "#008099", "Fleisch" = "#fad60d", "Hot and Cold" = "#262626")


pl2$label_color <- as.factor(sapply(unlist(ColsPerCat)[pl2$label_content], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"



# plot
ggplot(pl2, aes(x = parse_factor(pl2$cluster2, levels = c(unique(pl2$cluster2)[4], unique(pl2$cluster2)[2], unique(pl2$cluster2)[1], unique(pl2$cluster2)[3])), 
                y = pct_label, fill = parse_factor(label_content, levels = c("Unbekannt","Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fisch", "Geflügel", "Fleisch", "Hot and Cold")), color = label_color)) +
    geom_bar(stat = "identity", width = .6, color = NA) +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = levels(pl2$label_color))+
    geom_text(aes(label=ifelse(pl2$pct_label * 100 > 1.5, paste0(round(pl2$pct_label * 100, digits = 0),"%"),"")),size = 10, position = position_stack(vjust = 0.5)) +
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fisch", "Geflügel","Fleisch", "Hot and Cold (Buffet)")) +
    xlab("Anteil fleischhaltige an allen im HS 2017 gekauften Menüs") +
    ylab("Menü-Inhalt in Prozent") +
    guides(fill = guide_legend("Menü-Inhalt"), color = F)+
    mytheme


ggsave("plots/cluster_meat_190312.pdf",
       width = 18,
       height = 12,
       device = cairo_pdf)

