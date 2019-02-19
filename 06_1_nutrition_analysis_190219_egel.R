####
## analysis in nutritional data
####

# status: february 2019
# author: gian-andrea egeler


## load data nutrition and edit some cases: wide format------------
# check 05_01 load additional data
source("05_1_load_add_data_190128_egel.R")
source("08_theme_plots_180419_egel.R")

## summarise and visualize the data (take long data set)-----
library(gmodels)

df_ <- nutri_wide_
df_[grep("Pflanzlich+", df_$label_content), ]$label_content <- "Vegan"


df_$ebp_label <- factor(nutri_wide_$ebp_label, levels = c("ausgewogen", "akzeptabel", "unausgewogen"))
df_$teller_label <- factor(nutri_wide_$teller_label, levels = c("ausgewogen", "akzeptabel", "unausgewogen"))

CrossTable(df_$ebp_label, df_$label_content)
CrossTable(df_$teller_label, df_$label_content)

# ebp overall
ebp <-  df_ %>%  group_by(label_content, ebp_label) %>%
    summarise(tot = n()) %>%
    mutate(pct = tot / sum(tot)) %>% 
    ungroup() 


# ebp label
ebp_label <- df_ %>% group_by(ebp_label) %>% 
    summarise(tot = n()) %>% 
    mutate(pct = tot / sum(tot)) # calculate percentage of labels in ebp-model

#plot
ggplot(ebp, aes(y = tot, x = label_content, fill = ebp_label)) + 
    geom_bar(stat = "identity") + 
    ggtitle("ebp")


# teller overall
teller <- group_by(df_, label_content, teller_label) %>% 
    summarise(tot = n()) %>% 
    mutate(pct = tot / sum(tot))

#teller label
teller_label <- df_ %>% group_by(teller_label) %>% 
    summarise(tot = n()) %>% 
    mutate(pct = tot / sum(tot)) # calculate percentage of labels in teller-model


ggplot(teller, aes(y = tot, x = label_content, fill = teller_label)) + 
    geom_bar(stat = "identity") + 
    ggtitle("teller")


# plot order of ebp_points (attention old plot, not up to date)------------
# define order
order_ <- nutri_wide_ %>%
    arrange(ebp_points) %>%
    arrange(factor(label_content, levels = c("Pflanzlich","Pflanzlich+","Vegetarisch", "Fisch", "Fleisch"))) %>%
    select(meal_name.y)

# somehow not working => find a better way => aggregate(FUN=mean)
text1 <- aggregate(ebp_points ~label_content, data=nutri_wide_, FUN = function(ebp_points){sqrt(var(ebp_points))}) #try to calculate standard deviation 
text <- aggregate(ebp_points ~ label_content, data=nutri_wide_, FUN=mean)
text_ <- aggregate(ebp_points ~ label_content, data=nutri_wide_, FUN=median)               
txt <- left_join(text, text_, by="label_content") %>% 
    left_join(., text1, by = "label_content") %>%
    rename(mean = ebp_points.x, median = ebp_points.y, deviation= ebp_points) %>%
    mutate(label2 = paste("(","italic(Mdn)", "==", median,")")) %>%
    mutate(label1= paste("italic(bar(X))", "==", round(mean, digits = 1))) %>%
    mutate(label3 = paste(label1, label2, sep = "\n"))

# add date of creation
st = strftime(today(), format = "%d.%m.%y")

# plot
p <- ggplot(data = nutri_wide_, aes(x = meal_name.y,y = ebp_points, fill = label_content))+
    # geom_rect(fill = '#2F8F37', xmin = -Inf, xmax = Inf, ymin =0, ymax = Inf, alpha = 0.002)+ # color panel background, however alpha is not working
    # geom_rect(fill = '#efba29', xmin = -Inf, xmax = Inf, ymin =0, ymax = -12, alpha = 0.002)+ # color panel background
    # geom_rect(fill = '#c0322f', xmin = -Inf, xmax = Inf, ymin =-12, ymax = -40, alpha = 0.002)+ # color panel background
    geom_bar(stat="identity", position = "dodge", width = .6)+
    guides(fill = F)+
    scale_x_discrete(limits = order_$meal_name.y, guide_legend(""))+ # sort meal
    scale_fill_manual(values = c("Pflanzlich"="grey90", "Pflanzlich+"="#80ccff", "Vegetarisch" = "#c5b87c", "Fisch" = "#008099", "Fleisch" = "#fad60d"),
                      breaks = c("Pflanzlich","Pflanzlich+","Vegetarisch", "Fisch","Fleisch"),
                      labels = c("Pflanzlich (Fleischersatz)", "Pflanzlich","Vegetarisch", "Fisch", "Fleisch"))+
    scale_y_continuous(limits = c(-40, 20), breaks = seq(-40, 20, 10))+
    ylab("Ern‰hrungsphysiologische Balancepunkte (EBP)")+
    mytheme4
# theme(legend.position = "bottom")



# annotate text and lines
# attention lines need to be set new!!
p + annotate("segment", x="Panna E Pancetta", xend = "Paniertes MSC-Kabeljau-Filet" ,y = txt[txt$label_content == "Fleisch", ]$median, yend = txt[txt$label_content == "Fleisch", ]$median, size = 2, color = "grey20") + # annotate mean of meat
    annotate("segment", x="Herbstteller", xend = "Paprika-Kartoffel-Wedges" ,y = txt[txt$label_content == "Vegetarisch", ]$median, yend = txt[txt$label_content == "Vegetarisch", ]$median, size = 2, color = "grey20")+
    annotate("segment", x="Friedrice", xend = "Chili Sin Carne" ,y = txt[txt$label_content == "Pflanzlich+", ]$median, yend = txt[txt$label_content == "Pflanzlich+", ]$median, size = 2, color = "grey20")+
    annotate("segment", x="Orientalisches Seitan-Geschnetzeltes", xend = "Linguine Ticinese" ,y = txt[txt$label_content == "Pflanzlich", ]$median, yend = txt[txt$label_content == "Pflanzlich", ]$median, size = 2, color = "grey20")+
    # annotate("text", x="Kalbs-Adrio", y = c(15,12.5), label= txt$label3[1], size=5, color = "grey20", parse = T) + # annotate mean and sd of meat
    # annotate("text", x="Randen-Frischk‰se-Risotto", y = c(15,12.5), label= txt$label3[4], size=5, color = "grey20", parse = T)+
    # annotate("text", x="Nepal Linsen", y = c(15,12.5), label= txt$label3[3], size=5, color = "grey20", parse = T)+
    # annotate("text", x="Vegi-Burger", y = c(15,12.5), label= txt$label3[2], size=5, color = "grey20", parse = T) +
    labs(caption = "Daten: Claudia M¸ller, ZHAW") +
    annotate("rect", fill = '#2F8F37', xmin = -Inf, xmax = Inf, ymin =0, ymax = 20, alpha = 0.2)+
    annotate("rect", fill = '#efba29', xmin = -Inf, xmax = Inf, ymin =0, ymax = -12, alpha = 0.2)+
    annotate("rect", fill = '#c0322f', xmin = -Inf, xmax = Inf, ymin =-12, ymax = -40, alpha = 0.2)


ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/06_meetings/180910_dialog/poster/hintergrund/ebp/ebp_plots/ebp_180905_egel.pdf",
       width = 25,
       height = 14,
       dpi = 600,
       units="in",
       device= cairo_pdf)

# plot order of teller_points (old plot, not up to date)------------
# define order, same as ebp_points
order_ <- nutri_wide_ %>%
    arrange(ebp_points) %>%
    arrange(factor(label_content, levels = c("Pflanzlich","Pflanzlich+","Vegetarisch","Fisch", "Fleisch"))) %>%
    select(meal_name.y)

# somehow not working => find a better way => aggregate(FUN=mean)
text1 <- aggregate(teller_points ~label_content, data=nutri_wide_, FUN = function(teller_points){sqrt(var(teller_points))}) #try to calculate standard deviation 
text <- aggregate(teller_points ~ label_content, data=nutri_wide_, FUN=mean)
text_ <- aggregate(teller_points ~ label_content, data=nutri_wide_, FUN=median)               
txt <- left_join(text, text_, by="label_content") %>% 
    left_join(., text1, by = "label_content") %>%
    rename(mean = teller_points.x, median = teller_points.y, deviation= teller_points) %>%
    mutate(label2 = paste("(","italic(Mdn)", "==", median,")")) %>%
    mutate(label1= paste("italic(bar(X))", "==", round(mean, digits = 1))) %>%
    mutate(label3 = paste(label1, label2, sep = "\n"))

# add date of creation
st = strftime(today(), format = "%d.%m.%y")

# plot
p <- ggplot(nutri_wide_, aes(x = meal_name.y,y = teller_points, fill = label_content))+
    geom_bar(stat="identity", position = "dodge", width = .6)+
    guides(fill = guide_legend(title = ""))+
    scale_x_discrete(limits = order_$meal_name.y, guide_legend(""))+
    scale_fill_manual(values = c("Pflanzlich"="grey90", "Pflanzlich+"="#80ccff", "Vegetarisch" = "#c5b87c", "Fisch" = "#008099",  "Fleisch" = "#fad60d"),
                      breaks = c("Pflanzlich","Pflanzlich+","Vegetarisch","Fisch","Fleisch"),
                      labels = c("Pflanzlich (Fleischersatz)", "Pflanzlich","Vegetarisch", "Fisch","Fleisch"))+
    scale_y_continuous(limits = c(0, 9), breaks = seq(0, 9, 3))+
    ylab("Teller-Modell")+
    mytheme4 + 
    theme(legend.position = "bottom")



# annotate text and lines
# new lines need to be set!
p + annotate("segment", x="Panna E Pancetta", xend = "Paniertes MSC-Kabeljau-Filet" ,y = txt[txt$label_content == "Fleisch", ]$median, yend = txt[txt$label_content == "Fleisch", ]$median, size = 2, color = "grey20") + # annotate mean of meat
    annotate("segment", x="Herbstteller", xend = "Paprika-Kartoffel-Wedges" ,y = txt[txt$label_content == "Vegetarisch", ]$median, yend = txt[txt$label_content == "Vegetarisch", ]$median, size = 2, color = "grey20")+
    annotate("segment", x="Friedrice", xend = "Chili Sin Carne" ,y = txt[txt$label_content == "Pflanzlich+", ]$median, yend = txt[txt$label_content == "Pflanzlich+", ]$median, size = 2, color = "grey20")+
    annotate("segment", x="Orientalisches Seitan-Geschnetzeltes", xend = "Linguine Ticinese" ,y = txt[txt$label_content == "Pflanzlich", ]$median, yend = txt[txt$label_content == "Pflanzlich", ]$median, size = 2, color = "grey20")+
    # annotate("text", x="Kalbs-Adrio", y = c(15,12.5), label= txt$label3[1], size=5, color = "grey20", parse = T) + # annotate mean and sd of meat
    # annotate("text", x="Randen-Frischk‰se-Risotto", y = c(15,12.5), label= txt$label3[4], size=5, color = "grey20", parse = T)+
    # annotate("text", x="Nepal Linsen", y = c(15,12.5), label= txt$label3[3], size=5, color = "grey20", parse = T)+
    # annotate("text", x="Vegi-Burger", y = c(15,12.5), label= txt$label3[2], size=5, color = "grey20", parse = T) +
    labs(caption = "Daten: Claudia M¸ller, ZHAW")


ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/06_meetings/180910_dialog/poster/hintergrund/ebp/ebp_plots/teller_180926_egel.pdf",
       width = 25,
       height = 14,
       dpi = 600,
       units="in",
       device= cairo_pdf)


# compare ebp and tellermodell according meal content--------------- doging and stacking in same plot not possible in ggplot 

df_ <- df %>% 
    group_by(label_content, ebp_label) %>% # fist select ebp_label
    summarise(tot=n()) %>%
    mutate(pct=tot/sum(tot)) %>%
    ungroup() %>%
    mutate(label = factor(ebp_label, labels = c("akzeptabel", "ausgewogen", "unausgewogen")),
           method = "EBP-Modell") %>% # relevel for right order
    select(-ebp_label)


df_1 <- df %>% 
    group_by(label_content, teller_label) %>% # second select teller_label 
    summarise(tot=n()) %>%
    mutate(pct=tot/sum(tot)) %>%
    ungroup() %>%
    mutate(label = factor(teller_label, labels = c("akzeptabel", "ausgewogen", "unausgewogen")),
           method = "Teller-Modell") %>% # relevel for right order
    select(-teller_label)  %>%
    bind_rows(df_) # combine with data frame from above

text <- df %>% group_by(label_content) %>%
    summarise(tot = n()) %>%
    mutate(label = "n") %>%
    mutate(label2=paste(label,tot,sep=" = "))

ggplot(df_1, aes(x=label_content, y=tot, fill=(relevel(label, "ausgewogen"))))+
    geom_bar(stat="identity", position = "stack", width = .3) + 
    scale_fill_manual(values=c("unausgewogen" = "#cc2e2e", "akzeptabel" = "#f9c21b","ausgewogen"="#2a9634"))+
    #                       breaks=c("unausgewogen", "akzeptabel", "ausgewogen"),
    #                       labels = c("unausgewogen", "akzeptabel", "ausgewogen"))+
    xlab("")+
    ylab("Anzahl Men¸s")+
    guides(fill= guide_legend( title = ""))+
    geom_text(aes(label=paste(round(pct*100, digits = 1), "%", sep = "")),size = 5,position = position_stack(vjust = .5))+ # is not yet working dont know why
    # annotate("text",x=1:8, y=48, label=rep(c(text$label2[2],text$label2[3],text$label2[4],text$label2[1]),8), size=8)+
    scale_x_discrete(breaks = c("Pflanzlich","Pflanzlich+","Vegetarisch","Fleisch"),
                     limits = c("Pflanzlich","Pflanzlich+","Vegetarisch","Fleisch"),
                     labels = c("vegan (Fleischersatz)", "vegan (authentisch)","ovo-lakto-vegetarisch", "Fleisch oder Fisch"))+
    facet_wrap(method ~ .) + 
    mytheme + 
    theme(strip.text = element_text(size=20))+
    theme(legend.position = "bottom")+
    labs(caption = "Daten: Claudia M¸ller, ZHAW")



# save plot
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/06_meetings/180910_dialog/poster/hintergrund/ebp/ebp_plots/ebp_teller_180925_egel.pdf",
       width = 28,
       height = 12, 
       dpi = 600, 
       units="in",
       device = cairo_pdf)

# CrossTabs between both methods------------

CrossTable(nutri_wide_$ebp_label, nutri_wide_$teller_label, fisher = T)
chisq.test(nutri_wide_$ebp_label, nutri_wide_$teller_label)$expected # smaller than 5 => fishers exact test
fisher.test(nutri_wide_$ebp_label, nutri_wide_$teller_label)

# Korrelation between both methods------------
cor <- cor.test(nutri_wide_$ebp_points, nutri_wide_$teller_points, method = "spearman") # very low correlation
cor <- cor.test(nutri_wide_$ebp_points, nutri_wide_$teller_points, method = "kendall") 



#plot
cf <- coef(lm(df$ebp_points~df$teller_points)) # for plotting the correlation line intercept: -11.6690741 , slope: 0.7778705
ggplot(df,aes(x=teller_points,y=ebp_points))+
    geom_count()+
    geom_abline(intercept = cf[1], slope = cf[2]) +
    # geom_smooth(stat = "cor") + wondering if that would work
    ylab("EBP-Modell")+
    xlab("Tellermodell")+
    labs(size="Anzahl Men¸s\n")+
    scale_x_continuous(limits = c(0,9), breaks = seq(0,9,3)) + 
    scale_y_continuous(limits = c(-40, 20), breaks = seq(-40, 20, 10))+
    annotate("text", x=c(8,8) , y=c(-6,-7.6) , label=c("italic(rho) == 0.19", "italic(p) == 0.07"), parse=T, size=7)+ # cor$estimate, cor$p.value
    # geom_hline(yintercept=0, color="grey70", size=1) + geom_vline(xintercept=8, color="grey70", size=1)+
    mytheme

# save plot
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/06_meetings/180910_dialog/poster/hintergrund/ebp/ebp_plots/cor_ebp_teller_180925_egel.pdf",
       width = 18,
       height = 12, 
       dpi = 600, 
       units="in",
       device = cairo_pdf)

# compare both models ebp and teller------
#prepare data
df_ <- nutri_wide_ %>% 
    group_by(label_content, ebp_label) %>% # fist select ebp_label
    summarise(tot=n()) %>%
    mutate(pct=tot/sum(tot)) %>%
    ungroup() %>%
    mutate(label = factor(ebp_label, labels = c("akzeptabel", "ausgewogen", "unausgewogen")),
           method = "EBP-Modell") %>% # relevel for right order
    select(-ebp_label)

# prepare data second spep
df_1 <- df %>% 
    group_by(label_content, teller_label) %>% # second select teller_label 
    summarise(tot=n()) %>%
    mutate(pct=tot/sum(tot)) %>%
    ungroup() %>%
    mutate(label = factor(teller_label, labels = c("akzeptabel", "ausgewogen", "unausgewogen")),
           method = "Teller-Modell") %>% # relevel for right order
    select(-teller_label)  %>%
    bind_rows(df_) # combine with data frame from above

#plot
ggplot(df_1, aes(x=label_content, y=tot, fill=(relevel(label, "ausgewogen"))))+
    geom_bar(stat="identity", position = "stack", width = .3) + 
    scale_fill_manual(values=c("unausgewogen" = "#cc2e2e", "akzeptabel" = "#f9c21b","ausgewogen"="#2a9634"))+
    #                       breaks=c("unausgewogen", "akzeptabel", "ausgewogen"),
    #                       labels = c("unausgewogen", "akzeptabel", "ausgewogen"))+
    xlab("")+
    ylab("Anzahl Men√ºs")+
    guides(fill= guide_legend( title = ""))+
    geom_text(aes(label=paste(round(pct*100, digits = 1), "%", sep = "")),size = 5,position = position_stack(vjust = .5))+ # is not yet working dont know why
    # annotate("text",x=1:8, y=48, label=rep(c(text$label2[2],text$label2[3],text$label2[4],text$label2[1]),8), size=8)+
    scale_x_discrete(breaks = c("Pflanzlich","Pflanzlich+","Vegetarisch", "Fisch","Fleisch"),
                     limits = c("Pflanzlich","Pflanzlich+","Vegetarisch","Fisch", "Fleisch"),
                     labels = c("Vegan (Fleischersatz)", "Vegan (authentisch)","Ovo-lakto-vegetarisch", "Fisch", "Fleisch"))+
    facet_wrap(method ~ .) + 
    mytheme + 
    theme(strip.text = element_text(size=20)) # for what is that?? 

# labs(caption = "Berechnungen: Claudia M√ºller, ZHAW")


# save plot 
# needs to be finished in indesign
# that is also the path for the indesign file
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/06_meetings/180910_dialog/poster/hintergrund/ebp/ebp_plots/ebp_teller_180925_egel.pdf",
       width = 28,
       height = 12, 
       dpi = 600, 
       units="in",
       device = cairo_pdf)
