############
## analyse envir and nutritional data
###########

## status: 4.7.18 // egel


# loading data see load envir_nutri 17

## do some first calculations---------
# correlation between methods
cor.test(envir_nutri_tot$gwp_tot, envir_nutri_tot$ubp_tot, method = "spearman") # very high correlation

#plot
coef(lm(envir_nutri_tot$ubp_tot~envir_nutri_tot$gwp_tot)) # for plotting the correlation line intercept: 1079.591, slope: 1908.758 
ggplot(envir_nutri_tot,aes(gwp_tot,ubp_tot))+
    geom_point()+
    geom_abline(intercept = 1079.591, slope = 1908.758) +
    ylab("Umweltbelastungspunkte")+
    xlab("Treibhauspotential")+
    annotate("text", x=c(4.5,4.5) , y=c(8200,7700) , label=c("italic(rho) == 0.86\n","italic(p) < 0.001"), parse=T, size=7)+
    # labs(size="Anzahl Menüs\n")+ # if you use geom_counts
    mytheme

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/health_envir/plots/envir_methods_180613_egel.pdf",
       width = 12,
       height = 8,
       dpi=600,
       device = cairo_pdf)

# cross tables between methods nutri
cor.test(envir_nutri_tot$ebp_points, envir_nutri_tot$teller_points, method = "spearman")

# plot
# scale the x (0-9) and y (20 to -40) axis
coef(lm(envir_nutri_tot$ebp_points~envir_nutri_tot$teller_points)) # for plotting the correlation line intercept: -11.5159192, slope: 0.7778705
ggplot(envir_nutri_tot,aes(x=teller_points,y=ebp_points))+
    geom_count()+
    geom_abline(intercept = -11.5159192, slope = 0.7778705) +
    ylab("EBP-Modell")+
    xlab("Tellermodell")+
    labs(size="Anzahl Menüs\n")+
    annotate("text", x=c(8,8) , y=c(-6,-7.6) , label=c("italic(rho) == 0.17\n", "italic(p) == 0.02"), parse=T, size=7)+
    # geom_hline(yintercept=0, color="grey70", size=1) + geom_vline(xintercept=8, color="grey70", size=1)+
    mytheme

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/health_envir/plots/nutri_methods_180613_egel.pdf",
       width = 12,
       height = 8,
       dpi=600,
       device = cairo_pdf)

CrossTable(envir_nutri_tot$ebp_label, envir_nutri_tot$teller_label)


####----------
# prepare data for plots

info1$week <- isoweek(info1$date) # add week to dokumentation

df <- info1 %>%
    select(article_description, week, cycle, date, label_content, meal_name) %>%
    filter(duplicated(meal_name)) %>% # only duplicates (because of shop_description)
    left_join(nutri_wide, .,by = c("article_description", "date", "week", "cycle")) %>%
    filter(!duplicated(meal_name.x)) # no duplicates du to ???

## some changes in dataset, because of date (look someday for solution)
df[grep("Hot", df$meal_name.x),]$meal_name.y <- info1[grep("Hot", info1$meal_name)[1],]$meal_name
df[grep("Hot", df$meal_name.x),]$label_content <- "Pflanzlich+"
df[grep("85_Auberginen-Moussaka",df$meal_name.x),]$meal_name.y <- info1[grep("Moussaka", info1$meal_name)[1],]$meal_name
df[grep("85_Auberginen-Moussaka",df$meal_name.x),]$label_content <- "Vegetarisch"
df[(df$article_description == "Favorite") & (grepl("Quornragout",df$meal_name.x)),]$meal_name.y <- "Quornragout" # attention there are two quornragouts
df[(df$article_description == "Favorite") & (grepl("Quornragout",df$meal_name.x)),]$label_content <- "Vegetarisch"

df[grep("Schweins-P",df$meal_name.y),]$meal_name.y[2] <- "Schweins-Piccata Pol" # there are two Schweins-Piccata
df[grep("Gartengratin",df$meal_name.y),]$meal_name.y[1] <- "Kartoffel-Gemüsegratin" # there are two Gartengratins
df[grep("Schweins Cordon Bleu",df$meal_name.y),]$meal_name.y[1] <- "Schweins Cordon Bleu Broccoli" # there are two Schweins Cordon Bleu
df[grep("Schweins Cordon Bleu",df$meal_name.y),]$meal_name.y[2] <- "Schweins Cordon Bleu Karotten" # there are two Schweins Cordon Bleu
df[grep("Vesuvio",df$meal_name.y),]$meal_name.y[1] <- "Vesuvio (mit Ei)" # there are two Vesuvio


# plot order of ebp_points------------
# define order
order_ <- df %>%
    arrange(factor(label_content, levels = c("Pflanzlich","Pflanzlich+","Vegetarisch","Fleisch"))) %>%
    select(meal_name.y)

reorder(meal_name,-tot)

ggplot(df, aes(x = reorder(meal_name.y, -ebp_points),y = ebp_points, fill = label_content))+
    geom_bar(stat="identity", position = "dodge", width = .6)+
    guides(fill=F)+
    scale_x_discrete(limits = order_$meal_name.y)+
    scale_fill_manual(values = c("Pflanzlich"="grey90", "Pflanzlich+"="#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d"))+
    scale_y_continuous(limits = c(-40, 20), breaks = seq(-40, 20, 10))+
    ylab("EBP-Skala (-40 bis +20)")+
    xlab("Menü-Bezeichnung")+
    mytheme4

ggplot(df, aes(x = meal_name.y,y = ebp_points, fill = label_content))+
    geom_bar(stat="identity",  width = .6)+
    guides(fill=F)+
    scale_x_discrete(limits = df[order(df$ebp_points),]$meal_name.y)+
    scale_fill_manual(limits = c("Fleisch","Vegetarisch","Pflanzlich+","Pflanzlich"),
                      breaks = c("Fleisch","Vegetarisch","Pflanzlich+","Pflanzlich"),
                      values = c("Pflanzlich"="grey90", "Pflanzlich+"="#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d"))+
    scale_y_continuous(limits = c(-40, 20), breaks = seq(-40, 20, 10))+
    #theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))+
    ylab("EBP-Skala (-40 bis +20)")+
    xlab("Menü-Bezeichnung")+
    mytheme4


# save plot
ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/health_envir/plots/ebp_order_180704_egel.pdf",
       width = 25,
       height = 14, 
       dpi = 600, 
       device = cairo_pdf)

######
## save plots for presantation somewhere else
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/12_work_pack/wpIII.1_menu_choice/05_besprechungen/180706_sv_fm_meeting/input_gesundheit_online_exp_indd_180704/plots/ebp_order_180704_egel.pdf",
       width = 25,
       height = 14,
       dpi = 600,
       units="in",
       device= cairo_pdf)


# plot ebp according meal content---------------
df_ <- df %>% 
    group_by(label_content, ebp_label) %>%
    summarise(tot=n()) %>%
    mutate(pct=tot/sum(tot)) %>%
    ungroup() %>%
    mutate(ebp_label2 = factor(ebp_label, labels = c("akzeptabel", "ausgewogen", "unausgewogen"))) # relevel for right order

text <- df %>% group_by(label_content) %>%
    summarise(tot = n()) %>%
    mutate(label = "n") %>%
    mutate(label2=paste(label,tot,sep=" = "))

ggplot(df_, aes(x=label_content, y=tot, fill=relevel(ebp_label2,"ausgewogen")))+
    geom_bar(stat="identity", position = "stack", width = .6)+
    scale_fill_manual(values=c("unausgewogen" = "#cc2e2e", "akzeptabel" = "#f9c21b","ausgewogen"="#2a9634"))+
#                       breaks=c("unausgewogen", "akzeptabel", "ausgewogen"),
#                       labels = c("unausgewogen", "akzeptabel", "ausgewogen"))+
    xlab("\nNährwertprofile nach EBP-Modell")+
    ylab("Anzahl Menüs")+
    guides(fill=F)+
    geom_text(aes(label=paste(round(pct*100, digits = 1), "%", sep = "")),size = 5,position = position_stack(vjust = .5))+ # is not yet working dont know why
    annotate("text",x=1:4, y=48, label=c(text$label2[2],text$label2[3],text$label2[4],text$label2[1]), size=6)+
    scale_x_discrete(limits=c("Pflanzlich","Pflanzlich+","Vegetarisch","Fleisch"))+
    mytheme

#aes(label=ifelse(tot>4,tot,"")), size = 5, position = position_stack(vjust = 0.5)
#, labels=c("Pflanzlich","Pflanzlich+","Vegetarisch","Fleisch")

# save plot
ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/health_envir/plots/ebp_content_180704_egel.pdf",
       width = 12,
       height = 10, 
       dpi = 600, 
       device = cairo_pdf)

######
## save plots for presantation somewhere else
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/12_work_pack/wpIII.1_menu_choice/05_besprechungen/180706_sv_fm_meeting/input_gesundheit_online_exp_indd_180704/plots/ebp_content_180704_egel.pdf",
       width = 12,
       height = 10,
       dpi = 600,
       units="in",
       device= cairo_pdf)


# plot order of teller_points----------
ggplot(df, aes(x = meal_name.y ,y = teller_points, fill=label_content))+
    geom_bar(stat="identity", position = "dodge", width = .6)+
    guides(fill=F)+
    scale_x_discrete(limits = df[order(df$teller_points),]$meal_name.y)+
    scale_y_continuous(limits = c(0,9), breaks = seq(0,9,3))+
    scale_fill_manual(labels = c("Fleisch","Vegetarisch","Pflanzlich+","Pflanzlich"),
                      breaks = c("Fleisch","Vegetarisch","Pflanzlich+","Pflanzlich"),
                      values = c("Pflanzlich"="grey90", "Pflanzlich+"="#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d"))+
    #theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1))+
    ylab("Tellermodell-Skala (0 bis 9)")+
    xlab("Menü-Bezeichnung")+
    mytheme4


# save plot
ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/health_envir/plots/teller_order_180704_egel.pdf",
       width = 25,
       height = 14, 
       dpi = 600, 
       device = cairo_pdf)

######
## save plots for presantation somewhere else
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/12_work_pack/wpIII.1_menu_choice/05_besprechungen/180706_sv_fm_meeting/input_gesundheit_online_exp_indd_180704/plots/teller_order_180704_egel.pdf",
       width = 25,
       height = 14,
       dpi = 600,
       units="in",
       device= cairo_pdf)



## plot tellermodell according to meal content----------
df_ <- df %>% 
    group_by(label_content, teller_label) %>%
    summarise(tot=n()) %>%
    mutate(pct=tot/sum(tot)) %>%
    ungroup() %>%
    mutate(teller_label2 = factor(teller_label, labels = c("akzeptabel", "ausgewogen", "unausgewogen"))) # relevel for right order


text <- df %>% group_by(label_content) %>%
    summarise(tot = n()) %>%
    mutate(label = "n") %>%
    mutate(label2=paste(label,tot,sep=" = "))

ggplot(df_, aes(x=label_content, y=tot, fill=relevel(teller_label2, "ausgewogen")))+
    geom_bar(stat="identity", position = "stack", width = .6)+
    scale_fill_manual(values = c("unausgewogen" = "#cc2e2e", "akzeptabel" = "#f9c21b","ausgewogen"="#2a9634"))+
#                       breaks = c("unausgewogen", "akzeptabel", "ausgewogen"),
#                       labels = c("unausgewogen", "akzeptabel", "ausgewogen"))+
    xlab("\nNährwertprofile nach Teller-Modell")+
    ylab("Anzahl Menüs")+
    guides(fill=F)+
    geom_text(aes(label=paste(round(pct*100, digits = 1), "%", sep = "")),size = 5,position = position_stack(vjust = .5))+ # is not yet working dont know why
    annotate("text",x=1:4, y=48, label=c(text$label2[2],text$label2[3],text$label2[4],text$label2[1]), size=6)+
    scale_x_discrete(limits=c("Pflanzlich","Pflanzlich+","Vegetarisch","Fleisch"))+
    mytheme



# save plot
ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/health_envir/plots/teller_content_180704_egel.pdf",
       width = 12,
       height = 10, 
       dpi = 600, 
       device = cairo_pdf)

######
## save plots for presantation somewhere else
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/12_work_pack/wpIII.1_menu_choice/05_besprechungen/180706_sv_fm_meeting/input_gesundheit_online_exp_indd_180704/plots/teller_content_180704_egel.pdf",
       width = 12,
       height = 10,
       dpi = 600,
       units="in",
       device= cairo_pdf)
