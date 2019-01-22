######
# actual-target comparision
######

#Status: 5.7.18 // egel

#load data => see script load data 2017
#load data => see script load data 2015 to 2017


################
### Target and Actual Comparison 2017 => Basis and Intervention with and without locals => untill now no solution to deal with it
###############

# Actual: Locals are not included and Timefilter between 9 and 15 oclock 
df=filter(df_7,!grepl("Local+",df_7$article_description))
df_ist <- group_by(df,date,article_description,label_content,shop_description, condit)%>%
    summarise(tot_sold=n()) %>%
    mutate(Angebot="keine Locals")

df_ist_l <-  group_by(df_7,date,article_description,label_content,shop_description, condit)%>%
    summarise(tot_sold=n())
    
df_ist_l$Angebot <-  ifelse(df_ist_l$article_description[grepl("Local+", df_ist_l$article_description)] == TRUE,"mit Locals","keine Locals")


#  Target: locals are not included => see above
df_soll=read_excel("S:/pools/n/N-IUNR-nova-data/06_add_var/02_planung_verkaufszahlen/Mensa_geplant_180216_02egel.xlsx")
df_soll=rename(df_soll, article_description=Menülinie, date=Datum, shop_description= Ort)
df_soll$shop_description=str_replace(df_soll$shop_description,"Grüental","Grüental Mensa")
df_soll$shop_description=str_replace(df_soll$shop_description,"Vista","Vista Mensa")

# Merge both data frames
df_tot=left_join(df_ist,df_soll, by=c("date","article_description","shop_description"))

# prepare data for plot
# Actual sold
df_i=group_by(df_tot, label_content, condit) %>% 
    summarise(tot_sold=sum(tot_sold)) %>% 
    mutate(offer='Actual', xlab=paste(offer, condit,sep="\n")) %>% # add two variables Actual and xlab
    ungroup()
    # df_ist_t=group_by(df,condit) %>% mutate(pct=tot_sold/sum(tot_sold)) # calculate percentage pro shop_description

#target sold
df_s=group_by(df_tot, label_content, condit) %>% 
    summarise(tot_plan=sum(Geplant)) %>% 
    mutate(offer='Target', xlab=paste(offer, condit,sep="\n")) %>%
    ungroup()

# df_soll_t=group_by(df,condit) %>% mutate(pct=tot_plan/sum(tot_plan))

# concate both datasets: df_ist_t and df_soll_t
df_t=bind_rows(df_s,df_i) %>%
    mutate(tot=rowSums(cbind(tot_plan,tot_sold),na.rm = TRUE)) %>% # take tot_plan and tot_sold togheter
    select(-tot_plan, -tot_sold)
    
## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unknown" = "black","Vegan"="grey90", "Vegan+"="#80ccff", "Vegetarian" = "#c5b87c", "Meat" = "#fad60d","Hot and Cold"="#4c4848")

df_t$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_t$label_content], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

# total over canteen => information for annotate
lbt= df_t %>%
    group_by(xlab) %>%
    summarise(tot=sum(tot)) %>%
    mutate(tot2=format(tot, big.mark = "'", scientific = F)) %>% # add thousand seperator
    mutate(label = "n") %>%
    mutate(label2=paste(label,tot2,sep=" = ")) %>%
    ungroup()


# plot in de----
df_t$offer <- str_replace(df_t$offer,"Target","Geplant")
df_t$offer <- str_replace(df_t$offer,"Actual","Verkauft")
df_t$xlab <- paste(df_t$condit,df_t$offer, sep = "\n")


ggplot(df_t, aes(y=tot,x=xlab, fill=factor(label_content,levels=c("Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), label=format(df_t$tot, big.mark = "'", scientific = F), color=label_color)) +
    geom_bar(stat="identity", position = "stack", color=NA, width = .6) +
    xlab("\nGeplante und verkaufte Menüs in Basis- und Interventionwochen (Kalenderwochen 40 bis 51)") + 
    ylab("Geplante und verkaufte Menüs im HS 2017\n")+ 
    #     ggtitle("note: locals are not included
    #          selling time between 9 a.m. until 3 p.m.
    #          locals meals in total: 2602")+
    guides(fill=F,
           color=F)+
    # scale_y_continuous(labels = scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Pflanzlich","Pflanzlich+","Vegetarisch","Fleisch", "Hot and Cold"))+
    scale_color_manual(values = levels(df_t$label_color))+
    scale_x_discrete(limits=c("Basis\nGeplant","Basis\nVerkauft","Intervention\nGeplant","Intervention\nVerkauft"))+
    geom_text(size = 5, position = position_stack(vjust = 0.5))+
    annotate("text",x=1:4,y=13900,label=c(lbt$label2[3],lbt$label2[1],lbt$label2[4],lbt$label2[2]), size=6)+
    mytheme


ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse/plots/actual_target_comp_condition_de 2017 180615 03egel.png",
       width = 14,
       height = 8,
       dpi=600,
       units="in",
       device="png")

######
## save plots for presantation somewhere else
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/12_work_pack/wpIII.1_menu_choice/05_besprechungen/180706_sv_fm_meeting/ppp_indd_180627/plots/actual_target_comp_condition_de 2017 180615 04egel.pdf",
       width = 17,
       height = 9,
       dpi = 600,
       units="in",
       device= cairo_pdf)

################
### Target and Actual Comparison 2015:2017 => meal offer
###############

##prepare data
# target for 2015 and 2016 and 2017 for both canteens (per canteen 120 meals) and for both cycles: 480 melas in total
# per canten and cycle: meat:54, vegetarian:36, hot and cold: 30
year_1 <- rep(2015, times=480, each=1)
label_content <- rep(c("Meat","Vegetarian","Hot and Cold"), times=c(216,144,120))
df_soll_56 <- data_frame(year_1, label_content) %>% rename(year=year_1) %>% mutate(offer = "Geplant")

# soll for intervention:: per canten and cycle: meat: 45, vegetarian: 30, hot and cold: 30, vegan: 7, vegan+: 8
# soll for basis:: per canteen and cycle: meat: 60, vegetarian: 30, hot and cold: 30
year_2 <- rep(2017, times=480)
label_content_2 <- rep(c("Meat","Vegetarian","Hot and Cold","Vegan","Vegan+"), times=c(180,120,120,28,32))
df_soll_7 <- data_frame(year_2, label_content_2) %>% rename(year=year_2,label_content=label_content_2) %>% mutate(offer= "Geplant")

# Actual: Locals are included
# use documentation for that (not selling data)
info$week <- isoweek(info$date) # add week
info$condit <- ifelse(info$week %%2 == 0 & info$cycle == 1, "Basis", ifelse(info$week %%2 == 1 & info$cycle == 2,"Basis","Intervention")) # add condition
info$year <- year(info$date)
info$label_content <- str_replace(info$label_content,"Fleisch","Meat")
info$label_content <- str_replace(info$label_content,"Vegetarisch","Vegetarian")
info$label_content <- str_replace(info$label_content,"Pflanzlich","Vegan")

df_ist_7 <- info %>% mutate(offer = "Angebot") %>%
    select(year,label_content,offer)
    
#     group_by(info,year, label_content,shop_description, condit) %>% 
#     summarise(tot_sold=n()) %>% 
#     ungroup() %>%
#     mutate(offer = "Angebot") %>%
#     select(year,label_content,offer)

# Merge data frames
df_t= bind_rows(df_soll_56, df_soll_7, df_ist_7)

# Group data frame
df_ <- df_t %>%
    group_by(year,label_content, offer) %>%
    summarise(tot=n()) %>%
    ungroup() # otherwise dataframe is still grouped

# # calculate pct
# df_ <- df_ %>%
#     group_by(year, offer) %>%
#     mutate(pct=tot/sum(tot)) %>%
#     ungroup()

# define some variables
df_$xlab <- paste(df_$year,df_$offer, sep = "\n")
df_$xlab <- str_replace(df_$xlab,"2015\nGeplant","2015 - 2016\nGeplant")
df_$label_content <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

# define text for annotation
text <- group_by(df_, year, offer) %>%
    summarise(tot=sum(tot)) %>%
    mutate(label = "n") %>%
    mutate(label2=paste(label,tot,sep=" = "))

## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unknown" = "black","Vegan"="grey90", "Vegan+"="#80ccff", "Vegetarian" = "#c5b87c", "Meat" = "#fad60d","Hot and Cold"="#4c4848")

df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                     function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"


#plot in de
ggplot(df_, aes(y=tot,x=xlab, fill=factor(label_content,levels=c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), color=label_color)) +
    geom_bar(stat="identity", position = "stack", color=NA, width = .6) +
    xlab("\nGeplante und angebotene Menü-Optionen während Herbstsemester (Kalenderwochen 40 bis 51)") + 
    ylab("Geplante und angebotene Menü-Optionen")+ 
    #     ggtitle("note: locals are not included
    #          selling time between 9 a.m. until 3 p.m.
    #          locals meals in total: 2602")+
    guides(fill=F,
           color=F)+
    # scale_y_continuous(labels = scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Pflanzlich","Pflanzlich+","Vegetarisch","Fleisch", "Hot and Cold"))+
    scale_color_manual(values = levels(df_$label_color))+
    scale_x_discrete(limits=c("2015 - 2016\nGeplant","2017\nGeplant","2017\nAngebot"))+
    geom_text(aes(label=ifelse(tot>4,tot,"")), size = 5, position = position_stack(vjust = 0.5))+ #omit numbers to be shown smaller than 190
    annotate("text",x=1:3,y=620,label=c(text$label2[1],text$label2[1],text$label2[2]), size=6)+
    mytheme

# save plot
ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse/plots/actual_target_comp_menu_offer 2015-2017 180615 03egel.png",
       width = 14,
       height = 10,
       dpi=600,
       units="in",
       device="png")

######
## save plots for presantation somewhere else
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/12_work_pack/wpIII.1_menu_choice/05_besprechungen/180706_sv_fm_meeting/input_kassendaten_indd_180627/plots/actual_target_comp_menu_offer 2015-2017 180615 04egel.pdf",
              width = 17,
              height = 9,
              dpi = 600,
              units="in",
              device= cairo_pdf)
       

################
### Target and Actual Comparison 2017 Basis and Intervention => meal offer
###############

##prepare data
# target for Intervention and Basis both canteens (per canteen 240 meals), and for both cycles (2x)
# basis
year_2 <- rep(2017, times=240)
label_content_2 <- rep(c("Meat","Vegetarian","Hot and Cold"), times=c(120,60,60))
df_soll_7_b <- data_frame(year_2, label_content_2) %>% rename(year=year_2,label_content=label_content_2) %>% mutate(condit= "Basis", offer="Geplant")

#intervention
label_content_3 <- rep(c("Meat","Vegetarian","Hot and Cold","Vegan","Vegan+"), times=c(60,60,60,28,32))
df_soll_7_i <- data_frame(year_2, label_content_3) %>% rename(year=year_2,label_content=label_content_3) %>% mutate(condit= "Intervention", offer="Geplant")

# use documentation for that (not selling data)
info$week <- isoweek(info$date) # add week
info$condit <- ifelse(info$week %%2 == 0 & info$cycle == 1, "Basis", ifelse(info$week %%2 == 1 & info$cycle == 2,"Basis","Intervention")) # add condition
info$year <- year(info$date)
info$label_content <- str_replace(info$label_content,"Fleisch","Meat")
info$label_content <- str_replace(info$label_content,"Vegetarisch","Vegetarian")
info$label_content <- str_replace(info$label_content,"Pflanzlich","Vegan")

df_ist_7 <- info %>% 
    mutate(offer = "Angebot") %>%
    select(year,label_content, condit, offer)


# Merge data frames
df_t= bind_rows(df_soll_7_b, df_soll_7_i, df_ist_7)

# Group data frame
df_ <- df_t %>%
    group_by(year,label_content, condit, offer) %>%
    summarise(tot=n()) %>%
    ungroup()

# # calculate percentage
# df_ <- df_ %>%
#     group_by(year,condit, offer) %>%
#     mutate(pct= tot/sum(tot)) %>%
#     ungroup()

# define some variables
df_$xlab <- paste(df_$condit,df_$offer, sep = "\n")
df_$label_content <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

# define text for annotation
text <- group_by(df_, condit, offer) %>%
    summarise(tot=sum(tot)) %>%
    mutate(label = "n") %>%
    mutate(label2=paste(label,tot,sep=" = "))

## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unknown" = "black","Vegan"="grey90", "Vegan+"="#80ccff", "Vegetarian" = "#c5b87c", "Meat" = "#fad60d","Hot and Cold"="#4c4848")

df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                   function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

#plot
ggplot(df_, aes(y=tot,x=xlab, fill=factor(label_content,levels=c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), color=label_color)) +
    geom_bar(stat="identity", position = "stack", color=NA, width = .6) + # omit to higlight the boxes in geom_bar
    xlab("\nGeplante und angebotene Menü-Optionen während Basis- und Interventionswochen (Kalenderwochen 40 bis 51)") + 
    ylab("Geplante und angebotene Menü-Optionen")+ 
    #     ggtitle("note: locals are not included
    #          selling time between 9 a.m. until 3 p.m.
    #          locals meals in total: 2602")+
    guides(fill=F,
           color=F)+
    # scale_y_continuous(labels = scales::percent)+
    scale_colour_manual(values = levels(df_$label_color)) + #define color for geom_text
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$names,
                      labels = c("Unbekannt","Pflanzlich","Pflanzlich+","Vegetarisch","Fleisch", "Hot and Cold"))+
    scale_x_discrete(limits=c("Basis\nGeplant","Basis\nAngebot","Intervention\nGeplant","Intervention\nAngebot"))+
    geom_text(aes(label=ifelse(tot>4,tot,"")),size = 5, position = position_stack(vjust = 0.5))+ #omit numbers to be shown smaller than 190
    # scale_colour_manual(values=colours)+ # try print all vegan numbers black others white, not working
    annotate("text",x=1:4,y=320,label=c(text$label2[2],text$label2[1],text$label2[2],text$label2[3]), size=6)+
    mytheme

# save plot
ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse/plots/actual_target_menu_offer 2017 180614 03egel.png",
       width = 14,
       height = 8,
       dpi=600,
       units="in",
       device="png")

######
## save plots for presantation somewhere else
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/12_work_pack/wpIII.1_menu_choice/05_besprechungen/180706_sv_fm_meeting/input_kassendaten_indd_180627/plots/actual_target_menu_offer 2017 180614 04egel.pdf",
              width = 17,
              height = 9,
              dpi = 600,
              units="in",
              device= cairo_pdf)



################
### Target and Actual Comparison 2017 Grüental Vista => meal offer
###############

##prepare data
# target for Intervention and Basis both canteens (per canteen 240 meals), and for both cycles (2x)
# basis
year_2 <- rep(2017, times=120)
label_content_2 <- rep(c("Meat","Vegetarian","Hot and Cold"), times=c(60,30,30))
df_soll_7_b <- data_frame(year_2, label_content_2) %>% rename(year=year_2,label_content=label_content_2) %>% mutate(shop_description= "Grüental und Vista", condit="Basis", offer="Geplant")

#intervention
label_content_3 <- rep(c("Meat","Vegetarian","Hot and Cold","Vegan","Vegan+"), times=c(30,30,30,14,16))
df_soll_7_i <- data_frame(year_2, label_content_3) %>% rename(year=year_2,label_content=label_content_3) %>% mutate(shop_description= "Grüental und Vista", condit="Intervention", offer="Geplant")

# use documentation for that (not selling data)
info$week <- isoweek(info$date) # add week
info$condit <- ifelse(info$week %%2 == 0 & info$cycle == 1, "Basis", ifelse(info$week %%2 == 1 & info$cycle == 2,"Basis","Intervention")) # add condition
info$year <- year(info$date)
info$label_content <- str_replace(info$label_content,"Fleisch","Meat")
info$label_content <- str_replace(info$label_content,"Vegetarisch","Vegetarian")
info$label_content <- str_replace(info$label_content,"Pflanzlich","Vegan")

df_ist_7 <- info %>% 
    mutate(offer = "Angebot") %>%
    select(year, label_content, shop_description, condit,offer)

# Merge data frames
df_t= bind_rows(df_soll_7_b, df_soll_7_i, df_ist_7)

# Group data frame
df_ <- df_t %>%
    group_by(year,label_content, shop_description, condit, offer) %>%
    summarise(tot=n()) %>%
    ungroup()

# # calculate percentage
# df_ <- df_ %>%
#     group_by(year,condit, offer) %>%
#     mutate(pct= tot/sum(tot)) %>%
#     ungroup()

# define some variables
df_$xlab <- paste(df_$shop_description, df_$condit, df_$offer, sep = "\n")
df_$label_content <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

# define text for annotation
text <- group_by(df_, shop_description, condit, offer) %>%
    summarise(tot=sum(tot)) %>%
    mutate(label = "n") %>%
    mutate(label2=paste(label,tot,sep=" = "))

## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unknown" = "black","Vegan"="grey90", "Vegan+"="#80ccff", "Vegetarian" = "#c5b87c", "Meat" = "#fad60d","Hot and Cold"="#4c4848")

df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

#plot
ggplot(df_, aes(y=tot,x=xlab, fill=factor(label_content,levels=c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), color=label_color)) +
    geom_bar(stat="identity", position = "stack", color=NA, width = .6) + # omit to higlight the boxes in geom_bar
    xlab("\nGeplante und angebotene Menü-Optionen in Grüental und Vista (Kalenderwochen 40 bis 51)") + 
    ylab("Geplante und angebotene Menü-Otionen")+ 
    #     ggtitle("note: locals are not included
    #          selling time between 9 a.m. until 3 p.m.
    #          locals meals in total: 2602")+
    guides(fill=F,
           color=F)+
    # scale_y_continuous(labels = scales::percent)+
    scale_colour_manual(values = levels(df_$label_color)) + #define color for geom_text
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$names,
                      labels = c("Unbekannt","Pflanzlich","Pflanzlich+","Vegetarisch","Fleisch", "Hot and Cold"))+
    scale_x_discrete(limits=c("Grüental und Vista\nBasis\nGeplant","Grüental Mensa\nBasis\nAngebot","Vista Mensa\nBasis\nAngebot", "Grüental und Vista\nIntervention\nGeplant", "Grüental Mensa\nIntervention\nAngebot", "Vista Mensa\nIntervention\nAngebot"))+
    geom_text(aes(label=ifelse(tot>4,tot,"")),size = 5, position = position_stack(vjust = 0.5))+ #omit numbers to be shown smaller than 190
    # scale_colour_manual(values=colours)+ # try print all vegan numbers black others white, not working
    annotate("text",x=1:6,y=165,label=c(text$label2[3],text$label2[1],text$label2[5],text$label2[4],text$label2[2], text$label2[6]), size=6)+
    mytheme

# save plot
ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse/plots/actual_target_menu_offer_stand 2017 180629 egel.png",
       width = 14,
       height = 8,
       dpi=600,
       units="in",
       device="png")

######
## save plots for presantation somewhere else
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/12_work_pack/wpIII.1_menu_choice/05_besprechungen/180706_sv_fm_meeting/input_kassendaten_indd_180627/plots/actual_target_menu_offer_stand 2017 180629 egel.pdf",
       width = 17,
       height = 9,
       dpi = 600,
       units="in",
       device= cairo_pdf)


##################
### test how many days design where true => without buffet (makes it easier)
##################

# basis per canteen over both cycles-------------
year_2 <- rep(2017, times=240)
label_content_2 <- rep(c("Meat","Vegetarian","Hot and Cold"), times=c(120,60,60))
df_soll_7_b <- data_frame(year_2, label_content_2) %>% rename(year=year_2,label_content=label_content_2) %>% mutate(condit= "Basis", offer="Geplant")

# intervention per canteen over both cycles
label_content_3 <- rep(c("Meat","Vegetarian","Hot and Cold","Vegan","Vegan+"), times=c(90,60,60,14,16))
df_soll_7_i <- data_frame(year_2, label_content_3) %>% rename(year=year_2,label_content=label_content_3) %>% mutate(condit= "Intervention", offer="Geplant")

# basis angebot
df_angebot_b <- info_ %>%
    filter(condit == "Basis") %>%
    dcast(formula = date+shop_description ~ label_content, value.var = "label_content", fun.aggregate = length) %>%
    mutate(fleisch_pct = .$Fleisch / (.$Fleisch + .$Vegetarisch + .$Pflanzlich + .$`Pflanzlich+` + .$`Hot and Cold` + .$`NA`),
           vegetarisch_pct = .$Vegetarisch / (.$Fleisch + .$Vegetarisch + .$Pflanzlich + .$`Pflanzlich+` + .$`Hot and Cold` + .$`NA`),
           vegan_pct = .$Pflanzlich / (.$Fleisch + .$Vegetarisch + .$Pflanzlich + .$`Pflanzlich+` + .$`Hot and Cold` + .$`NA`),
           vegan_plus_pct = .$`Pflanzlich+` / (.$Fleisch + .$Vegetarisch + .$Pflanzlich + .$`Pflanzlich+` + .$`Hot and Cold` + .$`NA`),
           buffet_pct = .$`Hot and Cold` / (.$Fleisch + .$Vegetarisch + .$Pflanzlich + .$`Pflanzlich+` + .$`Hot and Cold` + .$`NA`))

# intervention angebot
df_angebot_i <- info_ %>%
    filter(condit == "Intervention") %>%
    dcast(formula = date+shop_description ~ label_content, value.var = "label_content", fun.aggregate = length) %>%
    mutate(fleisch_pct = .$Fleisch / (.$Fleisch + .$Vegetarisch + .$Pflanzlich + .$`Pflanzlich+` + .$`Hot and Cold`),
           vegetarisch_pct = .$Vegetarisch / (.$Fleisch + .$Vegetarisch + .$Pflanzlich + .$`Pflanzlich+` + .$`Hot and Cold`),
           vegan_pct = .$Pflanzlich / (.$Fleisch + .$Vegetarisch + .$Pflanzlich + .$`Pflanzlich+` + .$`Hot and Cold`),
           vegan_plus_pct = .$`Pflanzlich+` / (.$Fleisch + .$Vegetarisch + .$Pflanzlich + .$`Pflanzlich+` + .$`Hot and Cold`),
           buffet_pct = .$`Hot and Cold` / (.$Fleisch + .$Vegetarisch + .$Pflanzlich + .$`Pflanzlich+` + .$`Hot and Cold`))

#cast into wide format for comparisons
# basis
geplant_b <- df_soll_7_b %>% dcast(formula = year + condit + offer ~ label_content, value.var = "label_content", fun.aggregate = length) %>%
    mutate(meat_pct = .$Meat/(.$Meat + .$Vegetarian + .$`Hot and Cold`),
            vegetarian_pct = .$Vegetarian/(.$Meat + .$Vegetarian + .$`Hot and Cold`),
            buffet_pct = .$`Hot and Cold`/(.$Meat + .$Vegetarian + .$`Hot and Cold`))

# intervention
geplant_i <- dcast(df_soll_7_i,formula = year+ condit + offer ~ label_content, value.var = "label_content", fun.aggregate = length) %>%
    mutate(meat_pct = .$Meat / (.$Meat + .$Vegetarian + .$Vegan + .$`Vegan+` + .$`Hot and Cold`),
           vegetarian_pct = .$Vegetarian / (.$Meat + .$Vegetarian + .$Vegan + .$`Vegan+` + .$`Hot and Cold`),
           vegan_pct = .$Vegan / (.$Meat + .$Vegetarian + .$Vegan + .$`Vegan+` + .$`Hot and Cold`),
           vegan_plus_pct = .$`Vegan+` / (.$Meat + .$Vegetarian + .$Vegan + .$`Vegan+` + .$`Hot and Cold`),
           buffet_pct = .$`Hot and Cold` / (.$Meat + .$Vegetarian + .$Vegan + .$`Vegan+` + .$`Hot and Cold`))

# basis per canteen over both cycles-------------
year_2 <- rep(2017, times=3)
label_content_2 <- rep(c("Fleisch","Vegetarisch"), times=c(2,1))
df_soll_7_b <- data_frame(year_2, label_content_2) %>% rename(year=year_2,label_content=label_content_2) %>% mutate(condit= "Basis", offer="Geplant")

# intervention per canteen over both cycles
label_content_3 <- rep(c("Fleisch","Vegetarisch","Pflanzlich"), times=c(1,1,1))
df_soll_7_i <- data_frame(year_2, label_content_3) %>% rename(year=year_2,label_content=label_content_3) %>% mutate(condit= "Intervention", offer="Geplant")


# basis angebot
info_$label_content[grep("Pflanzlich+",info_$label_content)] <- "Pflanzlich"

df_angebot_b <- info_ %>%
    filter(condit == "Basis") %>%
    dcast(formula = date+shop_description ~ label_content, value.var = "label_content", fun.aggregate = length) %>%
    mutate(fleisch_pct = .$Fleisch / (.$Fleisch + .$Vegetarisch + .$Pflanzlich + .$`NA`),
           vegetarisch_pct = .$Vegetarisch / (.$Fleisch + .$Vegetarisch + .$Pflanzlich +  .$`NA`),
           vegan_pct = .$Pflanzlich / (.$Fleisch + .$Vegetarisch + .$Pflanzlich  + .$`NA`))

# intervention angebot
df_angebot_i <- info_ %>%
    filter(condit == "Intervention") %>%
    dcast(formula = date+shop_description ~ label_content, value.var = "label_content", fun.aggregate = length) %>%
    mutate(fleisch_pct = .$Fleisch / (.$Fleisch + .$Vegetarisch + .$Pflanzlich),
           vegetarisch_pct = .$Vegetarisch / (.$Fleisch + .$Vegetarisch + .$Pflanzlich ),
           vegan_pct = .$Pflanzlich / (.$Fleisch + .$Vegetarisch + .$Pflanzlich ))


# test


#cast into wide format for comparisons-----
# basis
geplant_b <- df_soll_7_b %>% dcast(formula = year + condit + offer ~ label_content, value.var = "label_content", fun.aggregate = length) %>%
    mutate(meat_pct = .$Fleisch/(.$Fleisch + .$Vegetarisch),
           vegetarian_pct = .$Vegetarisch/(.$Fleisch + .$Vegetarisch ))

# intervention
geplant_i <- dcast(df_soll_7_i,formula = year+ condit + offer ~ label_content, value.var = "label_content", fun.aggregate = length) %>%
    mutate(meat_pct = .$Fleisch / (.$Fleisch + .$Vegetarisch + .$Pflanzlich),
           vegetarian_pct = .$Vegetarisch / (.$Fleisch + .$Vegetarisch + .$Pflanzlich),
           vegan_pct = .$Pflanzlich / (.$Fleisch + .$Vegetarisch + .$Pflanzlich ))

# compare geplant with angebot
# easy way

df_b <- select(df_angebot_b, date, shop_description) %>%
    mutate(meat = ifelse(df_angebot_b$fleisch_pct == geplant_b$meat_pct,"true","not"),
           veg = ifelse(df_angebot_b$vegetarisch_pct == geplant_b$vegetarian_pct,"true","not"))

df_i <- select(df_angebot_i, date, shop_description) %>%
    mutate(meat = ifelse(df_angebot_i$fleisch_pct == geplant_i$meat_pct,"true","not"),
           veg = ifelse(df_angebot_i$vegetarisch_pct == geplant_i$vegetarian_pct,"true","not"),
           vegan = ifelse(df_angebot_i$vegan_pct == geplant_i$vegan_pct,"true","not")) 

str_count(df_b[,3:4],"true")
str_count(df_i[,3:5],"true")





# not working dont know why
df <- select(df_angebot_b, date) 
for (i in 1:nrow(df_angebot_b)){
    df$meat <- ifelse(df_angebot_b$fleisch_pct[i] == geplant_b$meat_pct,"true","not")
    df$veg <- ifelse(df_angebot_b$vegetarisch_pct[i] == geplant_b$vegetarian_pct, "true","not")
    
}



##################
### test for differences
##################


leveneTest(tot_plan~label_content, data=df_t)
# ao=aov(tot_plan~label_content+shop_description, data=df_t)
# summary(ao)
