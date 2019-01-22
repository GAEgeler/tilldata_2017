######
# actual-target comparision
######

#Status: 19.06.18 // egel

#load data => see script load data 2017
#load data => see script load data 2015 to 2017

# timefilter between 11 to 14 and with locals! => see script load data 2015 to 2017
df_ist=read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse/all data over year 180424 04egel.csv",delim = ';') # see script load data 2015 to 2017 180518 egel
df_ist=read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse/all data daily 15_17 180620 egel.csv",delim = ';'))
# df_soll=read_excel("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse/soll 2015-2017 180420 02egel.xlsx")

#define soll (target) sellings
year_1 <- rep(2015:2016, times=120, each=1)
label_content <- rep(c("Meat","Vegetarian","Hot and Cold"), times=c(108,72,60))
year_2 <- rep(2017, times=120)
label_content_2 <- rep(c("Meat","Vegetarian","Hot and Cold","Vegan","Vegan+"), times=c(45,30,30,7,8))
df_soll <- bind_rows(data_frame(year_1, label_content) %>% rename(year=year_1),data_frame(year_2,label_content_2) %>% rename(year=year_2, label_content=label_content_2))


###############
#### Target and Actual Comparison between 2015 - 2017 
###############

#prepare data SOLL
df_soll <- group_by(df_soll, year,label_content)%>% summarise(tot=n())%>%mutate(pct_soll=tot/sum(tot))
df_soll <- rename(df_soll, variable=label_content, pct=pct_soll)
df_soll <- select(df_soll,variable, year, pct)
df_soll$state = 'Target'

#prepare data IST 
df_ <- group_by(df_ist, variable, year) %>% summarise(value=sum(value)) # aggregate data
df_ <- df_ %>% 
    group_by(year) %>% # give in variable, you want to calculate percentage
    mutate(pct=value/sum(value))

df_ist2=select(df_,variable,year,pct) # select variables
df_ist2$state='Actual' # add new variable

# concatinate both datasets
df_t=rbind(df_ist2, df_soll)

# define xlab
df_t$xlab=paste(df_t$year,df_t$state,sep="\n")

#note: problems with ordering the strings => only via factor possible

ggplot(df_t, aes(y=pct,x=xlab, fill=factor(variable,levels=c("Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), label=paste0(round(df_t$pct*100,digits=0),"%"))) +
    geom_bar(stat="identity", position = "stack") +
    xlab("Target-Actual comparison per autumn semester") +
    ylab("Expected and sold meals in percent")+
    ggtitle("note: locales are included, \n         selling time from 11 a.m. until 2 p.m.")+
    guides(fill=guide_legend("meal content\n",
                             keywidth=.5,
                             keyheight=.5,
                             default.unit="inch"))+
    scale_y_continuous(labels = scales::percent)+
    scale_fill_manual(values = c("Vegan"="grey90", "Vegan+"="#80ccff", "Vegetarian" = "#c5b87c", "Meat" = "#fad60d","Hot and Cold"="#262626"), na.value = "grey50")+
    scale_x_discrete(limits=c("2015\nTarget","2015\nActual","2016\nTarget","2016\nActual","2017\nTarget","2017\nActual"))+ # sort x-axis manually
    geom_text(size = 5, position = position_stack(vjust = 0.5),col="#ffffff")+
    annotate(
        "text",x = 1:6, y = -.04, label = c("''","italic(N) == 26446","''","italic(N) == 26211","''","italic(N) == 26195"),size = 5,parse=T)+ # numbers are from df_ist: group_by(df_ist, year) %>% summarise(tot=sum(value))
    mytheme

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse/plots/actual_target_comp 2015-2017 180424 02egel.pdf",
       width = 14,
       height = 8,
       dpi=600,
       units="in",
       device="pdf")


################
### Target and Actual Comparison 2017
###############

# Actual: Locals are not included and Timefilter between 9 and 15 oclock
df=filter(df_7,!grepl("Local+",df_7$article_description))
df_ist=group_by(df,date,article_description,label_content,shop_description, meal_name)%>% 
    summarise(tot_sold=n()) %>%
    ungroup()

# Target: locals are not included => see above
df_soll=read_excel("S:/pools/n/N-IUNR-nova-data/06_add_var/02_planung_verkaufszahlen/Mensa_geplant_180216_02egel.xlsx") # data from the kitchen team
df_soll=rename(df_soll, article_description=Menülinie, date=Datum, shop_description= Ort)
df_soll$shop_description=str_replace(df_soll$shop_description,"Grüental","Grüental Mensa")
df_soll$shop_description=str_replace(df_soll$shop_description,"Vista","Vista Mensa")

# Merge both data frames
df_tot=left_join(df_ist,df_soll, by=c("date","article_description","shop_description"))

# prepare data for plot
# Actual sold
df=group_by(df_tot, label_content, shop_description) %>% 
    summarise(tot_sold=sum(tot_sold)) %>% 
    mutate(offer='Actual', xlab=paste(offer, shop_description,sep="\n")) # add two variables Actual and xlab
df_ist_t=group_by(df,shop_description) %>%
    mutate(pct=tot_sold/sum(tot_sold)) %>% # calculate percentage pro shop_description
    ungroup()
#target sold
df=group_by(df_tot, label_content, shop_description) %>% 
    summarise(tot_plan=sum(Geplant)) %>% 
    mutate(offer='Target', xlab=paste(offer, shop_description,sep="\n")) 
df_soll_t=group_by(df,shop_description) %>% 
    mutate(pct=tot_plan/sum(tot_plan)) %>%
    ungroup()

# concate both datasets: df_ist_t and df_soll_t
df_t=bind_rows(df_soll_t,df_ist_t)

# plot meal content => higher selling numbers because of the time filter (=40 meals difference)
# explanation difference between target and actual is in total 2729. All sold locals are 2601. Thus 127 meals are thrown away?? 

# total over canteen => information for annotate
lbt= df_t %>%
    group_by(shop_description) %>%
    summarise(tot_p=sum(tot_plan, na.rm=T),tot_s = sum(tot_sold, na.rm=T)) %>%
    ungroup()


ggplot(df_t, aes(y=pct,x=xlab, fill=factor(label_content,levels=c("Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), label=paste0(round(df_t$pct*100,digits=0),"%"))) +
    geom_bar(stat="identity", position = "stack") +
    xlab("Target-Actual comparison per canteen") +
    ylab("Expected and sold meals in 2017")+
    ggtitle("note: locals are not included,
         selling time: from 9 a.m. until 3 p.m.
         locals meals in total: 2602")+
    guides(fill=guide_legend("meal content\n",
                             keywidth=.5,
                             keyheight=.5,
                             default.unit="inch"))+
    scale_y_continuous(labels = scales::percent)+
    scale_fill_manual(values = c("Vegan"="grey90", "Vegan+"="#80ccff", "Vegetarian" = "#c5b87c", "Meat" = "#fad60d","Hot and Cold"="#262626"))+
    scale_x_discrete(limits=c("Target\nGrüental Mensa","Actual\nGrüental Mensa","Target\nVista Mensa","Actual\nVista Mensa"))+
    geom_text(size = 5, position = position_stack(vjust = 0.5),col="#ffffff")+
    annotate("text",x=1:4,y=-.05,label=c("italic(n)==13140","italic(n)==12498","italic(n)==13321", "italic(n)==11134"), size=5,parse=T)+
    mytheme

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse/plots/actual_target_comp 2017 180424 02egel.pdf",
              width = 14,
              height = 8,
              dpi=600,
              units="in",
              device="pdf")

################
### Target and Actual Comparison 2017 => Basis and Intervention with no locals
###############

# Actual: Locals are not included and Timefilter between 9 and 15 oclock
df=filter(df_7,!grepl("Local+",df_7$article_description))
df_ist=group_by(df,date,article_description,label_content,shop_description, condit)%>% summarise(tot_sold=n())

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
    mutate(label = "N") %>%
    mutate(label2=paste(label,tot2,sep=" = ")) %>%
    ungroup()

# plot in en
ggplot(df_t, aes(y=tot,x=xlab, fill=factor(label_content,levels=c("Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), label=format(df_t$tot, big.mark = "'", scientific = F), color=label_color)) +
    geom_bar(stat="identity", position = "stack", color=NA) +
    xlab("Target-Actual comparison per basis and intevention week") +
    ylab("Expected and sold meals in 2017")+
#     ggtitle("note: locals are not included
#          selling time between 9 a.m. until 3 p.m.
#          locals meals in total: 2602")+
    guides(fill=guide_legend("meal content\n",
                             keywidth=.5,
                             keyheight=.5,
                             default.unit="inch"),
           color=F)+
    # scale_y_continuous(labels = scales::percent)+
    scale_fill_manual(values = ColsPerCat)+
    scale_color_manual(values= levels(df_t$label_color))+
    scale_x_discrete(limits=c("Target\nBasis","Actual\nBasis","Target\nIntervention","Actual\nIntervention"))+
    geom_text(size = 5, position = position_stack(vjust = 0.5))+
    annotate("text",x=1:4,y=c(13700, 12500,13700, 12500),label=c(lbt$label2[3],lbt$label2[1],lbt$label2[4],lbt$label2[2]), size=6)+
    mytheme

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse/plots/actual_target_comp_condition 2017 180424 03egel.pdf",
       width = 14,
       height = 8,
       dpi=600,
       units="in",
       device="pdf")

# plot in de
df_t$offer <- str_replace(df_t$offer,"Target","Soll")
df_t$offer <- str_replace(df_t$offer,"Actual","Ist")
df_t$xlab <- paste(df_t$offer,df_t$condit, sep = "\n")
df_t$col <- ifelse(grepl("Vegan",df_t$label_content),"#000000","#ffffff") # not working, dont kno why
   

ggplot(df_t, aes(y=tot,x=xlab, fill=factor(label_content,levels=c("Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), label=format(df_t$tot, big.mark = "'", scientific = F), color=label_color)) +
    geom_bar(stat="identity", position = "stack", color=NA) +
    xlab("\nIst-Soll-Vergleich zwischen Basis- und Interventionswochen") + 
    ylab("Geplante und tatsächliche Menüverkäufe im 2017 ")+ 
    #     ggtitle("note: locals are not included
    #          selling time between 9 a.m. until 3 p.m.
    #          locals meals in total: 2602")+
    guides(fill=guide_legend("Menü-Inhalt\n",
                             keywidth=.5,
                             keyheight=.5,
                             default.unit="inch"),
           color=F)+
    # scale_y_continuous(labels = scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Pflanzlich","Pflanzlich+","Vegetarisch","Fleisch", "Hot and Cold"))+
    scale_color_manual(values = levels(df_t$label_color))+
    scale_x_discrete(limits=c("Soll\nBasis","Ist\nBasis","Soll\nIntervention","Ist\nIntervention"))+
    geom_text(size = 5, position = position_stack(vjust = 0.5))+
    annotate("text",x=1:4,y=c(13700, 12500,13700, 12500),label=c(lbt$label2[3],lbt$label2[1],lbt$label2[4],lbt$label2[2]), size=6)+
    mytheme


ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse/plots/actual_target_comp_condition_de 2017 180615 02egel.png",
       width = 14,
       height = 8,
       dpi=600,
       units="in",
       device="png")


################
### Target and Actual Comparison 2017 => Basis and Intervention with locals
###############

# Actual: Locals are included and Timefilter between 9 and 15 oclock
df_ist=group_by(df_7,date,article_description,label_content,shop_description, condit)%>% summarise(tot_sold=n()) %>% ungroup()

# Target: 
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
    summarise(tot_plan=sum(Geplant, na.rm=T)) %>% 
    mutate(offer='Target', xlab=paste(offer, condit,sep="\n")) %>%
    ungroup()

# df_soll_t=group_by(df,condit) %>% mutate(pct=tot_plan/sum(tot_plan))

# concate both datasets: df_ist_t and df_soll_t
df_t=bind_rows(df_s,df_i) %>%
    mutate(tot=rowSums(cbind(tot_plan,tot_sold),na.rm = TRUE)) %>% # take tot_plan and tot_sold togheter
    select(-tot_plan, -tot_sold) 
df_t$label_content <- ifelse(is.na(df_t$label_content),"Unknown",df_t$label_content)

## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unknown" = "black","Vegan"="grey90", "Vegan+"="#80ccff", "Vegetarian" = "#c5b87c", "Meat" = "#fad60d","Hot and Cold"="#4c4848")

df_t$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_t$label_content], # takes every label and their belonged color
                                     function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

# plot meal content => higher selling numbers because of the time filter (=40 meals difference)
# total over canteen => information for annotate
lbt= df_t %>%
    group_by(xlab) %>%
    summarise(tot=sum(tot)) %>%
    mutate(tot2=format(tot, big.mark = "'", scientific = F)) %>% # add thousand seperator
    mutate(label = "N") %>%
    mutate(label2=paste(label,tot2,sep=" = ")) %>%
    ungroup()

# plot in de
df_t$offer <- str_replace(df_t$offer,"Target","Soll") # translate englisch to german
df_t$offer <- str_replace(df_t$offer,"Actual","Ist")
df_t$xlab <- paste(df_t$offer,df_t$condit, sep = "\n")

ggplot(df_t, aes(y=tot,x=xlab, fill=factor(label_content,levels=c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), color=label_color)) +
    geom_bar(stat="identity", position = "stack", color = NA) +
    xlab("\nIst-Soll-Vergleich zwischen Basis- und Interventionswochen") + 
    ylab("Geplante und tatsächliche Menüverkäufe im 2017 ")+ 
    #     ggtitle("note: locals are not included
    #          selling time between 9 a.m. until 3 p.m.
    #          locals meals in total: 2602")+
    guides(fill=guide_legend("Menü-Inhalt\n",
                             keywidth=.5,
                             keyheight=.5,
                             default.unit="inch"),
           color=F)+
    # scale_y_continuous(labels = scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$names,
                      labels = c("Unbekannt","Pflanzlich","Pflanzlich+","Vegetarisch","Fleisch", "Hot and Cold"))+
    scale_x_discrete(limits=c("Soll\nBasis","Ist\nBasis","Soll\nIntervention","Ist\nIntervention"))+
    scale_color_manual(values = levels(df_t$label_color))+ # color the text with dark background white
    geom_text(aes(label=ifelse(df_t$tot>180,format(df_t$tot, big.mark = "'", scientific = F),"")),size = 5, position = position_stack(vjust = 0.5))+ #omit numbers to be shown smaller than 190
    annotate("text",x=1:4,y=13700,label=c(lbt$label2[3],lbt$label2[1],lbt$label2[4],lbt$label2[2]), size=6)+
    mytheme

#save plot
ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse/plots/actual_target_comp_condition_mitlocals_de 2017 180615 02egel.png",
       width = 14,
       height = 8,
       dpi=600,
       units="in",
       device="png")


################
### Target and Actual Comparison 2015:2017 => meal offer
###############

##prepare data
# target for 2015 and 2016 and 2017 for both canteens (per canteen 120 meals) and for both cycles: 480 melas in total
# per canten and cycle: meat:54, vegetarian:36, hot and cold: 30
year_1 <- rep(2015, times=480, each=1)
label_content <- rep(c("Meat","Vegetarian","Hot and Cold"), times=c(216,144,120))
df_soll_56 <- data_frame(year_1, label_content) %>% rename(year=year_1) %>% mutate(offer = "Soll")

# soll for intervention:: per canten and cycle: meat: 45, vegetarian: 30, hot and cold: 30, vegan: 7, vegan+: 8
# soll for basis:: per canteen and cycle: meat: 60, vegetarian: 30, hot and cold: 30
year_2 <- rep(2017, times=480)
label_content_2 <- rep(c("Meat","Vegetarian","Hot and Cold","Vegan","Vegan+"), times=c(210,120,120,14,16))
df_soll_7 <- data_frame(year_2, label_content_2) %>% rename(year=year_2,label_content=label_content_2) %>% mutate(offer= "Soll")

# actual for 2017
df_ist_7=group_by(df_7,date,year,article_description,label_content,shop_description, condit) %>%
    summarise(tot_sold = n())%>% 
    mutate(offer = "Ist") %>%
    ungroup() %>%
    select(year, label_content, shop_description, offer)

#check how many offers over both cycle
meal_offer <- df_ist_7 %>%
    group_by(label_content, shop_description) %>% # count all meal offers in 2017 over both canteens
    summarise(tot_offer=n())%>%
    ungroup()

# Merge data frames
df_t= bind_rows(df_soll_56, df_soll_7, df_ist_7)

# Group data frame
df_ <- df_t %>%
    group_by(year,label_content, offer) %>%
    summarise(tot=n()) %>%
    ungroup() # otherwise dataframe is still grouped

# define some variables
df_$xlab <- paste(df_$offer,df_$year, sep = "\n")
df_$xlab <- str_replace(df_$xlab,"Soll\n2015","Soll\n2015 - 2016")
df_$label_content <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

# define text for annotation
text <- group_by(df_, year, offer) %>%
    summarise(tot=sum(tot)) %>%
    mutate(label = "N") %>%
    mutate(label2=paste(label,tot,sep=" = "))

## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unknown" = "black","Vegan"="grey90", "Vegan+"="#80ccff", "Vegetarian" = "#c5b87c", "Meat" = "#fad60d","Hot and Cold"="#4c4848")

df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                     function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"


#plot in de
ggplot(df_, aes(y=tot,x=xlab, fill=factor(label_content,levels=c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), color=label_color)) +
    geom_bar(stat="identity", position = "stack", color=NA) +
    xlab("\nIst-Soll-Vergleich im Menü-Angebot über ganzes Herbstsemester") + 
    ylab("Geplantes und tatsächliches Menü-Angebot über beide Mensen")+ 
    #     ggtitle("note: locals are not included
    #          selling time between 9 a.m. until 3 p.m.
    #          locals meals in total: 2602")+
    guides(fill=guide_legend("Menü-Inhalt\n",
                             keywidth=.5,
                             keyheight=.5,
                             default.unit="inch"),
           color=F)+
    # scale_y_continuous(labels = scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Pflanzlich","Pflanzlich+","Vegetarisch","Fleisch", "Hot and Cold"))+
    scale_color_manual(values = levels(df_$label_color))+
    scale_x_discrete(limits=c("Soll\n2015 - 2016","Soll\n2017","Ist\n2017"))+
    geom_text(aes(label=ifelse(tot>10,tot,"")),size = 5, position = position_stack(vjust = 0.5))+ #omit numbers to be shown smaller than 190
    annotate("text",x=1:3,y=c(510,510,620),label=c(text$label2[1],text$label2[1],text$label2[2]), size=6)+
    mytheme

# save plot
ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse/plots/actual_target_comp_menu_offer 2015-2017 180615 02egel.png",
       width = 14,
       height = 10,
       dpi=600,
       units="in",
       device="png")


################
### Target and Actual Comparison 2017 Basis and Intervention => meal offer
###############

##prepare data
# target for Intervention and Basis both canteens (per canteen 240 meals), and for both cycles (2x)
# basis
year_2 <- rep(2017, times=240)
label_content_2 <- rep(c("Meat","Vegetarian","Hot and Cold"), times=c(120,60,60))
df_soll_7_b <- data_frame(year_2, label_content_2) %>% rename(year=year_2,label_content=label_content_2) %>% mutate(condit= "Basis", offer="Soll")

#intervention
label_content_3 <- rep(c("Meat","Vegetarian","Hot and Cold","Vegan","Vegan+"), times=c(90,60,60,14,16))
df_soll_7_i <- data_frame(year_2, label_content_3) %>% rename(year=year_2,label_content=label_content_3) %>% mutate(condit= "Intervention", offer="Soll")

# actual for 2017
df_ist_7=group_by(df_7,date,year,article_description,shop_description,label_content, condit) %>%
    summarise(tot_sold = n())%>% 
    mutate(offer = "Ist") %>%
    ungroup() %>%
    select(year, label_content, condit, offer) # to select, first ungroup dataframe, otherwiese an error will occur

# Merge data frames
df_t= bind_rows(df_soll_7_b, df_soll_7_i, df_ist_7)

# Group data frame
df_ <- df_t %>%
    group_by(year,label_content, condit, offer) %>%
    summarise(tot=n()) %>%
    ungroup()

# define some variables
df_$xlab <- paste(df_$offer,df_$condit, sep = "\n")
df_$label_content <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

# define text for annotation
text <- group_by(df_, condit, offer) %>%
    summarise(tot=sum(tot)) %>%
    mutate(label = "N") %>%
    mutate(label2=paste(label,tot,sep=" = "))

## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unknown" = "black","Vegan"="grey90", "Vegan+"="#80ccff", "Vegetarian" = "#c5b87c", "Meat" = "#fad60d","Hot and Cold"="#4c4848")

df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                   function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

#plot
ggplot(df_, aes(y=tot,x=xlab, fill=factor(label_content,levels=c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), color=label_color)) +
    geom_bar(stat="identity", position = "stack", color=NA) + # omit to higlight the boxes in geom_bar
    xlab("\nIst-Soll-Vergleich im Menü-Angebot über die Bedingungen") + 
    ylab("Geplantes und tatsächliches Menü-Angebot")+ 
    #     ggtitle("note: locals are not included
    #          selling time between 9 a.m. until 3 p.m.
    #          locals meals in total: 2602")+
    guides(fill=guide_legend("Menü-Inhalt\n",
                             keywidth=.5,
                             keyheight=.5,
                             default.unit="inch"),
           color=F)+
    # scale_y_continuous(labels = scales::percent)+
    scale_colour_manual(values = levels(df_$label_color)) + #define color for geom_text
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$names,
                      labels = c("Unbekannt","Pflanzlich","Pflanzlich+","Vegetarisch","Fleisch", "Hot and Cold"))+
    scale_x_discrete(limits=c("Soll\nBasis","Ist\nBasis","Soll\nIntervention","Ist\nIntervention"))+
    geom_text(aes(label=ifelse(tot>8,tot,"")),size = 5, position = position_stack(vjust = 0.5))+ #omit numbers to be shown smaller than 190
    # scale_colour_manual(values=colours)+ # try print all vegan numbers black others white, not working
    annotate("text",x=1:4,y=c(260,320,260,320),label=c(text$label2[2],text$label2[1],text$label2[2],text$label2[3]), size=6)+
    mytheme

# save plot
ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse/plots/actual_target_menu_offer 2017 180614 02egel.png",
       width = 14,
       height = 8,
       dpi=600,
       units="in",
       device="png")


##################
### test for differences
##################


leveneTest(tot_plan~label_content, data=df_t)
# ao=aov(tot_plan~label_content+shop_description, data=df_t)
# summary(ao)
