######
# Ingredience Analysis
######

#Status: 15.05.18 // egel

#load data => see script load data 2017
#load data => see script load data 2015 to 2017



###############
#### Target and Actual Comparison between 2015 - 2017 
###############
# timefilter between 11 to 14 and with locals! => see script load data 2015 to 2017
df_ist=read_delim("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse/all data over year 180424 03egel.csv",delim = ';') # see script load data 2015 to 2017 180518 egel
df_soll=read_excel("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse/soll 2015-2017 180420 02egel.xlsx")

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
df_ist=group_by(df,date,article_description,label_content,shop_description, meal_name)%>% summarise(tot_sold=n())

# Target: locals are not included
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
df_ist_t=group_by(df,shop_description) %>% mutate(pct=tot_sold/sum(tot_sold)) # calculate percentage pro shop_description

#target sold
df=group_by(df_tot, label_content, shop_description) %>% 
    summarise(tot_plan=sum(Geplant)) %>% 
    mutate(offer='Target', xlab=paste(offer, shop_description,sep="\n"))
df_soll_t=group_by(df,shop_description) %>% mutate(pct=tot_plan/sum(tot_plan))

# concate both datasets: df_ist_t and df_soll_t
df_t=bind_rows(df_soll_t,df_ist_t)

# plot meal content => higher selling numbers because of the time filter (=40 meals difference)
# explanation difference between target and actual is in total 2729. All sold locals are 2601. Thus 127 meals are thrown away?? 

# total over canteen => information for annotate
lbt= df_t %>%
    group_by(shop_description) %>%
    summarise(tot_p=sum(tot_plan, na.rm=T),tot_s = sum(tot_sold, na.rm=T))


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
### Target and Actual Comparison 2017 => Basis and Intervention
###############

# Actual: Locals are not included and Timefilter between 9 and 15 oclock
df=filter(df_7,!grepl("Local+",df_7$article_description))
df_ist=group_by(df,date,article_description,label_content,shop_description, condit)%>% summarise(tot_sold=n())

# Target: locals are not included
df_soll=read_excel("S:/pools/n/N-IUNR-nova-data/06_add_var/02_planung_verkaufszahlen/Mensa_geplant_180216_02egel.xlsx")
df_soll=rename(df_soll, article_description=Menülinie, date=Datum, shop_description= Ort)
df_soll$shop_description=str_replace(df_soll$shop_description,"Grüental","Grüental Mensa")
df_soll$shop_description=str_replace(df_soll$shop_description,"Vista","Vista Mensa")

# Merge both data frames
df_tot=left_join(df_ist,df_soll, by=c("date","article_description","shop_description"))

# prepare data for plot
# Actual sold
df=group_by(df_tot, label_content, condit) %>% 
    summarise(tot_sold=sum(tot_sold)) %>% 
    mutate(offer='Actual', xlab=paste(offer, condit,sep="\n")) # add two variables Actual and xlab
df_ist_t=group_by(df,condit) %>% mutate(pct=tot_sold/sum(tot_sold)) # calculate percentage pro shop_description

#target sold
df=group_by(df_tot, label_content, condit) %>% 
    summarise(tot_plan=sum(Geplant)) %>% 
    mutate(offer='Target', xlab=paste(offer, condit,sep="\n"))

df_soll_t=group_by(df,condit) %>% mutate(pct=tot_plan/sum(tot_plan))

# concate both datasets: df_ist_t and df_soll_t
df_t=rbind(df_soll_t,df_ist_t)

# plot meal content => higher selling numbers because of the time filter (=40 meals difference)

# total over canteen => information for annotate
lbt= df_t %>%
    group_by(condit) %>%
    summarise(tot_p=sum(tot_plan, na.rm=T),tot_s = sum(tot_sold, na.rm=T))

ggplot(df_t, aes(y=pct,x=xlab, fill=factor(label_content,levels=c("Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), label=paste0(round(df_t$pct*100,digits=0),"%"))) +
    geom_bar(stat="identity", position = "stack") +
    xlab("Target-Actual comparison per canteen") +
    ylab("Expected and sold meals in 2017")+
    ggtitle("note: locals are not included
         selling time between 9 a.m. until 3 p.m.
         locals meals in total: 2602")+
    guides(fill=guide_legend("meal content\n",
                             keywidth=.5,
                             keyheight=.5,
                             default.unit="inch"))+
    scale_y_continuous(labels = scales::percent)+
    scale_fill_manual(values = c("Vegan"="grey90", "Vegan+"="#80ccff", "Vegetarian" = "#c5b87c", "Meat" = "#fad60d","Hot and Cold"="#262626"))+
    scale_x_discrete(limits=c("Target\nBasis","Actual\nBasis","Target\nIntervention","Actual\nIntervention"))+
    geom_text(size = 5, position = position_stack(vjust = 0.5),col="#ffffff")+
    annotate("text",x=1:4,y=-.05,label=c("italic(n)==13359","italic(n)==11956","italic(n)==13102", "italic(n)==11676"), size=5,parse=T)+
    mytheme

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse/plots/actual_target_comp_condition 2017 180424 02egel.pdf",
       width = 14,
       height = 8,
       dpi=600,
       units="in",
       device="pdf")


#######
## protein ditribution
#######

# data
offer <- info1

# pro standort und ohne locals
offer <- filter(offer,!grepl("Local+",offer$article_description) & shop_description=="Grüental Mensa")
Hmisc::describe(offer$Tier_1)
Hmisc::describe(offer$Milchprodukt_1)
Hmisc::describe(offer$Vegi_Protein)
Hmisc::describe(offer$Ei)

# maybe melt data first for better analysis (at least menu infomation)
offer_long=melt(offer, id.vars = c("date","label_content"),measure.vars = c("meal_content_1","meal_content_2","meal_content_3","meal_content_4","meal_content_5","meal_content_6","meal_content_7"))
dfSummary(offer_long$value,max.distinct.values=260) # lots to do for a good analysis, however later on

# Cross tabs => see script .py better print options
ct=CrossTable(offer$Tier_1,offer$Milchprodukt_1,prop.t = F) # find a way to drop NAN's or na
print(ct)
ctable(offer$Tier_1, offer$Tier_2) # library summarytools


#load data with locals for local analysis
offer_l <-  info1
Hmisc::describe(offer_l$Tier_1)
Hmisc::describe(offer_l$Milchprodukt_1)
Hmisc::describe(offer_l$Vegi_Protein)
Hmisc::describe(offer_l$Ei)


##################
### test for differences
##################



leveneTest(tot_plan~label_content, data=df_t)
# ao=aov(tot_plan~label_content+shop_description, data=df_t)
# summary(ao)
