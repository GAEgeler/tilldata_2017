######
# Ingredience Analysis
######

#Status: 15.15.18 // egel

#######
## protein ditribution
#######

# data
offer <- info1

# per canteen, per cycle and without locals and buffet (hot and cold) 
offer <- filter(offer,!grepl("Local+",offer$article_description) & shop_description=="Grüental Mensa" & cycle == 1 & !offer$label_content == "Hot and Cold")
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

##plot data per canteen and without locals---------
# offer is per canteen and per cycle the same
# how to plot a useful grafic??

off <- offer %>%
    select(date, article_description,label_content,
           animal = Tier_1,
           milk = Milchprodukt_1,
           veg = Vegi_Protein,
           egg = Ei)
off <- filter(off, !article_description == "Hot and Cold")

off2 <- melt(off, id.vars = c("date","label_content"), measure.vars = c("animal","milk","veg","egg")) # put into long format fot plotting

# define colors
# first way => rainbow palette
set.seed(2)
cols = rainbow(30, s=.6, v=.9)[sample(1:30,30)] # take 31 colors randomly form the rainbow palette
# second way => http://tools.medialab.sciences-po.fr/iwanthue/
cols_ = c("#cf4d2b",
          "#49c35e",
          "#6dad32",
          "#5f69d7",
          "#b4c133",
          "#ce56b8",
          "#438434",
          "#cf3384",
          "#58c699",
          "#db435a",
          "#42c0c7",
          "#dc852f",
          "#7493e1",
          "#dcae3f",
          "#844ea3",
          "#a5a63a",
          "#5566a6",
          "#a48029",
          "#c88dd6",
          "#8ab768",
          "#e66897",
          "#3b8963",
          "#9c4a77",
          "#696e28",
          "#50a4d3",
          "#995c2c",
          "#dd89a3",
          "#bda769",
          "#ad4b4f",
          "#e48e6d")

# off3 <-  as_tibble(cbind(cols_,unique(off2$value))) %>% # concate arrays
#     rename(value=V1) %>% # rename variable
#     left_join(off2,tot_col, by="value")
# 
# off3$value <- str_replace_na(off2$value)# merge information of color 
# 
# ggplot(off2_) + 
#     geom_bar(aes(x=variable, y=..count.., fill=value), stat="count")+
#     geom_text(aes(x=variable, y=y,label=value), position=position_dodge(width=0.9))
#     scale_fill_manual(values=setNames(off3$cols_,off3$value))+
    
##################
# plot overall protein source -----
##################

off4 <- off2 %>%
    filter(!is.na(value)) %>%
    group_by(variable, value) %>%
    summarise(tot = n()) %>%
    mutate(pct_prot = tot/sum(tot)*100)

ggplot(off4, aes(x=variable, y=tot, fill=value,label=paste(off4$value,paste(round(off4$pct_prot, digits = 0),"%",sep=""),sep = " ")))+
    geom_bar(position = "stack", stat="identity")+
    geom_text(position="stack", vjust=1, size=4.6 )+
    annotate(
        "text",x = 1:4, y = -.1, label = c("italic(n) == 46","italic(n) == 54","italic(n) == 27","italic(n) == 30"),size = 5,parse=T)+ # Hmisc::describe(offer$Tier_1)
    scale_y_continuous(limits = c(0,60), breaks= seq(0,60,10))+
    scale_x_discrete(labels=c("animals","milk products","vegetarian proteins","egg products"))+
    scale_fill_manual(values = cols_)+
    xlab("sources of protein")+
    ylab("counts")+
    ggtitle("note: locals are not included
          90 meals were offered in total
          multiple entries are possible")+
    guides(fill=F)+
#     guides(fill = guide_legend(
#         "ingrediences\n",
#         keywidth = .5,
#         keyheight = .5,
#         default.unit ="inch"))+
    annotate("text",x=4, y=70, label="Date: 16.05")+
      mytheme

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/angebot analysen/plots/meal_offer 180516 egel.pdf",
       width = 20,
       height = 12,
       dpi=600,
       units="in",
       device="pdf")

#################
#### plot per meal label ---- 
# (however makes not really sense)
off5 <- off2 %>%
    filter(!is.na(value)) %>%
    group_by(label_content, value) %>%
    summarise(tot = n()) %>%
    mutate(pct_label = tot/sum(tot)*100)

ggplot(off5, aes(x=label_content, y=tot, fill=value))+
    geom_bar(position = "stack", stat="identity")+
    geom_text(aes(label=paste(value,paste(round(pct_label, digits = 0),"%",sep=""),sep = " ")), position="stack", vjust=1, size=4.6)+
    scale_y_continuous(limits = c(0,100), breaks= seq(0,100,20))+
    # scale_x_discrete(labels=c("meat","vegan","vegan+","vegetarian"))+
    scale_fill_manual(values = cols_)+
    xlab("meal content")+
    ylab("counts")+
    ggtitle("note: locals are not included
            90 meals were offered in total")+
    guides(fill=F)+
    #     guides(fill = guide_legend(
    #         "ingrediences\n",
    #         keywidth = .5,
    #         keyheight = .5,
    #         default.unit ="inch"))+
    mytheme


    
#per canteen and only locals 
offer_l <-  info1
offer_l <- filter(offer_l, shop_description=="Grüental Mensa")
Hmisc::describe(offer_l$Tier_1)
Hmisc::describe(offer_l$Milchprodukt_1)
Hmisc::describe(offer_l$Vegi_Protein)
Hmisc::describe(offer_l$Ei)
##################

##################
# plot per protein source----------
##################

## anmial ------
# prepare data
off_ <- off %>%
    filter(label_content == "Fleisch") %>% # filter only meat
    melt(id.vars = c("date","article_description","animal"), measure.vars = c("milk","veg","egg")) %>% # put it into long format for plotting: is this necessari?
    filter(!is.na(value)) %>% # exclude na in value
    group_by(animal, value) %>% # group it to count observations
    summarise(tot = n())

off_ <- off_ %>%
    group_by(animal) %>%
    mutate(pct_prot = tot/sum(tot)*100) # calculate percentage

#plot => annotate text is not working with geom_text, i guess problems with the y aesthetics

ggplot(off_, aes(x=animal, y=tot, fill=value,label=paste(value,paste(round(pct_prot, digits = 0),"%",sep=""),sep = " ")))+
    geom_bar(position = "stack", stat="identity")+
    scale_y_continuous(limits = c(0,30), breaks= seq(0,30,5))+
    # scale_x_discrete(labels=c("animals","milk products","vegetarian proteins","egg products"))+
    scale_fill_manual(values = cols_)+
    xlab("animal species")+
    ylab("counts")+
    ggtitle("note: locals are not included
          45 meat meals were offered in total")+ # why 46 and not 45?
    guides(fill=F)+
    #     guides(fill = guide_legend(
    #         "ingrediences\n",
    #         keywidth = .5,
    #         keyheight = .5,
    #         default.unit ="inch"))+
    geom_text(position="stack", vjust=1, size=4.6, check_overlap = TRUE)+ # check_overlap = TRUE provides a simple way to avoid overplotting of labels: labels that would otherwise overlap are omitted
    annotate(
         "text",x = 1:6, y = -.1, label = c("italic(n) == 6","italic(n) == 4","italic(n) == 6","italic(n) == 8","italic(n) == 20","italic(n) == 2"),size = 5,parse=T)+ # Hmisc::describe(offer$Tier_1)
    mytheme


ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/angebot analysen/plots/meat_offer 180516 egel.pdf",
       width = 20,
       height = 12,
       dpi=600,
       units="in",
       device="pdf")

# milk products ------

# prepare data
off_ <- off %>%
    filter(label_content == "Vegetarisch") %>% # filter vegetarian
    melt(id.vars = c("date","article_description"), measure.vars = c("milk","veg","egg")) %>% # put it into long format for plotting: is this necessari?
    filter(!is.na(value)) %>% # exclude na in value
    group_by(animal, value) %>% # group it to count observations
    summarise(tot = n())

off_ <- off_ %>%
    group_by(animal) %>%
    mutate(pct_prot = tot/sum(tot)*100) # calculate percentage

#plot => annotate text is not working with geom_text, i guess problems with the y aesthetics

ggplot(off_, aes(x=animal, y=tot, fill=value))+
    geom_bar(position = "stack", stat="identity")+
    scale_y_continuous(limits = c(0,30), breaks= seq(0,30,5))+
    # scale_x_discrete(labels=c("animals","milk products","vegetarian proteins","egg products"))+
    scale_fill_manual(values = cols_)+
    xlab("animal species")+
    ylab("counts")+
    ggtitle("note: locals are not included
          46 meat meals were offered in total")+ # why 46 and not 45? => reason Moussaka was offered vegetarian, however in the recepie meat was included => changed new version of dokumentation
    guides(fill=F)+
    #     guides(fill = guide_legend(
    #         "ingrediences\n",
    #         keywidth = .5,
    #         keyheight = .5,
    #         default.unit ="inch"))+
    geom_text(aes(label=paste(value,paste(round(pct_prot, digits = 0),"%",sep=""),sep = " ")), position="stack", vjust=1, size=4.6, check_overlap = TRUE)+ # check_overlap = TRUE provides a simple way to avoid overplotting of labels: labels that would otherwise overlap are omitted
    annotate(
        "text",x = 1:6, y = -.1, label = c("italic(n) == 6","italic(n) == 4","italic(n) == 6","italic(n) == 8","italic(n) == 20","italic(n) == 2"),size = 5,parse=T)+ # Hmisc::describe(offer$Tier_1)
    mytheme


ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/angebot analysen/plots/meat_offer 180516 egel.pdf",
       width = 20,
       height = 12,
       dpi=600,
       units="in",
       device="pdf")


# vegetarian proteins --------

# egg products ----------




