#######
## content analysis
#######

# status: 16.06.18 // egel


# load data 2017 see script

### plot data over both cycles and with label content------
#prepare data for plot

df_ <- group_by(df_7, condit ,week, label_content )%>% summarise(tot_sold=n())

df_ <- df_ %>% 
    group_by(week,condit) %>% # give in variable, you want to calculate percentage
    mutate(pct=(tot_sold/sum(tot_sold)))

# ranem NA to unknown
df_$label_content <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

# annotation for group size
text <- group_by(df_7, week) %>% summarise(tot=n()) %>%
    mutate(label = "italic(n)") %>%
    mutate(label2=paste(label,tot,sep="=="))

# define x-lab
df_$xlab <- paste(df_$week,df_$condit,sep = "\n\n")


## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unknown" = "black","Vegan"="grey90", "Vegan+"="#80ccff", "Vegetarian" = "#c5b87c", "Meat" = "#fad60d","Hot and Cold"="#4c4848")

df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                     function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

# plot
ggplot(df_, aes(y=pct,x=reorder(as.factor(xlab)),fill=factor(label_content, c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), color=label_color)) + 
    geom_bar(stat="identity", position = "fill", color=NA) +
    #ggtitle("Verkaufte Menüs: 3. + 4. HSW\n") +
    xlab("Herbstsemester (Kalenderwochen 40 bis 51)") +
    # xlab(cat('"winter semester weeks (Basis: "','meat','" week, Intervention: "','vegetarian','" week"'))+
    ylab("\nVerkaufte Menüs in Prozent")+
    guides(fill = guide_legend(
                "Menü-Inhalt\n",
                 keywidth = .5,
                 keyheight = .5,
                 default.unit ="inch"),
            color=F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Pflanzlich","Pflanzlich+","Vegetarisch","Fleisch","Hot and Cold"))+
    scale_color_manual(values = levels(df_$label_color))+
    geom_text(aes(label=ifelse(pct*100>1,paste0(round(pct*100, digits=0),"%"),"")),size = 5, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:12, y = 1.03, label = text$label2,parse=T, size=5) + # why so big differences to the first version
    mytheme

# save plot
ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/angebot analysen/plots/content_analysis_180614_03egel.png",
       width = 20, 
       height = 10, 
       dpi = 600, 
       device = "png")

######
## save plots for presantation somewhere else
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/12_work_pack/wpIII.1_menu_choice/05_besprechungen/180706_sv_fm_meeting/slides template Ordner/plots/content_analysis_180614_03egel.png",
       width= 20, 
       height = 10,
       dpi=600,
       units="in",
       type="cairo-png")

### statistical differences between semwk and label content-------------
CrossTable(df_7$semwk,df_7$label_content, chisq = T)
chisq.test(df_7$semwk, df_7$label_content)$stdres

adjp <- 0.05/(length(unique(df_7$semwk))*length(unique(df_7$label_content)))# adjusted p-value
qnorm(adjp, lower.tail = F) # critical z-value from websit:. +- 3.39 


### statistical differences in selling data between the semester weeks-----------
# selling differences between days?
sell_dat <- df_7 %>%
    mutate(day=wday(date, label = T)) %>% 
    group_by(day, semwk) %>%
    summarise(tot_sold=n())

leveneTest(sell_dat$tot_sold, as.factor(sell_dat$semwk))# are varianzes homogene
ao=(aov(sell_dat$tot_sold~as.factor(sell_dat$semwk))) # is this the right test?
summary(ao)
TukeyHSD(ao) # no selling differences between days


# selling differences between weeks?
sell_dat <- df_7 %>%
    mutate(day=wday(date, label = T)) %>% 
    group_by(day, semwk, condit) %>%
    summarise(tot_sold=n())

leveneTest(sell_dat$tot_sold, sell_dat$condit)# are varianzes homogene
ao=(aov(sell_dat$tot_sold ~ sell_dat$condit : sell_dat$day)) # is this the right test?
summary(ao)
TukeyHSD(ao) # no selling differences between same weekdays and condition (e.g. Monday Basis vs. Monday Intervention etc.)

