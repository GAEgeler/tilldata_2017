#######
## content analysis
#######

# status: 23.05.18 // egel


# load data 2017 see script

### plot data over both cycles and with label content------
#prepare data for plot

df_ <- group_by(df_7, condit ,semwk, label_content )%>% summarise(tot_sold=n())

df_ <- df_ %>% 
    group_by(semwk,condit) %>% # give in variable, you want to calculate percentage
    mutate(pct=(tot_sold/sum(tot_sold)))

# ranem NA to unknown
df_$label_content <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

# annotation for group size
text <- group_by(df_7, semwk) %>% summarise(tot=n()) %>%
    mutate(label = "italic(n)") %>%
    mutate(label2=paste(label,tot,sep="=="))

# plot
ggplot(df_, aes(y=pct,x=reorder(as.factor(semwk)),fill=factor(label_content, c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")))) + 
    geom_bar(stat="identity", position = "fill") +
    #ggtitle("Verkaufte Menüs: 3. + 4. HSW\n") +
    xlab("Herbstsemesterwochen") +
    # xlab(cat('"winter semester weeks (Basis: "','meat','" week, Intervention: "','vegetarian','" week"'))+
    ylab("\nVerkaufte Menüs in Prozent")+
    guides(fill = guide_legend(
        "Menü-Inhalt\n",
        keywidth = .5,
        keyheight = .5,
        default.unit ="inch"))+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = c("Hot and Cold"="#e5e5e5","Meat" = "#c5b87c","Vegetarian" = "#008099","Vegan+"="#6619e6","Vegan"="#fad60d", "Unknown"="grey50"),
                      breaks = c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold"),
                      labels = c("Unbekannt","Pflanzlich","Pflanzlich+","Vegetarisch","Fleisch","Hot and Cold"))+
    geom_text(aes(label=ifelse(pct*100>1,paste0(round(pct*100, digits=0),"%"),"")),size = 5, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:12, y = -.02, label = text$label2,parse=T, size=5) + # why so big differences to the first version
    mytheme

# save plot
ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/angebot analysen/plots/content_analysis_180523_egel.png",
       width = 20, 
       height = 10, 
       dpi = 600, 
       device = "png")



### statistical differences between basis and intervention-------------
CrossTable(df_7$semwk,df_7$label_content, chisq = T)
chisq.test(df_7$semwk, df_7$label_content)$stdres

adjp <- 0.05/(length(unique(df_7$semwk))*length(unique(df_7$label_content)))# adjusted p-value
qnorm(adjp, lower.tail = F) # critical z-value from websit:. +- 3.39 










# try another time----------------
df_ <- filter(df, week >= 40 & week <=51) # only week between 40 and 51
df_ <- group_by(df_,year,week,semwk) %>% summarise(tot_sold=sum(value))

ggplot(df_, aes(y=tot_sold,x=as.factor(semwk), color=as.character(year), group=year))+
    geom_line(size=1.3) +
    #ggtitle("Verkaufte Menüs: 3. + 4. HSW\n") +
    xlab("winter semester weeks") +
    ylab("sold meals per semester week")+
    guides(fill=guide_legend("years\n",
                             keywidth=.5,
                             keyheight=.5,
                             default.unit="inch"))+
    scale_color_manual(values = c("2015"="#fad60d","2016" = "#c5b87c","2017" = "#008099"),
                       name="years\n")+
    mytheme