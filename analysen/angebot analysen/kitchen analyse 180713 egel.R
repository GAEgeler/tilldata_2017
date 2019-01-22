#####
### menu-line: kitchen
#####


# status: 13.7.18 // egel


# load data see load data 2017


# prepare data

df <- df_7 %>%
    filter(grepl("Kitchen", df_7$article_description)) %>%
    select(article_description, date, week, condit, shop_description, label_content) %>%
    group_by(condit, label_content, week) %>%
    summarise(tot_sold = n())


df_ <- df %>%
    group_by(condit, week) %>%
    mutate(pct = tot_sold/sum(tot_sold))

# ranem NA to unknown
df_$label_content <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

# annotation for group size
text <- group_by(df_, week) %>% summarise(tot=sum(tot_sold)) %>%
    mutate(label = "italic(n)") %>%
    mutate(label2=paste(label,tot,sep="=="))

# define x-lab
df_$xlab <- paste(df_$week,df_$condit,sep = "\n")

# add date of creation
st <- strftime(today(), format = "%d.%m.%y")

## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unknown" = "black","Vegan"="grey90", "Vegan+"="#80ccff", "Vegetarian" = "#c5b87c", "Meat" = "#fad60d","Hot and Cold"="#4c4848")

df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

# plot
p <- ggplot(df_, aes(y=pct,x=reorder(as.factor(xlab)),fill=factor(label_content, c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), color=label_color)) + 
    geom_bar(stat="identity", position = "fill", color=NA, width = .6) +
    #ggtitle("Verkaufte Menüs: 3. + 4. HSW\n") +
    xlab("Herbstsemester (Kalenderwochen 40 bis 51)") +
    # xlab(cat('"winter semester weeks (Basis: "','meat','" week, Intervention: "','vegetarian','" week"'))+
    ylab("\nVerkaufte Kitchen-Menüs in Prozent")+
    guides(fill = guide_legend("Menü-Inhalt"),
           color=F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Pflanzlich (Fleischersatz)","Pflanzlich","Vegetarisch","Fleisch oder Fisch","Hot and Cold"))+
    scale_color_manual(values = levels(df_$label_color))+
    geom_text(aes(label=ifelse(pct*100>1.5,paste0(round(pct*100, digits=0),"%"),"")),size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:12, y = 1.03, label = text$label2,parse=T, size=8) + # why so big differences to the first version
    mytheme

p + labs(caption = "Quelle: Kassendaten und Dokumentation SV Schweiz", subtitles = paste("Stand: ", st))


ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/angebot analysen/plots/kitchen_analysis_180614_03egel.pdf",
       width = 25, 
       height = 14, 
       dpi = 600, 
       device = cairo_pdf)

# for sv protocol
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/12_work_pack/wpIII.1_menu_choice/05_besprechungen/180706_sv_fm_meeting/plots fürs protokoll/kitchen_180713_egel.pdf",
       width = 25,
       height = 14,
       dpi = 600,
       units="in",
       device= cairo_pdf)



#### statistical analyses: differ sellings of meal line "Kitchen" between weeks?
# pay attention which dataset you generate for comparisions!!
# test first for independent homogenicy
df_1 <- df_ %>%
    group_by(week) %>%
    summarise(tot_sold = sum(tot_sold))

leveneTest(df_1$tot_sold, as.factor(df_1$week))
ao <- aov(df_1$tot_sold ~ df_1$week)
summary.lm(ao)
TukeyHSD(ao)


# difference in selling data between the intervention and basisweek
df_1 <- df_ %>%
    group_by(condit, week) %>%
    summarise(tot_sold = sum(tot_sold))

leveneTest(df_1$tot_sold, df_1$condit)
ao <- aov(df_1$tot_sold ~ df_1$condit)
summary(ao)
TukeyHSD(ao)

# make the intervention a difference
# take vegan and vegan+ to vegetarian for comparison
df_[grep("Vegan$",df_$label_content),]$label_content <- "Vegetarian"
df_[grep("Vegan\\+", df_$label_content),]$label_content <- "Vegetarian"

df_1 <- df_ %>%
    group_by(condit,label_content) %>%
    summarise(tot_sold = sum(tot_sold))

ao <- aov(df_1$tot_sold ~ df_1$label_content)
summary.lm(ao)
TukeyHSD(ao)
