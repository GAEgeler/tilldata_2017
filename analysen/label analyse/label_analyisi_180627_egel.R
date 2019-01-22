#####
# label analyses
#####

# status: 27.06.18 // egel


# load data: see load daily data 2015-2017: second Part!

#### plot over the years the sellings per menu-line-----

## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unknown" = "black","Vegan"="#80ccff", "Vegetarian" = "#c5b87c", "Meat" = "#fad60d","Hot and Cold"="#4c4848")

df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

# define annotation
text <- group_by(df, year, shop_description) %>% summarise(tot=sum(tot_sold)) %>%
    mutate(tot2=format(tot, big.mark = "'", scientific = F)) %>% # add thousand seperator
    mutate(label = "n") %>%
    mutate(label2=paste(label,tot2,sep=" = "))

# plot data: in german----------
ggplot(df_, aes(y=tot_sold,x=xlab, fill=factor(variable2,levels=c("Unknown","Vegan","Vegetarian","Meat","Hot and Cold")), color = label_color)) +
    geom_bar(stat="identity", position = "stack", color=NA, width = .6) +
    #ggtitle("Verkaufte Menüs: 3. + 4. HSW\n") +
    xlab("Herbstsemester (Kalenderwochen 40 bis 51)") +
    ylab("Verkaufte Menüs pro Herbstsemester und Mensa")+
    guides(fill=F, 
           color=F)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$names,
                      labels = c("Unbekannt","Pflanzlich","Vegetarisch","Fleisch", "Hot and Cold"))+
    scale_color_manual(values = levels(df_$label_color))+
    geom_text(aes(label=ifelse(pct*100>2,paste0(round(pct*100, digits=0),"%"),"")), size = 5, position = position_stack(vjust = 0.5))+ # omit 1% annotation
    annotate("text",x = 1:6, y = 14700, label = text$label2, size=6)+
    mytheme



ggplot()