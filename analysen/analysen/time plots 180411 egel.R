#### Further Plots


### Stand: 17.04.18 // egel


## Plot intervention vs. basis weeks => problems with x-Axis look for solutions

see: https://stackoverflow.com/questions/44616530/axis-labels-on-two-lines-with-nested-x-variables-year-below-months/44616739#44616739
see: https://stackoverflow.com/questions/20571306/multi-row-x-axis-labels-in-ggplot-line-chart

## Plot only Basis with Weekdays

df_ <- df_7 %>% 
    filter(condit=="Basis" ) %>% 
    group_by(date, condit, label_content, semwk) %>% 
    summarise(tot_sold=n()) # %>%
    # mutate(dy=wday(date, label=T,abbr=T))

# to print date times need to convert to factor, after use reorder after date
df_$date <- as.POSIXct(df_$date, tz="CET",format="%Y-%m-%d")
df_$date2 <- factor(format(df_$date, format = '%a'))
df_$date3 <- factor(format(df_$date, format = '%d%b')) 
df_$date4 <- factor(paste(df_$date2,df_$date3, sep="\n\n"))

# change NA to string

df_$label_content2 <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

# oder factors use factor function with level in ggplot aes

#--------------
# theme

mytheme <- theme_bw()+ # definve theme for plot
    theme(plot.title = element_text(size = 20, face = "bold")) +
    theme(text = element_text(size=20),axis.title.y = element_text(size = 20)) +
    theme(axis.title.x = element_text(size = 20)) +
    theme(legend.text = element_text(size = 20),legend.title = element_text(size =20)) +
    theme(axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)))+
    theme(axis.title.x = element_text(size = 20,  margin = margin(t = 20, r = 0, b = 0, l = 0)))

##--------------
# plot data with bars for basis

setEPS()
postscript("over time base 180411 egel.eps",width = 20,
           height = 8)

ggplot(df_, aes(y=tot_sold,x=reorder(date4,date),fill=factor(label_content2, levels=c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), label=round(tot_sold,digits=0))) +
    geom_bar(stat="identity") +
    xlab("Basis: 'meat' week") +
    ylab("\nsold meals")+
    guides(fill = guide_legend(
        "meal content\n",
        keywidth = .5,
        keyheight = .5,
        default.unit ="inch"))+
    scale_fill_manual(values = c("Hot and Cold"="#e5e5e5","Meat" = "#c5b87c","Vegetarian" = "#008099","Vegan+"="#6619e6","Vegan"="#fad60d","Unknown" = "grey60"),
                      breaks = c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold"),
                      labels = c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold"))+
    geom_text(size = 5, position = position_stack(vjust = 0.5))+
    mytheme

dev.off()

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/analysen/plots/over time base 180412 egel.pdf",
       width = 20,
       height = 8,
       dpi=600,
       units="in",
       device="pdf")

#--------------
# plot data with bars for basis

df_ <- df_7 %>% 
    filter(condit=="Intervention" ) %>% 
    group_by(date, condit, label_content, semwk) %>% 
    summarise(tot_sold=n()) # %>%
# mutate(dy=wday(date, label=T,abbr=T))

# to print date times need to convert to factor, after use reorder after date
df_$date <- as.POSIXct(df_$date, tz="CET",format="%Y-%m-%d")
df_$date2 <- factor(format(df_$date, format = '%a'))
df_$date3 <- factor(format(df_$date, format = '%d%b')) 
df_$date4 <- factor(paste(df_$date2,df_$date3, sep="\n\n"))

# change NA to string

df_$label_content2 <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)


setEPS()
postscript("over time interv 180411 egel.eps",width = 18,
           height = 8)

ggplot(df_, aes(y=tot_sold,x=reorder(date4,date),fill=factor(label_content2, levels=c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), label=round(tot_sold,digits=0))) +
    geom_bar(stat="identity") +
    xlab("Intervention: 'vegetarian' week") +
    ylab("\nsold meals")+
    guides(fill = guide_legend(
        "meal content\n",
        keywidth = .5,
        keyheight = .5,
        default.unit ="inch"))+
    scale_fill_manual(values = c("Hot and Cold"="#e5e5e5","Meat" = "#c5b87c","Vegetarian" = "#008099","Vegan+"="#6619e6","Vegan"="#fad60d","Unknown" = "grey60"),
                      breaks = c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold"),
                      labels = c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold"))+
    geom_text(size = 5, position = position_stack(vjust = 0.5))+
    #     annotate(
    #         "text",x = 1:12, y = -.02, label = c("italic(n)==2120","italic(n)==2051","italic(n)==2028","italic(n)==2082","italic(n)==2283","italic(n)==2155","italic(n)==2309","italic(n)==2199","italic(n)==2289","italic(n)==2353","italic(n)==2225","italic(n)==2083"),parse=T,size=5) +
    mytheme

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/analysen/plots/over time interv 180412 egel.pdf",
       width = 20,
       height = 8,
       dpi=600,
       units="in",
       device="pdf")


dev.off()



# plot data with area
# use geom_area


### plot all meal sales over time

tim=group_by(df_7,date,label_content)%>% summarise(tot_sold=n())
tim=filter(tim,condit=="Basis")
tim=filter(tim,condit=="Intervention")


ggplot(tim,aes(x=date,y=tot_sold,color=label_content))+geom_line()








