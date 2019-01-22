#### Further Plots


### Stand: 9.5.18 // egel


## Plot intervention vs. basis weeks => do not include the locals (or seperately)

# see script load data

## Plot only Basis with Weekdays => no locals-----------
# prepare data

df_ <- df_7 %>% 
    filter(condit=="Basis", !grepl("Local ", df_7$article_description) ) %>% 
    group_by(date, condit, label_content, semwk) %>% 
    summarise(tot_sold=n()) # %>%
    # mutate(dy=wday(date, label=T,abbr=T))

# to print date times need to convert to factor, after use reorder after date
df_$date2 <- factor(format(df_$date, format = '%a'))
df_$date3 <- factor(format(df_$date, format = '%d%b')) 
df_$date4 <- factor(paste(df_$date2,df_$date3, sep="\n\n"))

# change NA to string
df_$label_content2 <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

# oder factors use factor function with level in ggplot aes
#--------------
# plot data with bars for basis => no locals

setEPS()
postscript("over time base 180411 egel.eps",width = 18,
           height = 8)

ggplot(df_, aes(y=tot_sold,x=reorder(date4,date),fill=factor(label_content2, levels=c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), label=round(tot_sold,digits=0))) +
    geom_bar(stat="identity") +
    xlab("Basis: 'meat' week") +
    ylab("\nsold meals")+
    ggtitle("note: locals are not included")+
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
    mytheme2

dev.off()

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/analysen/plots/base_nolocal_180509_egel.pdf",
       width = 21,
       height = 10,
       dpi=600,
       units="in",
       device="pdf")

#--------------
# plot data with bars for basis => with locals
# prepare data

df_ <- df_7 %>% 
    filter(condit=="Basis" ) %>% 
    group_by(date, condit, label_content, semwk) %>% 
    summarise(tot_sold=n()) # %>%
# mutate(dy=wday(date, label=T,abbr=T))

# to print date times need to convert to factor, after use reorder after date
df_$date2 <- factor(format(df_$date, format = '%a'))
df_$date3 <- factor(format(df_$date, format = '%d%b')) 
df_$date4 <- factor(paste(df_$date2,df_$date3, sep="\n\n"))

# change NA to string
df_$label_content2 <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

# plot data => with locals

setEPS()
postscript("over time interv 180411 egel.eps",width = 18,
           height = 8)

ggplot(df_, aes(y=tot_sold,x=reorder(date4,date),fill=factor(label_content2, levels=c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), label=round(tot_sold,digits=0))) +
    geom_bar(stat="identity") +
    xlab("Basis: 'meat' week") +
    ylab("\nsold meals")+
    ggtitle("note: locals are included")+
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
    mytheme2

dev.off()

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/analysen/plots/base_180509_egel.pdf",
       width = 22,
       height = 10,
       dpi=600,
       units="in",
       device="pdf")


## Plot only Intervention with Weekdays => no locals---------
# prepare data

df_ <- df_7 %>% 
    filter(condit=="Intervention", !grepl("Local ", df_7$article_description) ) %>% 
    group_by(date, condit, label_content, semwk) %>% 
    summarise(tot_sold=n()) # %>%
# mutate(dy=wday(date, label=T,abbr=T))

# to print date times need to convert to factor, after use reorder after date
df_$date2 <- factor(format(df_$date, format = '%a'))
df_$date3 <- factor(format(df_$date, format = '%d%b')) 
df_$date4 <- factor(paste(df_$date2,df_$date3, sep="\n\n"))

# change NA to string
df_$label_content2 <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

# oder factors use factor function with level in ggplot aes
#--------------
# plot data with bars for intervention => no locals

setEPS()
postscript("over time base 180411 egel.eps",width = 18,
           height = 8)

ggplot(df_, aes(y=tot_sold,x=reorder(date4,date),fill=factor(label_content2, levels=c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), label=round(tot_sold,digits=0))) +
    geom_bar(stat="identity") +
    xlab("Intervention: 'vegetarian' week") +
    ylab("\nsold meals")+
    ggtitle("note: locals are not included")+
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
    mytheme2

dev.off()

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/analysen/plots/interv_nolocal_180509_egel.pdf",
       width = 21,
       height = 10,
       dpi=600,
       units="in",
       device="pdf")

#--------------
# plot data with bars for intervention => with locals

df_ <- df_7 %>% 
    filter(condit=="Intervention" ) %>% 
    group_by(date, condit, label_content, semwk) %>% 
    summarise(tot_sold=n()) # %>%
# mutate(dy=wday(date, label=T,abbr=T))

# to print date times need to convert to factor, after use reorder after date
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
    ggtitle("note: locals are included")+
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
    mytheme2

dev.off()

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/analysen/plots/interv_180509_egel.pdf",
       width = 22,
       height = 10,
       dpi=600,
       units="in",
       device="pdf")


# plot data with area-----
# use geom_area => important that u take only three variables (vegetarian, meat, hot and cold) otherwise plot dont look good

### plot all meal sales over time

df_ <- df_7
gsub("Vegan\\+","Vegan",df_$label_content)) # how do i get out the + sign, an easy way!?! => regex
df_$label_content <-  str_replace_all(df_7$label_content,c("Vegan " = "Vegetarian","Vegan\\+"="Vegetarian")) # problems with vegan+
tim <- group_by(df_,date,label_content)%>% summarise(tot_sold=n())
tim <- filter(tim,condit=="Basis")
tim <- filter(tim,condit=="Intervention")


ggplot(tim,aes(x=date,y=tot_sold,color=label_content))+geom_line()








