#### Plots between Basis and Intervention weeks----------


### Stand: 15.5.18 // egel


## Plot intervention vs. basis weeks => do not include the locals (or seperately)

# see script load data

## Plot only Basis with Weekdays => no locals#-----------
# prepare data 

df_ <- df_7 %>% 
    filter(condit=="Basis", !grepl("Local ", df_7$article_description) ) %>% #exclude locals
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

# Plot only Basis with Weekdays => with locals------------
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

# Plot only Intervention with Weekdays => with locals--------------

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


ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/analysen/plots/interv_180509_egel.pdf",
       width = 22,
       height = 10,
       dpi=600,
       units="in",
       device="pdf")

# plot data with area-----
### plot all meal sales over time for every canteen separately


## plot area canteen grüental => with locals----------
#change label content => take vegan and vegetarian together
df_ <- df_7
df_$label_content <- gsub("Vegan\\+","Vegan",df_$label_content) # seems to work!?
df_$label_content2 <-  str_replace_all(df_$label_content,c("Vegan" = "Vegetarian","Vegan"="Vegetarian"))

# change NA to string
df_$label_content2 <- ifelse(is.na(df_$label_content2),"Unknown",df_$label_content2)

# prepare data
df <- df_ %>%
    filter(shop_description == "Grüental Mensa") %>%
    group_by(date, label_content2, shop_description, condit) %>%
    summarise(tot_sold=n())

# to print date times need to convert to factor, after use reorder after date
df$date2 <- factor(format(df$date, format = '%a'))
df$date3 <- factor(format(df$date, format = '%d%b')) 
df$dat_4 <- paste(df$date2,df_$date3, sep="\n\n")

# plotting without NA resp. Unknown => sum(df[df$label_content2=="Unknown",]$tot_sold)
ggplot(df, aes(y=tot_sold,x=date,fill=factor(label_content2, levels=c("Meat","Vegetarian","Hot and Cold")), label=round(tot_sold,digits=0))) +
    geom_area(stat="identity",position=position_dodge(width = 1))+
    xlab("Autumn semester weeks for canteen 'Grüental'") +
    ylab("\nsold meals")+
    ggtitle("note: locals are included,
          93 cases are excluded due to missing information
          total meal sold: 13863")+
    guides(fill = guide_legend(
        "meal content\n",
        keywidth = .5,
        keyheight = .5,
        default.unit ="inch"))+
    scale_fill_manual(values = c("Hot and Cold"="#e5e5e5","Meat" = "#c5b87c","Vegetarian" = "#008099","Vegan+"="#6619e6","Vegan"="#fad60d","Unknown" = "grey60"),
                      breaks = c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold"),
                      labels = c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold"))+
    # geom_text(size = 5, position = position_stack(vjust = 0.5))+
    #     annotate(
    #         "text",x = 1:12, y = -.02, label = c("italic(n)==2120","italic(n)==2051","italic(n)==2028","italic(n)==2082","italic(n)==2283","italic(n)==2155","italic(n)==2309","italic(n)==2199","italic(n)==2289","italic(n)==2353","italic(n)==2225","italic(n)==2083"),parse=T,size=5) +
    mytheme


ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/angebot analysen/plots/area_grüen_180515_egel.pdf",
       width = 30,
       height = 20,
       dpi=600,
       units="in",
       device="pdf")

## plot area canteen vista => with locals----------
#change label content => take vegan and vegetarian together
df_ <- df_7
df_$label_content <- gsub("Vegan\\+","Vegan",df_$label_content) # seems to work!?
df_$label_content2 <-  str_replace_all(df_$label_content,c("Vegan" = "Vegetarian","Vegan"="Vegetarian"))

# change NA to string
df_$label_content2 <- ifelse(is.na(df_$label_content2),"Unknown",df_$label_content2)

# prepare data
df <- df_ %>%
    filter(shop_description == "Vista Mensa") %>%
    group_by(date, label_content2, shop_description, condit) %>%
    summarise(tot_sold=n())

# to print date times need to convert to factor, after use reorder after date
df$date2 <- factor(format(df$date, format = '%a'))
df$date3 <- factor(format(df$date, format = '%d%b')) 
df$dat_4 <- paste(df$date2,df_$date3, sep="\n\n")

# plotting without NA resp. Unknown => sum(df[df$label_content2=="Unknown",]$tot_sold)
ggplot(df, aes(y=tot_sold,x=date,fill=factor(label_content2, levels=c("Meat","Vegetarian","Hot and Cold")), label=round(tot_sold,digits=0))) +
    geom_area(stat="identity",position=position_dodge(width = 1))+
    xlab("Autumn semester weeks for canteen 'Vista'") +
    ylab("\nsold meals")+
    ggtitle("note: locals are included,
          48 cases are excluded due to missing information
          total meal sold: 12230")+
    guides(fill = guide_legend(
        "meal content\n",
        keywidth = .5,
        keyheight = .5,
        default.unit ="inch"))+
    scale_fill_manual(values = c("Hot and Cold"="#e5e5e5","Meat" = "#c5b87c","Vegetarian" = "#008099","Vegan+"="#6619e6","Vegan"="#fad60d","Unknown" = "grey60"),
                      breaks = c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold"),
                      labels = c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold"))+
    # geom_text(size = 5, position = position_stack(vjust = 0.5))+
    #     annotate(
    #         "text",x = 1:12, y = -.02, label = c("italic(n)==2120","italic(n)==2051","italic(n)==2028","italic(n)==2082","italic(n)==2283","italic(n)==2155","italic(n)==2309","italic(n)==2199","italic(n)==2289","italic(n)==2353","italic(n)==2225","italic(n)==2083"),parse=T,size=5) +
    mytheme

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/angebot analysen/plots/area_vista_180515_egel.pdf",
       width = 30,
       height = 20,
       dpi=600,
       units="in",
       device="pdf")









