#####
# Local offer analyses
#####

# status 15.5.18 // egel

# for further information see https://rstudio-pubs-static.s3.amazonaws.com/74603_76cd14d5983f47408fdf0b323550b846.html

# load data => see script


#####-----
# subset all locals
#####---

df <- filter(df_7, grepl("Local+",df_7$article_description))
df_ <- group_by(df,date,label_content,article_description,meal_name,shop_description, condit) %>% summarise(tot=n())

###--------------
# describe data of locals
###--------------

Hmisc::describe(df_$label_content) # 64% Meat, and 36% Vegetarian
CrossTable(df_$label_content,df_$condit, chisq = T) # are there differences in local menu offer between Basis and Intervention weeks => NO 

# subset all sold meals
sold=group_by(df_, label_content, condit) %>% summarise(tot=sum(tot))

# check if differ between condition
boxplot(df_$tot~df_$label_content + df_$condit)
leveneTest(df_$tot~df_$label_content) # nicht signifikant, d.h. homogene gruppen
aov_t <- aov(df_$tot~df_$label_content:df_$condit)
summary(aov_t)
tuk <- TukeyHSD(aov_t) # print tuk to see result
plot(tuk)
# more sells of meat meals in Intervention and more sells of vegetarian in Basis


## Plot locals only Basis (axis with weekdays)###--------------
df_ <- df %>% 
    filter(condit=="Basis" ) %>%
    group_by(date,label_content,semwk) %>% summarise(tot_sold=n())

# to print date times need to convert to factor, after use reorder after date
df_$date2 <- factor(format(df_$date, format = '%a'))
df_$date3 <- factor(format(df_$date, format = '%d%b')) 
df_$date4 <- factor(paste(df_$date2,df_$date3, sep="\n\n"))

# change NA to string
df_$label_content2 <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

ggplot(df_, aes(y=tot_sold,x=reorder(date4,date),fill=factor(label_content2, levels=c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), label=tot_sold)) +
    geom_bar(stat="identity") +
    xlab("Basis: 'meat' weeks") +
    ylab("\nsold additional meals")+
    ggtitle("note: 1222 sold locals in total")+ # group_by(df,condit) %>% summarise(tot=n())
    guides(fill = guide_legend(
        "meal content\n",
        keywidth = .5,
        keyheight = .5,
        default.unit ="inch"))+
    scale_fill_manual(values = c("Hot and Cold"="#e5e5e5","Meat" = "#c5b87c","Vegetarian" = "#008099","Vegan+"="#6619e6","Vegan"="#fad60d","Unknown" = "grey60"),
                      breaks = c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold"),
                      labels = c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold"))+
    geom_text(size = 5, position = position_stack(vjust = 0.5))+
    #scale_x_discrete(label=abbreviate)+
    scale_y_continuous(breaks = seq(0,200,50), limits = c(0, 150))+
    mytheme2 # => see script mytheme

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/angebot analysen/plots/locals basis 180518 egel.pdf",
       width = 20,
       height = 12,
       dpi=600,
       units="in",
       device="pdf") 


## Plot loacls only for Intervention (axis with weekdays)###--------------
## prepare data
df_ <- df %>% 
    filter(condit=="Intervention" ) %>%
    group_by(date,label_content,semwk) %>% summarise(tot_sold=n())

# to print date times need to convert to factor, after use reorder after date
df_$date2 <- factor(format(df_$date, format = '%a'))
df_$date3 <- factor(format(df_$date, format = '%d%b')) 
df_$date4 <- factor(paste(df_$date2,df_$date3, sep="\n\n"))

# change NA to string Unknown
df_$label_content2 <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

ggplot(df_, aes(y=tot_sold,x=reorder(date4,date),fill=factor(label_content2, levels=c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), label=round(tot_sold,digits=0))) +
    geom_bar(stat="identity") +
    xlab("Intervention: 'vegetarian' weeks") +
    ylab("\nsold additional (resp. locals) meals")+
    ggtitle("note: 1380 sold locals in total")+
    guides(fill = guide_legend(
        "meal content\n",
        keywidth = .5,
        keyheight = .5,
        default.unit ="inch"))+
    scale_fill_manual(values = c("Hot and Cold"="#e5e5e5","Meat" = "#c5b87c","Vegetarian" = "#008099","Vegan+"="#6619e6","Vegan"="#fad60d","Unknown" = "grey60"),
                      breaks = c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold"),
                      labels = c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold"))+
    geom_text(size = 5, position = position_stack(vjust = 0.5))+
    scale_y_continuous(breaks = seq(0,200,50), limits = c(0, 150))+
    mytheme2

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/angebot analysen/plots/local interv 180518 egel.pdf",
       width = 20,
       height = 12,
       dpi=600,
       units="in",
       device="pdf") 


## Plot locals canteen Grüental (axis with weekdays)###--------------
## prepare data
df_ <- df %>% 
    filter(shop_description == "Grüental Mensa") %>%
    group_by(date,label_content,semwk) %>% summarise(tot_sold=n())

# to print date times need to convert to factor, after use reorder after date
# df_$date <- as.POSIXct(df_$date, tz="CET",format="%Y-%m-%d")
df_$date2 <- factor(format(df_$date, format = '%a'))
df_$date3 <- factor(format(df_$date, format = '%d%b')) 
df_$date4 <- factor(paste(df_$date2,df_$date3, sep="\n\n"))

# change NA to string Unknown
df_$label_content2 <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

#aplha is not really an option => facet_wrap seems better
ggplot(df_, aes(y=tot_sold,x=reorder(date4,date),fill=factor(label_content2, levels=c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), label=round(tot_sold,digits=0))) +
    geom_bar(stat="identity") +
    xlab("Autumn semester weeks in canteen 'Grüental'") +
    ylab("\nsold additional (resp. locals) meals")+
    ggtitle("note: 1458 sold meals in total")+ # group_by(df,shop_description) %>% summarise(tot=n())
    guides(fill = guide_legend(
        "meal content\n",
        keywidth = .5,
        keyheight = .5,
        default.unit ="inch"))+
    scale_fill_manual(values = c("Hot and Cold"="#e5e5e5","Meat" = "#c5b87c","Vegetarian" = "#008099","Vegan+"="#6619e6","Vegan"="#fad60d","Unknown" = "grey60"),
                      breaks = c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold"),
                      labels = c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold"))+
    geom_text(size = 5, position = position_stack(vjust = 0.5))+
    scale_y_continuous(limits = c(0,80), breaks = seq(0,100,25))+
    mytheme2

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/angebot analysen/plots/local grüental 180515 egel.pdf",
       width = 30,
       height = 18,
       dpi=600,
       units="in",
       device="pdf") 


## Plot locals canteen Vista (axis with weekdays)###--------------
## prepare data
df_ <- df %>% 
    filter(shop_description == "Vista Mensa") %>%
    group_by(date,label_content,semwk) %>% summarise(tot_sold=n())

# to print date times need to convert to factor, after use reorder after date
# df_$date <- as.POSIXct(df_$date, tz="CET",format="%Y-%m-%d")
df_$date2 <- factor(format(df_$date, format = '%a'))
df_$date3 <- factor(format(df_$date, format = '%d%b')) 
df_$date4 <- factor(paste(df_$date2,df_$date3, sep="\n\n"))

# change NA to string Unknown
df_$label_content2 <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

#aplha is not really an option => facet_wrap seems better
ggplot(df_, aes(y=tot_sold,x=reorder(date4,date),fill=factor(label_content2, levels=c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), label=round(tot_sold,digits=0))) +
    geom_bar(stat="identity") +
    xlab("Autumn semester weeks in canteen 'Vista'") +
    ylab("\nsold additional (resp. locals) meals")+
    ggtitle("note: 1144 sold meals in total")+ #
    guides(fill = guide_legend(
        "meal content\n",
        keywidth = .5,
        keyheight = .5,
        default.unit ="inch"))+
    scale_fill_manual(values = c("Hot and Cold"="#e5e5e5","Meat" = "#c5b87c","Vegetarian" = "#008099","Vegan+"="#6619e6","Vegan"="#fad60d","Unknown" = "grey60"),
                      breaks = c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold"),
                      labels = c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold"))+
    geom_text(size = 5, position = position_stack(vjust = 0.5))+
    scale_y_continuous(limits = c(0,80), breaks = seq(0,100,25))+
    mytheme2

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/angebot analysen/plots/local vista 180515 egel.pdf",
       width = 30,
       height = 18,
       dpi=600,
       units="in",
       device="pdf") 
