#####
# Local analyses
#####


# status 18.04 // egel

# for further information see https://rstudio-pubs-static.s3.amazonaws.com/74603_76cd14d5983f47408fdf0b323550b846.html

# load data => see script


#####-----
# subset all locals
#####---

df <- filter(df_7, grepl("Local+",df_7$article_description))
df_ <- group_by(df,date,label_content,article_description,meal_name,shop_description, condit) %>% summarise(tot=n())
#=> there are some discrepancies between Dokumentation from Michael Krauer and Till data

## Special cases

# e.g. 20.10 Local Kitchen or Local Favorite (check prise)
sear=filter(df, day(df$date)==20 & month(df$date)==10) # ist definately local Kitchen

# e.g. 23.10 there were 65 Favorites accounted as Local Favorites => changed randomly in python
# test: there should only be 22 Local Favorite now
sear=filter(df, day(df$date)==23 & month(df$date)==10 & df$shop_description == "Vista Mensa")

# e.g. 10.11 and 13.11 locals are missing in till data => do what?

# e.g. 15.11 big deviations from Dokumentation of Michael and Till data
sear=filter(df, day(df$date)==15 & month(df$date)==11 & df$shop_description == "Grüental Mensa")
# seems that every transaction is double => causes: appears two times in our dokumentation

# e.g. 17.11 missing Locals (10 Vegi-Burgers) in till data (built subset, not possible to figure out who bought the vegetarian burgers)
sear=filter(df_7, day(df_7$date)==17 & month(df_7$date)==11 & df_7$shop_description == "Vista Mensa" & df_7$article_description=="World")
# sear_card=filter(sear, total_amount == 14)
# sear_card2=filter(df_7, card_num %in% sear_card$card_num)


#####-----
# Look for differences between df_17 and df_7 after merging Dokumentation of Locals
#####---

d= group_by(df_17, card_num) %>% summarise(n=n()) # count incidences
d2=group_by(df_7, card_num) %>% summarise(n=n())

diffr=setdiff(d,d2) # 36 card_num more than in df_17 => reason double transactions on the same time (egel guess: some invitated  another)
diffr2=filter(df_17, card_num %in% diffr$card_num)

# see also this file for more information difference between datasets script: diff r_py 180418 egel.R


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

###--------------
# plot data with bars for locals
###--------------

## Plot only Basis with Weekdays
df_ <- df %>% 
    filter(condit=="Basis" ) %>%
    group_by(date,label_content,semwk,shop_description) %>% summarise(tot_sold=n())

# to print date times need to convert to factor, after use reorder after date
# df_$date <- as.POSIXct(df_$date, tz="CET",format="%Y-%m-%d")
df_$date2 <- factor(format(df_$date, format = '%a'))
df_$date3 <- factor(format(df_$date, format = '%d%b')) 
df_$date4 <- factor(paste(df_$date2,df_$date3, sep="\n\n"))

# change NA to string
df_$label_content2 <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

#--------------
# theme => see script mytheme


#--------------plot data
ggplot(df_, aes(y=tot_sold,x=reorder(date4,date),alpha=shop_description,fill=factor(label_content2, levels=c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), label=tot_sold)) +
    geom_bar(stat="identity") +
    xlab("Basis: 'meat' weeks") +
    ylab("\nsold additional (resp. locals) meals")+
    guides(fill = guide_legend(
        "meal content\n",
        keywidth = .5,
        keyheight = .5,
        default.unit ="inch"))+
    scale_fill_manual(values = c("Hot and Cold"="#e5e5e5","Meat" = "#c5b87c","Vegetarian" = "#008099","Vegan+"="#6619e6","Vegan"="#fad60d","Unknown" = "grey60"),
                      breaks = c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold"),
                      labels = c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold"))+
    scale_alpha_discrete(range = c(0.3, 1))+
    geom_text(size = 5, position = position_stack(vjust = 0.5))+
    mytheme


ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/analysen/plots/over time local meat weeks 180418 egel.pdf",
       width = 20,
       height = 8,
       dpi=600,
       units="in",
       device="pdf") 


## prepare data: only Intervention with Weekdays
df_ <- df %>% 
    filter(condit=="Intervention" ) %>%
    group_by(date,label_content,semwk,shop_description) %>% summarise(tot_sold=n())


# to print date times need to convert to factor, after use reorder after date
# df_$date <- as.POSIXct(df_$date, tz="CET",format="%Y-%m-%d")
df_$date2 <- factor(format(df_$date, format = '%a'))
df_$date3 <- factor(format(df_$date, format = '%d%b')) 
df_$date4 <- factor(paste(df_$date2,df_$date3, sep="\n\n"))

# change NA to string Unknown
df_$label_content2 <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

#--------------plot data
ggplot(df_, aes(y=tot_sold,x=reorder(date4,date),alpha=shop_description,fill=factor(label_content2, levels=c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold")), label=round(tot_sold,digits=0))) +
    geom_bar(stat="identity") +
    xlab("Intervention: 'vegetarian' weeks") +
    ylab("\nsold additional (resp. locals) meals")+
    guides(fill = guide_legend(
        "meal content\n",
        keywidth = .5,
        keyheight = .5,
        default.unit ="inch"))+
    scale_fill_manual(values = c("Hot and Cold"="#e5e5e5","Meat" = "#c5b87c","Vegetarian" = "#008099","Vegan+"="#6619e6","Vegan"="#fad60d","Unknown" = "grey60"),
                      breaks = c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold"),
                      labels = c("Unknown","Vegan","Vegan+","Vegetarian","Meat","Hot and Cold"))+
    scale_alpha_discrete(range = c(0.3, 1))+
    geom_text(size = 5, position = position_stack(vjust = 0.5))+
    mytheme

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/analysen/plots/over time local veg weeks 180418 egel.pdf",
       width = 20,
       height = 8,
       dpi=600,
       units="in",
       device="pdf") 

