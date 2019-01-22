##############
## Grafics SGA
##############

#load data

#Stand: 24.4.18 // Egeler

#------------
# Load data => see script
df_17 <- filter(df_new1, (hour(trans_date) >= 11) & (hour(trans_date) <= 13)) # 9.00 is included but not 15.00 is not included, dont know how to handle itdf_7 <- left_join(df_17,info, by = c("shop_description","date","article_description","cycle")) # differences to df_17 are transactions on the same time => see "analyse_180320_03egel"
df_7 <- left_join(df_17,info, by = c("shop_description","date","article_description","cycle")) # differences to df_17 are transactions on the same time => see "analyse_180320_03egel"


#-------------------
# data from 2017, without locals, to plot menu offer => see script meal_offer.R

menu_tot7 <- filter(df_7, !grepl("Local+",df_7$article_description))
menu_tot7 <- group_by(menu_tot7, label_content, week, semwk, year) %>% summarise(value=n()) # group it again, for merge with other data, no condit variable
menu_tot7 <- rename(menu_tot7, variable = label_content)
menu_tot7 <- select(menu_tot7, variable,value,year,week,semwk)

#----------------- 
#load data from 2015 and 2016
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_testdaten_analysen/explorativ/data")
dat_hs <- read.csv2("verkaufsdaten täglich HS 15-16 180221.csv")

df_old <-  data.frame(dat_hs[dat_hs$name %in% c('KitchenTotal','Green','FavoriteTotal','BuffetTotal'),])
df_old <- droplevels(df_old)
df_old$date <- as.Date(df_old$date,"%d.%m.%Y")
df_old$week <- strftime(df_old$date, format= "%V")

sor <- function(dat){
       select(dat, art_description, date, week, year, shop_description, tot_sold)}

names(df_old)[names(df_old) == "name"] <- "art_description"
names(df_old)[names(df_old) == "Verkaufte.Stücke"] <- "tot_sold"
names(df_old)[names(df_old) == "ort"] <- "shop_description"

df_tot <- sor(df_old)

# change names 
levels(df_tot$art_description)[levels(df_tot$art_description) == "BuffetTotal"] <- "Hot and Cold"
levels(df_tot$art_description)[levels(df_tot$art_description) == "KitchenTotal"] <- "Kitchen"
levels(df_tot$art_description)[levels(df_tot$art_description) == "FavoriteTotal"] <- "Favorite"

### 2015: calculation of the relative meal content
df <- as.data.frame(group_by(df_tot, year, art_description, week) %>% summarise(tot= sum(tot_sold)))
dt <- filter(df, year == 2015) 
dt <- droplevels(dt)

Fleisch_Fisch_Vogel <- dt[dt[,"art_description"]=="Favorite",4]*0.8 + dt[dt[,"art_description"]=="Kitchen",4] # 80% of Favorite meals and 100% of Kitchen meals contain meat
Vegetarisch <- dt[dt[,"art_description"]=="Green",4] + dt[dt[,"art_description"]=="Favorite",4]*.2 # 100% of Green meals and 20% of Favorite meals are vegetarian

menu_tot5 <- melt(data.frame(dt[dt[,"art_description"]=="Hot and Cold",4],Fleisch_Fisch_Vogel, Vegetarisch)) # create data.frame with variable hot and cold seperatly
levels(menu_tot5$variable)[1] <- "Hot and Cold"
levels(menu_tot5$variable)[2] <- "Meat"
levels(menu_tot5$variable)[3] <- "Vegetarian"
menu_tot5$year <- 2015 # add variable year
menu_tot5$week <- rep(36:52,3)
menu_tot5$semwk <- rep(-1:15,3)# add week numbers

### 2016: calculation of the relative meal content
df <- as.data.frame(group_by(df_tot, year, article, week) %>% summarise(tot= sum(total)))
dt <- filter(df, year == 2016) 
dt <- droplevels(dt)

Fleisch_Fisch_Vogel <- dt[dt[,"art_description"]=="Favorite",4]*0.8 + dt[dt[,"art_description"]=="Kitchen",4] # 80% of Favorite meals and 100% of Kitchen meals contain meat
Vegetarisch <- dt[dt[,"art_description"]=="Green",4] + dt[dt[,"art_description"]=="Favorite",4]*.2 # 100% of Green meals and 20% of Favorite meals are vegetarian

menu_tot6 <- melt(data.frame(dt[dt[,"art_description"]=="Hot and Cold",4],Fleisch_Fisch_Vogel, Vegetarisch)) # create data.frame with variable hot and cold seperatly
levels(menu_tot6$variable)[1] <- "Hot and Cold"
levels(menu_tot6$variable)[2] <- "Meat"
levels(menu_tot6$variable)[3] <- "Vegetarian" 
menu_tot6$year <- 2016
menu_tot6$week <- rep(35:51,3)
menu_tot6$semwk <- rep(-2:14,3)# add week numbers

# Concatinate data frames 2015 till 2017

menu_tot5 <- filter(menu_tot5, week >=40 & week <=51)
menu_tot6 <- filter(menu_tot6, week >=40 & week <=51)
menu_tot7 <- data.frame(menu_tot7) # for combining dataframe
df1 <- rbind(menu_tot5,menu_tot6, menu_tot7) #rbind
# df <- bind_rows(menu_tot7,menu_tot6,menu_tot5) # try that to deal with tibble data sets

write.csv2(df1,"all data over year 180424 egel.csv")# for ist-soll analysis (between 11 and 14 oclock and without locals) see folder
write_delim(df1,"all data over year 180424 02egel.csv",delim = ';')


######
# Chapter ploting
#####

######
# description sample 2017 (see pie_plot_180404_egel.xlsx)
#####

######
# meal content over 3 years in total => not in poster, only chi-square test
#####

#prepare data
levels(df1$variable)[5] <- 'Vegan' # change vegan* to vegan
df1 <- filter(df1, week >=40 & week <=51)
df_ <- complete(df1, variable,year, fill=list(values=0)) # complete missing cases
df_ <- group_by(df1, variable, year) %>% summarise(value=sum(value)) # aggregate data
df_ <- df_ %>% 
    group_by(year) %>% # give in variable, you want to calculate percentage
    mutate(pct=value/sum(value))

df_$variable <- factor(df_$variable, levels(df_$variable)[c(3,4,2,1)])

ggplot(df_, aes(y=value,x=as.character(year), fill=variable, label=paste0(round(pct*100,digits=0),"%"))) +
    geom_bar(stat="identity", position = "stack") +
    #ggtitle("Verkaufte Menüs: 3. + 4. HSW\n") +
    xlab("winter semeter") +
    ylab("sold meals")+
    guides(fill=guide_legend("meal content\n",
                             keywidth=.5,
                             keyheight=.5,
                             default.unit="inch"))+
    scale_fill_manual(values = c("Hot and Cold"="grey90","Meat" = "#c5b87c","Vegetarian" = "#008099","Vegan"="#fad60d"), na.value="#262626")+
    geom_text(size = 5, position = position_stack(vjust = 0.5))+
    mytheme

ggsave("C:/Users/egel/switchdrive/ZHAW/01_NOVANIMAL/01_meetings/180415 sga tagung/fig2_content_180404_v02.eps",
       width = 12,
       height = 8,
       dpi=600,
       units="in",
       device="eps") # moved into the Archive (in SGA folder)
