########
## First Analyses with till data
#######

#Stand: 28.3 // egel


# required libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(feather)
library(reshape2)
library(tidyr)
library(eeptools)
library(readr)

###########
### Preparation for analyses
###########

# load data hs17
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files")
dat <- read_rds("data_180320_03egel.Rda")
dat2 <- read_csv2("180320_joined_04egel.csv", locale = locale(encoding='LATIN1'))

# subset data: Favorite, Garden (= Hot n Cold), Kitchen (0,1,2,3,4), LocalFavorite, LocalKitchen, LocalWorld, World => Local Kitchen is missing?

articles <- c(
    "A10001" ,# Favorite
    "A10010", # Kitchen 0
    "A10011", # Kitchen 1
    "A10040" ,# Garden 
    "A10012" ,# Kitchen 2
    "A10013" ,# Kitchen 3
    "A10014" ,# Kitchen 4
    "A10025" ,# Local Favorite
    "A10026" ,# Local World
    "A10027" ,# Local Tössfeld ??
    "A10015" ,# World
    "A10020" ,# Local 0
    "A10021" ,#	Local 1
    "A10022" ,#	Local 2
    "A10023" ,#	Local 3
    "A10024"  # Local 4
)


articl_name <- c(
    "Favorite",
    "Kitchen 0",
    "Kitchen 1",
    "Garden" ,
    "Kitchen 2",
    "Kitchen 3",
    "Kitchen 4",
    "Local Favorite",
    "Local World",
    "Local Tössfeld",
    "World")
		

# df_new1 <- filter(dat, art_description %in% articl_name)
df_new1 <- filter(dat, art_code %in% articles)

# subset data II: only meals during lunch time, ATTENTION: dataset only using for individual comparisons. If we compare with the last years we need to filter only meals sold during 11 am and 2 pm 
rest <- subset(df_new1, !(strftime(df_new1$trans_date, '%H') %in% c('9','10','11','12','13','14','15'))) # are breaks includes? e.g. 9.00 or not?
write.csv2(rest,"data_dinner_wädenswil_180221.csv")
df_new1 <- subset(df_new1, (strftime(df_new1$trans_date, '%H') %in% c('9','10','11','12','13','14','15')))
df_new1 <- droplevels(df_new1) # drop useless levels

# change names of levels
df_new1$art_description <- as.factor(df_new1$art_description)
levels(df_new1$art_description)[3:6] <- c('Kitchen')
levels(df_new1$art_description)[levels(df_new1$art_description) == "Garden"] <- 'Hot and Cold'

# change levels of gender
levels(df_new1$Geschlecht)[1:2] <- 'female'
levels(df_new1$Geschlecht)[2:3] <- 'male'
levels(df_new1$Geschlecht)[levels(df_new1$Geschlecht)=="NULL"] <- NA

# change variable date to date format and add week number
df_new1$week <- as.integer(strftime(df_new1$date, format= "%V"))
df_new1$year <- as.integer(strftime(df_new1$date, format= "%Y"))

# change some variables to factors

df_new1$shop_description <- as.factor(df_new1$shop_description)
levels(df_new1$shop_description)[1] <- 'Grüental Mensa'

#age calculation => attention max age is 118, takes alwys date of today for calculation, e.g. born "actual today" in 1977
df_new1$Geburtsjahr1 <- as.Date(as.character(df_new1$Geburtsjahr2),"%Y") # change geburtsjahr in date format
age <- age_calc(na.omit(df_new1$Geburtsjahr1), units = "years") # omit NA for calculation
df_new1$age[!is.na(df_new1$Geburtsjahr2)] <- age # insert NA values again

write_rds(df_new1,"filtered_data_dub_180320_02egel.Rda") # include double transaction_id's
write_csv(df_new1,"filtered_data_dub_180320_02egel.csv")

# Attention, there are some special cases: talk over with Bruno

# ecxlude double or tripple transactions_id
df_new2 <- df_new1[!(duplicated(df_new1$transaction_id)),]
b <- df_new1[duplicated(df_new1$transaction_id),]
write.csv2(b,"duplicate transaction_id 180320 egel.csv")#interim result
write_rds(df_new2,"filtered_data_180320_02egel.Rda") 
write_csv(df_new2, "filtered_data_180320_02egel.csv") # comma seperated

#-----------------------------------------------------------------------------####################################

# load old data hs15 and hs16
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/explorativ/data")
dat_hs <- read.csv2("verkaufsdaten täglich HS 15-16 180221.csv")


# subset hs 15 and hs16
df_old <-  data.frame(dat_hs[dat_hs$name %in% c('KitchenTotal','Green','FavoriteTotal','BuffetTotal'),])
df_old <- droplevels(df_old)
df_old$date <- as.Date(df_old$date,"%d.%m.%Y")
df_old$week <- strftime(df_old$date, format= "%V")

df_hs5 <- df_old %>% filter(year == 2015)
df_hs6 <- df_old %>% filter (year == 2016)


#-----------------------------------------------------------------------------####################################
#-----------------------------------------------------------------------------####################################

### for the results, see => "ergebnisse testdata 180215 04egel.docx"

########
## 1. Aggregate data
########

# subset data: only meals during lunch time: 11 am and 2 pm 
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/explorativ/data")
df_new1 <- readRDS("filtered_data_dub_180228_egel.Rda")
df_new2 <- subset(df_new1, (strftime(df_new1$trans_date, '%H') %in% c('11','12','13','14'))) # 4 cases in Hot and Cold in 2017 less

# aggregate hs17 
df_hs17 <- dplyr::group_by(df_new2,article, date, week, year, place) %>% dplyr::summarise(total=n())
df_hs17 <- as.data.frame(df_hs17)

filt <- function(dat){
    filter(dat, week == 40 | week == 41)} # filter for week 40 and 41
sor <- function(dat){
    select(dat, article, date, week, year, place, total)} # change order

# sort hs15 and hs16
#hs15

df_hs15 <- filt(df_hs5)
df_hs15[1:3] <- NULL
df_hs15[2:3] <- NULL
df_hs15[3:6] <- NULL

# change names
colnames(df_hs15)[1] <- "article"
colnames(df_hs15)[2] <- "total"
colnames(df_hs15)[5] <- "place"

# sort for merging
df_hs15 <- sor(df_hs15)

#hs16
df_hs16 <- filt(df_hs6)
df_hs16[1:3] <- NULL
df_hs16[2:3] <- NULL
df_hs16[3:6] <- NULL

# change names
colnames(df_hs16)[1] <- "article"
colnames(df_hs16)[2] <- "total"
colnames(df_hs16)[5] <- "place"

# sort for merging
df_hs16 <- sor(df_hs16)

# merge data frames
df_tot <- rbind(df_hs15,df_hs16,df_hs17)

#change name of levels - built function for that
# change <- function(variable,dat,y,z){
#     for (word in dat[grepl("variable", names(dat))]){
#         levels(word)[levels(word) == y] <- z
#         dat$article2 <<- levels(dat$word)
#         }
# }    
# 
# change(df_tot,"article","Favorite","test") not working somehow

levels(df_tot$article)[levels(df_tot$article) == "BuffetTotal"] <- "Hot and Cold"
levels(df_tot$article)[levels(df_tot$article) == "KitchenTotal"] <- "Kitchen"
levels(df_tot$article)[levels(df_tot$article) == "FavoriteTotal"] <- "Favorite"

# save data
write.csv2(df_tot,"all hs sales 180202 04egel.csv") #interim result

#-----------------------------------
# load data
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/explorativ/data")
df_tot <- read.csv2("all hs sales 180202 04egel.csv")  # 4 cases are missing in hot and cold in year 2017 => because lunch time from 11 to 2

########
## 2. desciptive Analysis
########

#####
# a. totale sale over three years (2017: doubles trans_id's are included) => figure 1 (in the paper)
#####

# total sale over last three years (only calendar week 40 and 41) => ATTENTION: for comparison only Meals during 11 and 14 Uhr need to be counted

df <- as.data.frame(group_by(df_tot, year) %>% dplyr::summarise(tot=sum(total)))
df_p <- as.data.frame(group_by(df_tot, year, place) %>% dplyr::summarise(tot=sum(total)))
levels(df_p$place)[levels(df_p$place) == "grüental"] <- "Grüental Mensa"
levels(df_p$place)[levels(df_p$place) == "vista"] <- "Vista Mensa"

write.csv2(df, "tot_verkauf_180201_03egel.csv") # interim result


# combinde data sets
df$place <- 'Total'
df_t <- rbind(df,df_p)
df_t$place <- as.factor(df_t$place) # as.factor
df_t$place <- factor(df_t$place, levels(df_t$place)[c(2,1,3)]) #reorder levels for plot
df_t$year <- as.character(df_t$year) # change year to character for plot

#-----------
#plot functions

mytheme <- theme_bw()+ # definve theme for plot
    theme(plot.title = element_text(size = 20, face = "bold")) +
    theme(text = element_text(size=20),axis.title.y = element_text(size = 20)) +
    theme(axis.title.x = element_text(size = 20)) +
    theme(legend.text = element_text(size = 20),legend.title = element_text(size =20)) +
    theme(axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)))+
    theme(axis.title.x = element_text(size = 20,  margin = margin(t = 20, r = 0, b = 0, l = 0)))

y_scale <- function(x){
    scale_y_continuous(breaks=seq(0,max(x)+200,500), limits = c(0, max(x)+200))
}

myplot <- function(dat, x.var, y.var, z.var, cols,x.name,y.name, col.name){
    print(
        ggplot(dat, aes_string(x = x.var, y = y.var, group = z.var, color = z.var))+
            geom_point(size=5)+
            geom_line(size=0.7)+
            scale_color_manual(values=c(cols))+
            xlab(x.name) +
            ylab(y.name)+
            labs(color=col.name)+
            mytheme+
            y_scale(df_t$tot) # need always to update
    )
}

text <- function(x,x.just,x.width,x.size){
    geom_text(aes(
    label = paste0(round(x,digits = 0)), vjust =x.just
),colour = "black",position = position_dodge(width = x.width), size= x.size)}




myplot2 <- function(dat, x.var, y.var, z.var, cols,x.name,y.name, fil.name){
    print(
        ggplot(dat, aes_string(x = x.var, y = y.var, fill = z.var))+
            geom_bar(colour="black", stat="identity", position=position_dodge())+
            scale_fill_manual(values=c(cols))+
            xlab(x.name) +
            ylab(y.name)+
            guides(fill=guide_legend(title=fil.name))+
            mytheme
    )
}


#-------
#plot
myplot(df_t,"year",
               "tot","place",
               c("#fad60d","#c5b87c","#262626"),
               "Herbstemester", 
               "Verkaufte Menüs: Total 3. und 4. Herbstsemesterwoche",
               "Mensa")

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/explorativ/results/tot_sale_canteen_180202_05egel.png",
       width = 12, 
       height = 8, 
       dpi = 300, 
       units = "in", 
       device = "png")

#####
# b. total sold over menu lines (2017: doubles trans_id's are included) => figure 2
#####

# total sale per menu line over last three years

df <- as.data.frame(group_by(df_tot, article, year) %>% dplyr::summarise(tot=sum(total))) # somehow are 4 cases missing in hot and cold
write.csv2(df, "tot_verkauf_linien_180201_02egel.csv") #interim result

#overall WITH Locals

levels(df$article)[levels(df$article) == "Local Favorite"] <- 'Favorite'# sum local favorite and favorite
levels(df$article)[levels(df$article) == "Local Kitchen"] <- 'Kitchen'
levels(df$article)[levels(df$article) == "Local World"] <- 'Green/World'
levels(df$article)[levels(df$article) == "World"] <- 'Green/World'
levels(df$article)[levels(df$article) == "Green"] <- 'Green/World'
df <- group_by(df, article, year) %>% dplyr::summarise(tot=sum(tot)) # count toghether again

# to dos: write function for odering factors
df$article <- factor(df$article,levels(df$article)[c(1,2,4,3)]) # change order of factors
df$year <- as.character(df$year)

t <- myplot2(df,"article","tot","year",
    c("2017"="#fad60d","2016" = "#c5b87c","2015" = "#262626"),
    "Menü-Linien",
    "Verkaufte Menüs: 3. + 4. HSW",
    "Herbstsemester 3. + 4. HSW"
)

t + geom_text(data=df,aes(
    label = paste0(round(tot,digits = 0)), vjust =-.2
),colour = "black",position = position_dodge(width = .8), size= 8)

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/explorativ/results/tot_sale_line_180214_06egel.png",
       width = 15, 
       height = 15, 
       dpi = 300, 
       units = "in", 
       device = "png")

#####
# c. calculated meal content => figure 3
#####

# calculated menu content over last three years
#--------------
    ### 2015
df <- as.data.frame(group_by(df_tot, year, article, week) %>% summarise(tot= sum(total)))
dt <- filter(df, year == 2015) 
dt <- droplevels(dt)
write.table(dt, "tot_2015.csv", dec = ".", sep = ";") #interim result


    # calculation of the relative meal content

Fleisch_Fisch_Vogel <- dt[dt[,"article"]=="Favorite",4]*0.8 + dt[dt[,"article"]=="Kitchen",4] # 80% of Favorite meals and 100% of Kitchen meals contain meat
Vegetarisch <- dt[dt[,"article"]=="Green",4] + dt[dt[,"article"]=="Favorite",4]*.2 # 100% of Green meals and 20% of Favorite meals are vegetarian

menu_tot5 <- melt(data.frame(dt[dt[,"article"]=="Hot and Cold",4],Fleisch_Fisch_Vogel, Vegetarisch)) # create data.frame with variable hot and cold seperatly
levels(menu_tot5$variable)[1] <- "Hot and Cold"
levels(menu_tot5$variable)[2] <- "Fleisch Fisch Vogel"
menu_tot5$pct <- menu_tot5$value / sum(menu_tot5$value) * 100 # calculate percentage
menu_tot5$year <- 2015 # add variable year
menu_tot5$week <- c(40,41)
write.table(menu_tot5,"2015.csv",dec = ".", sep = ";") #interim result

#--------------
    ### 2016
df <- as.data.frame(group_by(df_tot, year, article, week) %>% summarise(tot= sum(total)))
dt <- filter(df, year == 2016) 
dt <- droplevels(dt)
write.table(dt, "tot_2016.csv", dec = ".", sep = ";") #interim result

    # calculation of the relative meal content

Fleisch_Fisch_Vogel <- dt[dt[,"article"]=="Favorite",4]*0.8 + dt[dt[,"article"]=="Kitchen",4] # 80% of Favorite meals and 100% of Kitchen meals contain meat
Vegetarisch <- dt[dt[,"article"]=="Green",4] + dt[dt[,"article"]=="Favorite",4]*.2 # 100% of Green meals and 20% of Favorite meals are vegetarian

menu_tot6 <- melt(data.frame(dt[dt[,"article"]=="Hot and Cold",4],Fleisch_Fisch_Vogel, Vegetarisch)) # create data.frame with variable hot and cold seperatly
levels(menu_tot6$variable)[1] <- "Hot and Cold"
levels(menu_tot6$variable)[2] <- "Fleisch Fisch Vogel"
menu_tot6$pct <- menu_tot6$value / sum(menu_tot6$value) * 100
menu_tot6$year <- 2016
menu_tot6$week <- c(40,41)
write.table(menu_tot6,"2016.csv", dec = ".", sep = ";") #interim result

#--------------
    ###2017 => check out Dokumentation and Raster Basis- und Interventionswochen

#load information from dokumentation merge with dataset

#load information about meal content
setwd("S:/pools/n/N-IUNR-nova-data/06_add_var/01_dokumentation")
info <- read.csv2("menu_inhalt_180215_08vori.csv", na.strings = "") # attention load always actual version
info$date <- as.Date(info$date, format = "%d.%m.%Y") # change format to date
info <- rename(info, art_description=meal, shop_description=place)

setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/explorativ/data")
df_17 <- readRDS("filtered_data_dub_180228_egel.Rda")
df_17 <- subset(df_17, (strftime(df_17$trans_date, '%H') %in% c('11','12','13','14'))) # 4 cases in Hot and Cold in 2017 less

# levels(df$art_description)[5] <- 'Local Kitchen'
levels(info$art_description)[5] <- 'Local Tössfeld'
df$date <- as.Date(df$date, format = "%Y-%m-%d")


test1 <- merge(df,info, by = c("shop_description","date","art_description"),all=T)
test2 <- group_by(test1,date,art_description,shop_description)%>% summarise(tot=n())




menu_tot7_l <- df_7 %>% 
    group_by(article, date, week, place, year, label_content) %>%
    dplyr::summarise(total=n()) # one observation is missing (see Dokumentation, mistake of tills worker)
menu_tot7_l$article <- factor(menu_tot7_l$article,levels(menu_tot7_l$article)[c(1,7,3,4,6,5,2)]) # change order of factors

levels(menu_tot7_l$label_content)[1] <- "Fleisch Fisch Vogel" # rename levels of label_content
levels(menu_tot7_l$label_content)[5] <- "Vegetarisch"
levels(menu_tot7_l$label_content)[3] <- "Pflanzlich"
levels(menu_tot7_l$label_content)[4] <- "Pflanzlich"
menu_tot7_l$label_content <- factor(menu_tot7_l$label_content,levels(menu_tot7_l$label_content)[c(1,4,3,2)]) # change order of factors

menu_tot7 <- group_by(menu_tot7_l, label_content, year, week) %>% summarise(value=sum(total)) # group it again
menu_tot7$pct <- menu_tot7$value / sum(menu_tot7$value) * 100
colnames(menu_tot7)[1] <- "variable"
menu_tot7 <- data.frame(menu_tot7) # ungroup data to data frame
menu_tot7 <- select(menu_tot7, variable, value, pct, year, week)
write.table(menu_tot7,"2017 local.csv", dec = ".", sep = ";") #interim result

df_l <- rbind(menu_tot5,menu_tot6, menu_tot7)

write.csv2(df_l,"all_year_testdata_180201_05egel.csv") #interim result

#----------------- 
#only locals

loc2 <- mutate(menu_tot7_l, condit=ifelse(date >= "2017-10-02" & date <= "2017-10-6", "Basiswoche","Interventionswoche")) # add new variable condition of experiment
loc <- group_by(loc2, article, label_content, year, condit) %>% summarise(value = sum(total))
loc <- loc[grepl("Local *",loc$article),]
write.csv2(loc, "locals_content_2017_180305_egel.csv") # contains basis and intervention plus all local menus


#------------------------------------------
## barplot load data

df_l <- read.csv2("all_year_testdata_180201_05egel.csv")
loc <- read.csv2("locals_content_2017_180305_02egel.csv") #load locals, added some variables in excel version 02, different then the first version
# expand and complete should do the dame thing, however didnt found the right solution

# with local menus

df_l <- group_by(df_l, variable, year) %>% summarise(value= sum(value))
df_l$variable <- factor(df_l$variable, levels(df_l$variable)[c(1,4,3,2)]) # reorder factors
df_l <- complete(df_l, variable,year, fill = list(values = 0)) # complete dataset and fill up with 0 when missing data
df_l$year <- as.character(df_l$year)
loc <- complete(loc,label_content,year, fill=list(values=0))
loc2 <- group_by(loc, label_content, as.character(year)) %>% summarise(value=sum(value))

p <- ggplot(df_l, aes(y=value,x=variable, fill=as.character(year))) +
    geom_bar(colour="black", stat="identity", position=position_dodge()) +
    #ggtitle("Verkaufte Menüs: 3. + 4. HSW\n") +
    xlab("Menü-Inhalt") +
    ylab("Verkaufte Menüs: 3. + 4. HSW")

p <- myplot2(df_l,"variable","value","year",
             c("2017"="#fad60d","2016" = "#c5b87c","2015" = "#262626"),
             "Menü-Inhalt",
             "Verkaufte Menüs: 3. + 4. HSW",
             "Herbstsemester 3. + 4. HSW"
             )

p +
    geom_text(data=df_l,aes(
        label = paste0(round(value,digits = 0)), vjust =-.2
    ),colour = "#000000",position = position_dodge(width = .8), size= 8) +
    theme_bw() +
    guides(fill=guide_legend(title="Herbstsemester 3. + 4. HSW"))+
    theme(plot.title = element_text(size = 20, face = "bold")) +
    theme(text = element_text(size=20),axis.title.y = element_text(size = 20)) +
    theme(axis.title.x = element_text(size = 20)) +
    theme(legend.text = element_text(size = 20),legend.title = element_text(size =
                                                                                20)) +
    theme(axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)))+
    theme(axis.title.x = element_text(size = 20,  margin = margin(t = 20, r = 0, b = 0, l = 0)))


p+ geom_bar(data = loc2, aes(y=value,x=label_content, fill=as.character(year), color=label_content),
            stat= "identity", position_dodge(), size=1.25) + 
    scale_fill_manual(values = c("2017"="#fad60d","2016" = "#c5b87c","2015" = "#262626")) + # when mapping be careful with the fill option, afterwards define every color
    scale_color_manual(values = c("Fleisch Fisch Vogel" = "black", "Pflanzlich" = "black", "Vegetarisch" ="black"))+ 
    guides(color=F) # hide guide of color
  

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/explorativ/results/menuinhalt_180202_10egel.png",
       width = 15, 
       height = 15, 
       dpi = 300, 
       units = "in", 
       device = "png")



#####
# e. visitor frequency, without double transaction_id's (spielt im endefekt keine rolle, einziger unterschied, es gibt weniger personen die über 10 Transaktionen haben) => figure 4
#####

setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/explorativ/data")
df_17 <- readRDS("filtered_data_180320_egel.Rda") 

visit2 <- dplyr::count(df_17, card_num, shop_code, age, Geschlecht, member) # counts card_num and delivers new variable n
visit2 <- data.frame(visit2) # deleting NA with is.na, omit.na, complete.cases not workling => better filter
colnames(visit2)[6] <- 'freq_visit'  #change name of "n" to freq_visit

#plot
mycol=c("#fad60d","#c5b87c")

vis <- filter(visit2, freq_visit <= 100 ) # excluded: 330 NA's

#-------------------------------------
na <- filter(df_17, is.na(card_num))
write.csv2(na,"missing_card_numb_180320.csv") # all payed cash

# special card cases
vis <- filter(visit2, freq_visit > length(unique(df_17$date))) # more than possible days of transactions (in this specific case more than 10 visits)
vis <- na.omit(vis$card_num) # vector containint special card numbers
spec <- filter(df_17, card_num %in% vis)
write.csv2(spec,"spec_trans_180222.csv")
#--------------------------------------


gr <- vis %>% filter(place == "Grüental Mensa") %>% 
    group_by(freq_visit, place) %>% # filter and grouping after frequence and place
    dplyr::summarise(count=n()) # count same cases
gr <- data.frame(gr) %>% # for calculation important to change to data.frame
    mutate(pct=count/sum(count))

vi <- vis %>% filter(place == "Vista Mensa") %>% 
    group_by(freq_visit, place) %>% # filter and grouping after frequence and place
    dplyr::summarise(count=n()) 
vi <- data.frame(vi) %>% # count same cases
    mutate(pct=count/sum(count))

visit3 <- data.frame(rbind(gr, vi))

ggplot(visit3, aes(x=as.factor(freq_visit),y=pct, fill=place)) +
    geom_bar(stat="identity", position = "dodge",colour="black")+
    scale_fill_manual(values = mycol)+
    scale_y_continuous(labels=scales::percent) +
    #ggtitle("Anzahl Besuche der Mensagäste in KW 40 und 41 (Quelle: NOVANIMAL, 2017)") +
    xlab("Besuchertage") +
    ylab("Anteil Personen")+
    guides(fill=guide_legend(title="Mensa"))+
#         geom_text(aes(
#             label = paste0(round(visit3$pct,digits = 0)), vjust =-.2
#         ),colour = "#000000",position = position_dodge(width = .8), size= 8) +
    theme_bw() +
    theme(plot.title = element_text(size = 20, face = "bold")) +
    theme(text = element_text(size=20),axis.title.y = element_text(size = 20)) +
    theme(axis.title.x = element_text(size = 20)) +
    theme(legend.text = element_text(size = 20),legend.title = element_text(size =
                                                                                20)) +
    theme(axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)))+
    theme(axis.title.x = element_text(size = 20,  margin = margin(t = 20, r = 0, b = 0, l = 0)))

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/explorativ/results/visit_180214_05egel.png",
       width = 15, 
       height = 15, 
       dpi = 300, 
       units = "in", 
       device = "png")

# plot pro standort

????? 


#########
## 3. Difference in Intervention and Base
#########

#load HS17 

setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/explorativ/data")
df_17 <- readRDS("filtered_data_dub_180228_egel.Rda")
df_7 <- mutate(df_17, condit=ifelse(date >= "2017-10-02" & date <= "2017-10-6", "Basiswoche","Interventionswoche")) # add new variable condition of experiment

# levels(df_7$article)[levels(df_7$article) == "Local Favorite"] <- "Zusatzangebot Favorite"
# levels(df_7$article)[levels(df_7$article) == "Local Kitchen"] <- "Zusatzangebot Kitchen"
# levels(df_7$article)[levels(df_7$article) == "Local World"] <- "Zusatzangebot World"


#####
# a. sale over menu lines => figure 5
#####

df <- df_7 %>% 
    group_by(article, condit) %>% # place can be added later
    dplyr::summarise(tot=n())
df$article <- factor(df$article,levels(df$article)[c(1,7,3,4,6,5,2)]) # change order of factors

levels(df$article)[levels(df$article) == "Local Favorite"] <- "Favorite"
levels(df$article)[levels(df$article) == "Local Kitchen"] <- "Kitchen"
levels(df$article)[levels(df$article) == "Local World"] <- "World"

df <- group_by(df, article, condit) %>% summarise(tot = sum(tot)) # group them again
df$article <- factor(df$article,levels(df$article)[c(1,2,3,4)])

#load info locals

loc <- read.csv2("locals_content_2017_180305_egel.csv")
levels(loc$article)[levels(loc$article) == "Local Favorite"] <- "Favorite"
levels(loc$article)[levels(loc$article) == "Local Kitchen"] <- "Kitchen"
levels(loc$article)[levels(loc$article) == "Local World"] <- "World"

loc <- group_by(loc, article, condit) %>% summarise(tot = sum(value))
loc <- complete(loc, condit, fill= c(values= list(0))) # take variable with missing values

#plot

p <- ggplot(df, aes(y=tot,x=article, fill=condit, colour =article)) +
    geom_bar(colour="black", stat="identity", position=position_dodge())+
    
    #ggtitle("Verkaufte Mahlzeiten in der Mensa Grüental ohne Locals (Quelle: NOVANIMAL, 2017)") +
    xlab("Menü-Linien") +
    ylab("Verkaufte Menüs: 3. + 4. HSW")+
    geom_text(aes(
        label = paste0(tot), vjust =-.2
    ),colour = "#000000",position = position_dodge(width = .8), size= 8) +
    theme_bw() +
#     scale_x_discrete(
#         labels=function(x) gsub("Zusatzangebot ","Zusatzangebot\n",x,fixed=TRUE))+
    guides(fill=guide_legend(title="Einteilung 3. + 4. HSW"))+
    theme(plot.title = element_text(size = 20, face = "bold")) +
    theme(text = element_text(size=20),axis.title.y = element_text(size = 20)) +
    theme(axis.title.x = element_text(size = 20)) +
    theme(legend.text = element_text(size = 20),legend.title = element_text(size =
                                                                                20)) +
    theme(axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)))+
    theme(axis.title.x = element_text(size = 20,  margin = margin(t = 20, r = 0, b = 0, l = 0)))

p+ geom_bar(data=loc, aes(y=tot, x=article, fill=condit, colour =article), stat= "identity", position= position_dodge(), size= 1.25) +
    scale_fill_manual(values = c("#fad60d","#c5b87c")) +
    scale_color_manual(values=c("Favorite"= "black", "Kitchen" = "black", "World" = "black"))+
    guides(color=F)

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/explorativ/results/sale_line17_180215_04egel.png",
       width = 15, 
       height = 15, 
       dpi = 300, 
       units = "in", 
       device = "png")

#####
# b. sale over meal content = figure 6
#####


#load information about meal content
setwd("S:/pools/n/N-IUNR-nova-data/06_add_var/01_dokumentation")
info <- read.csv2("menu_inhalt_180215 06egel.csv")
info$date <- as.Date(info$date, format = "%d.%m.%Y") # change format to date

df_7 <- merge(df_7,info, by = c("place","date","article")) # merge, one events is probably a mistake of the system

df <- df_7 %>% 
    group_by(article, date, condit, place, label_content) %>%
    dplyr::summarise(tot=n())
df$article <- factor(df$article,levels(df$article)[c(1,7,3,4,6,5,2)]) # change order of factors
levels(df$label_content)[1] <- "Fleisch Fisch Vogel"
levels(df$label_content)[5] <- "Vegetarisch"
levels(df$label_content)[3] <- "Pflanzlich"
levels(df$label_content)[4] <- "Pflanzlich" # eigenständige mahlzeit
df$label_content <- factor(df$label_content,levels(df$label_content)[c(1,4,3,2)]) # change order of factors

df <- df %>% 
    group_by(condit, label_content) %>%
    dplyr::summarise(tot=sum(tot))

#load and define locals
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/explorativ/data")
loc <- read.csv2("locals_content_2017_180305_egel.csv")
levels(loc$label_content)[1] <- "Fleisch Fisch Vogel" # rename levels of label_content
levels(loc$label_content)[3] <- "Vegetarisch"
levels(loc$label_content)[2] <- "Pflanzlich"

loc <- group_by(loc, label_content, condit) %>% summarise(tot=sum(value))
loc <- complete(loc, condit, fill=list(values=0))

loc$label_content <- factor(loc$label_content, levels(df$label_content)[c(1,3,2)])

#plot1
p <-  ggplot(df, aes(y=tot,x=label_content, fill=condit)) +
    geom_bar(colour="black", stat="identity", position=position_dodge()) +
    #ggtitle("Verkaufte Mahlzeiten im Jahresvergleich (Quelle: NOVANIMAL, 2017)\n") +
    xlab("Menü-Inhalt") +
    ylab("Verkaufte Menüs: 3. + 4. HSW")+
    geom_text(aes(
        label = paste0(round(tot,digits = 0)), vjust =-.2
    ),colour = "#000000",position = position_dodge(width = .8), size= 8) +
    theme_bw() +
    guides(fill=guide_legend(title="Einteilung 3. + 4. HSW"))+
    theme(plot.title = element_text(size = 20, face = "bold")) +
    theme(text = element_text(size=20),axis.title.y = element_text(size = 20)) +
    theme(axis.title.x = element_text(size = 20)) +
    theme(legend.text = element_text(size = 20),legend.title = element_text(size =
                                                                                20)) +
    theme(axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)))+
    theme(axis.title.x = element_text(size = 20,  margin = margin(t = 20, r = 0, b = 0, l = 0)))

p+ geom_bar(data = loc, aes(y=tot,x=label_content, fill=condit, color=label_content), stat="identity", position=position_dodge(), size=1.25)+
    scale_fill_manual(values = c("#fad60d","#c5b87c")) +
    scale_color_manual(values = c("Fleisch Fisch Vogel" = "black", "Vegetarisch" ="black","Pflanzlich" = "black"))+
    guides(color=F)

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/explorativ/results/menuinhalt2017_180215_04egel.png",
       width = 15, 
       height = 15, 
       dpi = 300, 
       units = "in", 
       device = "png")

# #plot2 (planzlich und eigenständig planzlich getrennt) => locals nicht getrennt angeschaut
# 
# df <- df_7 %>% 
#     group_by(article, date, condit, place, label_content) %>%
#     dplyr::summarise(tot=n())
# df$article <- factor(df$article,levels(df$article)[c(1,7,3,4,6,5,2)]) # change order of factors
# levels(df$label_content)[1] <- "Fleisch Fisch Vogel"
# levels(df$label_content)[5] <- "Vegetarisch"
# levels(df$label_content)[3] <- "Pflanzlich"
# levels(df$label_content)[4] <- "Pflanzlich eigenständig" # eigenständige mahlzeit
# df$label_content <- factor(df$label_content,levels(df$label_content)[c(1,5,3,4,2)]) # change order of factors
# 
# df <- df %>% 
#     group_by(condit, label_content) %>%
#     dplyr::summarise(tot=sum(tot))
# 
# ggplot(df, aes(y=tot,x=label_content, fill=condit)) +
#     geom_bar(colour="black", stat="identity", position=position_dodge()) +
#     scale_fill_manual(values = c("#fad60d","#c5b87c")) +
#     #ggtitle("Verkaufte Mahlzeiten im Jahresvergleich (Quelle: NOVANIMAL, 2017)") +
#     xlab("Menü-Inhalt mit Locals") +
#     ylab("Verkaufte Menüs: 3. + 4. HSW")+
#     geom_text(aes(
#         label = paste0(round(tot,digits = 0)), vjust =-.2
#     ),colour = "#000000",position = position_dodge(width = .8), size= 8) +
#     theme_bw() +
#     guides(fill=guide_legend(title="Einteilung 3. + 4. HSW"))+
#     theme(plot.title = element_text(size = 20, face = "bold")) +
#     theme(text = element_text(size=20),axis.title.y = element_text(size = 20)) +
#     theme(axis.title.x = element_text(size = 20)) +
#     theme(legend.text = element_text(size = 20),legend.title = element_text(size =
#                                                                                 20)) +
#     theme(axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)))+
#     theme(axis.title.x = element_text(size = 20,  margin = margin(t = 20, r = 0, b = 0, l = 0)))
# 
# ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/explorativ/results/menuinhalt2017_180215_03egel.png",
#        width = 15, 
#        height = 15, 
#        dpi = 300, 
#        units = "in", 
#        device = "png")

#########
## 4. Difference between calender weeks (3 and 4)
#########

#####
# a. diff over calendar week 40 and 41 = figure 7
#####
#---------------------
#load data and prepare data
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/explorativ/data")
df_tot <- read.csv2("all hs sales 180202 04egel.csv") # somehow are 4 cases missing in hot and cold 2017

# rename places
levels(df_tot$place)[levels(df_tot$place)=="grüental"] <- "Grüental Mensa"
levels(df_tot$place)[levels(df_tot$place)=="vista"] <- "Vista Mensa"


# aggregate and subset data for plot
df <- group_by(df_tot, article, week, year) %>% summarise(tot=sum(total))
loc <- data.frame(df[grepl("Local *", df$article),]) # all locals
# loc$week <- as.factor((loc$week)) # maybe the problem, however when loc changed to data.frame it worked
loc <- complete(loc,week, nesting(article, year), fill=list(value=NA)) 

#change levels of article for aggregation
df$article <- gsub("Local *","",df$article) # delete every local in string
df$article <- as.factor(df$article) # gsub changes factors to characters
levels(df$article)[levels(df$article)=="Green"] <- "Green/World"
levels(df$article)[levels(df$article)=="World"] <- "Green/World"
df_ <- group_by(df, week,  year) %>% summarise(tot=sum(tot))


df_ <- data.frame(df_)
df_$week <- as.factor(df_$week)
levels(df_$week)[1] <- "3. HSW"
levels(df_$week)[2] <- "4. HSW"

#----------------------
#plot for kw 40 and 41 aggregated => second dataset is used



ggplot(df_, aes(y=tot,x=as.character(year), group=as.character(week), color=as.character(week))) +
    geom_point(size=5) + 
    geom_line(size=0.7)+
    scale_y_continuous(breaks=seq(0, 2500, 500), limits=c(0, 2500))+
    #ggtitle("Verkaufte Mahlzeiten im Jahresvergleich (Quelle: NOVANIMAL, 2017)\n") +
    xlab("Herbstsemester") +
    ylab("Verkaufte Menüs: Total 3. + 4. HSW")+
    scale_color_manual(values = c("#fad60d","#c5b87c"))+
    geom_text(aes(
        label = paste0(round(tot,digits = 0)), vjust =-.9
    ),colour = "#000000", size= 5) +
    theme_bw() +
    # guides(fill=guide_legend(title="sdf"))+ not working => why?
    labs(color="Herbstsemester") +
    theme(plot.title = element_text(size = 20, face = "bold")) +
    theme(text = element_text(size=20),axis.title.y = element_text(size = 20)) +
    theme(axis.title.x = element_text(size = 20)) +
    theme(legend.text = element_text(size = 20),legend.title = element_text(size =
                                                                                20)) +
    theme(axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)))+
    theme(axis.title.x = element_text(size = 20,  margin = margin(t = 20, r = 0, b = 0, l = 0)))

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/explorativ/results/sold_week_180305_03egel.png",
       width = 15, 
       height = 15, 
       dpi = 300, 
       units = "in", 
       device = "png")


#####
# b. diff between menu-line and calendar week  = figure 8
#####

# plot interaction between kalender week and year as well as menu-line
# aggregate data
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/explorativ/data")
df_tot <- read.csv2("all hs sales 180202 04egel.csv") # somehow are 4 cases missing in Hot and Cold 2017

# rename places
levels(df_tot$place)[levels(df_tot$place)=="grüental"] <- "Grüental Mensa"
levels(df_tot$place)[levels(df_tot$place)=="vista"] <- "Vista Mensa"


# aggregate and subset data for plot
df_ <- group_by(df, article, week, year) %>% summarise(tot=sum(tot)) # see above, need to change fist levels then aggregate



df_$week <- as.factor(df_$week)
levels(df_$week)[1] <- "Kalenderwoche 40"
levels(df_$week)[2] <- "Kalenderwoche 41"

df_$article <- factor(df_$article, levels(df_$article)[c(1,2,4,3)]) # alwys double check


ggplot(df_, aes(y=tot,x=article,group=factor(interaction(week,year)), fill=as.character(year))) +
    geom_bar(colour="black", stat="identity", position=position_dodge()) + 
    facet_wrap(~week, ncol=2)+
    xlab("Menü-Linien") +
    ylab("Verkaufte Menüs: 3. + 4. HSW")+
    scale_fill_manual(values = c("2017"="#fad60d","2016" = "#c5b87c","2015" = "#262626"))+
    geom_text(aes(
        label = paste0(round(tot,digits = 0)), vjust =-.2
    ),colour = "#000000",position = position_dodge(width = .8), size= 6) +
    theme_bw() +
    guides(fill=guide_legend(title="Herbstsemester 3. + 4. HSW"))+
    theme(plot.title = element_text(size = 20, face = "bold")) +
    theme(text = element_text(size=20),axis.title.y = element_text(size = 20)) +
    theme(axis.title.x = element_text(size = 20)) +
    theme(legend.text = element_text(size = 20),legend.title = element_text(size =
                                                                                20)) +
    theme(axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)))+
    theme(axis.title.x = element_text(size = 20,  margin = margin(t = 20, r = 0, b = 0, l = 0)))+
    theme(strip.text = element_text(size=20), strip.background = element_rect(fill="white"))


ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/explorativ/results/menulinie_week_180305_03egel.png",
       width = 20, 
       height = 15, 
       dpi = 300, 
       units = "in", 
       device = "png")


#####
# c. diff between menu-content and calendar week = figure 9
#####

# load data

df_ <- read.csv2("all_year_testdata_180201_05egel.csv") #4 cases less in h&c => Mittagszeit von 11 bis 14 uhr

df_$week <- as.factor(df_$week)
levels(df_$week)[1] <- "Kalenderwoche 40"
levels(df_$week)[2] <- "Kalenderwoche 41"

df_$variable <- factor(df_$variable, levels(df_$variable)[c(1,4,3,2)])
df_  <- as_tibble(df_)
f <- df_ %>% complete(variable, nesting(year, week), fill=list(value = NA))


ggplot(f, aes(y=value,x=variable,group=factor(interaction(week,year)), fill=as.character(year))) +
    geom_bar(colour="black", stat="identity", position=position_dodge()) + 
    facet_wrap(~week, ncol=2)+
    xlab("Menü-Inhalt") +
    ylab("Verkaufte Menüs: 3. + 4. HSW")+
    scale_fill_manual(values = c("2017"="#fad60d","2016" = "#c5b87c","2015" = "#262626"))+
    geom_text(aes(
        label = paste0(round(value,digits = 0)), vjust =-.2
    ),colour = "#000000",position = position_dodge(width = .8), size= 6) +
    theme_bw() +
    guides(fill=guide_legend(title="Herbstsemester 3. + 4. HSW"))+
    theme(plot.title = element_text(size = 20, face = "bold")) +
    theme(text = element_text(size=20),axis.title.y = element_text(size = 20)) +
    theme(axis.title.x = element_text(size = 20)) +
    theme(legend.text = element_text(size = 20),legend.title = element_text(size =
                                                                                20)) +
    theme(axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)))+
    theme(axis.title.x = element_text(size = 20,  margin = margin(t = 20, r = 0, b = 0, l = 0)))+
    theme(strip.text = element_text(size=20), strip.background = element_rect(fill="white"))


ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/explorativ/results/menucontent_week_180306_02egel.png",
       width = 20, 
       height = 15, 
       dpi = 300, 
       units = "in", 
       device = "png")



