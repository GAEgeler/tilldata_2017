#####
## Tilldata analysis: 2015 till 2017
#####

# status: 21.06.18

# required libraries
library(onewaytests)

# laod data: see scripts load DAILY data 2015-2017

# plot overall sold per year, in englisch--------------
# prepare data
df_ <- menu_tot %>% 
    filter(week <=51) %>%
    group_by(year,week) %>% 
    summarise(tot_sold=sum(tot_sold))

# plot
ggplot(df_, aes(y=tot_sold,x=as.factor(week), color=as.character(year), group=year))+
    geom_line(size=1.3) +
    #ggtitle("Verkaufte Menüs: 3. + 4. HSW\n") +
    xlab("autumn semester weeks") +
    ylab("sold meals per semester week")+
    guides(fill=guide_legend("years\n",
                             keywidth=.5,
                             keyheight=.5,
                             default.unit="inch"))+
    scale_color_manual(values = c("2015"="#fad60d","2016" = "#c5b87c","2017" = "#008099"),
                       name="years\n")+
    mytheme

# plot  overall sold per year, in german--------------
# change eventually colors
ggplot(df_, aes(y=tot_sold,x=as.factor(week), color=as.character(year), group=year))+
    geom_line(size=1.3) +
    #ggtitle("Verkaufte Menüs: 3. + 4. HSW\n") +
    xlab("\nHerbstsemester (Kalenderwochen 40 bis 51)") +
    ylab("Verkaufte Menüs pro Semesterwoche\n")+
    guides(fill=guide_legend("years\n",
                             keywidth=.5,
                             keyheight=.5,
                             default.unit="inch"))+
    scale_color_manual(values = c("2015"="grey70","2016" = "#c5b87c","2017" = "#008099"),
                       name="Jahre\n")+
    scale_y_continuous(limits=c(0,3000), breaks = seq(0,3000,500))+
    mytheme

##-----------
ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/verkauf analysen/plots/sold_week_en 2015-2017 180522 egel.pdf",
       width = 12,
       height = 8,
       dpi=600,
       units="in",
       device= cairo_pdf)

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/verkauf analysen/plots/sold_week_en 2015-2017 180522 egel.png",
       width = 12,
       height = 8,
       dpi=600,
       units="in",
       device="png")

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/verkauf analysen/plots/sold_week_de 2015-2017 180614 02egel.png",
       width = 12,
       height = 8,
       dpi=600,
       units="in",
       type="cairo-png")

######
## save plots for presantation somewhere else
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/12_work_pack/wpIII.1_menu_choice/05_besprechungen/180706_sv_fm_meeting/ppp_indd_180627/plots/sold_week_de 2015-2017 180614 03egel.pdf",
       width = 17,
       height = 9,
       dpi = 600,
       units="in",
       device= cairo_pdf)

# statistical analysis: differences between selling years?------
# prepare data: 2017 has more variabels than the other years => aggregate variables
df <- menu_tot %>%
    filter(week <=51)
df$label_content <- gsub("Vegan\\+","Vegetarian",df$label_content) # add vegan to vegetarian menus
df$label_content <- gsub("Vegan","Vegetarian",df$label_content)



df_ <- df %>%
    group_by(label_content, year, semwk) %>% # group data
    summarise(tot_sold = sum(tot_sold))

# boxplot------------
#try with ggplot
png('S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/verkauf analysen/plots/boxplot_sale_year_180620_egel.png', 
    width = 13,
    height = 10,
    units = "in",
    res = 300,
    type="cairo-png")
boxplot(df$tot_sold~df$year+df$label_content,
        ylab="Verkaufte Menüs pro Tag und Menü-Inhalt", 
        xlab="Herbstsemester", 
        col= c("2015"="grey90","2016" = "#c5b87c","2017" = "#80ccff")) # boxplot to see distribution
dev.off()

filter(df,is.na(label_content) == FALSE) %>%
mutate(label_content2 = str_replace(.$label_content,"Vegetarian","Vegetarisch"),
           label_content2 = str_replace(.$label_content,"Meat","Fleisch")) %>%
mutate(xlab = paste(.$label_content2, year, sep = "\n")) %>%
ggplot(aes(xlab,tot_sold, fill=as.factor(year))) + 
    geom_boxplot(na.rm = T)+
    scale_fill_manual(values = c("2015"="grey90","2016" = "#c5b87c","2017" = "#80ccff"))+
    guides(fill = F)+
    scale_y_continuous(limits = c(0,550), breaks = seq(0,550,100))+
    xlab("\nHerbstsemester (Kalenderwochen 40 bis 51)")+
    ylab("Verkaufte Menüs pro Tag und Menü-Inhalt\n")+
    mytheme3

ggsave('S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/verkauf analysen/plots/boxplot_sale_year_180620_03egel.png', 
       width = 13,
       height = 10,
       units = "in",
       dpi = 600,
       type="cairo-png")

######
## save plots for presantation somewhere else
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/12_work_pack/wpIII.1_menu_choice/05_besprechungen/180706_sv_fm_meeting/ppp_indd_180627/plots/boxplot_sale_year_180620_04egel.pdf",
       width = 17,
       height = 8,
       dpi=600,
       units="in",
       device= cairo_pdf)

# statistical tests----------
leveneTest(df_$tot_sold~as.factor(df_$year)) # significant means not homogene variances => test with kruskal wallis
kruskal.test(tot_sold ~ as.factor(year), data=df_) # rank sum test method => not significant


# plot: compare selling data from 2015 till 2017--------------
# prepare data
df <- menu_tot %>%
    filter(week <= 51) # only week from 40 to 51
df$label_content <- gsub("\\+", "",df$label_content) # change vegan+ to vegan and take them toghether
df_ <- df %>%
    group_by(year, label_content) %>% # aggregate data for plotting
    summarise(tot_sold=sum(tot_sold)) %>%
    mutate(pct=tot_sold/sum(tot_sold)) %>% # new variable percenatge per year 
    ungroup()
    
df_$variable2 <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

# define annotation
text <- group_by(df, year) %>% summarise(tot=sum(tot_sold)) %>%
    mutate(tot2=format(tot, big.mark = "'", scientific = F)) %>% # add thousand seperator
    mutate(label = "N") %>%
    mutate(label2=paste(label,tot2,sep=" = "))

## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unknown" = "black","Vegan"="grey90", "Vegan+"="#80ccff", "Vegetarian" = "#c5b87c", "Meat" = "#fad60d","Hot and Cold"="#4c4848")

df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"


# plot data: in english---------
ggplot(df_, aes(y=tot_sold,x=as.character(year), fill=factor(variable2,levels=c("Unknown","Vegan","Vegetarian","Meat","Hot and Cold")), color=label_color)) +
    geom_bar(stat="identity", position = "stack", color=NA) +
    #ggtitle("Verkaufte Menüs: 3. + 4. HSW\n") +
    xlab("autumn semeter") +
    ylab("sold meals per autumn semester")+
    guides(fill=guide_legend("meal content\n",
                             keywidth=.5,
                             keyheight=.5,
                             default.unit="inch"),
           color=F)+
    scale_fill_manual(values = c("Unknown" = "#262626","Hot and Cold"="grey90","Meat" = "#c5b87c","Vegetarian" = "#008099","Vegan"="#fad60d"))+
    geom_text(label=ifelse(df_$pct*100>2,paste0(round(df_$pct*100, digits=0),"%"),""), size = 5, position = position_stack(vjust = 0.5))+ # omit 1% annotation
    annotate("text",x = 1:3, y = 27000, label = text$label2, size=5)+
    mytheme

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/verkauf analysen/plots/sold_autumn_en 2015-2017 180522 egel.png",
       width = 12,
       height = 8,
       dpi=600,
       units="in",
       device="png")

# plot data: in german----------
# prepare data
df <- menu_tot %>%
    filter(week <= 51) # only week from 40 to 51
df$label_content <- gsub("\\+", "",df$label_content) # change vegan+ to vegan and take them toghether
df_ <- df %>%
    group_by(year, label_content) %>% # aggregate data for plotting
    summarise(tot_sold=sum(tot_sold)) %>%
    mutate(pct=tot_sold/sum(tot_sold)) %>% # new variable percenatge per year 
    ungroup()

df_$variable2 <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

# define annotation
text <- group_by(df, year) %>% summarise(tot=sum(tot_sold)) %>%
    mutate(tot2=format(tot, big.mark = "'", scientific = F)) %>% # add thousand seperator
    mutate(label = "n") %>%
    mutate(label2=paste(label,tot2,sep=" = "))

## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unknown" = "black","Vegan"="#80ccff", "Vegetarian" = "#c5b87c", "Meat" = "#fad60d","Hot and Cold"="#4c4848")

df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"


#plot
ggplot(df_, aes(y=tot_sold,x=as.character(year), fill=factor(variable2,levels=c("Unknown","Vegan","Vegetarian","Meat","Hot and Cold")), color=label_color)) +
    geom_bar(stat="identity", position = "stack", color=NA, width = .5) +
    #ggtitle("Verkaufte Menüs: 3. + 4. HSW\n") +
    xlab("Herbstsemester (Kalenderwochen 40 bis 51)") +
    ylab("Verkaufte Menüs pro Herbstsemester")+
    guides(fill=F,
           color=F)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Pflanzlich","Vegetarisch","Fleisch", "Hot and Cold"))+
    scale_color_manual(values = levels(df_$label_color))+
    geom_text(aes(label=ifelse(pct*100>2,paste0(round(pct*100, digits=0),"%"),"")), size = 5, position = position_stack(vjust = 0.5))+ # omit 1% annotation
    annotate("text",x = 1:3, y = 27500, label = text$label2, size=6)+
    mytheme

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/verkauf analysen/plots/sold_autumn_de 2015-2017 180614 02egel.png",
       width = 12,
       height = 8,
       dpi=600,
       units="in",
       device="png")

######
## save plots for presantation somewhere else
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/12_work_pack/wpIII.1_menu_choice/05_besprechungen/180706_sv_fm_meeting/ppp_indd_180627/plots/sold_autumn_de 2015-2017 180614 03egel.pdf",
       width = 17,
       height = 9,
       dpi=600,
       units="in",
       device = cairo_pdf)


# plot: compare selling data from 2015 till 2017 per canteen--------------
# prepare data
df <- menu_tot %>%
    filter(week <= 51)
df$label_content <- gsub("\\+", "",df$label_content) # change vegan+ to vegan and take them toghether
df_ <- df %>%
    group_by(year, label_content, shop_description) %>% # aggregate data for plotting
    summarise(tot_sold=sum(tot_sold))
df_ <- df_%>%
    group_by(year, shop_description) %>%
    mutate(pct=tot_sold/sum(tot_sold)) # new variable percenatge per year 

#define some new variables
df_$variable2 <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)
df_$shop_description[grep("Grüental", df_$shop_description)] <- "Grüental" # shortname of canteen
df_$shop_description[grep("Vista", df_$shop_description)] <- "vista" # shortname of canteen

df_$xlab <- paste(df_$year,df_$shop_description, sep="\n")

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
    ylab("Verkaufte Menüs pro Herbstsemester")+
    guides(fill=F, 
           color=F)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$names,
                      labels = c("Unbekannt","Pflanzlich","Vegetarisch","Fleisch", "Hot and Cold"))+
    scale_color_manual(values = levels(df_$label_color))+
    geom_text(aes(label=ifelse(pct*100>2,paste0(round(pct*100, digits=0),"%"),"")), size = 5, position = position_stack(vjust = 0.5))+ # omit 1% annotation
    annotate("text",x = 1:6, y = 14700, label = text$label2, size=6)+
    mytheme

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/verkauf analysen/plots/sold_autumn_canteen_de 2015-2017 180614 02egel.png",
       width = 16,
       height = 10,
       dpi=600,
       units="in",
       device="png")

######
## save plots for presantation somewhere else
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/12_work_pack/wpIII.1_menu_choice/05_besprechungen/180706_sv_fm_meeting/ppp_indd_180627/plots/sold_autumn_canteen_de 2015-2017 180614 04egel.pdf",
       width = 17,
       height = 9,
       dpi=600,
       units="in",
       device = cairo_pdf)

# statistical analysis: differences between meal-content over the three years------
df <- menu_tot
df$variable <- gsub("Vegan\\+","Vegetarian",df$variable) # add vegan to vegetarian menus
df$variable <- gsub("Vegan","Vegetarian",df$variable)

df_ <- df %>%
    na.omit() %>% 
    group_by(variable, year, semwk) %>% # group data
    summarise(tot_sold = sum(value))

# is not really working, resp. some conditions are not given
boxplot(df_$tot_sold~df_$variable+df_$year) # boxplot to see distribution
leveneTest(df_[df_$year==2015,]$tot_sold,df_[df_$year==2015,]$variable) # no year has independed groups (resp. meal content)
aov_t <- aov(tot_sold ~ variable : as.factor(year), data=df_) # i guess not valid??
summary(aov_t)
tuk <- TukeyHSD(aov_t)

welch.test(tot_sold ~ as.factor(variable) + as.factor(year), data=df_)
friedman.test(df_$tot_sold, df_$variable, df_$year, df_$semwk)
friedman.test(df_)

# melt data for chi square test: see https://mathcracker.com/z_critical_values.php for critical values
df_ <- df %>%
    na.omit() %>% 
    group_by(variable, year) %>% # group data
    summarise(tot_sold = sum(value)) %>% 
    mutate(tot_sold2=round(tot_sold, digits=0))

test <- df %>%
    na.omit() %>%
    group_by(year) %>%
    mutate(pct=value/sum(value))

dM <- df_[rep(1:nrow(df_), df_$tot_sold2),1:2] # put data into long format, NA's are excluded
chisq.test(dM$variable, dM$year)$stdres # adjusted residuals with corrected p value of 0.05/(5*3) = 0.0033 and a critical value of +-2.94
# chisq.test(dM$variable, dM$year)$residuals
CrossTable(dM$variable, dM$year, chisq = T)
