#####
# label analyses
#####

# status: 27.06.18 // egel


# load data: see load daily data 2015-2017: second Part!

#### plot over the years the sellings per menu-line-----
# prepare data

df <- menu_tot_ %>%
    group_by(article_description, year) %>%
    summarise(tot_sold = sum(tot_sold)) %>%
    ungroup()


# change first some strings
df$article_description[grep("Local+",df$article_description)] <- "Local" # summarize all locals
df$article_description[grep("Green",df$article_description)] <- "Green/World"
df$article_description[grep("World",df$article_description)] <- "Green/World"

# aggregate again because of locale

df <- df %>% group_by(article_description, year) %>%
    summarise(tot_sold = sum(tot_sold))

df <- df %>% # calculate percentage
    group_by(year) %>% mutate(pct = tot_sold/sum(tot_sold))


## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Local" = "black","Favorite"="#c5b87c", "Green/World" = "#fad60d", "Kitchen" = "#008099","Hot and Cold"="#4c4848")

df$label_color <- as.factor(sapply(unlist(ColsPerCat)[df$article_description], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

# define annotation
text <- group_by(df, year) %>% summarise(tot=sum(tot_sold)) %>%
    mutate(tot2=format(tot, big.mark = "'", scientific = F)) %>% # add thousand seperator
    mutate(label = "n") %>%
    mutate(label2=paste(label,tot2,sep=" = "))

# plot data: in german----------
ggplot(df, aes(y=tot_sold,x=as.factor(year), fill=factor(article_description,levels=c("Local","Kitchen","Green/World","Favorite","Hot and Cold")), color = label_color)) +
    geom_bar(stat="identity", position = "stack", color=NA, width = .6) +
    #ggtitle("Verkaufte Menüs: 3. + 4. HSW\n") +
    xlab("Herbstsemester (Kalenderwochen 40 bis 51)") +
    ylab("Verkaufte Menüs pro Herbstsemester")+
    guides(fill=F, 
           color=F)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$names,
                      labels = c("Local","Kitchen","Green/World","Favorite","Hot and Cold"))+
    scale_color_manual(values = levels(df$label_color))+
    geom_text(aes(label=ifelse(pct*100>2,paste0(round(pct*100, digits=0),"%"),"")), size = 5, position = position_stack(vjust = 0.5))+ # omit 1% annotation
    annotate("text",x = 1:3, y = 27500, label = text$label2, size=6)+
    mytheme


ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/label analyse/plots/label 2015-2017 180628 egel.png",
       width = 12,
       height = 8,
       dpi=600,
       units="in",
       device="png")

######
## save plots for presantation somewhere else
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/12_work_pack/wpIII.1_menu_choice/05_besprechungen/180706_sv_fm_meeting/input_kassendaten_indd_180627/plots/label 2015-2017 180628 egel.pdf",
       width = 17,
       height = 9,
       dpi=600,
       units="in",
       device = cairo_pdf)


#### plot over the years the sellings per canteen-----
# prepare data
df <- menu_tot_ %>%
    group_by(article_description, shop_description, year) %>%
    summarise(tot_sold = sum(tot_sold)) %>%
    ungroup()


# change first some strings
df$article_description[grep("Local+",df$article_description)] <- "Local" # summarize all locals
df$article_description[grep("Green",df$article_description)] <- "Green/World"
df$article_description[grep("World",df$article_description)] <- "Green/World"

df$shop_description[grep("Grüental", df$shop_description)] <- "Grüental"
df$shop_description[grep("Vista", df$shop_description)] <- "Vista"

# aggregate again because of locale

df <- df %>% group_by(article_description, shop_description, year) %>%
    summarise(tot_sold = sum(tot_sold))

df <- df %>% # calculate percentage
    group_by(year, shop_description) %>% mutate(pct = tot_sold/sum(tot_sold))

# define xlab
df$xlab <- paste(df$year,df$shop_description, sep="\n")

## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Local" = "black","Favorite"="#c5b87c", "Green/World" = "#fad60d", "Kitchen" = "#008099","Hot and Cold"="#4c4848")

df$label_color <- as.factor(sapply(unlist(ColsPerCat)[df$article_description], # takes every label and their belonged color
                                   function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

# define annotation
text <- group_by(df, year, shop_description) %>% summarise(tot=sum(tot_sold)) %>%
    mutate(tot2=format(tot, big.mark = "'", scientific = F)) %>% # add thousand seperator
    mutate(label = "n") %>%
    mutate(label2=paste(label,tot2,sep=" = "))

# plot in german
ggplot(df, aes(y=tot_sold,x=xlab, fill=factor(article_description,levels=c("Local","Kitchen","Green/World","Favorite","Hot and Cold")), color = label_color)) +
    geom_bar(stat="identity", position = "stack", color=NA, width = .6) +
    #ggtitle("Verkaufte Menüs: 3. + 4. HSW\n") +
    xlab("Herbstsemester (Kalenderwochen 40 bis 51)") +
    ylab("Verkaufte Menüs pro Herbstsemester")+
    guides(fill=F, 
           color=F)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$names,
                      labels = c("Local","Kitchen","Green/World","Favorite","Hot and Cold"))+
    scale_color_manual(values = levels(df$label_color))+
    geom_text(aes(label=ifelse(pct*100>2,paste0(round(pct*100, digits=0),"%"),"")), size = 5, position = position_stack(vjust = 0.5))+ # omit 1% annotation
    annotate("text",x = 1:6, y = 14900, label = text$label2, size=6)+
    mytheme

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/label analyse/plots/label_canteen 2015-2017 180628 egel.png",
       width = 12,
       height = 8,
       dpi=600,
       units="in",
       device="png")

######
## save plots for presantation somewhere else
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/12_work_pack/wpIII.1_menu_choice/05_besprechungen/180706_sv_fm_meeting/input_kassendaten_indd_180627/plots/label_canteen 2015-2017 180628 egel.pdf",
       width = 17,
       height = 9,
       dpi=600,
       units="in",
       device = cairo_pdf)

