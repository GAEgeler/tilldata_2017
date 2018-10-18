## different plots -----

# status 16.10.18 // egel

# required packages
pack <- c("dplyr", "lubridate", "readr", "stringr", "readxl", "ggplot2", "reshape2")
lapply(pack, function(x){do.call("library", list(x))})


# load data => see script 04_load_data
# load themes for plotting
source("08_theme_plots_180419_egel.R")

### intervention vs. base: plot data over both cycles and with label content------
#prepare data for plot: aggregated data
df_ <- group_by(df_agg, condit ,week, label_content )%>% summarise(tot_sold=n())

df_ <- df_ %>% 
    group_by(week,condit) %>% # give in variable, you want to calculate percentage
    mutate(pct=(tot_sold/sum(tot_sold)))

# ranem NA to unknown
df_$label_content <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

# annotation for selling per week
text <- group_by(df_agg, week) %>% summarise(tot = n()) %>%
    mutate(label = "italic(n)") %>%
    mutate(label2 = paste(label, tot, sep="=="))

# define x-lab for plot
df_$xlab <- paste(df_$week, df_$condit, sep = "\n")

# define date of creation
st <- strftime(today(), format = "%B %Y")


## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unknown" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")
 
# detects dark color: for labelling the bars
isDark <- function(color) {
    (sum(grDevices::col2rgb(color) *c(299, 587,114))/1000 < 123)
}

df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

# barplot
p <- ggplot(df_, aes(y = pct,x = as.factor(xlab), fill = factor(label_content, c("Unknown", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("Herbstsemesterwochen (Kalenderwochen 40 bis 51)") +
    # xlab(cat('"winter semester weeks (Basis: "','meat','" week, Intervention: "','vegetarian','" week"'))+
    ylab("\nVerkaufte Menüs in Prozent")+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","vegan (Fleischersatz)", "vegan (authentisch)", "ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot and Cold"))+
    scale_color_manual(values = levels(df_$label_color))+
    geom_text(aes(label=ifelse(pct*100>1.5,paste0(round(pct*100, digits=0),"%"),"")),size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:12, y = 1.03, label = text$label2,parse=T, size=9) + # why so big differences to the first version
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz (2017)", subtitles = st)

# save plot as png and pdf
ggsave("plots/intervention_basis_181005_egel.png",
       width = 25, 
       height = 14, 
       dpi = 600, 
       device = "png")

ggsave("plots/intervention_basis_181005_egel.pdf",
       width = 25,
       height = 14,
       dpi = 600,
       units="in",
       device= cairo_pdf)

### intervention vs. base aggregated: plot data over both cycles and with label content------
#prepare data for plot: aggregated data
df_ <- group_by(df_agg, condit ,label_content )%>% summarise(tot_sold=n())

df_ <- df_ %>% 
    group_by(condit) %>% # give in variable, you want to calculate percentage
    mutate(pct=(tot_sold/sum(tot_sold)))

# ranem NA to unknown
df_$label_content <- ifelse(is.na(df_$label_content),"Unknown",df_$label_content)

# annotation for selling per week
text <- group_by(df_agg, condit) %>% summarise(tot = n()) %>%
    mutate(label = format(tot, big.mark = "'")) %>%
    mutate(label2 = paste("italic(n)", format(tot, scientific = F, big.mark = "'"), sep="==")) # with bigmark, parsing the text is not working anymore 


# define date of creation
st <- strftime(today(), format = "%B %Y")


## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unknown" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
isDark <- function(color) {
    (sum(grDevices::col2rgb(color) *c(299, 587,114))/1000 < 123)
}

df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

# barplot
p <- ggplot(df_, aes(y = pct, x = condit, fill = factor(label_content, c("Unknown", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("Herbstsemester HS17 (Basis: 'fleischlastige' Wochen, Intervention: 'pflanzenlastige' Wochen")+
    ylab("\nVerkaufte Menüs in Prozent")+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels = scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt", "vegan (Fleischersatz)", "vegan (authentisch)", "ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot and Cold"))+
    scale_color_manual(values = levels(df_$label_color))+
    geom_text(aes(label=ifelse(pct * 100 > 1.5, paste0(round(pct*100, digits = 0),"%"),"")),size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    annotate( 
        "text",x = 1:2, y = 1.03, label = paste("italic(n)", bquote(.(text$label)), sep= "="), parse = T, size = 9) + # why so big differences to the first version
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz (2017)", subtitles = st)


# save plot as png and pdf
ggsave("plots/intervention_basis_agg_181005_egel.png",
       width = 25, 
       height = 14, 
       dpi = 600, 
       device = "png")

ggsave("plots/intervention_basis_agg_181005_egel.pdf",
       width = 20,
       height = 18,
       dpi = 600,
       units="in",
       device= cairo_pdf)

# planed vs. real meal offer------
# target for 2015 and 2016 and 2017 for both canteens (per canteen 120 meals) and for both cycles: 480 melas in total
# per canten and cycle: meat:54 (30 + 30*.8), vegetarian:36, hot and cold: 30

df_plan_56 <- tibble(year = rep(2015, times=360, each=1),
                     label_content = rep(c("Fleisch","Vegetarisch"), times=c((120 + 120*.8),(120 + 120*.2))),
                     offer = "Geplant")

# plan for intervention:: per canten and cycle: meat: 45, vegetarian: 30, hot and cold: 30, vegan: 7, vegan+: 8
# plan for basis:: per canteen and cycle: meat: 60, vegetarian: 30, hot and cold: 30

df_plan_7 <- tibble(year = rep(2017, times=360),
                    label_content = rep(c("Fleisch","Vegetarisch","Pflanzlich","Pflanzlich+"), times=c(180,120,28,32)),
                    offer= "Geplant")

# Actual: Locals are included
# use documentation for that (not selling data)
# exclude hot and cold (only using filter() is deleting missing cases!)
df_actual_7 <- info_orig %>% filter(!grepl("Hot and Cold", info_orig$label_content)) %>% mutate(offer = "Angebot", year = year(date)) %>%
    select(year,label_content,offer)

# Merge data frames
df_t= bind_rows(df_plan_56, df_plan_7, df_actual_7)

# Group data frame
df_ <- df_t %>%
    group_by(year,label_content, offer) %>%
    summarise(tot=n()) %>%
    ungroup() # otherwise dataframe is still grouped

# define some variables
df_$xlab <- paste(df_$year,df_$offer, sep = "\n")
df_$xlab <- str_replace(df_$xlab,"2015\nGeplant","2015 - 2016\nGeplant")
df_$label_content <- ifelse(is.na(df_$label_content),"Unbekannt",df_$label_content)

# define text for annotation
text <- group_by(df_, year, offer) %>%
    summarise(tot=sum(tot)) %>%
    mutate(label = "n") %>%
    mutate(label2=paste(label,tot,sep=" = "))

## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unbekannt" = "black","Pflanzlich"="grey90", "Pflanzlich+"="#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"


#plot in de
ggplot(df_, aes(y=tot,x=xlab, fill=factor(label_content,levels=c("Unknown","Pflanzlich","Pflanzlich+","Vegetarisch","Fleisch","Hot and Cold")), color=label_color)) +
    geom_bar(stat="identity", position = "stack", color=NA, width = .6) +
    xlab("\nGeplante und angebotene Menü-Optionen während Herbstsemester (Kalenderwochen 40 bis 51)") + 
    ylab("Geplante und angebotene Menü-Optionen")+ 
    #     ggtitle("note: locals are not included
    #          selling time between 9 a.m. until 3 p.m.
    #          locals meals in total: 2602")+
    guides(fill=F,
           color=F)+
    # scale_y_continuous(labels = scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischsubstitut)","Pflanzlich (authentisch)","Vegetarisch","Fleisch oder Fisch", "Hot and Cold"))+
    scale_color_manual(values = levels(df_$label_color))+
    scale_x_discrete(limits=c("2015 - 2016\nGeplant","2017\nGeplant","2017\nAngebot"))+
    geom_text(aes(label=ifelse(tot>4,tot,"")), size = 8, position = position_stack(vjust = 0.5))+ #omit numbers to be shown smaller than 190
    annotate("text",x=1:3,y=500,label=c(text$label2[1],text$label2[1],text$label2[2]), size=8)+
    mytheme
