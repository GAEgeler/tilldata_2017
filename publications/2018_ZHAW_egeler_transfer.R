# analyses for tranfer magazine (part one)

# state: oktober 2018
# author: gian-andrea egeler


# load data
source("041_load_data_180802_egel.R") # fish label was added


# difference between conditions according sellings resp meal purchases

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
    mutate(label = paste("n =", tot),
           label_ = format(tot, big.mark = "'")) %>%
    mutate(label2 = paste("italic(n)", format(tot, scientific = F, big.mark = "'"), sep="==")) # with bigmark, parsing the text is not working anymore 


# define date of creation
st <- strftime(today(), format = "%B %Y")


## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unknown" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
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
        "text",x = 1:2, y = 1.03, label = text$label, parse = F, size = 9) + # use deparse and parse after (for thousand separator)
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz (2017)", subtitles = st)


# save plot as png and pdf
ggsave("plots/intervention_basis_agg_181005_egel.pdf",
       width = 25, 
       height = 14, 
       dpi = 600, 
       device = "png")
