# environmental impact analysis----
# load data df_agg_tot
source("04_load_data_180802_egel.R")
source("08_theme_plots_180419_egel.R") # mythemes

# how to calculate the daily impact: count sellings and multiplicate them with the tot_ubp and tot_gwp
# eclude all hot and cold and locals
t <- filter(df_agg_tot, label_content != "Hot and Cold" & !grepl("Local ", df_agg_tot$article_description)) %>% 
    mutate(condit = ifelse(.$cycle == 1 & .$week %%2 == 0, "Basis",
                           ifelse(.$cycle == 2 & .$week %%2 == 1, "Basis", "Intervention"))) %>% # add intervention and basisweek
    group_by(meal_name,week, article_description, condit, label_content, tot_ubp, tot_gwp) %>% 
    summarise(tot_sold = n()) %>% 
    mutate(imp_gwp = tot_sold * tot_gwp, # calculate total gwp according sellings e.g. one meal was sold 100 times (each meal contributes 1.4 gwp => total gwp is 140)
           imp_ubp = tot_sold * tot_ubp)

# plot gwp total----
df_ <- group_by(t, condit ,week, label_content )%>% summarise(imp_gwp=sum(imp_gwp))

df_ <- df_ %>% 
    group_by(week,condit) %>% # give in variable, you want to calculate percentage
    mutate(pct_gwp=(imp_gwp/sum(imp_gwp)))

# describe content and condition
aggregate(df_$pct_gwp ~df_$condit+df_$label_content, FUN = mean)


# annotation for selling per week
text <- group_by(t, week) %>% summarise(tot = sum(tot_sold)) %>%
    # mutate(label = "italic(CO[2]*eq)") %>%
    mutate(label2 = paste(label, round(tot,0), sep="=="))

# define x-lab for plot
df_$xlab <- paste(df_$week, df_$condit, sep = "\n")

# define date of creation
st <- strftime(today(), format = "%B %Y")


## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unknown" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
source("09_function_is_dark_190114_egel.R")

df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

# barplot
p <- ggplot(df_, aes(y = imp_gwp,x = as.factor(xlab), fill = factor(label_content, c("Unknown", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "stack", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("Herbstsemesterwochen (Kalenderwochen 40 bis 51)") +
    ylab(expression(paste("\n Anzahl Treibhausgase (THG) in ", "CO"[2],"eq")))+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot and Cold"))+
    scale_color_manual(values = levels(df_$label_color))+
    geom_text(aes(label=ifelse(pct_gwp*100>1.5,paste0(round(pct_gwp*100, digits=0),"%"),"")),size = 10, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    # annotate( 
    #     "text",x = 1:12, y = 1.03, label = text$label2,parse=T, size=6) + # why so big differences to the first version
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz (2017); Karen Muir (2017)", 
         subtitles = "Von 26'000 Transaktionen konnten für 76 Prozent (20'000) die Treibhausgase berechnet werden.\n Hot & Cold (Buffet) und Locals sind hier nicht berücksichtigt worden (Stand: Dezember 2018).")

# save plot as png and pdf
ggsave("plots/intervention_basis_gwp_181211_egel.pdf",
       width = 26, 
       height = 14, 
       dpi = 600, 
       device = cairo_pdf)

# plot ubp total----
df_ <- group_by(t, condit ,week, label_content )%>% summarise(imp_ubp=sum(imp_ubp))

df_ <- df_ %>% 
    group_by(week, condit) %>% # give in variable, you want to calculate percentage
    mutate(pct_ubp = (imp_ubp/sum(imp_ubp)))

# describe content and condition
aggregate(df_$pct_gwp ~df_$condit+df_$label_content, FUN = mean)

# annotation for selling per week
text <- group_by(t, week) %>% summarise(tot = sum(imp_ubp)) %>%
    mutate(label = "italic(UBP)") %>%
    mutate(label2 = paste(label, format(tot, digits = 2, scientific = F), sep="=="))

# define x-lab for plot
df_$xlab <- paste(df_$week, df_$condit, sep = "\n")

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
p <- ggplot(df_, aes(y = imp_ubp,x = as.factor(xlab), fill = factor(label_content, c("Unknown", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "stack", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("Herbstsemesterwochen (Kalenderwochen 40 bis 51)\n") +
    ylab("\nAnzahl Umweltbelastungspunkte (UBP)")+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels = function(x){format(x,big.mark = "'", scientific = F)})+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot and Cold")) +
    scale_color_manual(values = levels(df_$label_color))+
    geom_text(aes(label=ifelse(df_$pct_ubp * 100 > 1.5, paste0(round(df_$pct_ubp * 100, digits = 0),"%"),"")),size = 10, position = position_stack(vjust = 0.5)) + # omit 0% with ifelse()
    # annotate( 
    #     "text",x = 1:12, y = 1.03, label = text$label2,parse=T, size=6) + # why so big differences to the first version
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz (2017); Karen Muir (2017)", 
         subtitles = "Von 26'000 Transaktionen konnten für 76 Prozent (20'000) die Umweltbelastungspunkte berechnet werden.\n Hot & Cold (Buffet) und Locals sind hier nicht berücksichtigt worden (Stand: Dezember 2018).")

# save plot as png and pdf
ggsave("plots/intervention_basis_ubp_181211_egel.pdf",
       width = 26, 
       height = 14, 
       dpi = 600, 
       device = cairo_pdf)

# plot ubp mean per meal and week----
# how to calculate the daily impact: count sellings and multiplicate them with the tot_ubp and tot_gwp
# eclude all hot and cold and locals

# calculated gwp weightet according sellings
# unsure if the right way!

t <- filter(df_agg_tot, label_content != "Hot and Cold" & !grepl("Local ", df_agg_tot$article_description)) %>% 
    mutate(condit = ifelse(.$cycle == 1 & .$week %%2 == 0, "Basis",
                           ifelse(.$cycle == 2 & .$week %%2 == 1, "Basis", "Intervention"))) %>% 
    group_by(meal_name,week, article_description, condit, label_content, tot_ubp, tot_gwp) %>% 
    summarise(tot_sold = n()) %>% 
    mutate(imp_ubp = tot_sold * tot_ubp,
           imp_gwp = tot_sold * tot_gwp) %>% 
    ungroup() %>% 
    group_by(week) %>% 
    mutate(imp_week_ubp = sum(imp_ubp),
           imp_week_gwp = sum(imp_gwp)) %>% 
    ungroup() %>% 
    group_by(week, condit) %>% 
    mutate(pct_sold = tot_sold/sum(tot_sold),
           prop_ubp = pct_sold * imp_week_ubp,
           prop_gwp = pct_sold * imp_week_gwp)

# prepare data for plot           
df_ <- group_by(t, condit ,week, label_content)%>% summarise(imp_ubp_m=sum(prop_ubp),
                                                             tot_pct = sum(pct_sold))

# define x-lab for plot
df_$xlab <- paste(df_$week, df_$condit, sep = "\n")

## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unknown" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

# barplot
p <- ggplot(df_, aes(y = imp_ubp_m, x = as.factor(xlab), fill = factor(label_content, c("Unknown", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "stack", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("Herbstsemesterwochen (Kalenderwochen 40 bis 51)\n") +
    # xlab(cat('"winter semester weeks (Basis: "','meat','" week, Intervention: "','vegetarian','" week"'))+
    ylab("\nAnzahl Umweltbelastungspunkte (UBP)")+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels = function(x){format(x,big.mark = "'", scientific = F)})+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot and Cold")) +
    scale_color_manual(values = levels(df_$label_color))+
    geom_text(aes(label=ifelse(df_$pct_ubp * 100 > 1.5, paste0(round(df_$pct_ubp * 100, digits = 0),"%"),"")),size = 10, position = position_stack(vjust = 0.5)) + # omit 0% with ifelse()
    # annotate( 
    #     "text",x = 1:12, y = 1.03, label = text$label2,parse=T, size=6) + # why so big differences to the first version
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz (2017)", 
         subtitles = "Von 26'000 Transaktionen konnten für 76 Prozent (20'000) die Umweltbelastungspunkte berechnet werden.\n Hot & Cold (Buffet) und Locals sind hier nicht berücksichtigt worden (Stand: Dezember 2018).
         ")

# save plot as png and pdf
ggsave("plots/intervention_basis_ubp_181211_egel.pdf",
       width = 26, 
       height = 14, 
       dpi = 600, 
       device = cairo_pdf)
