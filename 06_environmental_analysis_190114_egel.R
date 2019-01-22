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

# environmental impact per meal and week----
# take only first cycle
# eclude all hot and cold and locals

t <- df_agg_tot %>% 
    filter(label_content != "Hot and Cold" & !grepl("Local ", df_agg_tot$article_description)) %>% 
    mutate(condit = ifelse(.$cycle == 1 & .$week %%2 == 0, "Basis",
                           ifelse(.$cycle == 2 & .$week %%2 == 1, "Basis", "Intervention"))) %>% 
    filter(cycle == 1) %>% 
    group_by(meal_name,week, article_description, condit, label_content, tot_ubp, tot_gwp) %>% 
    summarise(tot_sold = n()) %>% 
    ungroup() %>% 
    group_by(week, label_content, condit) %>% 
    summarise(tot_ubp = sum(tot_ubp),
              tot_gwp = sum(tot_gwp)) %>%  # sum for each label content total environmental impact (however not according sellings)
    mutate(imp_week_ubp = tot_ubp / 5,
           imp_week_gwp = tot_gwp / 5) # impact per week and label content
    

# prepare data for plot           
df_ <- t %>% 
    group_by(week, condit) %>% 
    mutate(pct_ubp = imp_week_ubp / sum(imp_week_ubp),
           pct_gwp = imp_week_gwp / sum(imp_week_gwp)) 

# define x-lab for plot
df_$xlab <- paste(df_$week, df_$condit, sep = "\n")

## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Unknown" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

# annotate
txt <- group_by(t, week) %>% summarise(tot_ubp_week = sum(imp_week_ubp)) %>% 
    mutate(label = paste("italic(UBP/Woche)",format(tot_ubp_week, scientific = F, digits = 0), sep = "==")) # big mark causes errors in ggplot

# plot ubp per week and meal content --------
p <- ggplot(df_, aes(y = pct_ubp, x = as.factor(xlab), fill = factor(label_content, c("Unknown", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "stack", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("Herbstsemesterwochen (Kalenderwochen 40 bis 51)\n") +
    ylab("\nAnzahl Umweltbelastungspunkte (UBP) pro Tag und Menü-Inhalt")+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels = scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot and Cold")) +
    scale_color_manual(values = levels(df_$label_color))+
    geom_text(aes(label=ifelse(df_$pct_ubp * 100 > 1.5, paste0(round(df_$pct_ubp * 100, digits = 0),"%"),"")),size = 10, position = position_stack(vjust = 0.5)) + # omit 0% with ifelse()
    annotate( 
         "text",x = 1:6, y = 1.03, label = txt$label,parse=T, size=6) + # why so big differences to the first version
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz (2017)", 
         subtitles = "Von 26'000 Transaktionen konnten für 76 Prozent (20'000) die Umweltbelastungspunkte berechnet werden.\n 
Hot & Cold (Buffet) und Locals sind hier nicht berücksichtigt worden.\n
Mittelwert pro Tag und Menü-Inhalt (Stand: Januar 2019).
         ")

# save plot as png and pdf
ggsave("plots/intervention_basis_ubp_daily_181211_egel.pdf", p,
       width = 26, 
       height = 14, 
       dpi = 600, 
       device = cairo_pdf)

# plot gwp per week and meal content --------

# annotate
txt <- group_by(t, week) %>% summarise(tot_gwp_week = sum(imp_week_gwp)) %>% 
    mutate(label = paste("italic(UBP/Woche)",format(tot_gwp_week, scientific = F, digits = 3), sep = "==")) # big mark causes errors in ggplot



p <- ggplot(df_, aes(y = pct_gwp, x = as.factor(xlab), fill = factor(label_content, c("Unknown", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "stack", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("Herbstsemesterwochen (Kalenderwochen 40 bis 51)\n") +
    ylab("\nAnzahl Treibhausgase (THG) pro Tag und Menü-Inhalt")+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels = scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot and Cold")) +
    scale_color_manual(values = levels(df_$label_color))+
    geom_text(aes(label=ifelse(df_$pct_gwp * 100 > 1.5, paste0(round(df_$pct_gwp * 100, digits = 0),"%"),"")),size = 10, position = position_stack(vjust = 0.5)) + # omit 0% with ifelse()
    annotate( 
        "text",x = 1:6, y = 1.03, label = txt$label,parse=T, size=6) + # why so big differences to the first version
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz (2017)", 
         subtitles = "Von 26'000 Transaktionen konnten für 76 Prozent (20'000) die Umweltbelastungspunkte berechnet werden.\n 
Hot & Cold (Buffet) und Locals sind hier nicht berücksichtigt worden.\n
Mittelwert pro Tag und Menü-Inhalt (Stand: Januar 2019).
         ")

# save plot as png and pdf
ggsave("plots/intervention_basis_gwp_daily_181211_egel.pdf", p,
       width = 26, 
       height = 14, 
       dpi = 600, 
       device = cairo_pdf)


# plot ubp per meal and week according sellings----
# sort and aggregate data
t <- df_agg_tot %>% 
    filter(label_content != "Hot and Cold" & !grepl("Local ", df_agg_tot$article_description)) %>% 
    mutate(condit = ifelse(.$cycle == 1 & .$week %%2 == 0, "Basis",
                           ifelse(.$cycle == 2 & .$week %%2 == 1, "Basis", "Intervention"))) %>% 
    group_by(meal_name,week, article_description, condit, label_content, tot_ubp, tot_gwp) %>% 
    summarise(tot_sold = n()) %>% 
    ungroup() %>% 
    group_by(week, label_content, condit) %>% 
    summarise(tot_ubp = sum(tot_ubp),
              tot_gwp = sum(tot_gwp)) %>%  # sum for each label content total environmental impact (however not according sellings)
    mutate(imp_week_ubp = tot_ubp / 5,
           imp_week_gwp = tot_gwp / 5) # impact per week and label content


# prepare data for plot           
df_ <- t %>% 
    group_by(week, condit) %>% 
    mutate(pct_ubp = imp_week_ubp / sum(imp_week_ubp),
           pct_gwp = imp_week_gwp / sum(imp_week_gwp)) 

# define x-lab for plot
df_$xlab <- paste(df_$week, df_$condit, sep = "\n")

