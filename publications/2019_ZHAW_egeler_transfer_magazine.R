# analyses for tranfer magazine

# state: january 2019
# author: gian-andrea egeler


# load data df_agg-----
source("04_1_load_data_190128_egel.R")

# group data
t <- df_agg %>% 
    filter(label_content != "Hot and Cold" & !grepl("Local ", df_agg$article_description)) %>% 
    group_by(meal_name, week, article_description, condit, label_content, tot_ubp, tot_gwp) %>% 
    summarise(tot_sold = n()) %>% 
    mutate(imp_gwp = tot_sold * tot_gwp, # calculate total gwp according sellings e.g. one meal was sold 100 times (each meal contributes 1.4 gwp => total gwp is 140)
           imp_ubp = tot_sold * tot_ubp)

# some information about hot and cold and local meals
#locals = 2600 (10%) => nrow(local)/nrow(df_agg)
# basis: 1222
# intervention: 1378

local <- df_agg %>% 
    filter(grepl("Local ", df_agg$article_description)) %>%  
    group_by(condit, week) %>% 
    summarise(tot = n())

summary.lm(aov(local$tot ~ local$condit))

#hot and cold = 3700 (14%) nrow(hnc)/nrow(df_agg)
# basis: 1892
# intervention: 1808
hnc <- df_agg %>% filter(label_content == "Hot and Cold") %>% 
     group_by(condit, week) %>% 
     summarise(tot = n())

summary.lm(aov(hnc$tot ~ hnc$condit))

# environmental impact for whole experiment-------
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
ColsPerCat=c("Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Fisch"="#4c4848")

# detects dark color: for labelling the bars
source("09_function_is_dark_190114_egel.R")

df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

# barplot
p <- ggplot(df_, aes(y = imp_gwp,x = as.factor(xlab), fill = factor(label_content, c("Unknown", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Fisch")), color = label_color)) + 
    geom_bar(stat = "identity", position = "stack", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("Herbstsemesterwochen (Kalenderwochen 40 bis 51)") +
    ylab(expression(paste("\n Global warming potential GWP (kg ", "CO"[2],"eq")))+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch", "Fisch"))+
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
ColsPerCat=c("Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Fisch"="#4c4848")

# detects dark color: for labelling the bars
df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

# barplot
ggplot(df_, aes(y = imp_ubp,x = as.factor(xlab), fill = factor(label_content, c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Fisch")), color = label_color)) + 
    geom_bar(stat = "identity", position = "stack", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("Herbstsemesterwochen (Kalenderwochen 40 bis 51)\n") +
    ylab("\n Umweltbelastungspunkte (UBP)")+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels = function(x){format(x,big.mark = "'", scientific = F)})+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Fisch")) +
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


# plot ubp and gwp per condition according meal content----
# sort and aggregate data
df <- df_agg %>% 
    filter(label_content != "Hot and Cold" & !grepl("Local ", df_agg$article_description)) %>% 
    group_by(condit, label_content, tot_ubp, tot_gwp) %>% 
    summarise(tot_sold = n()) %>%
    mutate(total_ubp = tot_ubp * tot_sold,
           total_gwp = tot_gwp * tot_sold) %>% 
    ungroup() 

# add percent   
df_ <- df %>% 
    group_by(condit, label_content) %>% 
    summarise(total_ubp = sum(total_ubp),
              total_gwp = sum(total_gwp)) %>% 
    mutate(pct_ubp = total_ubp / sum(total_ubp),
           pct_gwp = total_gwp / sum(total_gwp))  
    # melt(.) if i want to do a doblue x-axis plot

# annotate
txt <- group_by(df_, condit) %>% summarise(tot_ubp = sum(total_ubp),
                                           tot_gwp = sum(total_gwp)) %>% 
    mutate(txt_ubp = paste("n =", format(tot_ubp, scientific = T, digits = 3)),
           txt_gwp = paste("n =", round(tot_gwp, 0)))

# define date of creation
st <- strftime(today(), format = "%B %Y")

## check if the background color is dark or not
# see mytheme for function
# my colors for the plot
ColsPerCat=c("Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Fisch"="#4c4848")

# detects dark color: for labelling the bars
df_$label_color <- as.factor(sapply(unlist(ColsPerCat)[df_$label_content], # takes every label and their belonged color
                                    function(color) { if (isDark(color)) 'white' else 'black' })) # check if color is dark, than give back "white" else "black"

# barplot ubp
p <- ggplot(df_, aes(y = total_ubp, x = condit, fill = factor(label_content, c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Fisch")), color = label_color)) + 
    geom_bar(stat = "identity", position = "stack", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    ylab("\n Umweltbelastungspunkte (UBP)")+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels = function(x){format(x,big.mark = "'", scientific = F)})+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Fisch")) +
    scale_color_manual(values = levels(df_$label_color))+
    geom_text(aes(label=ifelse(df_$pct_ubp * 100 > 1.5, paste0(round(df_$pct_ubp * 100, digits = 0),"%"),"")),size = 6, position = position_stack(vjust = 0.5)) + # omit 0% with ifelse()
    annotate( 
         "text",x = 1:2, y = 45000000, label = txt$txt_ubp, size=6) + # attention thousand separator in text!
    mytheme

# calculate mean of ubp per meal 
b <- round(mean(df[df$condit == "Basis",]$tot_ubp))
i <- round(mean(df[df$condit == "Intervention",]$tot_ubp))

p + labs(caption = "Daten: Kassendaten SV Schweiz (2017)\n Berechnungen: Karen Muir (2017)", 
         subtitles = paste("Ein Gericht stiess in den Basiswochen durchnittlich ", b,"kg CO2-eq\n",
                                      "und in den Interventionswochen ", i,"kg CO2-eq aus"))

# referenzwert 20 Millionen UBP pro Jahr und Person

#save
ggsave("plots/ubp_condit_190130_egel.pdf",
       height = 12,
       width = 12,
       dpi = 300,
       device = cairo_pdf)


# barplot gwp
p <- ggplot(df_, aes(y = total_gwp, x = condit, fill = factor(label_content, c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Fisch")), color = label_color)) + 
    geom_bar(stat = "identity", position = "stack", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    ylab(expression(paste("\n Global warming potential GWP (kg ", "CO"[2],"eq)")))+
    guides(fill = guide_legend("Menü-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels = function(x){format(x,big.mark = "'", scientific = F)})+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Fisch")) +
    scale_color_manual(values = levels(df_$label_color))+
    geom_text(aes(label=ifelse(df_$pct_gwp * 100 > 1.5, paste0(round(df_$pct_gwp * 100, digits = 0),"%"),"")),size = 6, position = position_stack(vjust = 0.5)) + # omit 0% with ifelse()
    annotate( 
        "text",x = 1:2, y = 19000, label = txt$txt_gwp, size=6) + # attention thousand separator in text!
    mytheme

# calculate mean of gwp per meal 
b <- round(mean(df[df$condit == "Basis",]$tot_gwp), 2)
i <- round(mean(df[df$condit == "Intervention",]$tot_gwp), 2)
    
p + labs(caption = "Daten: Kassendaten SV Schweiz (2017)\n Berechnungen: Karen Muir (2017)", 
         subtitles = paste("Ein Gericht stiess in den Basiswochen durchnittlich ", b,"kg CO2-eq\n",
                           "und in den Interventionswochen ", i,"kg CO2-eq aus"))

# save plot as png and pdf
ggsave("plots/gwp_condit_190130_egel.pdf",
       height = 12,
       width = 12,
       dpi = 300,
       device = cairo_pdf)
