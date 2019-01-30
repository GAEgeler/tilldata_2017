# analyses for impact magazine

# state: january 2019
# author: gian-andrea egeler


# load individual data
source("04_load_data_180802_egel.R")
source("08_theme_plots_180419_egel.R") # themes for plots
source("09_function_is_dark_190114_egel.R") # load function isDark


# differences between men and women a la menuCH-------
# create age_groups
dat_g <- df_2017 %>% 
    mutate(category=cut(df_2017$age,breaks=c(-Inf, 25, 35, 50, 65, Inf), 
                        labels=c("16 bis 25-jährig","26 bis 34-jährig","35 bis 49-jährig","50 bis 64-jährig","keine Angaben"))) %>%
    group_by(gender, category, label_content) %>% 
    summarise(tot_sold = n()) %>% 
    mutate(pct = tot_sold / sum(tot_sold)) %>% 
    ungroup()

# create new variable xlab for plot    
dat <- dat_g %>% 
    mutate(gender = recode(gender, "F" = "Frauen", "M" = "Männer", .missing = "Spezialkarten")) %>% 
    mutate(xlab = paste(gender, category, sep = "\n")) %>% 
    mutate(label_content = ifelse(is.na(label_content), "Unbekannt", label_content)) %>% 
    arrange(category) # for order in plot

# define annotation
txt <- group_by(dat, xlab, category) %>% 
    summarise(tot = sum(tot_sold)) %>% 
    mutate(label = paste("italic(n)", tot, sep = "==")) %>% 
    arrange(category)

#prepare data for plot
ColsPerCat=c("Unbekannt" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
dat$label_color <- as.factor(sapply(unlist(ColsPerCat)[dat$label_content], 
                                    function(color) { if (isDark(color)) 'white' else 'black' })) 

#ATTENTION parse_factor throws errors because of umlaute use encoding latin1!!

#plot
p <- ggplot(dat, aes(y = pct, x = parse_factor(xlab, levels = c(), locale = locale(encoding = "latin1")), fill = factor(label_content, c("Unbekannt", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    ylab("\nVerkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("MenÃ¼-Inhalt\n"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat,
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot & Cold (Buffet)"))+
    scale_color_manual(values = levels(dat$label_color))+
    geom_text(aes(label=ifelse(pct<0.02,"",scales::percent(round(pct,2)))), size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    ggplot2annotate("text", x = 1:10, y = 1.03, label = txt$label, parse=T, size=9) + # NLP Package has annotate function as well
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")

# differences between men and women per label content-------
dat <- df_2017 %>% 
    group_by(gender, label_content, condit) %>% 
    summarise(tot_sold = n()) %>% 
    ungroup() %>% 
    mutate(gender = recode(gender, "F" = "Frauen", "M" = "Männer", .missing = "Spezialkarten"),
           label_content = recode(label_content, .missing = "Unbekannt")) %>% 
    group_by(label_content, gender) %>% 
    summarise(tot_sold = sum(tot_sold)) %>% 
    mutate(pct = tot_sold / sum(tot_sold)) %>% # add percent calculation
    mutate(xlab = paste(gender, label_content, sep = "\n"))


# define annotation
txt <- group_by(dat, label_content) %>% 
    summarise(tot = sum(tot_sold)) %>% 
    mutate(label = paste("italic(n)", tot, sep = "==")) %>% 
    mutate(label_content = factor(label_content, c("Unbekannt", "Hot and Cold", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch"))) %>% 
    arrange(label_content) # sort according dat

#prepare data for plot
ColsPerCat=c("Unbekannt" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
dat$label_color <- as.factor(sapply(unlist(ColsPerCat)[dat$label_content], 
                                    function(color) { if (isDark(color)) 'white' else 'black' })) 

#ATTENTION parse_factor throws errors because of umlaute use encoding latin1!!

#plot
p <- ggplot(dat, aes(y = pct, x = factor(label_content, c("Unbekannt", "Hot and Cold", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch")), fill = gender)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    ylab("Verkaufte Gerichte in Prozent")+
    guides(fill = guide_legend(""),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = c("Frauen" = "#99f200", "Männer" = "#6619e6", "Spezialkarten" = "#008099"), 
                      breaks = c("Spezialkarten", "Männer", "Frauen")) +
    scale_x_discrete( breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot & Cold (Buffet)"))+
    # scale_color_manual(values = levels(dat$label_color))+
    geom_text(aes(label=ifelse(pct<0.02,"",scales::percent(round(pct,2)))), size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    ggplot2::annotate("text", x = 1:6, y = 1.03, label = txt$label, parse=T, size=9) +
    coord_flip() +
    
    mytheme +
    theme(legend.position = "bottom")

p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")


# differences between men and women per label content per condition-------
dat <- df_2017 %>% 
    group_by(label_content, gender, condit) %>% 
    summarise(tot_sold = n()) %>%
    ungroup() %>% 
    group_by(gender, condit) %>% 
    mutate(pct = tot_sold / sum(tot_sold)) %>% # test if right
    ungroup() %>% 
    mutate(gender = recode(gender, "F" = "Frauen", "M" = "Männer", .missing = "Spezialkarten"),
           label_content = recode(label_content, .missing = "Unbekannt")) %>% 
    mutate(xlab = paste(gender, condit, sep = "\n"))


# define annotation
txt <- group_by(dat, gender, condit, xlab) %>% 
    summarise(tot = sum(tot_sold)) %>% 
    mutate(label = paste("italic(n)", tot, sep = "==")) %>% 
    arrange(xlab) # sort according dat

#prepare data for plot
ColsPerCat=c("Unbekannt" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")

# detects dark color: for labelling the bars
dat$label_color <- as.factor(sapply(unlist(ColsPerCat)[dat$label_content], 
                                    function(color) { if (isDark(color)) 'white' else 'black' })) 

#ATTENTION parse_factor throws errors because of umlaute use encoding latin1!!

#plot
p <- ggplot(dat, aes(y = pct, x = xlab, fill = factor(label_content, c("Unbekannt", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch", "Hot and Cold")), color = label_color)) + 
    geom_bar(stat = "identity", position = "fill", color = NA, width = .6) + # set color NA otherwise error occurs
    xlab("") +
    ylab("Verkaufte Gerichte in Prozent")+
    guides(fill = guide_legend(""),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat, 
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot & Cold (Buffet)")) +
    scale_color_manual(values = levels(dat$label_color))+
    geom_text(aes(label=ifelse(pct<0.02,"",scales::percent(round(pct,2)))), size = 8, position = position_stack(vjust = 0.5))+ # omit 0% with ifelse()
    ggplot2::annotate("text", x = 1:6, y = 1.03, label = txt$label, parse=T, size=9) +
    mytheme 

p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")

# save
ggsave("plots/gender_condit_190129_egel.pdf",
       height = 15,
       width = 18,
       dpi = 600,
       device = cairo_pdf)



# differences between men and women aggregated per label content-------
dat <- df_2017 %>% 
    group_by(gender, label_content) %>% 
    summarise(tot_sold = n()) %>% 
    mutate(pct = tot_sold / sum(tot_sold)) %>% # add percent calculation
    ungroup() %>% 
    mutate(gender = recode(gender, "F" = "Frauen", "M" = "Männer", .missing = "Spezialkarten"),
           label_content = recode(label_content, .missing = "Unbekannt")) %>% 
    filter(gender != "Spezialkarten")


# count male and female
g <- df_2017 %>% filter(!duplicated(ccrs)) %>% 
    group_by(gender) %>%  
    count() %>% 
    drop_na() %>% 
    ungroup() %>% 
    mutate(gender = recode(gender, "F" = "Frauen", "M" = "Männer"),
           label = paste(n, gender, sep = " "))

# define annotation
pl <- group_by(dat, gender) %>% 
    summarise(tot = sum(tot_sold)) %>% 
    mutate(label_ = paste("(n =", format(tot, big.mark = "'", scientific = F),")"),
           label = paste(g$label, label_, sep = "\n")) %>%
    left_join(dat, ., by = "gender") %>% 
    # mutate(xlab = paste(gender,g$label, label, sep = "\n")) %>% 
    mutate(label_content = parse_factor(label_content, levels = c())) %>% 
    arrange(gender, -tot_sold)

pl <- pl[order(pl$gender, -pl$tot_sold),] # somehow not possible


c("Unbekannt", "Hot and Cold", "Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch")

#prepare data for plot
ColsPerCat=c("Unbekannt" = "black","Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fleisch" = "#fad60d","Hot and Cold"="#4c4848")


#plot => its difficult to sort the data!!
p <- ggplot(pl, aes(y = pct, x = label,  fill = parse_factor(label_content, levels = c()))) + 
    geom_bar(stat = "identity", position = position_dodge(width = NULL) , color = NA, width = .8) + # set color NA otherwise error occurs
    xlab("") +
    ylab("Verkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("Menü-Inhalt"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = ColsPerCat, 
                      breaks = attributes(ColsPerCat)$name,
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot & Cold (Buffet)")) +
    geom_text(aes(label=ifelse(pct<0.02,"",scales::percent(round(pct,2)))), size = 8, position = position_dodge(width=.8), vjust=-0.25)+ # omit 0% with ifelse()
    mytheme

p + labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)")

# save
ggsave("plots/gender_aggregated_190130_egel.pdf",
       height = 15,
       width = 18,
       dpi = 600,
       device = cairo_pdf)

# only facet wrap sorts gender according sellings-----
# ATTENTION: code is not good jet
# define annotation
dat <- df_2017 %>% 
    group_by(gender, label_content) %>% 
    summarise(tot_sold = n()) %>% 
    mutate(pct = tot_sold / sum(tot_sold)) %>% # add percent calculation
    ungroup() %>% 
    mutate(gender = recode(gender, "F" = "Frauen", "M" = "Männer", .missing = "Spezialkarten"),
           label_content = recode(label_content, .missing = "Unbekannt")) %>% 
    filter(gender != "Spezialkarten") %>% 
    mutate(label_content = recode(label_content, "Pflanzlich" = "Vegan (Fleischersatz)", "Pflantlich+" = "Vegan (authentisch)", 
                                  "Vegetarisch" = "Ovo-lakto-vegetarisch", "Fleisch" = "Fleisch oder Fisch",
                                  "Hot and Cold" = "Hot & Cold (Buffet)")) 

#plot
source("12_function_order_facets_190130_egel.R") # functions to order within facets

# ordering works, however renaming is not possible anymore
# still some errors!
ggplot(dat, aes(y = pct, x = reorder_within(dat$label_content, -dat$tot_sold, dat$gender), fill = label_content)) + 
    geom_bar(stat = "identity", position = position_dodge(width = NULL) , color = NA, width = .8) + # set color NA otherwise error occurs
    xlab("") +
    ylab("Verkaufte Gerichte in Prozent")+
    guides(fill = guide_legend("Menü-Inhalt"),
           color = F)+
    scale_y_continuous(labels=scales::percent)+
    scale_x_reordered()+ # get back the origin names
    scale_fill_manual(values = ColsPerCat,
                      breaks = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot & Cold (Buffet)"),
                      labels = c("Unbekannt","Vegan (Fleischersatz)", "Vegan (authentisch)", "Ovo-lakto-vegetarisch", "Fleisch oder Fisch", "Hot & Cold (Buffet)")) +
    geom_text(aes(label=ifelse(pct<0.02,"",scales::percent(round(pct,2)))), size = 8, position = position_dodge(width=.8), vjust=-0.25)+ # omit 0% with ifelse()
    facet_wrap(~ gender, scales = "free") +
    mytheme
