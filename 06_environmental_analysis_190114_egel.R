# environmental impact analysis----

###
# state: january 2019
# author: gian-Andrea egeler
###

# overview of all 93 meals according meal content and meal substances-----
# load data
source("05_1_load_add_data_190128_egel.R") # do only need ubp_ and gwp_1
source("08_theme_plots_180419_egel.R")


# plot all menus ubp----
# melt into long format
# attention the ubp is containig all meals from both cycles => to avoid that extract all duplicates!
ubp_long <- melt(ubp_, id.vars = c("meal_name.y", "meal_name_comp", "date", "cycle", "article_description", "label_content.y", "label_content.x", "tot_ubp"), measure.vars = c("Kohlenhydrate", "Protein", "gemuse_fruchte", "ol_fett_nuss", "suss_salz_alk", "foodwaste", "zubereitung"), variable.name = "content", value.name = "ubp")

# plot order of ubp_points
# define order
order_ <- ubp_long %>% 
    filter(!duplicated(meal_name.y)) %>%
    arrange(tot_ubp) %>%
    arrange(factor(label_content.y, levels = c("Pflanzlich","Pflanzlich+","Vegetarisch", "Fisch", "Fleisch"))) %>%
    dplyr::select(meal_name_comp)

# somehow not working => find a better way => aggregate(FUN=mean)
#text1 <- aggregate(ubp ~label_content.y, data=ubp_long, FUN = function(ebp_points){sqrt(var(ebp_points))}) #try to calculate standard deviation 
text <- aggregate(tot_ubp ~ label_content.y, data=ubp_long, FUN=mean) # mean calculation
text_ <- aggregate(tot_ubp ~ label_content.y, data=ubp_long, FUN=median) # median             
txt <- left_join(text, text_, by="label_content.y") %>% 
    rename(mean = tot_ubp.x, median = tot_ubp.y) %>%
    mutate(label2 = paste("(","italic(bar(X))", "==", round(mean, digits = 0) ,")")) %>%
    mutate(label1= paste("italic(Median)", "==", round(median, digits = 0))) %>%
    mutate(label3 = paste(label1, label2, sep = "\n"))

# add date of creation
st = strftime(today(), format = "%d.%m.%y")

# plot
p <- ggplot(ubp_long, aes(x = meal_name_comp,y = ubp, fill = factor(content, levels = c("foodwaste", "suss_salz_alk", "ol_fett_nuss", "gemuse_fruchte", "Protein", "Kohlenhydrate", "zubereitung"))))+
    geom_bar(stat="identity", position = "stack", width = .6)+
    guides(fill = F)+
    scale_x_discrete(limits = unique(order_$meal_name_comp), guide_legend("")) +
    scale_fill_manual(values = c("zubereitung" = "#262626", "Kohlenhydrate" = "#fad60d", "Protein" = "#c5b87c", "gemuse_fruchte" = "#008099", "ol_fett_nuss" = "#80ccff", "suss_salz_alk" = "grey90", "foodwaste" = "#99f200"),
                      breaks = c("zubereitung", "Kohlenhydrate", "Protein", "gemuse_fruchte", "ol_fett_nuss", "suss_salz_alk", "foodwaste"),
                      labels = c("Zubereitung", "Kohlenhydrate", "Protein", "Gemüse & Früchte", "Ã–le, Fette & Nüsse", "Süsses/Salziges/Alkoholisches", "Foodwaste ohne Tellerrest"))+
    scale_y_continuous(limits = c(0, 10000), breaks = seq(0, 10000, 2500))+
    ylab("Umweltbelastungspunkte (UBP/Gericht)")+
    mytheme4
    theme(legend.position = "bottom")
     

# annotate text and lines
p + annotate("segment", x="Poulet Nasi Goreng mit Blattsalat", xend = "Rindsragout mit SpÃ¤tzli und Bohnen", y = txt[txt$label_content.y == "Fleisch", ]$median,yend = txt[txt$label_content.y == "Fleisch", ]$median, size = 2, color = "grey20") + # annotate median of meat meals
    annotate("segment", x="Kartoffel-Gemüsegratin", xend = "SpÃ¤tzli-Gemüsepfanne mit Blattsalat", y = txt[txt$label_content.y == "Vegetarisch", ]$median, yend = txt[txt$label_content.y == "Vegetarisch", ]$median, size = 2, color = "grey20")+ # annotate median of vegetarian meals
    annotate("segment", x="Linsen-Gemüsecurry mit Samosa", xend = "Aglio Olio mit Blattsalat", y = txt[txt$label_content.y == "Pflanzlich+", ]$median,  yend = txt[txt$label_content.y == "Pflanzlich+", ]$median, size = 2, color = "grey20")+
    annotate("segment", x="Falafel im Pitabrot mit Blattsalat", xend = "Caribbean Tofu und Kefen in Currysauce" , y = txt[txt$label_content.y == "Pflanzlich", ]$median, yend = txt[txt$label_content.y == "Pflanzlich", ]$median, size = 2, color = "grey20")+
    annotate("segment", x="Paniertes MSC-Kabeljau-Filet mit Reis und Zuchetti", xend = "Gebratene ASC-Lachs-Tranche mit Blattsalat" , y = txt[txt$label_content.y == "Fisch", ]$median, yend = txt[txt$label_content.y == "Fisch", ]$median, size = 2, color = "grey20")+
    # annotate("text", x="Grosis Hackt?tschli mit Polenta und Ratatouille", y = c(6800,6300) , label= txt$label3[1], size = 7, color = "grey20", parse = T) + # annotate mean and sd of meat
    # annotate("text", x="Marronirisotto", y = c(6800,6300) , label= txt$label3[4], size = 7, color = "grey20", parse = T)+
    # annotate("text", x="Friedrice", y = c(6800,6300) , label= txt$label3[3], size = 7, color = "grey20", parse = T)+
    # annotate("text", x="Vegi-Burger", y = c(6800,6300) , label= txt$label3[2], size = 7, color = "grey20", parse = T) +
    labs(caption = "Berechnungen: Karen Muir, ZHAW")

ggsave("plots/ubp_meals_190124_egel.pdf",
       width = 25,
       height = 14,
       dpi = 600,
       units="in",
       device= cairo_pdf)


# plot all menus gwp----
gwp_long <- melt(gwp_1, id.vars = c("meal_name", "meal_name_comp", "date", "cycle", "article_description", "label_content.y", "label_content.x", "tot_gwp"), measure.vars = c("Kohlenhydrate", "Protein", "gemuse_fruchte", "ol_fett_nuss", "suss_salz_alk", "foodwaste", "zubereitung"), variable.name = "content", value.name = "gwp")

# somehow not working => find a better way => aggregate(FUN=mean)
#text1 <- aggregate(ubp ~label_content.y, data=ubp_long, FUN = function(ebp_points){sqrt(var(ebp_points))}) #try to calculate standard deviation 
text <- aggregate(tot_gwp ~ label_content.y, data=gwp_long, FUN=mean) # mean calculation
text_ <- aggregate(tot_gwp ~ label_content.y, data=gwp_long, FUN=median) # median             
txt <- left_join(text, text_, by="label_content.y") %>% 
    #left_join(., text1, by = "label_content.y") %>%
    rename(mean = tot_gwp.x, median = tot_gwp.y) %>%
    mutate(label2 = paste("(","italic(Mdn)", "==", round(median, digits = 2) ,")")) %>%
    mutate(label1= paste("italic(bar(X))", "==", round(mean, digits = 2))) %>%
    mutate(label3 = paste(label2, label1, sep = "\n"))

# add date of creation
st = strftime(today(), format = "%d.%m.%y")

# plot
p <- ggplot(gwp_long, aes(x = meal_name_comp, y = gwp, fill = factor(content, levels = c("foodwaste", "suss_salz_alk", "ol_fett_nuss", "gemuse_fruchte", "Protein", "Kohlenhydrate", "zubereitung"))))+
    geom_bar(stat="identity", position = "stack", width = .6)+
    guides(fill = F)+
    scale_x_discrete(limits = order_$meal_name_comp, guide_legend("")) + # code from above!
    scale_fill_manual(values = c("zubereitung" = "#262626", "Kohlenhydrate" = "#fad60d", "Protein" = "#c5b87c", "gemuse_fruchte" = "#008099", "ol_fett_nuss" = "#80ccff", "suss_salz_alk" = "grey90", "foodwaste" = "#99f200"),
                      breaks = c("zubereitung", "Kohlenhydrate", "Protein", "gemuse_fruchte", "ol_fett_nuss", "suss_salz_alk", "foodwaste"),
                      labels = c("Zubereitung", "Kohlenhydrate", "Protein", "Gemüse & Früchte", "Ã–le, Fette & Nüsse", "Süsses/Salziges/Alkoholisches", "Foodwaste ohne Tellerrest"))+
    #scale_y_continuous(limits = c(-40, 20), breaks = seq(-40, 20, 10))+
    ylab(expression(paste("\n Global warming potential GWP (kg ", "CO"[2],"eq/Gericht)"))) +
    mytheme4
# theme(legend.position = "bottom")


# annotate text and lines (check always if library NLP is loaded => conflict with ggplot2 annotate())
p + ggplot2::annotate("segment", x="Poulet Nasi Goreng mit Blattsalat", xend = "Rindsragout mit SpÃ¤tzli und Bohnen", y = txt[txt$label_content.y == "Fleisch", ]$median,yend = txt[txt$label_content.y == "Fleisch", ]$median, size = 2, color = "grey20") + # annotate median of meat meals
    ggplot2::annotate("segment", x="Kartoffel-Gemüsegratin", xend = "SpÃ¤tzli-Gemüsepfanne mit Blattsalat", y = txt[txt$label_content.y == "Vegetarisch", ]$median, yend = txt[txt$label_content.y == "Vegetarisch", ]$median, size = 2, color = "grey20")+ # annotate median of vegetarian meals
    ggplot2::annotate("segment", x="Linsen-Gemüsecurry mit Samosa", xend = "Aglio Olio mit Blattsalat", y = txt[txt$label_content.y == "Pflanzlich+", ]$median,  yend = txt[txt$label_content.y == "Pflanzlich+", ]$median, size = 2, color = "grey20")+
    ggplot2::annotate("segment", x="Falafel im Pitabrot mit Blattsalat", xend = "Caribbean Tofu und Kefen in Currysauce" , y = txt[txt$label_content.y == "Pflanzlich", ]$median, yend = txt[txt$label_content.y == "Pflanzlich", ]$median, size = 2, color = "grey20")+
    ggplot2::annotate("segment", x="Paniertes MSC-Kabeljau-Filet mit Reis und Zuchetti", xend = "Gebratene ASC-Lachs-Tranche mit Blattsalat" , y = txt[txt$label_content.y == "Fisch", ]$median, yend = txt[txt$label_content.y == "Fisch", ]$median, size = 2, color = "grey20")+
    # annotate("text", x="Chickeria", y = c(3,2.75), label= txt$label3[1], size = 5, color = "grey20", parse = T) + # annotate mean and sd of meat
    # annotate("text", x="Aglio Olio", y = c(3,2.75), label= txt$label3[4], size = 5, color = "grey20", parse = T)+
    # annotate("text", x="Hot Pumkin", y = c(3,2.75) , label= txt$label3[3], size = 5, color = "grey20", parse = T)+
    # annotate("text", x="Linguine Ticinese", y = c(3,2.75), label= txt$label3[2], size = 5, color = "grey20", parse = T) +
    labs(caption = "Berechnungen: Karen Muir, ZHAW")

ggsave("plots/gwp_meals_190124_egel.pdf",
       width = 25,
       height = 16,
       dpi = 600,
       units="in",
       device= cairo_pdf)


# Boxplots for content only--------

# statistical analyses ubp-----
aov1 <- aov(log10(ubp_$tot_ubp) ~ ubp_$label_content.y)
summary.lm(aov1)
library(ggfortify)
autoplot(aov1) # boxplot ok, however not the residuals (check out with log10)
TukeyHSD(aov1)

# do some alternative checkings => parameter free analysis
library(PMCMR)
ubp_$label_content.y <- parse_factor(ubp_$label_content.y, levels = c())
kruskal.test(ubp_$tot_ubp ~ ubp_$label_content.y)
posthoc.kruskal.nemenyi.test(ubp_$tot_ubp, ubp_$label_content.y)# post hoc tests

# let that away
library(multcomp) # add letters for boxplot
ubp_$label_content.y <- parse_factor(ubp_$label_content.y, levels = c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fisch", "Fleisch")) # first parse characters to factors, otherwise multcomp package can handle the information
model <- aov(tot_ubp ~ label_content.y, data = ubp_) # important to right it that way resp. with data = , otherwise multicomp throws u an error
letters <- cld(glht(model, linfct = mcp(label_content.y="Tukey")))

# boxplot ubp-----
# add number of meals per meal content (in xlab)
dat <- ubp_ %>% 
    filter(!duplicated(.$meal_name_comp)) %>% # filter first ()
    group_by(label_content.y) %>% 
    summarise(sum_content = n()) %>% 
    left_join(ubp_, ., by = c("label_content.y")) %>% 
    mutate(label_content.z = dplyr::recode(label_content.y, "Pflanzlich" = "Vegan (Fleischersatz)", 
                                                       "Pflanzlich+" = "Vegan (authentisch)", 
                                                       "Vegetarisch" = "Ovo-lakto-vegetarisch"
                                                       )) %>% 
    mutate(xlab_ = paste("(", sum_content, ")"), 
          xlab_1 = paste(label_content.z, xlab_, sep = "\n"))

# colors per category
ColsPerCat=c("Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fisch"="#4c4848", "Fleisch" = "#fad60d")

# add median and min max to plot
data_summary <- function(x) {
    m <- median(x)
    ymin <- min(x)
    ymax <- max(x)
    return(c(y=m,ymin=ymin,ymax=ymax))
}

add_point <- function(x) {
    subset(x, x == max(x) | x == min(x))
}

# attention problems to sort data!
p <- ggplot(dat, aes(y = tot_ubp, x = factor(label_content.y, levels = c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fisch", "Fleisch")), fill = label_content.y)) + 
    geom_boxplot(outlier.shape = NA) + #drop outliers
    scale_fill_manual(values = alpha(ColsPerCat, .8)) +
    guides(fill =F) +
    scale_y_continuous(limits = c(0, 11000)) +
    scale_x_discrete(breaks = dat$label_content.y,
                     labels = dat$xlab_1) +
    labs(x = "Menü-Inhalt\n (Anzahl Gerichte)", y = "Umweltbelastungspunkte (UBP / Menü-Inhalt)") +
    #annotate("text", x = 1:5, y = 11000, label = letters$mcletters$Letters, size = 8) +
    mytheme


# annotate meadian
med <- aggregate(dat$tot_ubp ~ dat$label_content.y, FUN = median)
med <- arrange(med, label = factor(med$`dat$label_content.y`, levels = c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fisch", "Fleisch"))) # get order right!

# add ANNOTATION AND STATISTICS to plot
p + annotate("text", x = 1:5, y = 10000, label = paste("Median =", round(med$`dat$tot_ubp`), "UBP"), size = 8) + # annotate mean    
    # labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)") + # add source
    stat_summary(fun.data=data_summary, size = 0) +
    stat_summary(fun.y = add_point, geom="point") +
    stat_boxplot(geom='errorbar',coef=10, width = 0.2)

# save
ggsave("plots/boxplot_ubp_190128_egel.pdf",
       width = 18,
       height = 12,
       dpi = 300,
       device = cairo_pdf)

# violin plot ubp----
p <- ggplot(dat, aes(y = tot_ubp, x = factor(label_content.y, levels = c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fisch", "Fleisch")), fill = label_content.y)) + 
    geom_violin(trim =  F) + 
    scale_x_discrete(breaks = dat$label_content.y,
                     labels = dat$xlab_1) +
    scale_y_continuous(limits = c(0,12000), breaks = seq(0,12000, 1000)) +
    scale_fill_manual(values = alpha(ColsPerCat,.8)) +
    guides(fill = F) +
    labs(x = "Menü-Inhalt \n (Anzahl Gerichte)", y = "Treibhausgase (THG in  / Menü-Inhalt)") +
    annotate("text", x = 1:5, y = 11500, label = letters$mcletters$Letters, size = 8) +
    mytheme

p + stat_summary(fun.data=data_summary, size = 1.1, color = "grey60") # median and min max values
# p + stat_summary(fun.data = mean_sdl, mult = 1)    

#save
ggsave("plots/violin_ubp_190128_egel.pdf", # do not write p explicity, thus saves only p form first part (p <- ... and not the update p +)
       width = 18,
       height = 13,
       dpi = 600,
       device = cairo_pdf)

# boxplot gwp----
# statistical analyses gwp-----

# correlation between the groups
# tried to spread data to wide format, however not working either thus do it manually
# different group sizes is a pain => search for an alternative!
cor(gwp_1[gwp_1$label_content.y == "Pflanzlich+", ]$tot_gwp, gwp_1[gwp_1$label_content.y == "Pflanzlich", ]$tot_gwp)
cor(gwp_l$Pflanzlich, gwp_l$`Pflanzlich+`)


aov2 <- (aov(log10(gwp_1$tot_gwp) ~ gwp_1$label_content.y)) # attention between groups there are some correlations => usw typ II SS for ANOVA
summary.lm(aov2)

library(car)
Anova(aov2, type = 2) # seems not to differ from type 1??

library(ggfortify)
autoplot(aov2) # boxplot ok, however do a log10 transformation (attention does change the output massively)
TukeyHSD(aov2)

# do some non paramter analysis
gwp_1$label_content <- parse_factor(gwp_1$label_content, levels = c())
kruskal.test(gwp_1$tot_gwp ~gwp_1$label_content)
posthoc.kruskal.nemenyi.test(gwp_1$tot_gwp, gwp_1$label_content)# post hoc tests


# let that away
library(multcomp) # add letters for boxplot
gwp_1$label_content.y <- parse_factor(gwp_1$label_content.y, levels = c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fisch", "Fleisch")) # first parse characters to factors, otherwise multcomp package can handle the information
model <- aov(log10(tot_gwp) ~ label_content.y, data = gwp_1) # important to right it that way resp. with data = , otherwise multicomp throws u an error
letters <- cld(glht(model, linfct = mcp(label_content.y="Tukey")))

# add number of meals per meal content (in xlab)
dat <- gwp_1 %>% 
    filter(!duplicated(.$meal_name_comp)) %>% 
    group_by(label_content) %>% 
    summarise(sum_content = n()) %>% 
    left_join(gwp_1, ., by = c("label_content")) %>% 
    # mutate(label_content.y = factor(label_content.y, levels = c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch"))) %>% 
    mutate(label_content.z = dplyr::recode(label_content, "Pflanzlich" = "Vegan (Fleischersatz)", 
                                    "Pflanzlich+" = "Vegan (authentisch)", 
                                    "Vegetarisch" = "Ovo-lakto-vegetarisch"
    )) %>% 
    mutate(xlab_ = paste("(", sum_content, ")"), 
           xlab_1 = paste(label_content.z, xlab_, sep = "\n"))

# plot
p <- ggplot(dat, aes(y = tot_gwp, x = factor(label_content, levels = c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fisch", "Fleisch")), fill = label_content)) + 
    geom_boxplot(outlier.shape = NA) + 
    scale_fill_manual(values = alpha(ColsPerCat,.8)) + 
    guides(fill = F) +
    scale_y_continuous(limits = c(0,6)) +
    scale_x_discrete(breaks = dat$label_content,
                     labels = dat$xlab_1) +
    labs(x = "Menü-Inhalt\n (Anzahl Gerichte)", y = bquote("\n GWP (kg " ~ "CO"[2] ~"eq" ~ " / Menü-Inhalt)")) +
    # annotate("text", x = 1:5, y = max(gwp_1$tot_gwp)+1, label = letters$mcletters$Letters, size = 8) +
    mytheme

# annotate meadian
med <- aggregate(dat$tot_gwp ~ dat$label_content, FUN = median)
med <- arrange(med, label = factor(med$`dat$label_content`, levels = c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fisch", "Fleisch"))) # get order right!

# add ANNOTATION AND STATISTICS to plot
p + annotate("text", x = 1:5, y = 6, label = paste("Median =", round(med$`dat$tot_gwp`,2), "GWP"), size = 8) + # annotate mean    
    # labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)") + # add source
    stat_summary(fun.data=data_summary, size = 0) +
    stat_summary(fun.y = add_point, geom="point") +
    stat_boxplot(geom='errorbar', coef=10, width = 0.2) # add errorbar (with coef = 10, just give an arbitrarily big number here)


# save
ggsave("plots/boxplot_gwp_190128_egel.pdf",
       width = 18,
       height = 10,
       dpi = 300,
       device = "pdf") # cairo_pdf is not working because of the co2 expression! use pdf device => however pdf size is suspiciously small

# violin plot gwp----
p <- ggplot(dat, aes(y = tot_gwp, x = factor(label_content.y, levels = c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fisch", "Fleisch")), fill = label_content.y)) + 
    geom_violin(trim =  F) + 
    scale_x_discrete(breaks = dat$label_content.y,
                     labels = dat$xlab_1) +
    scale_y_continuous(limits = c(0,7), breaks = seq(0,7, 1)) +
    scale_fill_manual(values = alpha(ColsPerCat,.8)) +
    guides(fill = F) +
    labs(x = "Menü-Inhalt \n (Anzahl Gerichte)", y = expression(paste("\n Anzahl Treibhausgase (THG) in ", "CO"[2],"eq"))) +
    annotate("text", x = 1:5, y = 6.6, label = letters$mcletters$Letters, size = 8) +
    mytheme

p + stat_summary(fun.data=data_summary, size = 1.1, color = "grey60") # median and min max values
# p + stat_summary(fun.data = mean_sdl, mult = 1) # another method resp. median plus/minus 2sd   

#save
ggsave("plots/violin_gwp_190128_egel.pdf",
       width = 18,
       height = 13,
       dpi = 600,
       device = cairo_pdf)


# plot correlation ubp and gwp------
plot_cor <- cor.test(ubp_$tot_ubp, gwp_1$tot_gwp, method = "spearman")
a <- lm(ubp_$tot_ubp ~ gwp_1$tot_gwp)
#plot
library(ggrepel)

dat <- bind_cols(ubp_, gwp_1) %>% 
    dplyr::select(meal_name_comp, label_content.y, tot_ubp, tot_gwp) 
ggplot(dat, aes(y = tot_ubp, x = tot_gwp, color = label_content.y))+
    geom_point(size = 3.3)+
    scale_color_manual(values = ColsPerCat) +
    guides(color = guide_legend("Menü-Inhalt")) +
    scale_y_continuous(limits = c(0, 10000)) +
    ylab("Umweltbelastungspunkte (UBP/Gericht)")+
    xlab(expression(paste("\n Treibhausgase (THG in ", "CO"[2],"eq / Gericht)")))+
    annotate("text", x=c(4,4) , y=c(6200,5700) , label=c("italic(rho) == 0.83\n","italic(p) < 0.001"), parse=T, size=7,color = "grey50")+
    stat_smooth(aes(y = tot_ubp, x = tot_gwp), inherit.aes = F,
                method = "lm", 
                formula = y ~ x, 
                se = F, 
                color = "grey50", 
                size = 2, 
                fullrange = T, # should expand the line, however not working??
                span = 4) +
    mytheme + # somehow throws an error if it's the last line of the code
    geom_label_repel(aes(label= ifelse(dat$label_content.y == "Fisch", dat$meal_name_comp, "")), size=5, show.legend = FALSE) # adds text of meal_name
    

# save
ggsave("plots/cor_ubp_gwp_190128_egel.pdf",
       height = 13,
       width = 15,
       dpi = 600, 
       device = cairo_pdf)

