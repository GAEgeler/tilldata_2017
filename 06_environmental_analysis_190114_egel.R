# environmental impact analysis----

###
# state: january 2019
# author: gian-Andrea egeler
###

# overview of all 93 meals according meal content and meal substances-----
# load data
source("051_load_add_data_191028_egel.R") # attention load only ubp_ and gwp_
source("08_theme_plots_180419_egel.R")


# plot all menus ubp----
# melt into long format
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
                      labels = c("Zubereitung", "Kohlenhydrate", "Protein", "Gemüse & Früchte", "Öle, Fette & Nüsse", "Süsses/Salziges/Alkoholisches", "Foodwaste ohne Tellerrest"))+
    scale_y_continuous(limits = c(0, 10000), breaks = seq(0, 10000, 2500))+
    ylab("Umweltbelastungspunkte (UBP/Gericht)")+
    mytheme4
    theme(legend.position = "bottom")
     

# annotate text and lines
p + annotate("segment", x="Poulet Nasi Goreng mit Blattsalat", xend = "Rindsragout mit Spätzli und Bohnen", y = txt[txt$label_content.y == "Fleisch", ]$median,yend = txt[txt$label_content.y == "Fleisch", ]$median, size = 2, color = "grey20") + # annotate median of meat meals
    annotate("segment", x="Kartoffel-Gemüsegratin", xend = "Spätzli-Gemüsepfanne mit Blattsalat", y = txt[txt$label_content.y == "Vegetarisch", ]$median, yend = txt[txt$label_content.y == "Vegetarisch", ]$median, size = 2, color = "grey20")+ # annotate median of vegetarian meals
    annotate("segment", x="Linsen-Gemüsecurry mit Samosa", xend = "Aglio Olio mit Blattsalat", y = txt[txt$label_content.y == "Pflanzlich+", ]$median,  yend = txt[txt$label_content.y == "Pflanzlich+", ]$median, size = 2, color = "grey20")+
    annotate("segment", x="Falafel im Pitabrot mit Blattsalat", xend = "Caribbean Tofu und Kefen in Currysauce" , y = txt[txt$label_content.y == "Pflanzlich", ]$median, yend = txt[txt$label_content.y == "Pflanzlich", ]$median, size = 2, color = "grey20")+
    annotate("segment", x="Paniertes MSC-Kabeljau-Filet mit Reis und Zuchetti", xend = "Gebratene ASC-Lachs-Tranche mit Blattsalat" , y = txt[txt$label_content.y == "Fisch", ]$median, yend = txt[txt$label_content.y == "Fisch", ]$median, size = 2, color = "grey20")+
    # annotate("text", x="Grosis Hacktätschli mit Polenta und Ratatouille", y = c(6800,6300) , label= txt$label3[1], size = 7, color = "grey20", parse = T) + # annotate mean and sd of meat
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
    scale_x_discrete(limits = order_$meal_name_comp, guide_legend("")) +
    scale_fill_manual(values = c("zubereitung" = "#262626", "Kohlenhydrate" = "#fad60d", "Protein" = "#c5b87c", "gemuse_fruchte" = "#008099", "ol_fett_nuss" = "#80ccff", "suss_salz_alk" = "grey90", "foodwaste" = "#99f200"),
                      breaks = c("zubereitung", "Kohlenhydrate", "Protein", "gemuse_fruchte", "ol_fett_nuss", "suss_salz_alk", "foodwaste"),
                      labels = c("Zubereitung", "Kohlenhydrate", "Protein", "Gemüse & Früchte", "Öle, Fette & Nüsse", "Süsses/Salziges/Alkoholisches", "Foodwaste ohne Tellerrest"))+
    #scale_y_continuous(limits = c(-40, 20), breaks = seq(-40, 20, 10))+
    ylab(expression(paste("\n Treibhausgase (THG in ", "CO"[2],"eq / Gericht)"))) +
    mytheme4
# theme(legend.position = "bottom")


# annotate text and lines
p + annotate("segment", x="Poulet Nasi Goreng mit Blattsalat", xend = "Rindsragout mit Spätzli und Bohnen", y = txt[txt$label_content.y == "Fleisch", ]$median,yend = txt[txt$label_content.y == "Fleisch", ]$median, size = 2, color = "grey20") + # annotate median of meat meals
    annotate("segment", x="Kartoffel-Gemüsegratin", xend = "Spätzli-Gemüsepfanne mit Blattsalat", y = txt[txt$label_content.y == "Vegetarisch", ]$median, yend = txt[txt$label_content.y == "Vegetarisch", ]$median, size = 2, color = "grey20")+ # annotate median of vegetarian meals
    annotate("segment", x="Linsen-Gemüsecurry mit Samosa", xend = "Aglio Olio mit Blattsalat", y = txt[txt$label_content.y == "Pflanzlich+", ]$median,  yend = txt[txt$label_content.y == "Pflanzlich+", ]$median, size = 2, color = "grey20")+
    annotate("segment", x="Falafel im Pitabrot mit Blattsalat", xend = "Caribbean Tofu und Kefen in Currysauce" , y = txt[txt$label_content.y == "Pflanzlich", ]$median, yend = txt[txt$label_content.y == "Pflanzlich", ]$median, size = 2, color = "grey20")+
    annotate("segment", x="Paniertes MSC-Kabeljau-Filet mit Reis und Zuchetti", xend = "Gebratene ASC-Lachs-Tranche mit Blattsalat" , y = txt[txt$label_content.y == "Fisch", ]$median, yend = txt[txt$label_content.y == "Fisch", ]$median, size = 2, color = "grey20")+
    # annotate("text", x="Chickeria", y = c(3,2.75), label= txt$label3[1], size = 5, color = "grey20", parse = T) + # annotate mean and sd of meat
    # annotate("text", x="Aglio Olio", y = c(3,2.75), label= txt$label3[4], size = 5, color = "grey20", parse = T)+
    # annotate("text", x="Hot Pumkin", y = c(3,2.75) , label= txt$label3[3], size = 5, color = "grey20", parse = T)+
    # annotate("text", x="Linguine Ticinese", y = c(3,2.75), label= txt$label3[2], size = 5, color = "grey20", parse = T) +
    labs(caption = "Berechnungen: Karen Muir, ZHAW")

ggsave("plots/gwp_meals_190124_egel.pdf",
       width = 25,
       height = 15,
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
library(multcomp) # add letters for boxplot
ubp_$label_content.y <- parse_factor(ubp_$label_content.y, levels = c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fisch", "Fleisch")) # first parse characters to factors, otherwise multcomp package can handle the information
model <- aov(tot_ubp ~ label_content.y, data = ubp_) # important to right it that way resp. with data = , otherwise multicomp throws u an error
letters <- cld(glht(model, linfct = mcp(label_content.y="Tukey")))

# boxplot ubp-----
# add number of meals per meal content (in xlab)
dat <- ubp_ %>% 
    group_by(label_content.y) %>% 
    summarise(sum_content = n()) %>% 
    left_join(ubp_, ., by = c("label_content.y")) %>% 
    mutate(label_content.z = dplyr::recode(label_content.y, "Pflanzlich" = "Vegan (Fleischersatz)", 
                                                       "Pflanzlich+" = "Vegan (authentisch)", 
                                                       "Vegetarisch" = "Ovo-lakto-vegetarisch"
                                                       )) %>% 
    mutate(xlab_ = paste("(", sum_content, ")"), 
          xlab_1 = paste(label_content.z, xlab_, sep = "\n"))

# attention problems to sort data!
ggplot(dat, aes(y = tot_ubp, x = factor(label_content.y, levels = c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fisch", "Fleisch")), fill = label_content.y)) + 
    geom_boxplot() + 
    scale_fill_manual(values = alpha(ColsPerCat, .8)) +
    guides(fill =F) +
    scale_y_continuous(limits = c(0, 11000)) +
    scale_x_discrete(breaks = dat$label_content.y,
                     labels = dat$xlab_1) +
    labs(x = "Menü-Inhalt\n (Anzahl Gerichte)", y = "Umweltbelastungspunkte (UBP / Menü-Inhalt)") +
    annotate("text", x = 1:5, y = 11000, label = letters$mcletters$Letters, size = 8) +
    mytheme

# save
ggsave("plots/boxplot_ubp_190128_egel.pdf",
       width = 18,
       height = 12,
       dpi = 300,
       device = cairo_pdf)

# violin plot ubp----
# add median and min max to plot
data_summary <- function(x) {
    m <- median(x)
    ymin <- min(x)
    ymax <- max(x)
    return(c(y=m,ymin=ymin,ymax=ymax))
}

ColsPerCat=c("Pflanzlich" = "grey90", "Pflanzlich+" = "#80ccff", "Vegetarisch" = "#c5b87c", "Fisch"="#4c4848", "Fleisch" = "#fad60d")

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
library(multcomp) # add letters for boxplot
gwp_1$label_content.y <- parse_factor(gwp_1$label_content.y, levels = c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fisch", "Fleisch")) # first parse characters to factors, otherwise multcomp package can handle the information
model <- aov(log10(tot_gwp) ~ label_content.y, data = gwp_1) # important to right it that way resp. with data = , otherwise multicomp throws u an error
letters <- cld(glht(model, linfct = mcp(label_content.y="Tukey")))

# add number of meals per meal content (in xlab)
dat <- gwp_1 %>% 
    group_by(label_content.y) %>% 
    summarise(sum_content = n()) %>% 
    left_join(gwp_1, ., by = c("label_content.y")) %>% 
    # mutate(label_content.y = factor(label_content.y, levels = c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fleisch"))) %>% 
    mutate(label_content.z = dplyr::recode(label_content.y, "Pflanzlich" = "Vegan (Fleischersatz)", 
                                    "Pflanzlich+" = "Vegan (authentisch)", 
                                    "Vegetarisch" = "Ovo-lakto-vegetarisch"
    )) %>% 
    mutate(xlab_ = paste("(", sum_content, ")"), 
           xlab_1 = paste(label_content.z, xlab_, sep = "\n"))

# plot
ggplot(dat, aes(y = tot_gwp, x = factor(label_content.y, levels = c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fisch", "Fleisch")), fill = label_content.y)) + 
    geom_boxplot() + 
    scale_fill_manual(values = alpha(ColsPerCat,.8)) + 
    guides(fill = F) +
    scale_y_continuous(limits = c(0,6)) +
    scale_x_discrete(breaks = dat$label_content.y,
                     labels = dat$xlab_1) +
    labs(x = "Menü-Inhalt\n (Anzahl Gerichte)", y = expression(paste("\n Anzahl Treibhausgase (THG) in ", "CO"[2],"eq"))) +
    annotate("text", x = 1:5, y = max(gwp_1$tot_gwp)+1, label = letters$mcletters$Letters, size = 8) +
    mytheme

# save
ggsave("plots/boxplot_gwp_190128_egel.pdf",
       width = 18,
       height = 10,
       dpi = 300,
       device = cairo_pdf)

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


################
# load data df_agg_tot
source("04_load_data_180802_egel.R")

# how to calculate the daily impact: count sellings and multiplicate them with the tot_ubp and tot_gwp
# eclude all hot and cold and locals
# where is df_agg_tot coming from??
t <- filter(df_agg, label_content != "Hot and Cold" & !grepl("Local ", df_agg_tot$article_description)) %>% 
    mutate(condit = ifelse(.$cycle == 1 & .$week %%2 == 0, "Basis",
                           ifelse(.$cycle == 2 & .$week %%2 == 1, "Basis", "Intervention"))) %>% # add intervention and basisweek
    group_by(meal_name,week, article_description, condit, label_content, tot_ubp, tot_gwp) %>% 
    summarise(tot_sold = n()) %>% 
    mutate(imp_gwp = tot_sold * tot_gwp, # calculate total gwp according sellings e.g. one meal was sold 100 times (each meal contributes 1.4 gwp => total gwp is 140)
           imp_ubp = tot_sold * tot_ubp)

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

