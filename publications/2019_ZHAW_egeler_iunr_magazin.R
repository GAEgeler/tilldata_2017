# analyses for iunr magazine

# state: march 2019
# author: gian-andrea egeler


# load data df_agg-----
source("05_1_load_add_data_190128_egel.R")
source("08_theme_plots_180419_egel.R")


# boxplot ubp-----
# add number of meals per meal content (in xlab)

# ATTENTION is not anymore up to date, due to chicken in new dataset!
ubp_old <- ubp_ %>%
    mutate(label_content.y = gsub("Geflügel", "Fleisch", ubp_$label_content.y))

dat1 <- ubp_old %>%
    filter(!duplicated(ubp_old$meal_name_comp)) %>%  # omit duplicates!!
    group_by(label_content.y) %>% 
    summarise(sum_content = n()) %>% 
    left_join(ubp_old, ., by = c("label_content.y")) %>% 
    mutate(label_content.z = dplyr::recode(label_content.y, "Pflanzlich" = "Vegan (Fleischersatz)", 
                                           "Pflanzlich+" = "Vegan (authentisch)", 
                                           "Vegetarisch" = "Ovo-lakto-vegetarisch"
    )) %>% 
    mutate(xlab_ = paste("(", sum_content, ")"), 
           xlab_1 = paste(label_content.z, xlab_, sep = "\n")) %>% 
    filter(!duplicated(ubp_$meal_name_comp)) # omit duplicates!!

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
p <- ggplot(dat1, aes(y = tot_ubp, x = factor(label_content.y, levels = c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fisch", "Fleisch")), fill = label_content.y)) + 
    geom_boxplot(outlier.shape = NA) + #drop outliers
    scale_fill_manual(values = alpha(ColsPerCat, .8)) +
    guides(fill =F) +
    scale_y_continuous(limits = c(0, 11000)) +
    scale_x_discrete(breaks = dat1$label_content.y,
                     labels = dat1$xlab_1) +
    labs(x = "Kategorien\n (Anzahl Gerichte)", y = "Umweltbelastungspunkte (UBP / Kategorie)") +
    #annotate("text", x = 1:5, y = 11000, label = letters$mcletters$Letters, size = 8) +
    mytheme


# annotate meadian
med <- aggregate(dat1$tot_ubp ~ dat1$label_content.y, FUN = median)
med <- arrange(med, label = factor(med$`dat1$label_content.y`, levels = c("Pflanzlich", "Pflanzlich+", "Vegetarisch", "Fisch", "Fleisch"))) # get order right!

# add ANNOTATION AND STATISTICS to plot
p + annotate("text", x = 1:5, y = 10000, label = paste("Median =", round(med$`dat1$tot_ubp`), "UBP"), size = 8) + # annotate mean    
    # labs(caption = "Daten: Kassendaten SV Schweiz und ZHAW (2017)") + # add source
    stat_summary(fun.data=data_summary, size = 0) +
    stat_summary(fun.y = add_point, geom="point") +
    stat_boxplot(geom='errorbar',coef=10, width = 0.2)

# save
ggsave("plots/boxplot_ubp_190128_02egel.eps",
       width = 18,
       height = 12,
       dpi = 300,
       device = "eps")

