source("04_1_load_data_190128_egel.R")

#Total verkaufte Menüs pro Tag und Menülinie sowie der prouentuale Anteil der einzellnen Menülinien am Tagesverkauf
df_sold_menus <- df_agg %>% group_by(date, article_description) %>% summarise(n=n()) %>% mutate(freq = n / sum(n))



df_agg <- merge(df_agg, df_sold_menus)

# Variabel die beschreibt welcher Inhalt auf dem Kitchen angebogten wurde, wird nur für Kitchen Menüs gemacht
df_agg <- df_agg %>% mutate(kitchen = ifelse(article_description=="Kitchen" & label_content=="Fleisch", "Fleisch",
                                             ifelse(article_description=="Kitchen" & label_content=="Geflügel", "Fleisch",
                                                    ifelse(article_description=="Kitchen" & label_content=="Fisch", "Fisch", 
                                                           ifelse(article_description=="Kitchen" & label_content=="Vegetarisch", "Vegetarisch",
                                                                  ifelse(article_description=="Kitchen" & label_content=="Pflanzlich", "Pflanzlich", 
                                                                         ifelse(article_description=="Kitchen" & label_content=="Pflanzlich+", "Pflanzlich+", "NA")))))))
# Nur ein Eintrag pro Tag 
df_kitchen <- df_agg %>% filter(kitchen!="NA") %>%  distinct(date, kitchen, .keep_all = T) 


# Summieren aller Prozente pro Menüinhalt und der Anzahl Tage an denen der Menüinhalt auf Kitchen angeboten wurde. Summierte Prozentanteile dann teilen durch die Anzahl Tage
# um die Durchschnittliche Nachfrage der einzellnen Menüinhalte zu erhalten. 
df_nachfrage1 <- df_kitchen %>% group_by(kitchen) %>% summarise(total_off=n())
df_nachfrage2 <- df_kitchen %>% group_by(kitchen) %>% summarise(add_freq=sum(freq))
df_nachfrage <- merge(df_nachfrage1, df_nachfrage2) 


df_nachfrage$avr_demand <- df_nachfrage$add_freq/df_nachfrage$total_off


# doing the same for intervention and basis week 
# basis                   
df_kitchen_b <- df_kitchen %>% filter(condit=="Basis")                 

df_nachfrage_b1 <- df_kitchen_b %>% group_by(kitchen) %>% summarise(total_off=n())
df_nachfrage_b2 <- df_kitchen_b %>% group_by(kitchen) %>% summarise(add_freq=sum(freq))
df_nachfrage_b <- merge(df_nachfrage_b1, df_nachfrage_b2) 


# intervention
df_kitchen_i <- df_kitchen %>% filter(condit=="Intervention")                 

df_nachfrage_i1 <- df_kitchen_i %>% group_by(kitchen) %>% summarise(total_off=n())
df_nachfrage_i2 <- df_kitchen_i %>% group_by(kitchen) %>% summarise(add_freq=sum(freq))
df_nachfrage_i <- merge(df_nachfrage_i1, df_nachfrage_i2) 


df_nachfrage_i$avr_demand <- df_nachfrage_i$add_freq/df_nachfrage_i$total_off

df_nachfrage_b$avr_demand <- df_nachfrage_b$add_freq/df_nachfrage_b$total_off