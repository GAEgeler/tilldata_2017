# vori 190819
# Anzahl verkaufte Gerichte pro Menülinie und demografische Merkmale der Mensabesucher. Beides für die Acht Befragungstage 

library("tidyverse")
library("gmodels")
# Laden der Daten
source("04_1_load_data_190128_egel.R")

# Für die Anzahl verkaufte Gerichte pro Menülinie an den Befragungstagen wird df_agg benützt ----

# Filter nach Befragungstage 
df_bef_sold <- df_agg %>% 
  filter(date=="2017-10-17" | date=="2017-10-19" | date=="2017-11-06" | date=="2017-11-08" | date=="2017-11-14" | date=="2017-11-16" | date=="2017-11-21"| date=="2017-11-30") %>% 
  group_by(date, article_description) %>% 
  summarise(tot = n())

write_delim(df_bef_sold, "verkaufte_gerichte_befragungstage_190819_vori.csv", delim=';')


# Für die domografischen Variablen der Mensabesucher an den Befragungstagen wird df_2017 benützt ----

df_bef_demo <- df_2017 %>% 
  filter(date=="2017-10-17" | date=="2017-10-19" | date=="2017-11-06" | date=="2017-11-08" | date=="2017-11-14" | date=="2017-11-16" | date=="2017-11-21"| date=="2017-11-30") 

df_bef_demo <- df_bef_demo %>% 
  mutate(member, member=ifelse(is.na(member), "Unbekannt", member)) %>% 
  mutate(gender, gender=ifelse(is.na(gender), "Unbekannt", gender))

CrossTable(df_bef_demo$member, df_bef_demo$gender)
  