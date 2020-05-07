#Descriptive Stats: Weather and Choice of food

# status: january 2020
# author: gian-andrea egler

# load packages
library(tidyverse)


#load data
source("04_load_data_HS17_190128_egel.R")


#plot
# first plot, however how to control for sellings resp. meat and veg where solt the most
ggplot(df_agg, aes(y = sun, x = label_content)) +
    geom_bar(stat = "identity") 

# try to group different and only for the dates
df_ <- df_agg %>% 
    group_by(label_content, date, sun) %>% 
    summarise(tot = n()) %>% 
    filter(label_content == "Vegetarisch") %>% # here you can set the filter
    ggplot(aes(x = date, y = tot, fill = label_content, group = interaction(sun, label_content), 
               color = interaction(sun, label_content))) +
    geom_point()+
    geom_line()


