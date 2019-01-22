####### 
## individual analysis
#######

# status: 15.06.18 // egel


# load data see script load data 2017


## create test sample------------
set.seed(4)
# sam.list <- sample(df_7$card_num,10)
samp <- filter(df_7, card_num %in%  sample(df_7$card_num,10))


# select only card_num and label_content with sample data-----
test <- samp %>% 
    filter(!is.na(card_num)) %>% # omit NA's in card_num
    select(card_num, label_content) %>% # shoud do the same
    dcast(formula = card_num ~ label_content, value.var="label_content", fun.aggregate= length) %>% # reshape into wide format and aggregate lenght of entries
    rename(Unknown = 'NA') # rename NA to unknown

test2 <- test %>%
    mutate(tot_buy = rowSums(test[,-1])) %>% # exclude card_num for sum of rows
    
test3 <-test2 %>%
    mutate(meaty=test2$Meat/test2$tot_buy, 
           meatless=(test2$Vegan+test2$`Vegan+`+test2$Vegetarian)/test2$tot_buy,
           buffet = test2$`Hot and Cold`/test2$tot_buy,
           cont_unkn= test$Unknown/test2$tot_buy) # calculate meat proportion


## select label content of every card_num and calculate proportions-----
# use dot (.) to refer to actual modified data frame
df_ <- df_7 %>% 
    filter(!is.na(card_num)) %>% # omit NA's in card_num
    select(card_num, label_content, Geschlecht, member, age) %>% # select variable of interest
    dcast(formula = card_num+Geschlecht+age+member ~ label_content, value.var="label_content", fun.aggregate= length) %>% # reshape into wide format and aggregate after occurencies of label content
    rename(Unknown = 'NA') %>% # rename NA to unknown
    mutate(tot_buy = rowSums(.[,-c(1:4)])) %>% # exclude card_num and information of person for sum of rows
    mutate(meaty=.$Meat/.$tot_buy, 
           meatless= (.$Vegan+.$`Vegan+`+.$Vegetarian)/.$tot_buy,
           veggi=(.$Vegetarian)/.$tot_buy,
           vegan=(.$Vegan+.$`Vegan+`)/.$tot_buy,
           buffet = .$`Hot and Cold`/.$tot_buy,
           cont_unkn= .$Unknown/.$tot_buy) %>% # calculate meat, vegetarian, buffet and unknown proportions
    # mutate(diff_= as.character(ifelse(.$meaty > .$meatless,1,0))) # calculate meat and vegetarian proportions


#step by step solutions, may be clearer------
step1 <- df_7 %>% 
    filter(!is.na(card_num)) %>% # omit NA's in card_num
    select(card_num, label_content, Geschlecht, member, age) %>% # select variable of interest
    dcast(formula = card_num+Geschlecht+age+member ~ label_content, value.var="label_content", fun.aggregate= length) %>% # reshape into wide format and aggregate after occurencies of label content
    rename(Unknown = 'NA') %>% # rename NA to unknown

step2 <- step1 %>%
    mutate(tot_buy = rowSums(step1[,-c(1:4)]))# exclude card_num and information of person for sum of rows
    
step3 <-step2 %>%
    mutate(meaty=step2$Meat/step2$tot_buy, 
           meatless=(step2$Vegan+step2$`Vegan+`+step2$Vegetarian)/step2$tot_buy,
           buffet = step2$`Hot and Cold`/step2$tot_buy,
           cont_unkn= step2$Unknown/step2$tot_buy) 

# how many people eat more meat than vegie meals, compared to itself
# could be a distinction between meatlovers and flexitarians?
step4 <- step3 %>%
    mutate(diff_= as.character(ifelse(step3$meaty > step3$meatless,1,0))) 
Hmisc::describe(test$diff_) # 56 percent of people consume more meat than veggi meals


## first raw clustering, should be distinct--------
# people only eating once in the canteen are excluded


# cluster meat_lovers: proportion of meaty meals over 75 percent of all chosen meals
# 80
meat_lovers <- df_ %>% 
    filter(tot_buy>1 & meaty ==1)
# describe this cluster
Hmisc::describe(meat_lovers$Geschlecht) # gender distribution
describe(meat_lovers[meat_lovers$age<100,]$age) # mean of age
Hmisc::describe(meat_lovers$member) # member distribution
mean(meat_lovers$tot_buy) # mean of visit days

# meat avoiders: proportion of vegetarian meals are 100 percent: in other words, always choosing vegetarian
# 70
meat_avoiders <- df_ %>%
    filter(tot_buy>1 & meatless == 1)
# describe this cluster
Hmisc::describe(meat_avoiders$Geschlecht)
describe(meat_avoiders[meat_avoiders$age<100,]$age)
Hmisc::describe(meat_avoiders$member)
mean(meat_lovers$tot_buy)

# vegans: proportion of vegans are 100 percent
# only 1
vegan <- df_ %>%
    filter(tot_buy>1 & vegan == 1)

# buffet eaters: proportion of buffet meals ofer 75 percent of all chosen meals
# 34
buffet_eaters <- df_ %>% 
    filter(tot_buy>1 & buffet == 1)
# describe this cluster
Hmisc::describe(buffet_eaters$Geschlecht)
describe(buffet_eaters[buffet_eaters$age<100,]$age)
Hmisc::describe(buffet_eaters$member)
mean(buffet_eaters$tot_buy)

#onetimers: eating once at the canteen
# 171 persons
single <- df_ %>%
    filter(tot_buy == 1)
# describe this cluster
Hmisc::describe(single$Geschlecht)
describe(single[single$age<100,]$age)
Hmisc::describe(single$member)
mean(single$tot_buy)


# flexitarians: all the rest :) 
# 1247
flexies <- df_ %>%
    filter(tot_buy>1 & meaty <1 & meatless < 1 & buffet < 1)
# describe this cluster
Hmisc::describe(flexies$Geschlecht)
describe(flexies[flexies$age<100,]$age)
Hmisc::describe(flexies$member)
mean(flexies$tot_buy)



## filter card_nums which contains entries in basis and intervention
# maybe analyse only fliexies

flexies <- df_ %>%
    filter(tot_buy>1 & meaty <1 & meatless < 1 & buffet < 1)

df_test <- df_7 %>%
    filter(card_num %in% flexies$card_num) %>%
    filter(!is.na(card_num)) %>%
    select(card_num, label_content, Geschlecht, member, age, condit) %>%
    dcast(formula = card_num+Geschlecht+age+member ~ label_content+condit, value.var="label_content" ,fun.aggregate= length) %>%
    mutate(Veg_Basis = .$Vegan_Basis + .$`Vegan+_Basis` + .$Vegetarian_Basis,
           Veg_Intervention = .$Vegan_Intervention + .$`Vegan+_Intervention` + .$Vegetarian_Intervention) %>%
    select(card_num, Geschlecht, member, age, `Hot and Cold_Basis`, `Hot and Cold_Intervention`,Meat_Basis, Meat_Intervention,
           Veg_Basis,Veg_Intervention)

# had the intervention an influence?
df_test$test <- ifelse(df_test$Meat_Basis>df_test$Meat_Intervention & df_test$Veg_Basis< df_test$Veg_Intervention,1,0)

## open questions, later to handle
## loop through card_num and count all meal choices => slow way of above?----------
# problems to append new row to the dataframe => how to solve this?

ids <- unique(samp$card_num) # only valid card nummers


for (card in 1:length(ids)){ # loop through ids
    id <- ids[card] # select the right id
    dat.card <- as.character(na.omit(samp[samp$card_num == id,]$label_content)) #search all valid meal chocies resp. buyings
    dat.card$card_nr <- id
    assign(paste("test",id,sep="."),as_tibble(dat.card)) #assign a value to a name, in this case "test"
        }



# clusters of nutrition and age
# prepare data
# ATTENTION 118 cases were not in plots, thus gender is missing
df <- df_7 %>%
    mutate(age_group = cut(age,breaks=c(-Inf, 25, 35, 50, 65, Inf), # menuCH age groups
                           labels=c("16 bis 25-jährig","26 bis 34-jährig","35 bis 49-jährig","50 bis 64-jährig","keine Angaben"))) %>%
    group_by(age_group, label_content, Geschlecht) %>%
    summarise(tot_sold = n()) %>%
    ungroup() %>%
    filter(!is.na(.$age_group)) # exclude cash payers (2728)

#plot meat buyers
df_ <- df %>%
    filter(label_content == "Meat" ) %>%
    mutate(tot=sum(tot_sold))
ggplot(df_, aes(age_group, tot_sold, fill=Geschlecht)) +
    geom_bar(stat="identity", position = "dodge", width = .5) +
    scale_fill_manual(values = c(male="#c5b87c",female="#fad60d"), na.value="grey50", 
                      labels = c("weiblich","männlich")) +
    xlab("\nAltersgruppen")+
    ylab("Verkaufte fleischhaltige Menüs im Herbstsemester 2017\n")+
    guides(fill=guide_legend("Geschlecht\n",
                             keywidth=.5,
                             keyheight=.5,
                             default.unit="inch"))+
    annotate("text", x="keine Angaben", y=2000, label=paste("italic(n)",df_$tot[1], sep = "=="), parse = T, size=10)+
    mytheme

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/individual analysis/plots/age_meat_180626_egel.png",
       width = 14,
       height = 8,
       dpi = 600,
       units = "in",
       type = "cairo-png")

#plot vegetarian buyers
df_ <-  df %>%
    filter(label_content == "Vegetarian" ) %>%
    mutate(tot = sum(tot_sold))
ggplot(df_,aes(age_group, tot_sold, fill=Geschlecht)) +
    geom_bar(stat="identity", position = "dodge", width = .5) +
    scale_fill_manual(values = c(male="#c5b87c",female="#fad60d"), na.value="grey50", 
                      labels = c("weiblich","männlich")) +
    xlab("\nAltersgruppen")+
    ylab("Verkaufte vegetarische Menüs im Herbstsemester 2017\n")+
    guides(fill=guide_legend("Geschlecht\n",
                             keywidth=.5,
                             keyheight=.5,
                             default.unit="inch"))+
    annotate("text", x="keine Angaben", y=1000, label=paste("italic(n)",df_$tot[1], sep = "=="), parse = T, size=10)+
    mytheme

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/individual analysis/plots/age_vegetarian_180626_egel.png",
       width = 14,
       height = 8,
       dpi = 600,
       units = "in",
       type = "cairo-png")


#plot vegan buyers
df_ <- df%>%
    filter(label_content == "Vegan" ) %>%
    mutate(tot=sum(tot_sold))
    
ggplot(df_,aes(age_group, tot_sold, fill=Geschlecht)) +
    geom_bar(stat="identity", position = "dodge", width = .5) +
    scale_fill_manual(values = c(male="#c5b87c",female="#fad60d"), na.value="grey50", 
                      labels = c("weiblich","männlich")) +
    xlab("\nAltersgruppen")+
    ylab("Verkaufte pflanzliche (fleischsubstituierte) Menüs im Herbstsemester 2017\n")+
    guides(fill=guide_legend("Geschlecht\n",
                             keywidth=.5,
                             keyheight=.5,
                             default.unit="inch"))+
    annotate("text", x="keine Angaben", y=150, label=paste("italic(n)",df_$tot[1], sep = "=="), parse = T, size=10)+
    mytheme

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/individual analysis/plots/age_vegan_180626_egel.png",
       width = 14,
       height = 8,
       dpi = 600,
       units = "in",
       type = "cairo-png")

#plot vegan+ buyers
df_ <- df %>%
    filter(label_content == "Vegan+" ) %>%
    mutate(tot = sum(tot_sold))
    
ggplot(df_,aes(age_group, tot_sold, fill=Geschlecht)) +
    geom_bar(stat="identity", position = "dodge", width = .5) +
    scale_fill_manual(values = c(male="#c5b87c",female="#fad60d"), na.value="grey50", 
                      labels = c("weiblich","männlich")) +
    xlab("\nAltersgruppen")+
    ylab("Verkaufte pflanzliche (authentische) Menüs im Herbstsemester 2017\n")+
    guides(fill=guide_legend("Geschlecht\n",
                             keywidth=.5,
                             keyheight=.5,
                             default.unit="inch"))+
    annotate("text", x="keine Angaben", y=300, label=paste("italic(n)",df_$tot[1], sep = "=="), parse = T, size=10)+
    mytheme

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/individual analysis/plots/age_veganplus_180626_egel.png",
       width = 14,
       height = 8,
       dpi = 600,
       units = "in",
       type = "cairo-png")

#plot buffet buyers
df_ <- df %>%
filter(label_content == "Hot and Cold" ) %>% 
mutate(tot=sum(tot_sold)) 
ggplot(df_,aes(age_group, tot_sold, fill=Geschlecht)) +
        geom_bar(stat="identity", position = "dodge", width = .5) +
        scale_fill_manual(values = c(male="#c5b87c",female="#fad60d"), na.value="grey50", 
                          labels = c("weiblich","männlich")) +
        xlab("\nAltersgruppen")+
        ylab("Verkaufte 'Hot and Cold' Menüs im Herbstsemester 2017\n")+
        guides(fill=guide_legend("Geschlecht\n",
                                 keywidth=.5,
                                 keyheight=.5,
                                 default.unit="inch"))+
        annotate("text", x="keine Angaben", y=500, label=paste("italic(n)",df_$tot[1], sep = "=="), parse = T, size=10)+
        mytheme

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/individual analysis/plots/age_h&c_180626_egel.png",
       width = 14,
       height = 8,
       dpi = 600,
       units = "in",
       type = "cairo-png")
