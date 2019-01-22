########
## First Difference Analyses with till data
#######

#Stand: 28.3 // egel


# required libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(feather)
library(reshape2)
library(tidyr)
library(eeptools)
library(readr)
library(Hmisc)
library(gmodels)
library(car)
library(onewaytests)

# see load data script

#####
## Difference tests

setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/ist soll analyse")
df_tot=read_delim("all data over year 180424 02egel.csv",delim = ';') # includs only semester weeks 3 to 14

#doing anova testing => attention name should be same, resp same counts for all years!!
df_tot$variable <- str_replace(df_tot$variable,pattern = "Vegan","Vegetarian")

boxplot(df_tot$value~df_tot$variable + df_tot$year)
leveneTest(df_tot$value~df_tot$variable) # nicht homogene gruppen, d.h. keine anova möglich
# aov_t <- aov(df_tot$value~df_tot$variable+df_tot$year)
# summary(aov_t)
# tuk <- TukeyHSD(aov_t)
# plot(tuk)

# see for more information http://www.analyticsforfun.com/2014/06/performing-anova-test-in-r-results-and.html
# anova testing, for showing differences between years
tot2 <- group_by(df_tot, variable,week,year) %>% summarise(tot=sum(value))
boxplot(df_tot$value~df_tot$year) # attention with string Vegetarian*
leveneTest(df_tot$value~df_tot$year)# nicht homogene gruppen => see https://stats.stackexchange.com/questions/91872/alternatives-to-one-way-anova-for-heteroskedastic-data 
oneway.test(value~year, data=df_tot, na.action = na.omit) # test für inhomogene gruppen
kruskal.test(value ~ year, data=df_tot) # anderer test für inhomogene gruppen
welch.test(value~year, data=df_tot) #alternatively welch test => robust anova, however dont work
# not allowed:
# aov_t <- aov(df_tot$value~df_tot$year)
# summary(aov_t)
# tuk <- TukeyHSD(aov_t)
# plot(tuk)


# differences between gender and meal content
chisq.test(df_7$Geschlecht,df_7$label_content) # unterscheiden sich significant
chisq.test(df_7$condit,df_7$label_content) # unterscheiden sich significant
Hmisc::describe(df_7$condit,df_7$label_content)
Hmisc::describe(df_7$Geschlecht,df_7$label_content)


# differences between condition: meat weeks and vegetarian weeks
sumwk <- group_by(df_7, semwk, condit) %>% summarise(tot_sol=n())
leveneTest(sumwk$tot_sol~sumwk$condit)# test for homogenicy, not signifikant=> means homogenity is given
shapiro.test(sumwk$tot_sol)# test for normal distrubution, not signifikant => means normal distribution is given
qqnorm(sumwk$tot_sol) # shows normal distribution
boxplot(sumwk$tot_sol~sumwk$condit)
t.test(sumwk$tot_sol~sumwk$condit) # t.test is allowed and not significant
kruskal.test(tot_sol~condit, data=sumwk) # not working


# differences between meat weeks
# another method
meatwk <- filter(sumwk, condit=="Basis")
meatwk$semwk <- as.character(meatwk$semwk)
leveneTest(meatwk$tot_sol~meatwk$semwk)# are varianzes homogene => dont know how to test
ao=(aov(meatwk$tot_sol~meatwk$semwk)) # anova is not working with semwk as group
summary(ao)
tuk <- TukeyHSD(ao)
plot(tuk)

oneway.test(tot_sol~semwk, data=meatwk) # nicht genug beobachtungen
kruskal.test(tot_sol~semwk, data=meatwk) # result makes no sense

# differences between veg weeks
vegwk= filter(sumwk, condit == "Intervention")
# vegwk$semwk <- as.factor(vegwk$semwk)
leveneTest(vegwk$tot_sol~vegwk$semwk)
summary(aov(vegwk$tot_sol~vegwk$semwk))
kruskal.test(tot_sol~semwk, data=vegwk) # makes no sense









