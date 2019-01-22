#########
## Single Card numbers
#########

# Status: 8.5.18 // egel

#required packages

library(summarytools)
library(rcompanion)

# see load data 2017 script

######----------
# all single card numbers
length(unique(df_7$card_num))
single=df_7[!duplicated(df_7$card_num),] # one NA is still in the data
single=single[!(is.na(single$card_num)),] # delete this case


#####------
# distributions: member, rabate sv, gender
# crosstabs between member and gender
Hmisc::describe(single$member) # member distribution
freq(single$member)# another method for member distribution
Hmisc::describe(single$rab_descript) # another member variable (from till)
Hmisc::describe(single$Geschlecht)# gender distribution

ctable(single$Geschlecht, single$member)
ctable(single$Geschlecht, single$rab_descript) # is not clear which variable is more reliable, wait till pädi answers
CrossTable(single$Geschlecht,single$rab_descript, chisq = T)


####----------
# mean age
# beware that some are 118 old and omit na cases
mean(single[single$age < 100,]$age, na.rm = T)
describe(single[single$age < 100,]$age)

#----------------------------------------
#----------------------------------------
#----------------------------------------

####---------
# number single of transactions (unique cases)
n_distinct(df_7$transaction_id)

# number of double transactions
sum(duplicated(df_7$transaction_id))
dub <- df_7[duplicated(df_7$transaction_id),]

####-----------
#all cash payers => with double ID's!!
n_distinct(df_7[df_7$pay_description=="Bargeld CHF",]$transaction_id)
cash=df_7[df_7$pay_description=="Bargeld CHF",]
campuscard=anti_join(df_7,cash)
Hmisc::describe(cash$label_content)

######----------
# how many cases eat in both canteens

freq <- df_7[!duplicated(df_7$transaction_id),] # exclude mehrfachtransaktionen
freq <- freq[!freq$pay_description == "Bargeld CHF",] # exclude cash payers
freq <- group_by(freq, card_num,shop_description) %>% 
    summarise(visit=n())

grental <- filter(freq, card_num, shop_description == "Grüental Mensa") # only cases in grüental
vista <- filter(freq, card_num, shop_description == "Vista Mensa") # only cases in vista
both <- inner_join(grental, vista, by="card_num") %>% # merge data with unique identifier card_num
    mutate(tot_visit= visit.x + visit.y) %>% # sum together both visits
    rename(shop_descr_grüen=shop_description.x, shop_descr_vist = shop_description.y, visit_grüen=visit.x, visit_vist=visit.y) # rename variables

#######--------
# special cases, how eat more than 60 times in canteens
fr <- filter(freq, visit > 60)
special <- filter(df_7, card_num %in% fr$card_num)
write_delim(special,"S:/pools/n/N-IUNR-nova-data/02_kassendaten/02_berichte/special_cases_180507_egel.csv" ,delim = ";")

####---------
#plot Frequency for both canteen separately
# exclude mehrfachzahlende and cash payers (no card_number)

freq <- df_7[!duplicated(df_7$transaction_id),] # exclude mehrfachtransaktionen
freq <- freq[!freq$pay_description == "Bargeld CHF",] # exclude cash payers
freq <- group_by(freq, card_num,shop_description) %>% 
    summarise(visit=n()) # count same cases

# plot for canteen grüental

fr <- freq %>% 
    filter(shop_description == "Grüental Mensa") %>% # filter for mensa grüental
    group_by(shop_description, visit) %>% 
    mutate(visit_counts=sum(visit))
fr2 <- group_by(fr, visit_counts, shop_description) %>% 
    summarise(tot=n()) %>%
    mutate(pct=visit_counts/sum(visit_counts))


ggplot(fr, aes(x=reorder(as.factor(visit)),y=pct, fill=shop_description, alpha=shop_description)) +
    geom_bar(stat="identity",colour="black" )+
    scale_fill_manual(values =  c("#c5b87c","#008099"))+
    scale_y_continuous(labels=scales::percent) +
    # scale_alpha_discrete(range = c(0.3, 1))+
    scale_alpha_manual(values=c(0.5, 1), guide=F)+
    xlab("Besuchertage") +
    ylab("Anteil Personen")+
    guides(fill=guide_legend(title="Mensa"))+
    #         geom_text(aes(
    #             label = paste0(round(visit3$pct,digits = 0)), vjust =-.2
    #         ),colour = "#000000",position = position_dodge(width = .8), size= 8) +
    mytheme2

dev.off()

# save plot to berichte (two formats eps and pdf)   
ggsave("Z:/n/N-IUNR-nova-data/02_kassendaten/02_berichte/frequ/visit_freq2_180502_egel.png",
       width = 20, 
       height = 10, 
       dpi = 600, 
        
       device = "png")

####----
# differences between grundgesamtheit and stichprobe

# grund <- read_delim("S:/pools/n/N-IUNR-nova-data/00_grundgesamtheit dep N/campus_cards_180502_egel.csv", delim = ';')
grund <- read_delim("S:/pools/n/N-IUNR-nova-data/00_grundgesamtheit dep N/campus_cards_gender_180502_egel.csv", delim = ';', col_names = F) %>%
    rename(member = X1, counts=X2) %>%
    mutate(sample = 'grund') # add new variable

# prepare data for stichprobe resp. quasi experiment sample
stich <- single %>%
    group_by(member, Geschlecht) %>%
    count() %>% # count occurencies
    rename(counts=n) %>% # rename variable
    mutate(sample = 'stich') # add new variable

# rename some variables in accordance with gender
stich[2,1] <- "Mitarbeiterin"
stich[3,1] <- "Mitarbeiter"
stich[5,1] <- "Studentin"
stich[6,1] <- "Student"

stich$Geschlecht <- NULL # delete variable gender

# coerce both data frames
gesamt=bind_rows(grund, stich) # combine datasets

# however test adequately? 
t.test(counts ~ sample, data=gesamt) # no differences between two samples

# try another way => chi square
dM <- melt(grund) # melt data according to entries in counts
dM <- dM[rep(1:nrow(dM), dM$value),1:2] # replicates the variables member and sample as many as counts in values are, e.g. replicates Lernende 7 times, because count is 7

dM2 <- stich[rep(1:nrow(stich),stich$counts),]
dM2$counts <- NULL

tot=bind_rows(dM,dM2)

chiSquare(member ~ sample, data=tot)
CrossTable(tot$member, tot$sample, chisq = T) # same result, seems to be different

chisq.test(tot$member, tot$sample)$stdres # adjusted residuals
sig <- .05 # significant level
# for critical z-values see: https://mathcracker.com/z_critical_values.php
sigajd <- sig/(length(unique(tot$member))*length(unique(tot$sample)))# adjust for multiple testing with bonferroni => critical value +- 2.96

# another method doing the same => however not working
pairwiseNominalIndependence(tot$member,tot$sample,
                            fisher = FALSE,
                            gtest  = FALSE,
                            chisq  = TRUE,
                            method = "fdr")



