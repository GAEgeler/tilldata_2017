#########
## Single Card numbers
#########

# Status: 15.6.18 // egel

#required packages
library(summarytools)
library(rcompanion)

# see load data 2017 script


# all single card numbers------
length(unique(df_7$card_num))
single=df_7[!duplicated(df_7$card_num),] # one NA is still in the data
single=single[!(is.na(single$card_num)),] # delete this case


# distributions: member, rabate sv, gender---------
# crosstabs between member and gender
Hmisc::describe(single$member) # member distribution
freq(single$member)# another method for member distribution
Hmisc::describe(single$rab_descript) # another member variable (from till)
Hmisc::describe(single$Geschlecht)# gender distribution

ctable(single$Geschlecht, single$member)
ctable(single$Geschlecht, single$rab_descript) # is not clear which variable is more reliable, wait till pädi answers
CrossTable(single$Geschlecht,single$rab_descript, chisq = T)


# mean age--------
# beware that some are 118 old and omit na cases
mean(single[single$age < 100,]$age, na.rm = T)
describe(single[single$age < 100,]$age)


# number single of transactions (unique cases)---------
n_distinct(df_7$transaction_id)

# number of double transactions
sum(duplicated(df_7$transaction_id))
dub <- df_7[duplicated(df_7$transaction_id),]


#all cash payers => with double ID's!!---------
n_distinct(df_7[df_7$pay_description=="Bargeld CHF",]$transaction_id)
cash=df_7[df_7$pay_description=="Bargeld CHF",]
campuscard=anti_join(df_7,cash)
Hmisc::describe(cash$label_content)


# how many cases eat in both canteens--------

freq <- df_7[!duplicated(df_7$transaction_id),] # exclude mehrfachtransaktionen
freq <- freq[!freq$pay_description == "Bargeld CHF",] # exclude cash payers
freq <- group_by(freq, card_num,shop_description) %>% 
    summarise(visit=n())

grental <- filter(freq, card_num, shop_description == "Grüental Mensa") # only cases in grüental
vista <- filter(freq, card_num, shop_description == "Vista Mensa") # only cases in vista
both <- inner_join(grental, vista, by="card_num") %>% # merge data with unique identifier card_num
    mutate(tot_visit= visit.x + visit.y) %>% # sum together both visits
    rename(shop_descr_grüen=shop_description.x, shop_descr_vist = shop_description.y, visit_grüen=visit.x, visit_vist=visit.y) # rename variables


# special cases, how eat more than 60 times in canteens-------
fr <- filter(freq, visit > 60)
special <- filter(df_7, card_num %in% fr$card_num)
write_delim(special,"S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/frequenzanalyse/special_cases_180507_egel.csv" ,delim = ";")


#plot Frequency for both canteen separately---------
# exclude mehrfachzahlende and cash payers (no card_number)
freq <- df_7[!duplicated(df_7$transaction_id),] # exclude mehrfachtransaktionen
freq <- freq[!freq$pay_description == "Bargeld CHF",] # exclude cash payers
freq <- group_by(freq, card_num,shop_description) %>% 
    summarise(visit=n()) # count same cases

# plot for canteen grüental
fr <- freq %>% 
    filter(shop_description == "Grüental Mensa") %>% # filter for mensa grüental
    group_by(shop_description, visit) %>% 
    summarise(visit_counts=n()) %>%
    mutate(pct=visit_counts/sum(visit_counts))

ggplot(fr, aes(x=reorder(as.factor(visit)),y=pct, fill=shop_description, alpha=shop_description)) +
    geom_bar(stat="identity",colour="black" )+
    scale_fill_manual(values =  c("#c5b87c"))+
    scale_y_continuous(labels=scales::percent,limits = c(0,.19), breaks = seq(0,.2,.025)) +
    # scale_alpha_discrete(range = c(0.3, 1))+
    scale_alpha_manual(values=c(0.7), guide=F)+
    xlab("Besuchertage") +
    ylab("Anteil Personen")+
    guides(fill=F )+ # no legend needed
    #         geom_text(aes(
    #             label = paste0(round(visit3$pct,digits = 0)), vjust =-.2
    #         ),colour = "#000000",position = position_dodge(width = .8), size= 8) +
    mytheme2

# save plot to berichte (two formats eps and pdf)   
ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/frequenzanalyse/plots/visit_freq_grüen_180523_egel.png",
       width = 20, 
       height = 10, 
       dpi = 600, 
       device = "png")

# plot for canteen vista
fr <- freq %>% 
    filter(shop_description == "Vista Mensa") %>% # filter for mensa grüental
    group_by(shop_description, visit) %>% 
    summarise(visit_counts=n()) %>%
    mutate(pct=visit_counts/sum(visit_counts))

ggplot(fr, aes(x=reorder(as.factor(visit)),y=pct, fill=shop_description, alpha=shop_description)) +
    geom_bar(stat="identity",colour="black" )+
    scale_fill_manual(values =  c("#008099"))+
    scale_y_continuous(labels=scales::percent, limits = c(0,.19), breaks = seq(0,.2,.025)) +
    # scale_alpha_discrete(range = c(0.3, 1))+
    scale_alpha_manual(values=c(0.7), guide=F)+
    xlab("Besuchertage") +
    ylab("Anteil Personen")+
    guides(fill=F)+
    #         geom_text(aes(
    #             label = paste0(round(visit3$pct,digits = 0)), vjust =-.2
    #         ),colour = "#000000",position = position_dodge(width = .8), size= 8) +
    mytheme2
# save plot
ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/frequenzanalyse/plots/visit_freq_vista_180523_egel.png",
       width = 20, 
       height = 10, 
       dpi = 600, 
       device = "png")

# plot Frequency for both canteens ------
# prepare data => build groups and fill in zeros if necessary (complete)
# old labels: labels=c("1-12","13-24","25-36","37-48","49-60","60-95")
fr <- freq %>% 
    mutate(category=cut(visit, breaks = c(-Inf,12,24,36,48,60,Inf), labels=c("max. 1x\n pro Woche","max. 2x\n pro Woche","max. 3x\n pro Woche","max. 4x\n pro Woche", "max. 5x\n pro Woche","mehr als 5x\n pro Woche"))) %>%
    group_by(shop_description, category) %>% 
    summarise(visit_counts=n()) %>% # count how hoften a visit occurs, e.g. oneday visitors occur 200 times 
    mutate(pct=visit_counts/sum(visit_counts))
    

# plot
ggplot(fr, aes(x=category,y=pct, fill=shop_description)) +
    geom_bar(stat="identity",colour=NA, position = "dodge", width = .6)+
    scale_fill_manual(values =  c("#fad60d","#c5b87c"))+
    scale_y_continuous(labels=scales::percent) +
    # scale_alpha_discrete(range = c(0.6, .8), guide=F)+
    # scale_alpha_manual(values=c(0.5, 1), guide=F)+
    xlab("\nDurchschnittliche Mensabesuche pro Woche") +
    ylab("Anteil Mensabesucher")+
    # ggtitle("Anmerkung: 23'446 Transaktionen wurden berücksichtigt")+
    guides(fill=F)+
    #         geom_text(aes(
    #             label = paste0(round(visit3$pct,digits = 0)), vjust =-.2
    #         ),colour = "#000000",position = position_dodge(width = .8), size= 8) +
    mytheme

# save plot
ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/frequenzanalyse/plots/visit_freq_both_180614_02egel.png",
       width = 20, 
       height = 10, 
       dpi = 600, 
       device = "png")

######
## save plots for presantation somewhere else
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/12_work_pack/wpIII.1_menu_choice/05_besprechungen/180706_sv_fm_meeting/slides template Ordner/plots/visit_freq_both_180614_02egel.png",

ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/12_work_pack/wpIII.1_menu_choice/05_besprechungen/180706_sv_fm_meeting/ppp_indd_180627/plots/visit_freq_both_180614_03egel.pdf",
       width = 17,
       height = 9,
       dpi = 600,
       units="in",
       device= cairo_pdf)

# differences between grundgesamtheit and stichprobe--------

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


# differences between gender, member and frequency------

# exclude mehrfachzahlende and cash payers (no card_number)
freq <- df_7[!duplicated(df_7$transaction_id),] # exclude mehrfachtransaktionen
freq <- freq[!freq$pay_description == "Bargeld CHF",] # exclude cash payers
freq <- group_by(freq, card_num,shop_description, Geschlecht, member) %>% 
    summarise(visit=n()) # count same cases

# in 138 cases is the information of the member missing => this cases are lernende from strickhof
tot <- freq %>%
    group_by(shop_description,member, Geschlecht) %>%
    summarise(visit_tot=sum(visit))

# change some entries
tot[2,2] <- "Mitarbeiterin"
tot[3,2] <- "Mitarbeiter"
tot[5,2] <- "Studentin"
tot[6,2] <- "Student"
tot[8,2] <- "Mitarbeiterin"
tot[9,2] <- "Mitarbeiter"
tot[11,2] <- "Studentin"
tot[12,2] <- "Student"
tot$Geschlecht <- NULL

tot2 <- tot[rep(1:nrow(tot),tot$visit_tot),1:2]

CrossTable(tot2$member, tot2$shop_description, chisq = T) # no statistial differences between gender and visits
chisq.test(tot2$member,tot2$shop_description)$stdres

adjp <- 0.05/(length(unique(tot2$member))*length(unique(tot2$shop_description))) # corrected p-value according to bonferroni
qnorm(adjp) # critical z-value, is another than form the website: +- 2.87



