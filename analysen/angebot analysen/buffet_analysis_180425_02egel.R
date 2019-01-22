#########
## Buffet Offer Analysis
########

# status: 27.6.18 // egel

# load data
buffet=read_excel("S:/pools/n/N-IUNR-nova-data/06_add_var/01_dokumentation/buffet_animal_180425_03egel.xlsx")


# for analysis bring it to long format
long_buffet=melt(buffet, id.vars = c("date","buffet_animal_comp"), measure.vars = c("ingred_1","ingred_2","ingred_3","ingred_4","ingred_5"))
long_buffet=rename(long_buffet,content=value)

# describe buffet offer
# Hmisc package
di=Hmisc::describe(long_buffet$content)
di$values
# psych package
describe.by(long_buffet, long_buffet$content)
# summarytools package => has olso ctabs and freq
freq(long_buffet$content) # best way to print frequencies
dfSummary(long_buffet)


# Buffet per Condition
info_$week <- week(info_$date) # see load data 2017
info_$condit <- ifelse((info_$cycle ==1 & info_$week %%2 == 0),"Basis",ifelse((info_$cycle == 2 & info_$week %%2 == 1),"Basis","Intervention"))

df_ <- info_ %>%
    filter(article_description == "Hot and Cold") %>%
    group_by(week,condit, shop_description) %>%
    summarise(tot=sum(buffet_animal_comp, na.rm=T)) %>%
    mutate(pct_all = tot/(12*5)) %>% # after RM and AM told me that they serve in average in total 14 components (4 hot and 10 cold) per day
    mutate(mean_ = tot/5) # mean per week
    
#xlab
df_$xlab <- paste(df_$week,df_$condit, sep = "\n")

# define date of creation
st <- strftime(today(), format = "%d.%m.%y")

#plot numbers of animal components per canteen
p <- ggplot(df_, aes(x = reorder(as.factor(df_$xlab)), y = tot))+
    geom_bar(aes(fill= shop_description), stat="identity", position = "dodge", width = .7)+
    xlab("\nHerbstsemester (Kalenderwochen 40 bis 51)")+
    ylab("Anzahl Fleisch-Komponenten auf dem Buffet")+
    scale_fill_manual(values = c("Grüental Mensa" = "#fad60d", "Vista Mensa" = "#c5b87c"))+
    guides(fill= guide_legend("Mensa / Standort\n"))+
    geom_text(aes(reorder(as.factor(df_$xlab)), y = tot, group = shop_description, label = round(mean_, digits = 1)), position = position_dodge(width = 1),vjust = -0.5, size = 7)+
    mytheme

p + labs(caption="Quelle: ZHAW, Kassendaten SV Schweiz", subtitle = paste("Stand :",st))

# save plot
ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/angebot analysen/plots/ buffet condition 180614 02egel.png",
           width = 16,
           height = 10,
           dpi=600,
           units="in",
           device="png")

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/angebot analysen/plots/ buffet condition 180614 02egel.pdf",
       width = 25,
       height = 14,
       dpi=600,
       units="in",
       device=cairo_pdf)

######
## save plots for presantation somewhere else
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/12_work_pack/wpIII.1_menu_choice/05_besprechungen/180706_sv_fm_meeting/ppp_indd_180627/plots/buffet condition 180614 03egel.pdf",
       width = 17,
       height = 9,
       dpi = 600,
       units="in",
       device= cairo_pdf)

# save for sv protocol
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/12_work_pack/wpIII.1_menu_choice/05_besprechungen/180706_sv_fm_meeting/plots fürs protokoll/buffet_canteen_180713_egel.pdf",
       width = 25,
       height = 14,
       dpi = 600,
       units="in",
       device= cairo_pdf)



#plot pct of animal components per canteen
p <- ggplot(df_, aes(reorder(as.factor(df_$xlab)),pct_all, fill= shop_description))+
    geom_bar(stat="identity", position = "dodge", width = .7)+
    xlab("\nHerbstsemester (Kalenderwochen 40 bis 51)")+
    ylab("Anteil Fleisch-Komponenten auf dem Buffet")+
    scale_y_continuous(limits = c(0,.30), breaks = seq(0,.30,.05),labels = scales::percent,)+
    scale_fill_manual(values = c("Grüental Mensa" = "#fad60d", "Vista Mensa" = "#c5b87c"))+
    guides(fill= guide_legend("Mensa / Standort\n"))+
    # geom_text(aes(label=pct_all),position = position_dodge(), size=7)+
    mytheme

p + labs(caption="Quelle: ZHAW, Kassendaten SV Schweiz", subtitle = paste("Stand: ", st))

ggsave("S:/pools/n/N-IUNR-nova-data/02_kassendaten/01_analysen/angebot analysen/plots/ buffet percent 180713 egel.pdf",
       width = 25,
       height = 14,
       dpi=600,
       units="in",
       device=cairo_pdf)

# save for sv protocol
ggsave("S:/pools/n/N-IUNR-Allgemein/Zentren/gruppe_NOVA/04_f&e/97103165041001_NOVANIMAL_arbeit/12_work_pack/wpIII.1_menu_choice/05_besprechungen/180706_sv_fm_meeting/plots fürs protokoll/buffet_pct_180713_egel.pdf",
       width = 25,
       height = 14,
       dpi = 600,
       units="in",
       device= cairo_pdf)