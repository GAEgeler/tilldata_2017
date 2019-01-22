#########
## Differences between member and rabate
#########

# status: 18.04 // egel
#--------# mail to pädi on 18.04 for further clarifications

####------
# Check differences between member and rab_description
####
setwd("S:/pools/n/N-IUNR-nova-data/02_kassendaten/00_original/Versand an ZHAW/files")
df_new1 <- read_delim("filtered_data_r_notime_180403_egel.csv",delim = ';',locale = locale(encoding = 'LATIN1'), quote=",") # attention to time zone, somethimes differ between date and trans_date!

# df_new1$member <- str_replace(df_new1$member,"Lernende","Lernende Strickhof") => ask bruno or pädi again whats the difference between #NV and Schüler Lerndende Partner

df_new1$rab_descript <- str_replace(df_new1$rab_descript,"Mitarbeiter","Mitarbeitende")
df_new1$rab_descript <- str_replace(df_new1$rab_descript,"Schüler Lernende Partner","Lernende")

unique(df_new1$member)
unique(df_new1$rab_descript)

df <- df_new1[!(df_new1$member == df_new1$rab_descript),] # search for differences
df <- df[!(is.na(df$transaction_id)),] # omit NA

write_delim(df,"diff member_rabate_180417_egel.csv",delim=';')

