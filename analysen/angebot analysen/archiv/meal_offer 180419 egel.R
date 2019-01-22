######
# Ingredience Analysis
######

#Status: 15.15.18 // egel

#######
## protein ditribution
#######

# data
offer <- info1

# pro standort und ohne locals
offer <- filter(offer,!grepl("Local+",offer$article_description) & shop_description=="Grüental Mensa")
Hmisc::describe(offer$Tier_1)
Hmisc::describe(offer$Milchprodukt_1)
Hmisc::describe(offer$Vegi_Protein)
Hmisc::describe(offer$Ei)

# maybe melt data first for better analysis (at least menu infomation)
offer_long=melt(offer, id.vars = c("date","label_content"),measure.vars = c("meal_content_1","meal_content_2","meal_content_3","meal_content_4","meal_content_5","meal_content_6","meal_content_7"))
dfSummary(offer_long$value,max.distinct.values=260) # lots to do for a good analysis, however later on

# Cross tabs => see script .py better print options
ct=CrossTable(offer$Tier_1,offer$Milchprodukt_1,prop.t = F) # find a way to drop NAN's or na
print(ct)
ctable(offer$Tier_1, offer$Tier_2) # library summarytools


#load data with locals for local analysis
offer_l <-  info1
Hmisc::describe(offer_l$Tier_1)
Hmisc::describe(offer_l$Milchprodukt_1)
Hmisc::describe(offer_l$Vegi_Protein)
Hmisc::describe(offer_l$Ei)
