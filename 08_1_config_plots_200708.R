#####################
# set parameters for ploting 
#####################


#### status: july 2020 // gian-andrea egeler

# set parmeters for ggsave

## plot with inches: comes bigger-------
# default inches

## define margins
# case 1. bar plots & faceted bar plots: e.g. figure 20
h_1 <- 15 # height
w_1 <- 25 # width

# case 2. bar plot concerning meal line: figure 3
h_12 <- 20
w_12 <- 16

# case 3. bar plot concerning trunover: figure 4
w_13 <- 40

# case 4. bar plot concerning facet_wrap 4 lines e.g. figure 8
h_14 <- 8

# case 4. bar plot concerning sellings over 12 weeks: figure 6
w_15 <- 30

# case 5. bar plot concerning visiter frequency per type: figure 17
h_15 <- 10


# case 6. bar plot concerning impact figure: figure 5 
h_16 <- 5

# case treemap
h_t <- 20
w_t <- 20


##plot with cm: comes bit smaller, according to the margins of the indesign layout--------
## define units
units_ <- "cm" # see github in case you need to change

## define margins
# case 1. barplots & barplots
#h_2 <-  # height
#w_2 <-  # width

# that would be with the mythme0.1 (however does not fit perfectly into the report)
# ggsave("plots/selling_HS_17_egel.pdf",p,
#        width = 20,
#        height = 12,
#        units = "cm",
#        dpi = 300,
#        device = cairo_pdf)