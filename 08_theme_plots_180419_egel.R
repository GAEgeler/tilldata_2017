######
# My Theme for plotting
######


###
# state: december 2018
# author: gian-Andrea egeler
###

# required packages
library(ggplot2)

# function to increase vertical spacing between legend keys
# source: https://github.com/tidyverse/ggplot2/issues/2844
draw_key_polygon3 <- function(data, params, size) {
    lwd <- min(data$size, min(size) / 4)
    
    grid::rectGrob(
        width = grid::unit(0.6, "npc"),
        height = grid::unit(0.6, "npc"),
        gp = grid::gpar(
            col = data$colour,
            fill = alpha(data$fill, data$alpha),
            lty = data$linetype,
            lwd = lwd * .pt,
            linejoin = "mitre"
        ))
}

# register new key drawing function, 
# the effect is global & persistent throughout the R session
GeomBar$draw_key = draw_key_polygon3

### MY Theme 0: for regular plots -------------------

mytheme <- theme_bw()+ # definve theme for plot
    theme(plot.title = element_text(size = 20, face = "bold"),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20, face = "plain"),
          legend.text = element_text(size = 30),
          legend.title = element_text(size =30),
          strip.text = element_text(size=30),
          axis.title.y = element_text(size = 25, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(size = 25,  margin = margin(t = 20, r = 0, b = 0, l = 0)),
          plot.subtitle=element_text(margin=margin(b=15),size = 20),
          plot.caption=element_text(margin=margin(t=15), face="italic", size=20),
          legend.key = element_rect(color = NA, fill = NA), # see for that part the funktion draw_key_ploygon3
          legend.key.size = unit(1.5, "cm"))

    
### MY Theme 1: for plots in presentations -------------------

mytheme1 <- theme_bw()+ # definve theme for plot
    theme(plot.title = element_text(size = 30, face = "bold"),
          axis.text.x = element_text(size=28),
          axis.text.y = element_text(size=30, face = "plain"),
          legend.text = element_text(size = 35),
          legend.title = element_text(size =35),
          axis.title.y = element_text(size = 40, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(size = 40,  margin = margin(t = 20, r = 0, b = 0, l = 0)),
          plot.subtitle=element_text(margin=margin(b=15),size = 20),
          plot.caption=element_text(margin=margin(t=15), face="italic", size=20),legend.key = element_rect(color = NA, fill = NA),
          legend.key.size = unit(1.5, "cm"))



## MY Theme 2: for plots in working paper ----------------------
#see how to change fonts: https://stackoverflow.com/questions/34522732/changing-fonts-in-ggplot2
mytheme2 <- 


### MY Theme 3: for plots without lines on the left and on the top -------------------

mytheme3 <- theme_bw()+ # definve theme for plot
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(size = 20, face = "bold"),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20, face = "plain"),
          legend.text = element_text(size = 20),
          legend.title = element_text(size =20),
          axis.title.y = element_text(size = 25, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(size = 25,  margin = margin(t = 20, r = 0, b = 0, l = 0)))



### MY Theme 4: for plots with text of x axis 90 degrees -------------------


mytheme4 <- theme_bw()+ # definve theme for plot
    theme(plot.title = element_text(size = 20, face = "bold"),
          axis.text.x = element_text(size=17,angle = 90, vjust=0.5, hjust=1),
          axis.text.y = element_text(size=20, face = "plain"),
          legend.text = element_text(size = 20),
          legend.title = element_text(size =20),
          axis.title.y = element_text(size = 25, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(size = 25,  margin = margin(t = 20, r = 0, b = 0, l = 0)),
          plot.subtitle=element_text(margin=margin(b=15),size = 20),
          plot.caption=element_text(margin=margin(t=15), face="italic", size=20))





