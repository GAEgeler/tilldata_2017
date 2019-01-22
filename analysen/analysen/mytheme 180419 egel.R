######
# My Theme for plotting
######


# Status: 19.04 // egel


library(extrafont) # be carefull with device = "postscript"
loadfonts() # takes a while for loading all fonts
# somehow, not working with postscript

### MY Theme 1: for regular plots -------------------

mytheme <- theme_bw()+ # definve theme for plot
    theme(plot.title = element_text(size = 20, face = "bold"),
          axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20, face = "plain"),
          legend.text = element_text(size = 20),
          legend.title = element_text(size =20),
          axis.title.y = element_text(size = 25, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(size = 25,  margin = margin(t = 20, r = 0, b = 0, l = 0)))


### MY Theme 2: for more information in plot ----------------------

mytheme2 <- theme_bw()+ # definve theme for plot
    theme(plot.title = element_text(size = 20, face = "bold"),
          axis.text.x = element_text(size=16),
          axis.text.y = element_text(size=20, face = "plain"),
          legend.text = element_text(size = 20),
          legend.title = element_text(size =20),
          axis.title.y = element_text(size = 25, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(size = 25,  margin = margin(t = 20, r = 0, b = 0, l = 0)))




### MY Theme 3: for plots without lines left and up -------------------

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


mytheme3 <- theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank(), axis.line = element_line(colour = "black"))+ # definve theme for plot
    theme(plot.title = element_text(size = 20, face = "bold")) +
    theme(text = element_text(size=20),axis.title.y = element_text(size = 20)) +
    theme(axis.title.x = element_text(size = 20)) +
    theme(legend.text = element_text(size = 20),legend.title = element_text(size =20)) +
    theme(axis.title.y = element_text(size = 20, margin = margin(t = 0, r = 20, b = 0, l = 0)))+
    theme(axis.title.x = element_text(size = 20,  margin = margin(t = 20, r = 0, b = 0, l = 0)))



### MY Theme 3: for plots with text of x axis 90 degrees -------------------


mytheme4 <- theme_bw()+ # definve theme for plot
    theme(plot.title = element_text(size = 20, face = "bold"),
          axis.text.x = element_text(size=17,angle = 90, vjust=0.5, hjust=1),
          axis.text.y = element_text(size=20, face = "plain"),
          legend.text = element_text(size = 20),
          legend.title = element_text(size =20),
          axis.title.y = element_text(size = 25, margin = margin(t = 0, r = 20, b = 0, l = 0)),
          axis.title.x = element_text(size = 25,  margin = margin(t = 20, r = 0, b = 0, l = 0)))



### function to check if the background color is dark or not -------------------
# source: https://trendct.org/2016/01/22/how-to-choose-a-label-color-to-contrast-with-background/
# source 2: https://stackoverflow.com/questions/49716005/how-to-control-label-color-depending-on-fill-darkness-of-bars

isDark <- function(color) {
    (sum(grDevices::col2rgb(color) *c(299, 587,114))/1000 < 123)
}



#### function to save plots
# source: https://stackoverflow.com/questions/26551359/r-ggsave-save-thumbnail-size-200-x-200-scaled-image

plot.save <- function(plot, 
                      width = 800, 
                      height = 500, 
                      text.factor = 1, 
                      filename = paste0(
                          format(
                              Sys.time(), 
                              format = '%Y%m%d-%H%M%S'), '-Rplot.png'
                      )
) {
    
    dpi <- text.factor * 100
    width.calc <- width / dpi
    height.calc <- height / dpi
    
    ggsave(filename = filename,
           dpi = dpi,
           width = width.calc,
           height = height.calc,
           units = 'in',
           plot = plot)
}