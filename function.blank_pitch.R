###################################################
###       Function to create a blank pitch      ###
###################################################

#Packages
library(ggplot2)

blank_pitch<- function(grass_col="#538032",
                       line_col="#ffffff",
                       background_col="#538032",
                       size=12,
                       pitchlength=120,
                       pitchwidth=80) {
  p<- ggplot() +
    #Blank pitch  
    geom_rect(aes(xmin = 0, xmax = pitchlength,
                  ymin = 0, ymax = pitchwidth),
              colour = line_col, fill = grass_col)+
    xlim(c(-10,pitchlength+10))+
    ylim(c(-10,pitchwidth+10))+
    theme(
      axis.text=element_blank(),
      axis.ticks.length=unit(0, "lines"), 
      axis.title=element_blank(),
      legend.position="none",
      strip.background = element_rect(colour = background_col, fill = background_col, size = .5),
      panel.background=element_rect(fill=background_col,colour=background_col), 
      panel.grid.major=element_blank(), 
      panel.grid.minor=element_blank(), 
      panel.spacing=element_blank(), 
      plot.background=element_blank(), 
      plot.margin=unit(c(0, 0, 0, 0), "lines"), 
      plot.title=element_text(size=size*1.2), 
      strip.text.y=element_text(colour=background_col,size=size,angle=270),
      strip.text.x=element_text(size=size*1))
  
  return(p)
}
