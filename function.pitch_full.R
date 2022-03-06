#####################################################
###   Function to create a pitch with markings    ###
#####################################################

#Packages
library(ggplot2)

pitch_full<- function(grass_col="#538032",
                      line_col="#ffffff",
                      background_col="#538032",
                      size=12,
                      pitchlength=120,
                      pitchwidth=80) {
  
  #Create data frames for penalty box D
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 1000){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  Dleft<- circleFun(center = c(0.1 * pitchlength, 0.5 * pitchwidth), diameter = 22)
  Dleft<- Dleft[which(Dleft$x >= 0.17 * pitchlength),]
  
  Dright<- circleFun(center = c((1 - 0.1) * pitchlength, 0.5 * pitchwidth), diameter = 22)
  Dright<- Dright[which(Dright$x <= (1 - 0.17) * pitchlength),]
  
  #Plot
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
        strip.text.x=element_text(size=size*1))+
      #Centre circle
      annotation_custom(grob = grid::circleGrob(r = grid::unit(1,  "npc"),
                                                gp = grid::gpar(col = line_col, fill = grass_col, lwd = 2)),
                        xmin = 0.43 * pitchlength, xmax = 0.57 * pitchlength,
                        ymin = 0.43 * pitchwidth, ymax = 0.57 * pitchwidth)+
      #Middle spot
      geom_point(aes(x = 0.5 * pitchlength, y = 0.5 * pitchwidth), colour = line_col)+
      #Halfway line
      geom_segment(aes(x = 0.5*pitchlength, xend = 0.5*pitchlength, 
                       y = 0, yend = pitchwidth),colour = line_col) +
      #RHS penalty box
      geom_rect(aes(xmin = 0.83 * pitchlength, xmax = pitchlength,
                    ymin = 0.211 * pitchwidth, ymax = 0.799 * pitchwidth),
                colour = line_col, fill = grass_col) +
      #RHS penalty spot
      geom_point(aes(x=0.885 * pitchlength, y = 0.5 * pitchwidth), colour = line_col)+
      #LHS penalty box
      geom_rect(aes(xmin = 0, xmax = 0.17 * pitchlength,
                    ymin = 0.211 * pitchwidth, ymax = 0.799 * pitchwidth),
                colour = line_col, fill = grass_col) +
      #LHS penalty spot
      geom_point(aes(x=0.115 * pitchlength, y = 0.5 * pitchwidth), colour = line_col)+
      #RHS 6-yard box
      geom_rect(aes(xmin = (1 - 0.058) * pitchlength, xmax = pitchlength,
                    ymin = 0.368 * pitchwidth, ymax = (1 - 0.368) * pitchwidth),
                colour = line_col, fill = grass_col) +
      #LHS 6-yard box
      geom_rect(aes(xmin = 0, xmax = 0.058 * pitchlength,
                    ymin = 0.368 * pitchwidth, ymax = (1 - 0.368) * pitchwidth),
                colour = line_col, fill = grass_col) +
      #Left D
      geom_path(data = Dleft, aes(x = x, y = y), colour = line_col) +
      #Right D
      geom_path(data = Dright, aes(x = x, y = y), colour = line_col)
  
  return(p)
  
}
