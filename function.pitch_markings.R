#####################################################
###     Function to create pitch markings and     ###
###              no pitch background              ###
#####################################################

#Packages
library(ggplot2)

pitch_markings<- function(grass_col="#538032",
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
  
  center_circle<- circleFun(center = c(0.5 * pitchlength, 0.5 * pitchwidth), diameter=30)
  
  Dleft<- circleFun(center = c(0.1 * pitchlength, 0.5 * pitchwidth), diameter = 22)
  Dleft<- Dleft[which(Dleft$x >= 0.17 * pitchlength),]
  
  Dright<- circleFun(center = c((1 - 0.1) * pitchlength, 0.5 * pitchwidth), diameter = 22)
  Dright<- Dright[which(Dright$x <= (1 - 0.17) * pitchlength),]
  
  #Plot
  p<- ggplot() +
    #Center circle
    geom_path(data = center_circle, aes(x = x, y = y), colour = line_col)+
    #Middle spot
    geom_point(aes(x = 0.5 * pitchlength, y = 0.5 * pitchwidth), colour = line_col)+
    #Halfway line
    geom_segment(aes(x = 0.5*pitchlength, xend = 0.5*pitchlength, 
                     y = 0, yend = pitchwidth),colour = line_col) +
    #RHS penalty box
    geom_rect(aes(xmin = 0.83 * pitchlength, xmax = pitchlength,
                  ymin = 0.211 * pitchwidth, ymax = 0.799 * pitchwidth),
              colour = line_col, fill = grass_col, alpha = 0.2) +
    #RHS penalty spot
    geom_point(aes(x=0.885 * pitchlength, y = 0.5 * pitchwidth), colour = line_col)+
    #LHS penalty box
    geom_rect(aes(xmin = 0, xmax = 0.17 * pitchlength,
                  ymin = 0.211 * pitchwidth, ymax = 0.799 * pitchwidth),
              colour = line_col, fill = grass_col, alpha = 0.2) +
    #LHS penalty spot
    geom_point(aes(x=0.115 * pitchlength, y = 0.5 * pitchwidth), colour = line_col)+
    #RHS 6-yard box
    geom_rect(aes(xmin = (1 - 0.058) * pitchlength, xmax = pitchlength,
                  ymin = 0.368 * pitchwidth, ymax = (1 - 0.368) * pitchwidth),
              colour = line_col, fill = grass_col, alpha = 0.2) +
    #LHS 6-yard box
    geom_rect(aes(xmin = 0, xmax = 0.058 * pitchlength,
                  ymin = 0.368 * pitchwidth, ymax = (1 - 0.368) * pitchwidth),
              colour = line_col, fill = grass_col, alpha = 0.2) +
    #Left D
    geom_path(data = Dleft, aes(x = x, y = y), colour = line_col) +
    #Right D
    geom_path(data = Dright, aes(x = x, y = y), colour = line_col)
  
  return(p)
  
}
