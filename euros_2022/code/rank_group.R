###########################################
###             Euros 2022              ###
###            Rank by Group            ###
###########################################

#Packages
library(tidyverse)
library(ggimage)
library(showtext)
library(ggtext)
library(png)
library(grid)

#Data
dat<- read.csv("euros_2022/eurorank2022.csv")
dat_mut<- mutate(dat, 
             Country = as.factor(ï..Country),
             Group = as.factor(paste("Group", Group, sep = " ")),
             Worldrank_pos = case_when(Country=="Northern Ireland" ~ 40, #Bring NI up so plot doesn't look so weird
                                       TRUE ~ as.numeric(Worldrank)))
str(dat_mut)

#Colours for plotting
backcol<- "#362257"
maincol<- "#df3a61"
subcol<- "#60c6e9"
annotatecol<- "#ffffff"

#Font
#Register font
font_add(family="euro2022", regular="euros_2022/UEFAEuro-Book.ttf")
#Load font
showtext_auto()

#Prepare logo for insertion
logo<- readPNG("euros_2022/UEFA_Women's_Euro_2022_logo.png", native = T)
logogrob<- rasterGrob(logo)

ggplot(dat_mut, aes(x=Group, y=Worldrank_pos)) +
  ggimage::geom_image(aes(image=Flag),
                      size=0.07, by="width",
                      #position = position_jitter(height = 0.5, width=0)
                      ) +
  geom_text(aes(label=Worldrank, group=Group),
            nudge_x = 0.3,
            colour = annotatecol,
            size = 6) +
  scale_y_reverse() +
  scale_x_discrete(expand = expansion(mult = -0.1)) +
  theme_classic() +
  theme(plot.margin = unit(c(1.5,1,1.5,1), "lines"),
        plot.background = element_rect(fill=backcol, colour=backcol),
        panel.background = element_rect(fill=backcol, colour=backcol),
        axis.line = element_blank(),
        axis.text.x = element_text(colour = subcol, size = 16),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_markdown(colour = maincol, size=26),
        text = element_text(family = "euro2022"),
        plot.caption = element_text(size = 12, colour = maincol)) +
  labs(title = "FIFA world <span style ='color:#ffffff'>ranking</span> 
       of countries in the<br>UEFA Women's  EURO 2022",
       caption = "World ranking correct as of June 17 2022") +
  geom_curve(aes(x = 3.2, y = 10, xend = 3.2, yend = 5.3),
             arrow = arrow(length = unit(0.03, "npc")),
             col=subcol,
             curvature = 0.3,
             size = 1) +
  annotate(geom="text", x=3, y=12.5, label = "2017 champions\nNetherlands", col=subcol, size = 6) +
  geom_curve(aes(x = 0, y = 13, xend = 0.8, yend = 8),
             arrow = arrow(length = unit(0.03, "npc")),
             col=subcol,
             curvature = -0.3,
             size = 1) +
  annotate(geom="text", x=0, y=16, label = "Hosted in\nEngland", col=subcol, size=6) +
  coord_cartesian(xlim=c(-0.5,4.7),clip="off") +
  annotation_custom(logogrob,
                    xmin=-0.3, xmax=0.4, ymin=-78)
