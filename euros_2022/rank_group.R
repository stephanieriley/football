###########################################
###             Euros 2022              ###
###            Rank by Group            ###
###########################################

#Packages
library(tidyverse)
library(ggimage)
library(showtext)

#Data
dat<- read.csv("euros_2022/eurorank2022.csv")
dat_mut<- mutate(dat, 
             Country = as.factor(ï..Country),
             Group = as.factor(paste("Group", Group, sep = " ")),
             Worldrank_pos = case_when(Country=="Northern Ireland" ~ 35, #Bring NI up so plot doesn't look so weird
                                       TRUE ~ as.numeric(Worldrank)))
str(dat_mut)

#Colours for plotting
grasscol<- "#538032"
linecol<- "#ffffff"

#Font
#Register font
font_add(family="euro2022", regular="euros_2022/UEFAEuro-Book.ttf")
#Load font
showtext_auto()


ggplot(dat_mut, aes(x=Group, y=Worldrank_pos)) +
  ggimage::geom_image(aes(image=Flag),
                      size=0.07, by="width",
                      #position = position_jitter(height = 0.5, width=0)
                      ) +
  geom_text(aes(label=Worldrank, group=Group),
            nudge_x = 0.3,
            colour = linecol) +
  scale_y_reverse() +
  labs(title = "FIFA world ranking of countries by group in the\nUEFA Euro 2022 tournament",
       caption = "World ranking correct as of June 17 2022") +
  theme_classic() +
  theme(plot.margin = unit(c(1.5,1,1.5,1), "lines"),
        plot.background = element_rect(fill=grasscol, colour=grasscol),
        panel.background = element_rect(fill=grasscol, colour=grasscol),
        axis.line = element_blank(),
        axis.text.x = element_text(colour = linecol, size = 14),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        title = element_text(colour = linecol, size=22),
        text = element_text(family = "euro2022", colour = linecol)) +
  geom_curve(aes(x = 3.2, y = 10, xend = 3.2, yend = 5.3),
             arrow = arrow(length = unit(0.03, "npc")),
             col=linecol) +
  annotate(geom="text", x=3, y=12, label = "2017 champions\nNetherlands", col=linecol, size = 6) +
  geom_curve(aes(x = 0, y = 15, xend = 0.6, yend = 11),
             arrow = arrow(length = unit(0.03, "npc")),
             col=linecol) +
  annotate(geom="text", x=0, y=20, label = "Host nations\nEngland and\nNorthern Ireland", col=linecol, size=6) +
  coord_cartesian(xlim=c(0.1,4),clip="off")

