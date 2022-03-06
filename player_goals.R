##################################################
###        Plot a given player's goals         ###
##################################################

#Packages
library(tidyverse)
library(StatsBombR)
library(grid)
library(gtable)

#Read in data provided by StatsBomb (https://statsbomb.com/)
comps<- FreeCompetitions() %>%
  filter(competition_name == "FA Women's Super League" & season_name == "2020/2021")
matches<- FreeMatches(comps)

#Select entries from matches where goal is scored
matchgoal<- data.frame()
for(i in 1:nrow(matches)) {
  matchgoal<- rbind(matchgoal, 
                    get.matchFree(matches[i,]) %>%
                      filter(shot.outcome.name == "Goal") %>%
                      select(match_id, id:second, related_events, location, play_pattern.name, team.name, player.name:position.name, shot.type.name, shot.body_part.name, shot.technique.name))
}

#Pull out pitch location and turn into 2 variables
matchgoal$loc_x<- NULL
matchgoal$loc_y<- NULL
for(i in 1:nrow(matchgoal)) {
  matchgoal$loc_x[i]<- as.data.frame(matchgoal$location[i])[1,]
  matchgoal$loc_y[i]<- as.data.frame(matchgoal$location[i])[2,]
}

#Filter for player and plot
player<- "Leah Williamson"
dat<- filter(matchgoal, player.name == player)
blank_pitch() +
  xlim(c(90,120)) +
  coord_flip() +
  geom_rect(aes(xmin=90, xmax=120,
                ymin=0, ymax=80),
            colour="#ffffff", fill="#538032", alpha=0.2) +
  geom_density_2d_filled(data = dat, aes(x = loc_x, y = loc_y, fill = ..level..),
                         contour_var = "ndensity", 
                         breaks = seq(0.1, 1.0, length.out = 10)) +
  scale_fill_brewer(palette = "YlOrRd") +
  geom_point(data = dat, aes(x = loc_x, y = loc_y)) +
  pitch_markings() +
  annotate("text", x=92, y=0.5, hjust=0, label=paste0("Goals scored by\n", player), colour = "#ffffff") +
  labs(caption = "Data provided by StatsBomb (https://statsbomb.com/)")



  
  
