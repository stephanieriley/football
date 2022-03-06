##################################################
###            Plot Miedema's goals            ###
##################################################

#Packages
library(tidyverse)
library(StatsBombR)
library(ggsoccer)

#Read in data provided by StatsBomb (https://statsbomb.com/)
comps<- FreeCompetitions() %>%
  filter(competition_name == "FA Women's Super League")
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

#Filter for Miedema and plot
filter(matchgoal, player.name == "Vivianne Miedema") %>%
  ggplot(aes(x=loc_x, y=loc_y)) +
  StatsBombR::annotate_pitchSB(fill = "#538032", col = "#ffffff") +
  geom_point() +
  coord_flip() +
  scale_x_continuous(limits = c(90, 120)) +
  theme_classic() +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        axis.line = element_blank()) +
  labs(caption = "Data provided by StatsBomb (https://statsbomb.com/)")

