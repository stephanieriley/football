##################################################
###        Passes         ###
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
player<- "Leah Williamson"
matchpass<- data.frame()
for(i in 1:nrow(matches)) {
  matchpass<- rbind(matchpass, 
                    get.matchFree(matches[i,]) %>%
                      filter(type.name == "Pass") %>%
                      select(match_id, id:second, related_events, location, team.name, player.name, position.name, pass.length, pass.angle, pass.end_location, pass.recipient.name, pass.height.name, pass.body_part.name))

}

maxpass<- matchpass %>%
  group_by(player.name) %>%
  mutate(meanpass=mean(pass.length)) %>%
  ungroup() %>%
  select(player.name, team.name, meanpass) %>%
  distinct_all() %>%
  slice_max(meanpass, n=10)%>%
  mutate(rank = dense_rank(desc(meanpass)),
         ypos = (70*rank/max(rank))) #Pitch width is 80
#Need to change y pos so that arrows are evenly split up the width of the pitch


pitch_full() +
  geom_segment(data = maxpass, aes(x=0, xend=meanpass, y=ypos, yend=ypos, col=team.name), size = 2,
               arrow = arrow(length = unit(0.5, "cm")))
