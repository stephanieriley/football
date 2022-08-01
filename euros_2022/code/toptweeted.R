###########################################
###         Top tweeted players         ###
###              #WEURO2022             ###
###########################################

#Packages
library(tidyverse)
library(tidytext)
library(showtext)
library(ggwordcloud)
library(png)
library(grid)


#Read in data
dat<- readRDS("../TwitterAPI/FootballAnalysis/final_tweets.RDS")

#Read in players
playerstats<- read.csv("euros_2022/data/playerstats.csv", header = T) 
players_squad<- playerstats %>%
  mutate(surname = tolower(str_replace(Player, "^\\S* ", "")),
         country = tolower(str_replace(Squad, "^\\S* ", ""))) %>%
  filter(country %in% c("england", "germany"))
players<- players_squad$surname


#Remove all links
dat$updated_text<- gsub("https.*", "", dat$text)
dat$updated_text<- gsub("http.*", "", dat$updated_text)

#Read in the stop words
data("stop_words")

#Keep only tweets from this weekend
weekend<- seq(as.Date("2022-07-30"), as.Date("2022-08-01"), by="days")
dat_weekend<- dat %>%
  mutate(date = as.Date(created_at)) %>%
  filter(date %in% weekend)


#Count the number of times each player has been tweeted
player_count<- dat_weekend %>%
  dplyr::select(updated_text) %>%
  unnest_tokens(output = word,
                input = updated_text) %>% #Puts each word in the tweet to a new row
  anti_join(stop_words) %>% #Remove stop words
  #Keep only terms where player is mentioned
  filter(grepl(paste(players, collapse = "|"), word)) %>%
  #Sometimes usernames have been used (e.g. bmeado9) so want to extract exactly which player that is
  mutate(player = toupper(str_extract(word, paste(players, collapse = "|")))) %>%
  count(player, sort = T)


#Read in colour palettes
source("euros_2022/code/palette.R")

#Font
#Register font
font_add(family="euro2022", regular="euros_2022/UEFAEuro-Book.ttf")
#Load font
showtext_auto()

#Prepare logo for insertion to plot
logo<- readPNG("euros_2022/data/UEFA_Women's_Euro_2022_logo.png", native = T)
logogrob<- rasterGrob(logo)

#PLOT
ggplot(player_count, aes(label = player, size = n, color = n)) +
  geom_text_wordcloud(family = "euro2022",
                      shape = "triangle-upright") +
  #geom_text_wordcloud_area() +
  scale_size_area(max_size = 25) +
  scale_colour_gradientn(colours = cols7) + 
  theme_minimal() +
  theme(plot.background = element_rect(color = backcol, fill = backcol),
        panel.background = element_rect(color = backcol, fill = backcol),
        title = element_text(colour = "#ffffff"),
        plot.caption = element_text(hjust=0.5, size=54),
        panel.spacing=unit(c(0,0,0,0), "cm"),
        plot.margin=unit(c(0,0,0,0), "cm")) +
  annotation_custom(logogrob, xmin=0.43, xmax=0.57, ymin=-0.85) +
  labs(caption = "#WEURO2022") 

