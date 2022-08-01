###########################################
###         WSL 2021/2022 results       ###
###########################################

#Packages
library(tidyverse)
library(stringr)
library(jpeg)
library(grid)

#Read in results (from Wikipedia)
data<- read.delim("WSL/data/results2122.txt", sep=",")
#Club badge urls
badge<- data.frame(team = unique(data$Home),
                   logourl = c("https://womenscompetitions.thefa.com/-/media/project/femaleengagement/clubslogo/wsl/manchester-united-womens-football-team-crest.ashx",
                               "https://womenscompetitions.thefa.com/-/media/project/femaleengagement/clubslogo/wc/aston-villa-womens-football-team-crest.ashx",
                               "https://womenscompetitions.thefa.com/-/media/project/femaleengagement/clubslogo/wsl/everton-womens-football-team-crest.ashx",
                               "https://www.logolynx.com/images/logolynx/65/65948b3e57ffbd846dbd16188e72392c.jpeg", #Tottenham
                               "https://womenscompetitions.thefa.com/-/media/project/femaleengagement/clubslogo/wsl/arsenal-womens-football-team-crest.ashx",
                               "https://womenscompetitions.thefa.com/-/media/project/femaleengagement/clubslogo/wsl/brighton-womens-football-team-crest.ashx",
                               "https://womenscompetitions.thefa.com/-/media/project/femaleengagement/clubslogo/wsl/west-ham-womens-football-team-crest.ashx",
                               "https://womenscompetitions.thefa.com/-/media/project/femaleengagement/clubslogo/wsl/chelsea-womens-football-team-crest.ashx",
                               "https://yt3.ggpht.com/ytc/AKedOLQkKfgnvpjbQA8o78DVwfDpayfYKn5OH0rbnN8K=s900-c-k-c0x00ffffff-no-rj", #Reading
                               "https://womenscompetitions.thefa.com/-/media/project/femaleengagement/clubslogo/wsl/birmingham-womens-football-team-crest.ashx",
                               "https://womenscompetitions.thefa.com/-/media/project/femaleengagement/clubslogo/wc/leicester-womens-football-team-crest.ashx",
                               "https://womenscompetitions.thefa.com/-/media/project/femaleengagement/clubslogo/wsl/manchester-city-womens-football-team-crest.ashx"))

#Tidy data
dat<- inner_join(data, badge, by = c("Home"="team")) %>% #Add logo URL
  rename(homelogo = logourl) %>%
  inner_join(badge, by = c("Away"="team")) %>% 
  rename(awaylogo = logourl) %>%
  mutate(
    #Scores have â€“ icon separating them, so replce with "-"
    Score = str_replace_all(Score, "â\200“", "-"), #Separate score
         #Pull the first value to get the homescore
         homescore = as.numeric(stringr::str_sub(Score, 1, 1)),
         #Pull the final value to get the away score
         awayscore = as.numeric(stringr::str_sub(Score, -1, -1)),
         #Create factor to see if the home team won, lost or drew
         result = as.factor(case_when(homescore > awayscore ~ "homewin",
                                      homescore < awayscore ~ "homelose",
                                      homescore == awayscore ~ "draw")))

#Plot all scores
p<- ggplot(dat, aes(x=Away, y=fct_reorder(Home, desc(Home)))) +
  geom_tile(aes(fill=result)) +
  geom_text(aes(label=Score)) +
  scale_x_discrete(position = "top") +
  scale_fill_manual(values=c(homewin="#D9F8C4", homelose="#F37878", draw="#F9F9C5"),
                    labels = c("Home win", "Home lose", "Draw")) +
  theme_classic() +
  theme(plot.background = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  labs(y = "Home")

#Extract the coords of the diagonal (/)
b<- ggplot_build(p)
diag<- filter(b$data[[1]], x==y)
# filter(b$data[[1]], x==max(x)+1-x)

#Want the -1 gradient diagonal (going this way \ not this way /)
xmin<- sort(diag$xmin)
xmax<- sort(diag$xmax)
ymin<- sort(diag$ymin, decreasing = T)
ymax<- sort(diag$ymax, decreasing = T)

#Collect the logo urls into one data frame
datlogo<- arrange(dat, Home) %>%
  select(homelogo) %>%
  distinct(homelogo)

#Sequentially add the club badges to the diagonal
for (i in seq_len(nrow(datlogo))) {
  z <- tempfile()
  print("tempfile created")
  download.file(datlogo[i,1],z,mode="wb")
  print("download file complete")
  pic <- readJPEG(z)
  print("readjpeg complete")
  logogrob<- rasterGrob(pic)
  print("logo grob")
  p<- p + annotation_custom(logogrob, xmin = xmin[i], xmax = xmax[i], 
                            ymin = ymin[i], ymax = ymax[i])
  
  file.remove(z)
  print("file removed")
}

#Print plot
p

