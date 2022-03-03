#Using data provided by Statsbomb

#Read in JSON file
url<- "https://raw.githubusercontent.com/statsbomb/open-data/master/data/matches/37/42.json"
data<- jsonlite::read_json(url)

