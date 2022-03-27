
## LOAD PACKAGES
library("fitzRoy")
library("tidyverse")
library("here")

## LOAD DATA
load(here::here("data", "gms.Rda"))
load(here::here("data", "game_lookup.Rda"))

## Create a dataset with a rating for each team
all.teams <- convert_results(gms) %>% select(Team) %>% unique() %>% t()
ratings <- as.list(rep(1500, times=length(all.teams)))
names(ratings) <- all.teams

## Add adjustment for GC and GWS
ratings[c("GWS","Gold Coast")] <- 1100

gms_s <- game.lookup %>% subset(year(Date)>=2003) %>% 
  select(Game, Date, Home.Team, Home.Points, Away.Team, Away.Points, Home.Goals, Home.Behinds, Away.Goals, Away.Behinds, HomeAdv.flag, Margin) %>%
  mutate(result.home = ifelse(Home.Points>Away.Points,1,ifelse(Home.Points==Away.Points,0.5,0)),
         result.away = ifelse(Home.Points<Away.Points,1,ifelse(Home.Points==Away.Points,0.5,0)),
         Home.SS = Home.Goals+Home.Behinds,
         Away.SS = Away.Goals+Away.Behinds,
         ss.result.home = ifelse(Home.SS>Away.SS,1,ifelse(Home.SS==Away.SS,0.5,0)),
         ss.result.away = ifelse(Home.SS<Away.SS,1,ifelse(Home.SS==Away.SS,0.5,0)),
         preELOhome = NA, 
         preELOaway = NA,
         postELOhome = NA,
         postELOaway = NA,
         expectedHome = NA,
         expectedAway = NA)

kfactor=53

for (idx in 1:dim(gms_s)[1]){
  
  homeTeamName <- gms_s[["Home.Team"]][idx]
  awayTeamName <- gms_s[["Away.Team"]][idx]
  
  homeAdvantage <- 110 * gms_s[["HomeAdv.flag"]][idx]
  
  homeTeamRating <- as.numeric(ratings[homeTeamName]) + homeAdvantage
  awayTeamRating <- as.numeric(ratings[awayTeamName]) 
  expectedHome <- 1 / (1 + 10^((awayTeamRating - homeTeamRating)/400))
  expectedAway <- 1 - expectedHome
  
  resultHome <- ( gms_s[["result.home"]][idx] * 0.60 ) + ( gms_s[["ss.result.home"]][idx] * 0.40 )
  resultAway <- ( gms_s[["result.away"]][idx] * 0.60 ) + ( gms_s[["ss.result.away"]][idx] * 0.40 )
  
  ratings[homeTeamName] <- as.numeric(ratings[homeTeamName]) + kfactor*(resultHome - expectedHome)
  ratings[awayTeamName] <- as.numeric(ratings[awayTeamName]) + kfactor*(resultAway - expectedAway)
  
  gms_s[["preELOhome"]][idx] <- homeTeamRating
  gms_s[["preELOaway"]][idx] <- awayTeamRating
  gms_s[["postELOhome"]][idx] <- as.numeric(ratings[homeTeamName]) + kfactor*(resultHome - expectedHome)
  gms_s[["postELOaway"]][idx] <- as.numeric(ratings[awayTeamName]) + kfactor*(resultAway - expectedAway)
  gms_s[["expectedHome"]][idx] <- expectedHome
  gms_s[["expectedAway"]][idx] <- expectedAway
  
}

hm_tmp <- gms_s %>% select(Game,Home.Team,preELOhome,postELOhome,preELOaway,postELOaway,expectedHome) %>% 
                    rename(Team = Home.Team, 
                           preELO = preELOhome, 
                           postELO = postELOhome, 
                           preELOopp = preELOaway, 
                           postELOopp = postELOaway,
                           expectedResult = expectedHome)

wy_tmp <- gms_s %>% select(Game,Away.Team,preELOaway,postELOaway,preELOhome,postELOhome,expectedAway) %>% 
                    rename(Team = Away.Team, 
                           preELO = preELOaway, 
                           postELO = postELOaway, 
                           preELOopp = preELOhome, 
                           postELOopp = postELOhome,
                           expectedResult = expectedAway)

gameslongform.elo <- rbind.data.frame(hm_tmp,wy_tmp) %>% mutate( preELOdiff = (preELO-preELOopp)/preELOopp )


games.elo <- gms_s %>% mutate(outcome = ifelse(Margin>0,1,ifelse(Margin==0,0.5,0)))

remove(hm_tmp, wy_tmp, gms_s, all.teams, ratings)


games.elo %>% filter(year(Date)==2012) %>%
                      mutate(expectedResult = ifelse(expectedHome > 0.5,1,0)) %>%
                      mutate(result = expectedResult == outcome) %>%
                      summarise(mean = mean(result))


