# MAKE SCRIPT TO MAKE TIPS

rrr = 23

## LOAD PACKAGES
library("fitzRoy")
library("tidyverse")
library("here")
library("lubridate")

## LOAD DATA
gms <- get_match_results() %>% filter(substr(Date,1,4) >= 1990)


game.lookup <- gms %>% 
               select(Game, Date, Round.Number, Home.Team, Away.Team, Venue, Home.Team, Home.Points, Away.Team, Away.Points, Home.Goals, Home.Behinds, Away.Goals, Away.Behinds) %>%
               mutate(Season = lubridate::year(Date)) %>%
               rename(game.id = Game, Round = Round.Number) %>%
               mutate(Home.Team.city = case_when(Home.Team %in% c("Adelaide","Port Adelaide") ~ "Adelaide",
                                                 Home.Team %in% c("Geelong") ~ "Geelong",
                                                 Home.Team %in% c("GWS","Sydney") ~ "Sydney",
                                                 Home.Team %in% c("Brisbane Lions","Gold Coast") ~ "Brisbane",
                                                 Home.Team %in% c("West Coast","Fremantle") ~ "Perth",
                                                 TRUE ~ "Melbourne")) %>%
               mutate(Away.Team.city = case_when(Away.Team %in% c("Adelaide","Port Adelaide") ~ "Adelaide",
                                                 Away.Team %in% c("Geelong") ~ "Geelong",
                                                 Away.Team %in% c("GWS","Sydney") ~ "Sydney",
                                                 Away.Team %in% c("Brisbane Lions","Gold Coast") ~ "Brisbane",
                                                 Away.Team %in% c("West Coast","Fremantle") ~ "Perth",
                                                 TRUE ~ "Melbourne")) %>%
               mutate(city = case_when(Venue %in% c("Adelaide Oval","Football Park") ~ "Adelaide",
                                       Venue %in% c("Kardinia Park") ~ "Geelong",
                                       Venue %in% c("S.C.G.","Stadium Australia","Sydney Showground","Blacktown") ~ "Sydney",
                                       Venue %in% c("Gabba","Cazaly's Stadium","Carrara") ~ "Brisbane",
                                       Venue %in% c("W.A.C.A.","Subiaco","Perth Stadium") ~ "Perth",
                                       Venue %in% c("Bellerive Oval","North Hobart","York Park") ~ "Tasmania",
                                       Venue %in% c("Bruce Stadium","Manuka Oval") ~ "Canberra",
                                       Venue %in% c("Wellington","Jiangwan Stadium") ~ "International",
                                       Venue %in% c("Marrara Oval","Traeger Park") ~ "Darwin",
                                       TRUE ~ "Melbourne")) %>%
               mutate(HomeAdv.flag = ifelse(Home.Team.city == city & Away.Team.city != city, 1, 0)) %>%
               select(-c(Home.Team.city,Away.Team.city,city))


## Create a dataset with a rating for each team
all.teams <- convert_results(gms) %>% select(Team) %>% unique() %>% t()
ratings <- as.list(rep(1500, times=length(all.teams)))
names(ratings) <- all.teams

## Add adjustment for GC and GWS
ratings[c("GWS","Gold Coast")] <- 1100

gms_s <- game.lookup %>% subset(year(Date)>=2003) %>% 
  select(game.id, Date, Home.Team, Home.Points, Away.Team, Away.Points, Home.Goals, Home.Behinds, Away.Goals, Away.Behinds, HomeAdv.flag) %>%
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
  gms_s[["resultHome"]][idx] <- resultHome
  gms_s[["resultAway"]][idx] <- resultAway
  gms_s[["preRatingHome"]][idx] <- as.numeric(ratings[homeTeamName])
  gms_s[["preRatingAway"]][idx] <- as.numeric(ratings[awayTeamName])
  gms_s[["changeHome"]][idx] <- kfactor*(resultHome - expectedHome)
  gms_s[["changeAway"]][idx] <- kfactor*(resultAway - expectedAway)
  
}

hm_tmp <- gms_s %>% select(game.id,Date,Home.Team,preELOhome,postELOhome,preELOaway,postELOaway,expectedHome,changeHome) %>% 
                    rename(Team = Home.Team, 
                           preELO = preELOhome, 
                           postELO = postELOhome, 
                           preELOopp = preELOaway, 
                           postELOopp = postELOaway,
                           expectedResult = expectedHome,
                           ELOchange = changeHome)

wy_tmp <- gms_s %>% select(game.id,Date,Away.Team,preELOaway,postELOaway,preELOhome,postELOhome,expectedAway,changeAway) %>% 
                    rename(Team = Away.Team, 
                           preELO = preELOaway, 
                           postELO = postELOaway, 
                           preELOopp = preELOhome, 
                           postELOopp = postELOhome,
                           expectedResult = expectedAway,
                           ELOchange = changeAway)

gameslongform.elo <- rbind.data.frame(hm_tmp,wy_tmp) %>% mutate( preELOdiff = (preELO-preELOopp)/preELOopp )


latest_elo <- gameslongform.elo %>% 
              group_by(Team) %>%
              filter(game.id == max(game.id)) %>%
              select(Team, postELO)


this_weeks_games <- get_fixture(season = lubridate::year(Sys.Date())) %>%
                    filter(Round == rrr) %>% 
                    mutate(Home.Team.city = case_when(Home.Team %in% c("Adelaide","Port Adelaide") ~ "Adelaide",
                                                      Home.Team %in% c("Geelong") ~ "Geelong",
                                                      Home.Team %in% c("GWS","Sydney") ~ "Sydney",
                                                      Home.Team %in% c("Brisbane Lions","Gold Coast") ~ "Brisbane",
                                                      Home.Team %in% c("West Coast","Fremantle") ~ "Perth",
                                                      TRUE ~ "Melbourne")) %>%
                    mutate(Away.Team.city = case_when(Away.Team %in% c("Adelaide","Port Adelaide") ~ "Adelaide",
                                                      Away.Team %in% c("Geelong") ~ "Geelong",
                                                      Away.Team %in% c("GWS","Sydney") ~ "Sydney",
                                                      Away.Team %in% c("Brisbane Lions","Gold Coast") ~ "Brisbane",
                                                      Away.Team %in% c("West Coast","Fremantle") ~ "Perth",
                                                      TRUE ~ "Melbourne")) %>%
                    mutate(city = case_when(Venue %in% c("Adelaide Oval","Football Park") ~ "Adelaide",
                                            Venue %in% c("Kardinia Park") ~ "Geelong",
                                            Venue %in% c("S.C.G.","Stadium Australia","Sydney Showground","Blacktown") ~ "Sydney",
                                            Venue %in% c("Gabba","Cazaly's Stadium","Carrara") ~ "Brisbane",
                                            Venue %in% c("W.A.C.A.","Subiaco","Perth Stadium") ~ "Perth",
                                            Venue %in% c("Bellerive Oval","North Hobart","York Park") ~ "Tasmania",
                                            Venue %in% c("Bruce Stadium","Manuka Oval") ~ "Canberra",
                                            Venue %in% c("Wellington","Jiangwan Stadium") ~ "International",
                                            Venue %in% c("Marrara Oval","Traeger Park") ~ "Darwin",
                                            TRUE ~ "Melbourne")) %>%
                    mutate(HomeAdv.flag = ifelse(Home.Team.city == city & Away.Team.city != city, 1, 0)) %>%
                    merge(.,latest_elo,by.x="Home.Team",by.y="Team") %>% mutate(.,homeELO = ifelse(HomeAdv.flag==1, postELO+110, postELO)) %>%
                    merge(.,latest_elo,by.x="Away.Team",by.y="Team") %>% rename(.,awayELO=postELO.y) %>%
                    select(Date,Season,Round,Home.Team,Away.Team,Venue,homeELO,awayELO) %>%
                    mutate(.,tip = ifelse(homeELO<awayELO,Away.Team,Home.Team))


weekly_changes <- gameslongform.elo %>% 
                  group_by(Team) %>%
                  filter(game.id == max(game.id)) %>%
                  select(Team, postELO, ELOchange)

remove(hm_tmp, wy_tmp, gms, gms_s, all.teams, ratings, latest_elo, game.lookup)

