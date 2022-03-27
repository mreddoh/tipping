
## INITIAL LIBRARY PACKAGES
library("fitzRoy")
library("tidyverse")
library("here")
library("lubridate")

## LOAD DATA
load(here::here("data", "ps.Rda"))
load(here::here("data", "gms.Rda"))
load(here::here("data", "game_lookup.Rda"))


## Create variables that look at defence, midfield and offence strength based on i50s and scores.

home_stats <- ps %>% filter(Home.team==Playing.for, year(Date)>=1998) %>%
                     rename(team = Playing.for) %>%
                     group_by(Date, Venue, team) %>%
                     summarise(SS_home = max(HQ4G+HQ4B), I50_home = sum(Inside.50s)) %>%
                     select(Date, Venue, team, SS_home, I50_home)
                      
away_stats <- ps %>% filter(Away.team==Playing.for, year(Date)>=1998) %>%
                     rename(team = Playing.for) %>%
                     group_by(Date, Venue, team) %>%
                     summarise(SS_away = max(AQ4G+AQ4B), I50_away = sum(Inside.50s)) %>%
                     select(Date, Venue, team, SS_away, I50_away)

team_stats <- merge(home_stats,away_stats,by=c("Date","Venue")) %>%
              mutate(FWDrating.home = SS_home / I50_home,
                     MIDrating.home = I50_home / I50_away,
                     BCKrating.home = SS_away / I50_away,
                     FWDrating.away = SS_away / I50_away,
                     MIDrating.away = I50_away / I50_home,
                     BCKrating.away = SS_home / I50_home)

## TRY ELO FORMAT BUT WITH EACH SECTION
### I.E. ELO FWD LINE, WIN/LOSS BASED ON WINNING INSIDE 50 COUNT
### DO THIS FOR EACH SECTION AND THEN RUN REGRESSION ON GAME RATING DIFFERENCES
### TO FIND THE PROPORTION GIVEN TO EACH SECTION LINE.
### THEN I'LL NEED TO LOOK AT WHETHER THIS DIFFERS BY TEAM...

home.ratings <- team_stats %>% 
                select(Date,Venue,team.x,FWDrating.home,MIDrating.home,BCKrating.home) %>% 
                rename(Team=team.x,
                       FWDrating=FWDrating.home,
                       MIDrating=MIDrating.home,
                       BCKrating=BCKrating.home)

away.ratings <- team_stats %>% 
                select(Date,Venue,team.y,FWDrating.away,MIDrating.away,BCKrating.away) %>% 
                rename(Team=team.y,
                       FWDrating=FWDrating.away,
                       MIDrating=MIDrating.away,
                       BCKrating=BCKrating.away)

ratings <- rbind(home.ratings,away.ratings)


## SET INITIAL PARAMETERS
t = 20 # max length of historical variables

## TRANSFORM DATA
gms_tmp <- ratings %>% 
  select(Date, Team, FWDrating, MIDrating, BCKrating) %>%
  group_by(Team) %>% 
  mutate(team_game_id = seq_along(Date)) %>%
  sapply(rep.int, times=t) %>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  arrange(Date,Team) %>%
  mutate(game.shift = rep(1:t,dim(ratings)[1]),
         join.id = as.numeric(team_game_id) - game.shift,
         FWDrating = as.numeric(FWDrating),
         MIDrating = as.numeric(MIDrating),
         BCKrating = as.numeric(BCKrating)) %>%
  filter(join.id > 0)


previous_games <- gms_tmp %>% 
  select(Team, team_game_id, FWDrating, MIDrating, BCKrating) %>% 
  unique() %>%
  merge(gms_tmp, ., by.x=c("Team", "join.id"), by.y=c("Team", "team_game_id")) %>% 
  select(Team, Date, game.shift, FWDrating.y, MIDrating.y, BCKrating.y)

POSratings <- previous_games %>%
              group_by(Team, Date) %>%
              summarise(avg_FWDrating_p5 = mean(FWDrating.y[game.shift <= 5]),
                        avg_FWDrating_p10 = mean(FWDrating.y[game.shift <= 10]),
                        avg_FWDrating_p15 = mean(FWDrating.y[game.shift <= 15]),
                        avg_FWDrating_p20 = mean(FWDrating.y[game.shift <= 20]),
                        avg_BCKrating_p5 = mean(BCKrating.y[game.shift <= 5]),
                        avg_BCKrating_p10 = mean(BCKrating.y[game.shift <= 10]),
                        avg_BCKrating_p15 = mean(BCKrating.y[game.shift <= 15]),
                        avg_BCKrating_p20 = mean(BCKrating.y[game.shift <= 20]),
                        avg_MIDrating_p5 = mean(MIDrating.y[game.shift <= 5]),
                        avg_MIDrating_p10 = mean(MIDrating.y[game.shift <= 10]),
                        avg_MIDrating_p15 = mean(MIDrating.y[game.shift <= 15]),
                        avg_MIDrating_p20 = mean(MIDrating.y[game.shift <= 20]),
                        n = n()
                        ) %>%
                        filter(n==20) %>% 
                        mutate(Date = as.Date(as.numeric(Date), origin = "1970-01-01")) %>% 
                        select(-n)
  

## just have to look at differentials
opponentJoined_POSratings <- game.lookup %>%
                             select(Game,Date,Home.Team,Away.Team,Venue) %>% 
                             merge(.,POSratings,by.x=c("Home.Team","Date"),by.y=c("Team","Date")) %>% 
                             merge(.,POSratings,by.x=c("Away.Team","Date"),by.y=c("Team","Date"))


names(opponentJoined_POSratings) <- names(opponentJoined_POSratings) %>% 
                             gsub(".x",".home",.,fixed = TRUE) %>% 
                             gsub(".y",".away",.,fixed = TRUE)

home_diff <- select(opponentJoined_POSratings,ends_with(".home")) - select(opponentJoined_POSratings,ends_with(".away"))
home_diff <- opponentJoined_POSratings %>% select(Game,Home.Team) %>% cbind(.,home_diff)
names(home_diff) <- names(home_diff) %>% gsub(".home",".diff",.,fixed = TRUE) %>% gsub("Home.","",.,fixed = TRUE)

away_diff <- select(opponentJoined_POSratings,ends_with(".away")) - select(opponentJoined_POSratings,ends_with(".home"))
away_diff <- opponentJoined_POSratings %>% select(Game,Away.Team) %>% cbind(.,away_diff)
names(away_diff) <- names(away_diff) %>% gsub(".away",".diff",.,fixed = TRUE) %>% gsub("Away.","",.,fixed = TRUE)

gameslongform.fundamentals <- rbind(home_diff,away_diff)
        

remove(ps, gms_tmp, previous_games, team_stats)
remove(away_diff,home_diff,opponentJoined_POSratings)
remove(away_stats,away.ratings,home_stats,home.ratings,ratings,POSratings,t)
