
## INITIAL LIBRARY PACKAGES
library("fitzRoy")
library("tidyverse")
library("here")

## LOAD DATA
load(here::here("data", "ps.Rda"))


## Create variables that look at defence, midfield and offence strength based on i50s and scores.

home_stats <- ps %>% filter(Home.team==Playing.for) %>%
                     rename(team = Playing.for) %>%
                     group_by(Date, Venue, team) %>%
                     summarise(SS_home = max(HQ4G+HQ4B), I50_home = sum(Inside.50s)) %>%
                     select(Date, Venue, team, SS_home, I50_home)
                      
away_stats <- ps %>% filter(Away.team==Playing.for) %>%
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


