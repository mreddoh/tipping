
## INITIAL LIBRARY PACKAGES
library("fitzRoy")
library("tidyverse")
library("here")
library("lubridate")

## LOAD DATA
load(here::here("data", "ps.Rda"))


home_stats <- ps %>% filter(Home.team==Playing.for, year(Date)>=1998) %>%
  rename(team = Playing.for) %>%
  group_by(Date, Venue, team) %>%
  summarise(I50_home = sum(Inside.50s),
            Tackles_home = sum(Tackles),
            Disposals_home = sum(Kicks+Handballs),
            Clangers_home = sum(Clangers)) %>%
  select(Date, Venue, team, I50_home, Tackles_home, Disposals_home, Clangers_home)

away_stats <- ps %>% filter(Away.team==Playing.for, year(Date)>=1998) %>%
  rename(team = Playing.for) %>%
  group_by(Date, Venue, team) %>%
  summarise(I50_away = sum(Inside.50s),
            Tackles_away = sum(Tackles),
            Disposals_away = sum(Kicks+Handballs),
            Clangers_away = sum(Clangers)) %>%
  select(Date, Venue, team, I50_away, Tackles_away, Disposals_away, Clangers_away)

team_stats <- merge(home_stats,away_stats,by=c("Date","Venue")) %>% 
              mutate(I50_diff.home = I50_home - I50_away,
                     Tackles_diff.home = Tackles_home - Tackles_away,
                     Disposals_diff.home = Disposals_home - Disposals_away,
                     Clangers_diff.home = Clangers_home - Clangers_away,
                     TackleRate_diff.home = ( Tackles_home / Disposals_away ) - ( Tackles_away / Disposals_home ),
                     I50_diff.away = I50_away - I50_home,
                     Tackles_diff.away = Tackles_away - Tackles_home,
                     Disposals_diff.away = Disposals_away - Disposals_home,
                     Clangers_diff.away = Clangers_away - Clangers_home,
                     TackleRate_diff.away = ( Tackles_away / Disposals_home ) - ( Tackles_home / Disposals_away ))



