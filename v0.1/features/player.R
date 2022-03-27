
## INITIAL LIBRARY PACKAGES
library("fitzRoy")
library("tidyverse")
library("here")

## LOAD DATA
load(here::here("data", "ps.Rda"))


player_stats <- ps %>% select(Season, Round, Date, group_id, ID, First.name, Surname, Playing.for, 
                              Kicks, Marks, Handballs, Goals, Behinds, 
                              Hit.Outs, Tackles, Rebounds, Inside.50s,
                              Clearances, Clangers, Frees.For, Frees.Against,      
                              Brownlow.Votes, Contested.Possessions, Uncontested.Possessions, Contested.Marks,
                              Marks.Inside.50, One.Percenters, Bounces, Goal.Assists,
                              Time.on.Ground..) %>%
                       rename(match.id = group_id, player.id = ID) %>% 
                       group_by(player.id) %>% 
                       mutate(cGames = seq_along(Date),
                              cGoals = cumsum(Goals),
                              cDisposals = cumsum(Kicks + Handballs),
                              cGoalassists = cumsum(Goal.Assists),
                              cScoringshots = cumsum(Goals + Behinds)) %>%
                       select(Season, Round, Date, match.id, player.id, cGames, cGoals, cScoringshots, cGoalassists, cDisposals)




