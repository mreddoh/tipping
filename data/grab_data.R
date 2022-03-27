## GRAB DATA
library(here)
library(tidyverse)
library(fitzRoy)

gms <- fetch_results(season = 1990:2022, comp = "AFLM", source = "afltables")
save(gms,file=here::here("data", "gms.Rda"))
     
ps <- fetch_player_stats(season = 1990:2022, comp = "AFLM", source = "afltables")
save(ps,file=here::here("data", "ps.Rda"))


## GET TIPSTER DATA
tipsters <- fetch_squiggle_data("tips")
save(tipsters,file = here::here("data", "tipsters.Rda"))


w <- results_weather %>% filter(Rainfall>=0)

#let's look at the weather data, when is it from?
w %>% group_by(substr(date,1,4)) %>% summarise(count=n())
#all in 2017.
#might leave that data there for the minute...