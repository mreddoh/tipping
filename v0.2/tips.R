# MAKE SCRIPT TO MAKE TIPS

rrr = 4

## LOAD PACKAGES ----
library(fitzRoy)
library(tidyverse)
library(here)
library(lubridate)
library(reshape2)

## LOAD DATA ----
gms <- fetch_results(season = 2003:2022, comp = "AFLM", source = "afltables")

ps <- fetch_player_stats(season = 2019:2022, comp = "AFLM", source = "afltables")

game.lookup <- gms %>% 
  select(Game, Date, Round.Number, Home.Team, Away.Team, Venue, Home.Team, Home.Points, Away.Team, Away.Points, Home.Goals, Home.Behinds, Away.Goals, Away.Behinds) %>%
  mutate(Season = year(Date)) %>%
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
  mutate(HomeAdv.flag = ifelse(Home.Team.city == city & Away.Team.city != city, 1, 0),
         Margin = Home.Points - Away.Points) %>%
  select(-c(Home.Team.city,Away.Team.city,city))


## CREATE ELO VARIABLES ----
source(here::here("v0.2", "elo", "elo.R"), echo = TRUE)

latest_elo <- gameslongform.elo %>% 
              group_by(Team) %>%
              filter(game.id == max(game.id)) %>%
              select(Team, postELO)

this_weeks_games <- fetch_fixture_footywire(season = lubridate::year(Sys.Date())) %>%
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
                    mutate(HomeAdv.flag = ifelse(Home.Team.city == city & Away.Team.city != city, 1, 0)) 

this_weeks_games.ELO <- this_weeks_games %>%
                    merge(.,latest_elo,by.x="Home.Team",by.y="Team") %>% mutate(.,homeELO = ifelse(HomeAdv.flag==1, postELO+110, postELO)) %>%
                    merge(.,latest_elo,by.x="Away.Team",by.y="Team") %>% rename(.,awayELO=postELO.y) %>%
                    mutate(tip = ifelse(homeELO<awayELO,Away.Team,Home.Team),
                           expectedResult = 1 / (1 + 10^((awayELO - homeELO)/400))) %>% 
                    select(Date,Season,Round,Home.Team,Away.Team,expectedResult)


## CREATE OTHER VARIABLES ----
### form.5.diff_f
form_temp <- gms %>%
  pivot_longer(cols = c(Home.Team,Away.Team), values_to = "Team") %>% 
  mutate(Score.for = ifelse(name == "Home.Team", Home.Points, Away.Points),
         Score.against = ifelse(name == "Home.Team", Away.Points, Home.Points),
         Margin = ifelse(name == "Home.Team", Margin, Margin*-1)) %>% 
  select(Date, Team, Margin) %>% 
  group_by(Team) %>% 
  arrange(desc(Date)) %>% 
  mutate(id = row_number()) %>% 
  filter(id <= 5) %>% 
  summarise(form.5 = mean(Margin))
  
this_weeks_games.form <- left_join(this_weeks_games,form_temp,by=c("Home.Team" = "Team")) %>% rename(form.5.home = form.5) %>% 
  left_join(.,form_temp,by=c("Away.Team" = "Team")) %>% rename(form.5.away = form.5) %>% 
  mutate(form.5.diff = form.5.home - form.5.away) %>%
  mutate(form.5.diff_f = case_when(form.5.diff <= -85 ~ -85, form.5.diff >= 85 ~ 85, TRUE ~ form.5.diff)) %>% 
  select(Date,Season,Round,Home.Team,Away.Team,form.5.diff_f)


### avg_FWDrating_p10.diff_f
### avg_MIDrating_p10.diff_f
### avg_BCKrating_p10.diff_f

home_stats <- ps %>% filter(Home.team==Playing.for, year(Date)>=1998) %>%
  rename(team = Playing.for) %>%
  group_by(Date, Local.start.time, Venue, team) %>%
  summarise(SS_home = max(HQ4G+HQ4B), I50_home = sum(Inside.50s)) %>%
  select(Date, Venue, Local.start.time, team, SS_home, I50_home)

away_stats <- ps %>% filter(Away.team==Playing.for, year(Date)>=1998) %>%
  rename(team = Playing.for) %>%
  group_by(Date, Local.start.time, Venue, team) %>%
  summarise(SS_away = max(AQ4G+AQ4B), I50_away = sum(Inside.50s)) %>%
  select(Date, Venue, Local.start.time, team, SS_away, I50_away)

fundamentals_temp <- merge(home_stats,away_stats,by=c("Date","Venue","Local.start.time")) %>%
  mutate(FWDrating.home = SS_home / I50_home,
         MIDrating.home = I50_home / I50_away,
         BCKrating.home = SS_away / I50_away,
         FWDrating.away = SS_away / I50_away,
         MIDrating.away = I50_away / I50_home,
         BCKrating.away = SS_home / I50_home) %>% 
  pivot_longer(cols = c(team.x,team.y), values_to = "Team") %>% 
  mutate(name = ifelse(name=="team.x","Home.Team","Away.Team"),
         Team = replace_teams(Team)) %>% 
  mutate(FWDrating = ifelse(name == "Home.Team", FWDrating.home, FWDrating.away),
         MIDrating = ifelse(name == "Home.Team", MIDrating.home, MIDrating.away),
         BCKrating = ifelse(name == "Home.Team", BCKrating.home, BCKrating.away)) %>% 
  select(Date, Team, BCKrating, MIDrating, FWDrating) %>% 
  group_by(Team) %>% 
  arrange(desc(Date)) %>% 
  mutate(id = row_number()) %>% 
  filter(id <= 10) %>% 
  summarise(avg_FWDrating_p10 = mean(FWDrating),
            avg_MIDrating_p10 = mean(MIDrating),
            avg_BCKrating_p10 = mean(BCKrating))
  
this_weeks_games.fundamentals <- left_join(this_weeks_games,fundamentals_temp,by=c("Home.Team" = "Team")) %>% 
    rename(avg_FWDrating_p10.home = avg_FWDrating_p10,
           avg_MIDrating_p10.home = avg_MIDrating_p10,
           avg_BCKrating_p10.home = avg_BCKrating_p10) %>% 
  left_join(.,fundamentals_temp,by=c("Away.Team" = "Team")) %>% 
    rename(avg_FWDrating_p10.away = avg_FWDrating_p10,
           avg_MIDrating_p10.away = avg_MIDrating_p10,
           avg_BCKrating_p10.away = avg_BCKrating_p10) %>% 
  mutate(avg_BCKrating_p10.diff = avg_BCKrating_p10.home - avg_BCKrating_p10.away,
         avg_MIDrating_p10.diff = avg_MIDrating_p10.home - avg_MIDrating_p10.away,
         avg_FWDrating_p10.diff = avg_FWDrating_p10.home - avg_FWDrating_p10.away) %>%
  mutate(avg_FWDrating_p10.diff_f = case_when(avg_FWDrating_p10.diff <= -0.5 ~ -0.5, avg_FWDrating_p10.diff >= 0.5 ~ 0.5, TRUE ~ avg_FWDrating_p10.diff))  %>%
  mutate(avg_MIDrating_p10.diff_f = case_when(avg_MIDrating_p10.diff <= -0.5 ~ -0.5, avg_MIDrating_p10.diff >= 0.5 ~ 0.5, TRUE ~ avg_MIDrating_p10.diff))  %>%
  mutate(avg_BCKrating_p10.diff_f = case_when(avg_BCKrating_p10.diff <= -0.175 ~ -0.175, avg_BCKrating_p10.diff >= 0.175 ~ 0.175, TRUE ~ avg_BCKrating_p10.diff)) %>% 
  select(Date,Season,Round,Home.Team,Away.Team,avg_FWDrating_p10.diff_f,avg_MIDrating_p10.diff_f,avg_BCKrating_p10.diff_f)


load(here("models","model_v.02.Rda"))

Tips <- this_weeks_games %>% 
  left_join(.,this_weeks_games.ELO,by=c("Date","Season","Round","Home.Team","Away.Team")) %>% 
  left_join(.,this_weeks_games.form,by=c("Date","Season","Round","Home.Team","Away.Team")) %>% 
  left_join(.,this_weeks_games.fundamentals,by=c("Date","Season","Round","Home.Team","Away.Team")) %>%
  mutate(fitted.results = predict(model,newdata=.,type='response')) %>%
  mutate(fitted.results = ifelse(fitted.results > 0.5,1,0)) %>%
  mutate(Tip = ifelse(fitted.results == 1, Home.Team, Away.Team))

Tips$Tip





