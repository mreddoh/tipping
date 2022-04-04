
## SET PACKAGES
library("fitzRoy")
library("dplyr")
library("lubridate")
library("here")

## LOAD DATA
load(here::here("data", "gms.Rda"))

## set the seed to make your partition reproducible
set.seed(1991)

mysample <- gms %>%
  pivot_longer(cols = c(Home.Team,Away.Team), values_to = "Team") %>% 
  mutate(Score.for = ifelse(name == "Home.Team", Home.Points, Away.Points),
         Score.against = ifelse(name == "Home.Team", Away.Points, Home.Points),
         Margin = ifelse(name == "Home.Team", Margin, Margin*-1),
         game.id = Game) %>% 
  filter(year(Date)>=2003 & year(Date) <= 2021) %>%
  merge(., gameslongform.elo, by=c("Team", "game.id")) %>%
  merge(., gameslongform.form, by=c("Team", "game.id")) %>%
  merge(., gameslongform.fundamentals, by=c("Team", "game.id")) %>%
  mutate(outcome = ifelse(Margin>0,1,ifelse(Margin==0,0.5,0)),
         Status = ifelse(name=="Home.Team","Home","Away")) %>%
  filter(outcome != 0.5) %>%
  mutate(rand = runif(nrow(.)))


## SAVE DATASET
save(mysample,file=here::here("data","mysample.Rda"))

