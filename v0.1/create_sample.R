
## SET PACKAGES
library("fitzRoy")
library("dplyr")
library("lubridate")
library("here")

## LOAD DATA
load(here::here("data", "gms.Rda"))

## set the seed to make your partition reproducible
set.seed(1991)

mysample <- convert_results(gms) %>% filter(year(Date)>=2003) %>%
            merge(., gameslongform.elo, by=c("Team", "Game")) %>%
            merge(., gameslongform.form, by=c("Team", "Game")) %>%
            mutate(outcome = ifelse(Margin>0,1,ifelse(Margin==0,0.5,0)) ) %>%
            filter(outcome != 0.5) %>%
            mutate(rand = runif(nrow(.)))


## SAVE DATASET
save(mysample,file=here::here("data","mysample.Rda"))

