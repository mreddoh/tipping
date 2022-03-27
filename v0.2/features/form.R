
## LOAD LIBRARY PACKAGES
library("fitzRoy")
library("tidyverse")
library("here")

## LOAD DATA
load(here::here("data", "gms.Rda"))

## SET INITIAL PARAMETERS
t = 20 # max length of historical variables

## TRANSFORM DATA
gms_tmp <- convert_results(gms) %>% 
           select(Game, Team, Margin) %>%
           group_by(Team) %>% 
           mutate(team_game_id = seq_along(Game)) %>%
           sapply(rep.int, times=t) %>%
           as.data.frame(stringsAsFactors = FALSE) %>%
           arrange(Game,Team) %>%
           mutate(game.shift = rep(1:t,dim(convert_results(gms))[1]),
                  join.id = as.numeric(team_game_id) - game.shift,
                  Margin = as.numeric(Margin)) %>%
           filter(join.id > 0)

previous_games <- gms_tmp %>%
                  select(Team, team_game_id, Margin) %>%
                  unique() %>%
                  merge(gms_tmp, ., by.x=c("Team", "join.id"), by.y=c("Team", "team_game_id")) %>% 
                  select(Team,Game,game.shift,Margin.y) %>%
                  mutate(game.shift=paste('p', game.shift, sep="_")) %>%
                  spread(game.shift, Margin.y)

## Add in a loop to create the characteristics.
form_char <- previous_games[complete.cases(previous_games), ]

game_seq <- seq(5,t,5)

for (i in game_seq){
  
  form_char <- form_char %>% mutate( rowMeans(select(., num_range("p_", 1:i))) )
  colnames(form_char)[dim(form_char)[2]] <- paste0("form.",i)
  
  form_char <- form_char %>% mutate( rowSums(select(., num_range("p_", 1:i)) > 0) )
  colnames(form_char)[dim(form_char)[2]] <- paste0("win.",i)
  
  form_char <- form_char %>% mutate( apply(select(., num_range("p_", 1:i)),1,max) )
  colnames(form_char)[dim(form_char)[2]] <- paste0("mxwin.",i)
  
  form_char <- form_char %>% mutate( apply(select(., num_range("p_", 1:i)),1,min) )
  colnames(form_char)[dim(form_char)[2]] <- paste0("mnwin.",i)

}

gameslongform.form <- form_char %>% 
                      select(.,c(-num_range("p_", 1:t))) %>% 
                      merge(., ., by="Game") %>%
                      filter(Team.x != Team.y) %>%
                      select(.,-Team.y) %>%
                      rename(Team = Team.x)

names(gameslongform.form) <- names(gameslongform.form) %>% 
                             gsub(".x",".own",.,fixed = TRUE) %>% 
                             gsub(".y",".opp",.,fixed = TRUE)

## ADD DIFFERENCE COLUMNS
diff <- select(gameslongform.form,ends_with(".own")) - select(gameslongform.form,ends_with(".opp"))
names(diff) <- names(diff) %>% gsub(".own",".diff",.,fixed = TRUE)

gameslongform.form <- cbind(gameslongform.form,diff)

remove(gms_tmp, previous_games, form_char, diff)

