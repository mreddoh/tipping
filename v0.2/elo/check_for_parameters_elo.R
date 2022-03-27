
## LOAD DATA
load(here::here("data", "game_lookup.Rda"))

#Create dataset

#subset to 2003 onwards.
gms <- game.lookup %>% subset(year(Date)>=1995 & year(Date)<=2017)


ss_ns = 0.4
h_ns = seq(100,125,1)
number_sequence = 53

remove(chk)

for (i in h_ns){

for (j in number_sequence){

for (k in ss_ns){
  ss.ratio = k
  p.ratio = 1 - k

all.teams <- convert_results(gms) %>% select(Team) %>% unique() %>% t()
ratings <- as.list(rep(1500, times=length(all.teams)))
names(ratings) <- all.teams

## Add adjustment for GC and GWS
ratings[c("GWS","Gold Coast")] <- 1100

gms_s <- gms %>% select(Game, Date, Home.Team, Home.Points, 
                                    Away.Team, Away.Points, 
                                    Home.Goals, Home.Behinds, 
                                    Away.Goals, Away.Behinds, HomeAdv.flag) %>%
                 mutate(result.home = ifelse(Home.Points>Away.Points,1,ifelse(Home.Points==Away.Points,0.5,0))) %>%
                 mutate(result.away = ifelse(Home.Points<Away.Points,1,ifelse(Home.Points==Away.Points,0.5,0))) %>%
                 mutate(Home.SS = Home.Goals+Home.Behinds) %>%
                 mutate(Away.SS = Away.Goals+Away.Behinds) %>%
                 mutate(ss.result.home = ifelse(Home.SS>Away.SS,1,ifelse(Home.SS==Away.SS,0.5,0))) %>%
                 mutate(ss.result.away = ifelse(Home.SS<Away.SS,1,ifelse(Home.SS==Away.SS,0.5,0))) %>%
                 mutate(preELOhome = NA, preELOaway = NA)

kfactor=j

for (idx in 1:dim(gms_s)[1]){
  
  homeTeamName <- gms_s[["Home.Team"]][idx]
  awayTeamName <- gms_s[["Away.Team"]][idx]
  
  homeAdvantage <- i * gms_s[["HomeAdv.flag"]][idx]
  
  homeTeamRating <- as.numeric(ratings[homeTeamName]) + homeAdvantage
  awayTeamRating <- as.numeric(ratings[awayTeamName]) 
  expectedHome <- 1 / (1 + 10^((awayTeamRating - homeTeamRating)/400))
  expectedAway <- 1 - expectedHome
  resultHome <- ( gms_s[["result.home"]][idx] * ss.ratio ) + ( gms_s[["ss.result.home"]][idx] * p.ratio )
  resultAway <- ( gms_s[["result.away"]][idx] * ss.ratio ) + ( gms_s[["ss.result.away"]][idx] * p.ratio )
  
  ratings[homeTeamName] <- as.numeric(ratings[homeTeamName]) + kfactor*(resultHome - expectedHome)
  ratings[awayTeamName] <- as.numeric(ratings[awayTeamName]) + kfactor*(resultAway - expectedAway)
  
  gms_s[["preELOhome"]][idx] <- homeTeamRating
  gms_s[["preELOaway"]][idx] <- awayTeamRating
 
}

exphi <- 1 / (1 + 10^((gms_s$preELOaway - gms_s$preELOhome)/400))
expai <- 1 - exphi

mse <- gms_s %>% mutate(se = (exphi-result.home)^2+(expai-result.away)^2 ) %>% summarise(msse = sum(se)) %>% as.numeric(.)

if(exists("chk")==FALSE){chk <- cbind(homeAdvantage,mse)}
chk <- rbind(chk,cbind(homeAdvantage,mse))

}

}
  
}

ggplot(as.data.frame(chk)) + geom_tile(aes(fill = mse, x = kfactor, y = homeAdvantage) )






