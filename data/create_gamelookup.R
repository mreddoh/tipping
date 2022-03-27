## GAME LOOKUP CREATION

## LOAD DATA
load(here::here("data", "gms.Rda"))

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
               mutate(HomeAdv.flag = ifelse(Home.Team.city == city & Away.Team.city != city, 1, 0)) %>%
               select(-c(Home.Team.city,Away.Team.city,city))



## SAVE DATA
save(game.lookup,file = here::here("data", "game_lookup.Rda"))