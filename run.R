
## SET VERSION
version <- "v0.2"

##LOAD DATA
load(here::here("data", "gms.Rda"))
load(here::here("data", "game_lookup.Rda"))
load(here::here("data", "ps.Rda"))

## CREATE ELO VARIABLES
  source(here::here(version, "elo", "elo.R"), echo = TRUE)
## CREATE/ADD BASIC FEATURES
  source(here::here(version, "features", "form.R"), echo = TRUE)
  source(here::here(version, "features", "fundamentals.R"), echo = TRUE)
  source(here::here(version, "features", "team.R"), echo = TRUE)
## CREATE/ADD PLAYER FEATURES
  source(here::here(version, "features", "player.R"), echo = TRUE)

## CREATE SAMPLE
source(here::here(version, "create_sample.R"), echo = TRUE)

## MODEL!
source(here::here(version, "model.R"), echo = TRUE)


