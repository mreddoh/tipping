
## SET VERSION
version <- "v0.2"

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


