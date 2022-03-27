
### LOAD PACKAGES ----
library(tidyverse)
library(here)
library(lubridate)
library(fitzRoy)

### GRAB DATA - RAW ----
raw_data <- fetch_results(season = 2000:2022, comp = "AFLM", source = "afltables")

