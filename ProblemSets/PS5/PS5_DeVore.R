# First Unguided Data Scraping (Airline Hub)
library(tidyverse)
library(rvest)

url <- 'https://en.wikipedia.org/wiki/Airline_hub'
html_path <- '#mw-content-text > div.mw-parser-output > table'

hub <- read_html(url) %>% html_nodes(html_path) %>% html_table() %>% '[['(1)


# Fredr API (Airline Pilot Employment Data)
library(fredr)
fredr_set_key(Sys.getenv("FRED_API_KEY"))

pilotemp <- fredr(series_id = "CES4348100001", observation_start = as.Date("1990-01-01"), observation_end = as.Date("2023-01-01"))
