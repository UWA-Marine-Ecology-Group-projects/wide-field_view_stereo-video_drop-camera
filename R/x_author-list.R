library(tidyverse)
library(googlesheets4)

url <- 'https://docs.google.com/spreadsheets/d/1XFzqpzq4BEW0X-1S9YsLsZz5tjCISf-V8ChNJ8Jq1b4/edit?usp=sharing'
address <- read_sheet(url, sheet = "address") %>%
  glimpse()

authors_alph <- read_sheet(url, sheet = "authors") %>%
  separate_wider_delim(code, delim = ", ", 
                       names = c("code", "code_2", "code_3"),
                       too_few = "align_start") %>%                             # Just join it by the first affiliation for now cos im dumb
  dplyr::filter(!is.na(first_name),
                co_author %in% "Yes") %>%
  left_join(address) %>%
  dplyr::filter(is.na(order)) %>%
  arrange(first_name) %>%
  dplyr::mutate(order = 5:51) %>%
  # dplyr::mutate(order = if_else(is.na(order),
  #                              seq(from = 5, to = 51, by = 1),
  #                              order)) %>%
  # arrange(order) %>%
  glimpse()

authors <- read_sheet(url, sheet = "authors") %>%
  separate_wider_delim(code, delim = ", ", 
                       names = c("code", "code_2", "code_3"),
                       too_few = "align_start") %>%                             # Just join it by the first affiliation for now cos im dumb
  dplyr::filter(!is.na(first_name),
                co_author %in% "Yes") %>%
  left_join(address) %>%
  dplyr::filter(!is.na(order)) %>%
  bind_rows(authors_alph) %>%
  arrange(order) %>%
  glimpse()

cat(paste(authors$first_name, authors$initial, authors$last_name, authors$number))

