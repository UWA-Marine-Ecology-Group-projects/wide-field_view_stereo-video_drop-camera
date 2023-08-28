rm(list = ls())

# libraries ---
library(tidyverse)
library(GlobalArchive)
library(googlesheets4)
library(RCurl)
library(lubridate)
library(hms)

# Functions
data.dir <- "data/raw"

ga.read.tm <- function(flnm) {
  read.delim(flnm, header = T, skip = 4, stringsAsFactors = FALSE, colClasses = "character") %>%
    dplyr::mutate(campaign.naming = str_replace_all(flnm, paste0(data.dir ,"/"),"")) %>%
    tidyr::separate(campaign.naming, into = c("campaignid"), sep = "/", extra = "drop", fill = "right") %>%
    dplyr::mutate(campaignid = str_replace_all(.$campaignid,"_[^_]+$", "")) %>%
    ga.clean.names() %>%
    dplyr::select(campaignid, filename, image.row, image.col, broad, morphology, type)
}

ga.select.habitat.metadata <- function(data) {
  data %>%
    dplyr::select(campaignid, sample, latitude, longitude, date_time, site, location, status,
                  depth, successful_habitat_forward, observer_habitat_forward) %>%
    dplyr::filter(successful_habitat_forward %in% "Yes")
}

# designate project-specific cache
# options(gargle_oauth_cache = ".secrets")
# # check the value of the option, if you like
# gargle::gargle_oauth_cache()
# googlesheets4::gs4_auth()
# 2

# Read in rosetta stone ----
url <- "https://docs.google.com/spreadsheets/d/1tcvHnD8LtPmjro8eOMdOS6k6HKdND86mMAOk6AS_gfc/edit?usp=sharing"

stone <- read_sheet(url, sheet = "rosetta stone") %>%
  dplyr::filter(!`Schema name` %in% "TM schema_Fine.Morph.Type") %>%
  ga.clean.names() %>%
  dplyr::rename(clean.code = code) %>%
  dplyr::select(-c(schema.name, notes)) %>%
  select(-fine) %>%
  distinct() 

# download current one from github here:
# https://github.com/GlobalArchiveManual/annotation-schema/tree/main/output/habitat
levels <- read_csv("data/raw/benthic.annotation.schema.forward.facing.20230714.135113.csv", col_types = cols(.default = "c")) %>%
  dplyr::rename(code = CAAB_code) %>%
  dplyr::mutate(code = as.character(code)) %>%
  dplyr::select(-c(qualifiers, Parent_CAAB))

# Read in habitat data to clean
data <- list.files(path = data.dir,
                   recursive = T,
                   pattern = "Dot Point Measurements.txt",
                   full.names = T) %>%
  purrr::map_dfr(~ga.read.tm(.)) %>%
  dplyr::mutate(campaignid = case_when(campaignid %in% c("2020-10_south-west_BOSS_east",
                                                         "2020-10_south-west_BOSS_north",
                                                         "2020-10_south-west_BOSS_south",
                                                         "2020-10_south-west_BOSS_west",
                                                         "2020-11_south-west_BOSS_multibeamed") ~ 
                                         "2020-10_south-west_BOSS",
                                       campaignid %in% "2021-03_West-Coast_BOSS" ~ "2021-03_West-Coast_BOSS")) %>%
  dplyr::mutate(opcode = str_replace_all(filename, c("N.jpg" = "","E.jpg" = "","S.jpg" = "",
                                                     "W.jpg" = "",".jpg" = "",".JPG" = ""))) %>%
  dplyr::mutate_all(~na_if(., '')) %>%
  dplyr::mutate(broad = ifelse(is.na(broad), "Unscorable", broad)) %>%
  glimpse()

data.with.stone <- left_join(data, stone)
find.missing <- data.with.stone %>% dplyr::filter(is.na(clean.code)) # none missing

# Now data has correct CAAB code can join with the correct schema (L1 - L5)
data.with.levels <- data.with.stone %>%
  dplyr::mutate(code = clean.code) %>%
  dplyr::left_join(levels) %>%
  dplyr::select(-c(broad, morphology, type, clean.code))

missing.level <- data.with.levels %>%
  dplyr::filter(is.na(level_1)) # none = good

summarised <- data.with.levels %>%
  dplyr::mutate(number = 1) %>%
  dplyr::group_by(campaignid, opcode, code, level_1, level_2, level_3, level_4, level_5, 
                  level_6, level_7, level_8, family, genus, species) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::rename(sample = opcode) %>%
  dplyr::rename(caab_code = code) %>%
  dplyr::filter(!sample %in% c("235NREDO", "235EREDO", "235SREDO", "235WREDO",
                               "278NREDO", "278EREDO", "278SREDO", "278WREDO", 
                               "FH96N"))

metadata <- read.csv("data/raw/2020-2021_south-west_BOSS-BRUV.Metadata.csv") %>%
  dplyr::mutate(successful_habitat_forward = "Yes",
                observer_habitat_forward = "Claude Spencer") %>%
  dplyr::mutate(time = ifelse(str_length(time) == 4, paste0("0", time), time)) %>%
  dplyr::mutate(time = ifelse(str_length(time) == 5, paste0(time, ":00"), time)) %>%
  dplyr::mutate(time = ifelse(str_length(time) == 7, paste0("0", time), time)) %>%
  separate(date, into = c("year", "month", "day"), sep = c(4, 6)) %>%
  dplyr::mutate(date = paste(year, month, day, sep = "-")) %>%
  dplyr::mutate(date_time = paste0(date, "T", time, "+08:00")) %>%
  ga.select.habitat.metadata() %>%
  glimpse()

write.csv(summarised, "data/tidy/2021-2022_SwC_BOSS_Habitat.csv",
          row.names = F)
write.csv(metadata, "data/tidy/2021-2022_SwC_BOSS_Metadata.csv",
          row.names = F)
