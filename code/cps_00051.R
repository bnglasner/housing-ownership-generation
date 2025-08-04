# How have homeownership rates changed across generations by age?
# Ben Glasner | 12/09/2024

rm(list = ls())
options(scipen = 999)
set.seed(42)

###########################
###   Load Packages     ###
###########################
library(dplyr)
library(tidyr)
library(ipumsr)
library(readxl)
library(stringr)
library(tigris)
library(tidycensus)
library(purrr)
library(plotly)
library(scales)

###########################
###   Set Paths         ###
###########################
# Define root directories by user
project_directories <- list(
  "Benjamin Glasner" = "C:/Users/Benjamin Glasner/EIG Dropbox/Benjamin Glasner/GitHub/housing-ownership-generation",
  "bngla"             = "C:/Users/bngla/EIG Dropbox/Benjamin Glasner/GitHub/housing-ownership-generation",
  "Research"          = "C:/Users/Research/EIG Dropbox/Benjamin Glasner/GitHub/housing-ownership-generation"
)

# Identify current user and assign paths
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop("Root folder for current user is not defined.")
}
path_project <- project_directories[[current_user]]
path_data    <- file.path(path_project, "data")
path_output  <- file.path(path_project, "output")

###########################
###   Load IPUMS Data   ###
###########################
setwd(path_data)
ddi  <- read_ipums_ddi("cps_00051.xml")
data <- read_ipums_micro(ddi)

#########################################
###   Clean and Prepare Variables     ###
#########################################

data <- data %>%
  filter(YEAR >= 1976, AGE >= 19) %>%
  mutate(
    # Derive birth year
    birthyear = YEAR - AGE,
    
    # Assign generation labels
    generation = case_when(
      birthyear <= 1927 ~ "Greatest",
      birthyear %in% 1928:1945 ~ "Silent",
      birthyear %in% 1946:1964 ~ "Boomers",
      birthyear %in% 1965:1980 ~ "Gen-X",
      birthyear %in% 1981:1996 ~ "Millennials",
      birthyear %in% 1997:2012 ~ "Gen-Z",
      birthyear >= 2013 ~ "Gen-Alpha",
      TRUE ~ NA_character_
    ),
    
    # Define homeownership: only heads and spouses with OWNERSHP == 10 (owned/mortgage-free or with mortgage)
    home_owner = case_when(
      RELATE %in% c(101, 201, 202, 203) & OWNERSHP == 10        ~ 1,
      RELATE %in% c(101, 201, 202, 203) & OWNERSHP %in% c(21,22) ~ 0,
      RELATE %in% 301:1260                                       ~ 0,
      TRUE                                                       ~ NA_real_
    ),
    
    home_owner_worst = case_when(
      OWNERSHP == 10        ~ 1,
      OWNERSHP %in% c(21,22) ~ 0,
      TRUE                                                       ~ NA_real_
    ),
    
    home_owner_bad = case_when(
      RELATE %in% c(101, 201, 202, 203,501,701) & OWNERSHP == 10        ~ 1,
      RELATE %in% c(101, 201, 202, 203,501,701) & OWNERSHP %in% c(21,22) ~ 0,
      RELATE %in% 901:1260                                       ~ 0,
      TRUE                                                       ~ NA_real_
    )
  )

#########################################
###   Summarize Homeownership Rates   ###
#########################################
age_averages <- data %>%
  group_by(generation, AGE) %>%
  summarise(
    home_owner_avg = round(100*weighted.mean(home_owner, w = ASECWTH, na.rm = TRUE),1),
    
    .groups = "drop"
  )

age_averages_bad <- data %>%
  filter(RELATE %in% c(101)) %>%
  group_by(generation, AGE) %>%
  summarise(
    home_owner_avg = round(100*weighted.mean(home_owner, w = ASECWTH, na.rm = TRUE),1),
    
    .groups = "drop"
  )

age_averages_wide <- age_averages %>%
  pivot_wider(
    names_from = generation,
    values_from = home_owner_avg
  )

setwd(path_output)
writexl::write_xlsx(age_averages_wide, path = "Ownership_rate_by_generation.xlsx")