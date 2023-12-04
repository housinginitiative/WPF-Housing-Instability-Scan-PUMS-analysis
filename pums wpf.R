library(tidyverse)
library(tidycensus)
library(sf)
library(mapview)

#### PUMS ####
vars_pums <- pums_variables %>% 
  filter(year == 2021 & survey == "acs5")

pums <- get_pums(
  variables = c("GRPIP", # gross rent income percentage
                "NP", # people in household
                "BDSP", # number bedrooms
                "MIG", # lived here 1 year ago
                "TEN", # renter vs owner vs mortaged
                "PLM", # plumbing
                "KIT", # kitchen
                "HUPAC", # children in hh
                "HHLDRRAC1P", # race of householder
                "HINCP"), # household income

  state = "PA",
  puma = c("03201", "03202", "03203", "03204", "03205", "03206", "03207", "03208",
           "03209", "03210", "03211"),
  survey = "acs5",
  year = 2021,
  recode = TRUE,
  rep_weights = "household"
)

sum(pums$NP)

pums_clean <- pums %>%
  filter(GRPIP <= 100 &
         BDSP >= 0 &
         TEN_label != "N/A (GQ/vacant)" &
         KIT_label != "N/A (GQ)" &
         PLM_label != "N/A (GQ)")

          
#### CLS surveys ####

st_write(pums_clean, "pums2021.csv")
