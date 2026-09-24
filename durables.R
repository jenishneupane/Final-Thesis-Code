# Purpose: Build household durable-asset ownership indicators, one
# column per asset, for the wealth index.

#installing packages
library(haven)   
library(dplyr)   
library(stringr)
library(labelled)
library(tidyr)



setwd("/Users/jenishneupane/Desktop/Folders/Data/Nepal Living Standard Surveys I-IV/NLSSIV_2022-23/NLSSIV_2022-23/data/stata format")

#loading the dataset
durable_assets <- read_dta("S06C.dta")

# code for durable assets, observations are mapped on these codes for durable assets
keep_codes <- c(501, 503, 504, 505, 506, 509, 510, 512,
                515, 516, 519, 521, 523)

# only keeping the durable assets which we need for computing PCA
durable_assets <- durable_assets %>%
  filter(s06c_code %in% keep_codes)

# binary recoding
durable_assets <- durable_assets %>%
  mutate(owns = if_else(q06_03c == 1, 1, 0))


# reshaping durable sasset data to wide format with one coumn per asset type
durable_assets_wide <- durable_assets %>%
  select(psu_number, hh_number, s06c_code, owns) %>%
  pivot_wider(
    names_from = s06c_code,
    values_from = owns,
    values_fill = 0
  )

# renaming the asset code into asset name
durable_assets_wide <- durable_assets_wide %>%
  rename(
    radio      = `501`,
    bicycle    = `503`,
    motorcycle = `504`,
    car        = `505`,
    fridge     = `506`,
    washer     = `509`,
    fan        = `510`,
    tv         = `512`,
    inverter   = `515`,
    solar      = `516`,
    mobile     = `519`,
    computer   = `521`,
    furniture  = `523`
  )



# savings the durable dataset
write_dta(durable_assets_wide, "durables.dta")


