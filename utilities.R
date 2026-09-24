library(haven)   # For reading .dta files
library(dplyr)   # For data manipulation 
library(stringr) # For string manipulation (like padding, concatenation)
library(labelled) # for showing labelled value in dataset


setwd("/Users/jenishneupane/Desktop/Folders/Data/Nepal Living Standard Surveys I-IV/NLSSIV_2022-23/NLSSIV_2022-23/data/stata format")

#loading the dataset containing utilities section
utilities <- read_dta("S02.dta")
View(utilities)

# keeping the required variables
utilities <- utilities %>%
  select (psu_number, hh_number, q02_00, q02_19, q02_21, q02_27, q02_28, q02_29, q02_32, q02_33, q02_31_a1, q02_31_b1, q02_31_c1) %>%
  rename (
    idcode = q02_00
  )

#binary recoding
utilities <- utilities %>%
  mutate(
    water_piped_in      = if_else(q02_19 == 1, 1, 0),  # Piped (within compound)
    water_piped_out     = if_else(q02_19 == 2, 1, 0),  # Piped (outside compound)
    water_tubewell      = if_else(q02_19 == 3, 1, 0),  # Tubewell / hand pump
    water_covered_well  = if_else(q02_19 == 4, 1, 0),  # Covered well
    water_open_well     = if_else(q02_19 == 5, 1, 0),  # Open well
    water_spring        = if_else(q02_19 == 6, 1, 0),  # Spring water
    water_river         = if_else(q02_19 == 7, 1, 0),  # River / rivulet
    water_bottled       = if_else(q02_19 == 8, 1, 0),  # Jar / bottled water
    water_other         = if_else(q02_19 == 9, 1, 0)   # Other source
  )

# for calculating PCA, we need to remove the obvious choice 
utilities <- utilities %>%
  select(-water_open_well)  # treating open well as reference

# recoding for other questions
utilities <- utilities %>%
  mutate(
    water_piped_house = if_else(q02_21 == 1, 1, 0)
  )

utilities <- utilities %>%
  mutate(
    toilet_flush_sewer = if_else(q02_27 == 1, 1, 0),
    toilet_flush_septic = if_else(q02_27 == 2, 1, 0),
    toilet_latrine      = if_else(q02_27 == 3, 1, 0),
    toilet_public       = if_else(q02_27 == 4, 1, 0),
    toilet_none         = if_else(q02_27 == 5, 1, 0)
  )

utilities <- utilities %>%
  select(-toilet_none)


utilities <- utilities %>%
  mutate(
    light_electric = if_else(q02_28 == 1, 1, 0),
    light_solar    = if_else(q02_28 == 2, 1, 0),
    light_biogas   = if_else(q02_28 == 3, 1, 0),
    light_kerosene = if_else(q02_28 == 4, 1, 0),
    light_other    = if_else(q02_28 == 5, 1, 0)
  )

utilities <- utilities %>%
  select(-light_kerosene)


utilities <- utilities %>%
  mutate(
    has_electricity_meter = if_else(q02_29 == 1, 1, 0)
  )

utilities <- utilities %>%
  mutate(
    cook_firewood  = if_else(q02_32 == 1, 1, 0),
    cook_lpg       = if_else(q02_32 == 2, 1, 0),
    cook_electric  = if_else(q02_32 == 3, 1, 0),
    cook_dung      = if_else(q02_32 == 4, 1, 0),
    cook_kerosene  = if_else(q02_32 == 5, 1, 0),
    cook_biogas    = if_else(q02_32 == 6, 1, 0),
    cook_other     = if_else(q02_32 == 7, 1, 0)
  )

utilities <- utilities %>%
  select(-cook_firewood)


utilities <- utilities %>%
  mutate(
    stove_open      = if_else(q02_33 == 1, 1, 0),
    stove_mudclay   = if_else(q02_33 == 2, 1, 0),
    stove_smokeless = if_else(q02_33 == 3, 1, 0),
    stove_kerosene  = if_else(q02_33 == 4, 1, 0),
    stove_gas       = if_else(q02_33 == 5, 1, 0),
    stove_electric  = if_else(q02_33 == 6, 1, 0),
    stove_other     = if_else(q02_33 == 7, 1, 0)
  )

utilities <- utilities %>%
  select(-stove_open)


utilities <- utilities %>%
  mutate(
    has_landline = if_else(q02_31_a1 == 1, 1, 0),
    has_cable_tv = if_else(q02_31_b1 == 1, 1, 0),
    has_internet = if_else(q02_31_c1 == 1, 1, 0)
  )


#removing the variables that aren't required
utilities <- utilities %>%
  select(
    -q02_19,     # main drinking water source
    -q02_21,     # piped water into house
    -q02_27,     # toilet type
    -q02_28,     # lighting source
    -q02_29,     # electricity meter
    -q02_32,     # cooking fuel
    -q02_33,     # stove type
    -q02_31_a1,  # landline
    -q02_31_b1,  # cable/satellite
    -q02_31_c1   # internet
  )

View(utilities)
write_dta(utilities, "utilities.dta")



