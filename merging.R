# Purpose: Merge the cleaned sections with the wealth index and survey
# weights, then construct the variables used in the analysis.

#installing packages
library(haven)   
library(dplyr)   
library(stringr) 
library(labelled) 
library(purrr)
library(forcats) 
library(tidyr)

setwd("/Users/jenishneupane/Desktop/Folders/Data/Nepal Living Standard Surveys I-IV/NLSSIV_2022-23/NLSSIV_2022-23/data/stata format")

s01 <- read_dta("S01_clean.dta")


s02 <- read_dta("S02_clean.dta") %>%
  select(-idcode)

merged <- left_join(s01, s02, by = c("psu_number", "hh_number"))

s04 <- read_dta("S04_clean.dta")
merged <- left_join(merged, s04, by = c("psu_number", "hh_number", "idcode"))


s07 <- read_dta("S07_clean.dta")

merged <- left_join(merged, s07, by = c("psu_number", "hh_number", "idcode"))

s08 <- read_dta("S08_clean.dta")
merged <- left_join(merged, s08, by = c("psu_number", "hh_number", "idcode"))

#View(merged)

s09 <- read_dta("S09_clean.dta")
merged <- left_join(merged, s09, by = c("psu_number", "hh_number", "idcode"))


s10 <- read_dta("S10_clean.dta")


# checking if there's duplicates in section 10, since there are more obv than section 9 
s10 %>%
  count(psu_number, hh_number, idcode) %>%
  filter(n > 1)

# keeping distinct observation after knowing that there's issue with duplicates
s10 <- s10 %>%
  distinct(psu_number, hh_number, idcode, .keep_all = TRUE)


merged <- left_join(merged, s10, by = c("psu_number", "hh_number", "idcode"))

wealth <- read_dta("wealth_index.dta")

merged <- left_join(merged, wealth, by = c("psu_number", "hh_number"))

weight <- read_dta("weight.dta")

merged <- left_join(merged, weight, by = c("psu_number", "hh_number"))

#View(merged)

merged <- merged %>%
  select ( -season, -prov_code, -domain, - urbrur.x )


# the merging is done, now need to finalize data in order for running model

merged <- merged %>%
  filter(member_cat == 1)

merged <- merged %>%
  mutate(ncd = if_else(ncd == 1,1,0))

# Recode selected NCD types
merged <- merged %>%
  mutate(
    ncd_group = case_when(
      ncd_type == 2  ~ 1,  # Asthma/Respiratory
      ncd_type == 11 ~ 2,  # High/Low Blood Pressure
      ncd_type == 12 ~ 3,  # Gastrointestinal
      ncd_type == 5  ~ 4,  # Diabetes
      ncd_type == 13 ~ 5,  # Orthopedic
      TRUE ~ NA_real_     # Set others as NA
    )
  )



merged <- merged %>%
  mutate(
    ncd_group = factor(ncd_group,
                       levels = 1:5,
                       labels = c("Asthma", "BloodPressure", "Gastric", "Diabetes", "Ortho")
    )
  )


merged <- merged %>%
  mutate(lfp = if_else (
    wagework == 1 | selfwork == 1 | unpaidwork == 1 | absent_work == 1, 1, 0
  ))


merged <- merged %>%
  mutate(
    employment_status = case_when(
      wagework == 1 | selfwork == 1 | unpaidwork == 1 ~ "employed",
      wagework != 1 & selfwork != 1 & unpaidwork != 1 & absent_work == 1 ~ "unemployed",
      TRUE ~ "not_in_labor_force"
    )
  )


#recoding for male
merged <- merged %>%
  mutate(male = if_else(gender == 1, 1, 0))

#recoding for married/not married

merged <- merged %>%
  mutate(martial = if_else(martial == 2, 1, 0))


# for education, changing it into continuous variable
# merged <- merged %>%
#   mutate(education_years = case_when(
#     highest_education == 0  ~ 0,
#     highest_education == 1  ~ 1,
#     highest_education == 2  ~ 2,
#     highest_education == 3  ~ 3,
#     highest_education == 4  ~ 4,
#     highest_education == 5  ~ 5,
#     highest_education == 6  ~ 6,
#     highest_education == 7  ~ 7,
#     highest_education == 8  ~ 8,
#     highest_education == 9  ~ 9,
#     highest_education == 10 ~ 10,
#     highest_education == 11 ~ 10,
#     highest_education == 12 ~ 12,
#     highest_education == 13 ~ 16,
#     highest_education == 14 ~ 18,
#     highest_education == 15 ~ 18,
#     highest_education == 17 ~ 0,
#     highest_education == 98 ~ NA_real_,
#     TRUE ~ NA_real_
#   )) %>%
#   mutate(education_years = if_else(
#     is.na(education_years) & edu_background == 2,
#     case_when(
#       currently_education == 0  ~ 0,
#       currently_education == 1  ~ 1,
#       currently_education == 2  ~ 2,
#       currently_education == 3  ~ 3,
#       currently_education == 4  ~ 4,
#       currently_education == 5  ~ 5,
#       currently_education == 6  ~ 6,
#       currently_education == 7  ~ 7,
#       currently_education == 8  ~ 8,
#       currently_education == 9  ~ 9,
#       currently_education == 10 ~ 10,
#       currently_education == 11 ~ 10,
#       currently_education == 12 ~ 12,
#       currently_education == 13 ~ 16,
#       currently_education == 14 ~ 18,
#       currently_education == 15 ~ 18,
#       currently_education == 17 ~ 0,
#       currently_education == 98 ~ NA_real_,
#       TRUE ~ NA_real_
#     ),
#     education_years
#   ))

merged <- merged %>%
  mutate(education_years = case_when(
    edu_background == 1 ~ 0,  # Never attended school
    
    # Use highest_education only if edu_background == 2
    edu_background == 2 & highest_education == 0  ~ 0,
    edu_background == 2 & highest_education == 1  ~ 1,
    edu_background == 2 & highest_education == 2  ~ 2,
    edu_background == 2 & highest_education == 3  ~ 3,
    edu_background == 2 & highest_education == 4  ~ 4,
    edu_background == 2 & highest_education == 5  ~ 5,
    edu_background == 2 & highest_education == 6  ~ 6,
    edu_background == 2 & highest_education == 7  ~ 7,
    edu_background == 2 & highest_education == 8  ~ 8,
    edu_background == 2 & highest_education == 9  ~ 9,
    edu_background == 2 & highest_education == 10 ~ 10,
    edu_background == 2 & highest_education == 11 ~ 10,
    edu_background == 2 & highest_education == 12 ~ 12,
    edu_background == 2 & highest_education == 13 ~ 16,
    edu_background == 2 & highest_education == 14 ~ 18,
    edu_background == 2 & highest_education == 15 ~ 18,
    edu_background == 2 & highest_education == 17 ~ 0,
    edu_background == 2 & highest_education == 98 ~ NA_real_,
    
    # Fallback: currently_education, only if edu_background == 3
    edu_background == 3 & currently_education == 0  ~ 0,
    edu_background == 3 & currently_education == 1  ~ 1,
    edu_background == 3 & currently_education == 2  ~ 2,
    edu_background == 3 & currently_education == 3  ~ 3,
    edu_background == 3 & currently_education == 4  ~ 4,
    edu_background == 3 & currently_education == 5  ~ 5,
    edu_background == 3 & currently_education == 6  ~ 6,
    edu_background == 3 & currently_education == 7  ~ 7,
    edu_background == 3 & currently_education == 8  ~ 8,
    edu_background == 3 & currently_education == 9  ~ 9,
    edu_background == 3 & currently_education == 10 ~ 10,
    edu_background == 3 & currently_education == 11 ~ 10,
    edu_background == 3 & currently_education == 12 ~ 12,
    edu_background == 3 & currently_education == 13 ~ 16,
    edu_background == 3 & currently_education == 14 ~ 18,
    edu_background == 3 & currently_education == 15 ~ 18,
    edu_background == 3 & currently_education == 17 ~ 0,
    edu_background == 3 & currently_education == 98 ~ NA_real_,
    
    TRUE ~ NA_real_
  ))



merged <- merged %>%
  mutate(
    miss_edu_background = is.na(edu_background),
    miss_highest   = is.na(highest_education),
    miss_current   = is.na(currently_education)
  )

table(merged$miss_edu_background, merged$miss_highest)

merged %>%
  count(miss_edu_background, miss_highest, miss_current)


#View(merged)

# binary recoding of urban rural
merged <- merged %>%
  mutate(urban = if_else(urbrur.y == 1, 1, 0))

#age squared
merged <- merged %>%
  mutate(age_sq = age^2)

# one child, two child, more than two child

merged <- merged %>%
  mutate(
    one_child = if_else(total_children == 1, 1, 0),
    two_child = if_else(total_children == 2, 1, 0),
    more_than_two_child = if_else(total_children > 2, 1, 0)
  )

# for young children age < 5 Instrument Variable

merged <- merged %>%
  mutate(young_child = if_else(age <= 5, 1, 0))




# for young children, extracting their parent's IDs
#need to look into how these codebase is working, writing code just for the sake of it
young_child_parents <- merged %>%
  filter(young_child == 1) %>%
  mutate(
    father_pid = na_if(father_pid, "NA"),
    mother_pid = na_if(mother_pid, "NA")
  ) %>%
  select(father_pid, mother_pid) %>%
  pivot_longer(cols = everything(), values_to = "parent_pid") %>%
  filter(!is.na(parent_pid)) %>%
  distinct(parent_pid) %>%
  mutate(has_young_child = 1) 


# merging young_child_parent into original data-set

merged <- merged %>%
  left_join(young_child_parents, by = c("pid" = "parent_pid")) %>%
  mutate(has_young_child = replace_na(has_young_child, 0))


# interaction variable
merged <- merged %>%
  mutate(married_youngchild = if_else(martial == 1 & has_young_child == 1, 1, 0))

#unsafe water

unsafe_codes <- c(4, 5, 6, 7, 9)  # Open well, spring, river, other

merged <- merged %>%
  mutate(unsafe_water = if_else(drinkingwater_source %in% c(4, 5, 6, 7, 9), 1, 0))


#for sanitation
merged <- merged %>%
  mutate(
    unimproved_sanitation = if_else(toilet_type %in% c(3, 4, 5), 1, 0)
  )


table(merged$urban, merged$unimproved_sanitation)

merged <- merged %>%
  rename(urbrur = urbrur.y)

# saving final merged data for analysis
write_dta(merged, "final_merge.dta")







