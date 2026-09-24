# Purpose: Clean the NLSS IV sections used in the analysis and build
# household and person identifiers from the roster (S01).

#installing packages
library(haven)  
library(dplyr)   
library(stringr)
library(labelled)

#setting the directory
setwd("/Users/jenishneupane/Desktop/Folders/Data/Nepal Living Standard Surveys I-IV/NLSSIV_2022-23/NLSSIV_2022-23/data/stata format")

#creating some variables required for analysis
section1 <- read_dta("S01.dta")


# Create a padded household string and unique household ID
section1 <- section1 %>%
  mutate(
    hh_str   = str_pad(as.character(hh_number), 4, pad = "0"),
    hhid_str = str_c(psu_number, "_", hh_str)
  )

section1 <- section1 %>%
  group_by(hhid_str) %>%
  mutate(household_size = n()) %>%
  ungroup()

#create a unique person id for each individual

section1 <- section1 %>%
  mutate(
    pid = str_c(hhid_str, "_", idcode)
  )

# creating parents PIDs
section1 <- section1 %>%
  mutate(
    father_pid = ifelse(!is.na(q01_11), str_c(hhid_str, "_", q01_11), NA_character_),
    mother_pid = ifelse(!is.na(q01_14), str_c(hhid_str, "_", q01_14), NA_character_)
  )

# Count children for fathers
father_count <- section1 %>%
  filter(!is.na(father_pid)) %>%
  count(father_pid, name = "num_father_children") %>%
  rename(pid = father_pid)

# Count children for mothers
mother_count <- section1 %>%
  filter(!is.na(mother_pid)) %>%
  count(mother_pid, name = "num_mother_children") %>%
  rename(pid = mother_pid)

# STEP 4: Merge child counts back to the parent's own row
section1 <- section1 %>%
  left_join(father_count, by = "pid") %>%
  left_join(mother_count, by = "pid") %>%
  mutate(
    total_children = rowSums(across(c(num_father_children, num_mother_children)), na.rm = TRUE)
  )


# since we got the required variables, removing the one which was made in process of formation
section1 <- section1 %>%
  select(-num_father_children, -num_mother_children)



# selecting variables

# S01
section1 <- section1 %>%
  select(psu_number, hh_number, idcode,
         q01_02, q01_03, q01_04, q01_05, q01_07,
         household_size, father_pid, mother_pid, total_children, pid, member_cat, abroad_status) %>%
  rename(gender = q01_02,
         age = q01_03,
         hh_head = q01_04,
         martial = q01_05,
         caste = q01_07
  )

write_dta(section1, "S01_clean.dta")





# S02
section2 <- read_dta("S02.dta") %>%
  select(psu_number, hh_number, q02_00, q02_19, q02_20, q02_24, q02_25, q02_27) %>%
  rename(
    idcode = q02_00,
    drinkingwater_source = q02_19,
    otherwater_source = q02_20,
    sanitary_system =  q02_24,
    garbage_disposal = q02_25,
    toilet_type =  q02_27
  )



#saving section 2 cleaned data
write_dta(section2, "S02_clean.dta")




# S04
section4 <- read_dta("S04.dta") %>%
  select(psu_number, hh_number, idcode, q04_03, q04_03_a, q04_04, q04_04_a) %>%
  rename(
    district = q04_03,
    urbrur = q04_03_a,
    mig_district = q04_04,
    mig_urbrur = q04_04_a
    
  )

#saving section 4 cleaned data
write_dta(section4, "S04_clean.dta")




# S07
section7 <-read_dta("S07.dta")


section7 <- read_dta("S07.dta") %>%
  select(psu_number, hh_number, idcode, q07_02, q07_03, q07_04, q07_06, q07_12)%>%
  rename(
    read = q07_02,
    write= q07_03,
    edu_background = q07_04,
    highest_education = q07_06,
    currently_education = q07_12
  )


section7 <- section7 %>%
  mutate(across(where(is.labelled), to_factor))

# saving section 7 cleaned data  
write_dta(section7, "S07_clean.dta")


# S08
section8 <- read_dta("S08.dta") %>%
  select(psu_number, hh_number, idcode, q08_02, q08_03, q08_04) %>%
  rename(
    ncd = q08_02,
    ncd_type = q08_03,
    ncd_onset = q08_04
  )


# saving section 8 cleaned data
write_dta(section8, "S08_clean.dta")


# S09
section9 <- read_dta("S09.dta") %>%
  select(psu_number, hh_number, idcode, q09_02, q09_03, q09_04, q09_05, q09_09_a) %>%
  rename(
    wagework = q09_02,
    selfwork = q09_03,
    unpaidwork = q09_04,
    absent_work = q09_05,
    employment_code = q09_09_a
    
  )

# saving section 9 cleaned data
write_dta(section9, "S09_clean.dta")


# S10
section10 <- read_dta("S10.dta") %>%
  select(psu_number, hh_number, idcode, q10_03, q10_05) %>%
  rename(
    work_lastyear = q10_03,
    work_condition = q10_05
  )

#saving section 10 cleaned data
write_dta(section10, "S10_clean.dta")



# count missing values in one variable
poverty <- read_dta("poverty.dta")
poverty <- poverty %>%
  mutate(across(where(is.labelled), to_factor))


         