# Purpose: Merge the cleaned sections with the wealth index and survey
# weights, then construct the variables used in the analysis.

library(haven)
library(dplyr) 
library(stringr) 
library(labelled)
library(purrr)
library(forcats) 
library(tidyr)
library(gtsummary)
library(ggplot2)
library(skimr)



setwd("/Users/jenishneupane/Desktop/Folders/Data/Nepal Living Standard Surveys I-IV/NLSSIV_2022-23/NLSSIV_2022-23/data/stata format")

data <- read_dta("final_merge.dta")

View(data)
skim(data)

#looking at whole dataset first and then figuring out 
data <- data %>%
  filter(age >= 10 & age <=64)

summary(data$age)
summary(data$household_size)
summary(data$wealth_index)

prop.table(table(data$male))
prop.table(table(data$urban))
prop.table(table(data$lfp))
prop.table(table(data$ncd))

#View(summary_table)

table(data$ncd)


data %>%
  filter(ncd == 1) %>%
  summarise(sd_lfp = sd(lfp, na.rm = TRUE))



#lfp
data %>%
  filter(ncd == 0) %>%
  summarise(mean_lfp = mean(lfp, na.rm = TRUE), sd_lfp = sd(lfp, na.rm = TRUE))

data %>%
  filter(ncd == 1) %>%
  summarise(mean_lfp = mean(lfp, na.rm = TRUE), sd_lfp = sd(lfp, na.rm = TRUE))

#age
data %>%
  filter(ncd == 0) %>%
  summarise(mean_age = mean(age, na.rm = TRUE), sd_age = sd(age, na.rm = TRUE))

data %>%
  filter(ncd == 1) %>%
  summarise(mean_age = mean(age, na.rm = TRUE), sd_age = sd(age, na.rm = TRUE))


#education 

data %>%
  filter(ncd == 0) %>%
  summarise(mean_edu = mean(education_years, na.rm = TRUE), sd_edu = sd(education_years, na.rm = TRUE))

data %>%
  filter(ncd == 1) %>%
  summarise(mean_edu = mean(education_years, na.rm = TRUE), sd_edu = sd(education_years, na.rm = TRUE))

# urban
data %>%
  filter(ncd == 0) %>%
  summarise(mean_urban = mean(urban, na.rm = TRUE), sd_urban = sd(urban, na.rm = TRUE))

data %>%
  filter(ncd == 1) %>%
  summarise(mean_urban = mean(urban, na.rm = TRUE), sd_urban = sd(urban, na.rm = TRUE))

# hhsize
data %>%
  filter(ncd == 0) %>%
  summarise(mean_hh = mean(household_size, na.rm = TRUE), sd_hh = sd(household_size, na.rm = TRUE))

data %>%
  filter(ncd == 1) %>%
  summarise(mean_hh = mean(household_size, na.rm = TRUE), sd_hh = sd(household_size, na.rm = TRUE))

#wealth_index
data %>%
  filter(ncd == 0) %>%
  summarise(mean_wi = mean(wealth_index, na.rm = TRUE), sd_wi = sd(wealth_index, na.rm = TRUE))

data %>%
  filter(ncd == 1) %>%
  summarise(mean_wi = mean(wealth_index, na.rm = TRUE), sd_wi = sd(wealth_index, na.rm = TRUE))


#gender wise lfp
data %>%
  filter(gender == 1, ncd == 0) %>%
  summarise(mean_lfp = mean(lfp, na.rm = TRUE), sd_lfp = sd(lfp, na.rm = TRUE))

data %>%
  filter(gender == 1, ncd == 1) %>%
  summarise(mean_lfp = mean(lfp, na.rm = TRUE), sd_lfp = sd(lfp, na.rm = TRUE))

data %>%
  filter(gender == 2, ncd == 0) %>%
  summarise(mean_lfp = mean(lfp, na.rm = TRUE), sd_lfp = sd(lfp, na.rm = TRUE))

data %>%
  filter(gender == 2, ncd == 1) %>%
  summarise(mean_lfp = mean(lfp, na.rm = TRUE), sd_lfp = sd(lfp, na.rm = TRUE))

#gender wise age

# Male, NCD = 0
data %>%
  filter(gender == 1, ncd == 0) %>%
  summarise(mean_age = mean(age, na.rm = TRUE), sd_age = sd(age, na.rm = TRUE))

# Male, NCD = 1
data %>%
  filter(gender == 1, ncd == 1) %>%
  summarise(mean_age = mean(age, na.rm = TRUE), sd_age = sd(age, na.rm = TRUE))

# Female, NCD = 0
data %>%
  filter(gender == 2, ncd == 0) %>%
  summarise(mean_age = mean(age, na.rm = TRUE), sd_age = sd(age, na.rm = TRUE))

# Female, NCD = 1
data %>%
  filter(gender == 2, ncd == 1) %>%
  summarise(mean_age = mean(age, na.rm = TRUE), sd_age = sd(age, na.rm = TRUE))


# Male, NCD = 0
data %>%
  filter(gender == 1, ncd == 0) %>%
  summarise(mean_edu = mean(education_years, na.rm = TRUE), sd_edu = sd(education_years, na.rm = TRUE))

# Male, NCD = 1
data %>%
  filter(gender == 1, ncd == 1) %>%
  summarise(mean_edu = mean(education_years, na.rm = TRUE), sd_edu = sd(education_years, na.rm = TRUE))

# Female, NCD = 0
data %>%
  filter(gender == 2, ncd == 0) %>%
  summarise(mean_edu = mean(education_years, na.rm = TRUE), sd_edu = sd(education_years, na.rm = TRUE))

# Female, NCD = 1
data %>%
  filter(gender == 2, ncd == 1) %>%
  summarise(mean_edu = mean(education_years, na.rm = TRUE), sd_edu = sd(education_years, na.rm = TRUE))



# Male, NCD = 0
data %>%
  filter(gender == 1, ncd == 0) %>%
  summarise(mean_urban = mean(urban, na.rm = TRUE), sd_urban = sd(urban, na.rm = TRUE))

# Male, NCD = 1
data %>%
  filter(gender == 1, ncd == 1) %>%
  summarise(mean_urban = mean(urban, na.rm = TRUE), sd_urban = sd(urban, na.rm = TRUE))

# Female, NCD = 0
data %>%
  filter(gender == 2, ncd == 0) %>%
  summarise(mean_urban = mean(urban, na.rm = TRUE), sd_urban = sd(urban, na.rm = TRUE))

# Female, NCD = 1
data %>%
  filter(gender == 2, ncd == 1) %>%
  summarise(mean_urban = mean(urban, na.rm = TRUE), sd_urban = sd(urban, na.rm = TRUE))





# Male, NCD = 0
data %>%
  filter(gender == 1, ncd == 0) %>%
  summarise(mean_hh = mean(household_size, na.rm = TRUE), sd_hh = sd(household_size, na.rm = TRUE))

# Male, NCD = 1
data %>%
  filter(gender == 1, ncd == 1) %>%
  summarise(mean_hh = mean(household_size, na.rm = TRUE), sd_hh = sd(household_size, na.rm = TRUE))

# Female, NCD = 0
data %>%
  filter(gender == 2, ncd == 0) %>%
  summarise(mean_hh = mean(household_size, na.rm = TRUE), sd_hh = sd(household_size, na.rm = TRUE))

# Female, NCD = 1
data %>%
  filter(gender == 2, ncd == 1) %>%
  summarise(mean_hh = mean(household_size, na.rm = TRUE), sd_hh = sd(household_size, na.rm = TRUE))





# Male, NCD = 0
data %>%
  filter(gender == 1, ncd == 0) %>%
  summarise(mean_wi = mean(wealth_index, na.rm = TRUE), sd_wi = sd(wealth_index, na.rm = TRUE))

# Male, NCD = 1
data %>%
  filter(gender == 1, ncd == 1) %>%
  summarise(mean_wi = mean(wealth_index, na.rm = TRUE), sd_wi = sd(wealth_index, na.rm = TRUE))

# Female, NCD = 0
data %>%
  filter(gender == 2, ncd == 0) %>%
  summarise(mean_wi = mean(wealth_index, na.rm = TRUE), sd_wi = sd(wealth_index, na.rm = TRUE))

# Female, NCD = 1
data %>%
  filter(gender == 2, ncd == 1) %>%
  summarise(mean_wi = mean(wealth_index, na.rm = TRUE), sd_wi = sd(wealth_index, na.rm = TRUE))


#View(data)

#some visualisation

ggplot(data, aes(x = factor(ncd), fill = factor(ncd))) +
  geom_bar(aes(y = ..prop.., group = 1)) +
  labs(x = "NCD Status", y = "Proportion in Labor Force", fill = "NCD") +
  facet_wrap(~ gender) +
  theme_minimal()

ggplot(data, aes(x = education_years, fill = factor(ncd))) +
  geom_histogram(binwidth = 1, position = "dodge") +
  facet_wrap(~ gender) +
  labs(x = "Years of Schooling", y = "Count", fill = "NCD") +
  theme_minimal()

ggplot(data, aes(x = wealth_index, colour = factor(ncd))) +
  geom_density(fill = NA, size = 1) +     # outline only, no shading
  labs(x = "ncd", colour = "lfp") +
  theme_minimal()


#analysis

vars_needed <- c("lfp", "ncd",
                 "unimproved_sanitation", "unsafe_water",
                 "age", "age_sq", "education_years",
                 "male", "urban", "wealth_index",
                 "household_size", "married_youngchild")

d <- na.omit(data[ , vars_needed])

data <- data %>%
  select(lfp, ncd, unimproved_sanitation, unsafe_water, age, age_sq, education_years, male, urban, wealth_index,
         household_size, married_youngchild)

sel <- ncd ~ unimproved_sanitation + unsafe_water +
  age + age_sq + education_years + male + urban + wealth_index

out <- lfp ~ ncd + age + age_sq + education_years + male + urban  + wealth_index +
  household_size + married_youngchild

mod <- selection(selection = sel,
                 outcome   = out,
                 data      = d,
                 method    = "ml")   # probit–probit by default

summary(mod)

#tabulate(data$ncd)
#table(data$ncd)




