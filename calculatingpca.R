# Purpose: Build a single household wealth indicator using Principal
# Component Analysis

# installing packages
library(haven)  
library(dplyr)  
library(stringr) 
library(labelled) 
library(corrr) 
library(GGally) 

setwd("/Users/jenishneupane/Desktop/Folders/Data/Nepal Living Standard Surveys I-IV/NLSSIV_2022-23/NLSSIV_2022-23/data/stata format")

#loading the dataset
housing_quality <- read_dta("housing_quality_PCA.dta")
utilities <- read_dta("utilities.dta")
durables <- read_dta("durables.dta")




#merging wtr psu, hh_number
full_data <- housing_quality %>%
  left_join(utilities, by = c("psu_number", "hh_number")) %>%
  left_join(durables, by = c("psu_number", "hh_number"))


#removing variables that isn't required
pca_data <- full_data %>%
  select(-household_size, -idcode.x, -idcode.y)

# Remove constant columns (zero variance), there was a column containing observation that were 
# not different which led trouble to calculate PCA since there isn't any variations
pca_data <- pca_data %>%
  select(where(~ var(.) > 0))


# View(pca_data)

#giving the PCA result
pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)

pca_result

# combining it with dataset
pca_data$wealth_index <- pca_result$x[, 1]

summary (pca_data$wealth_index)


# checking if the co-relation is correct or not (it may not be correct; if this is the case, researchers flip the sign)
loadings <- pca_result$rotation[, 1]  # PC1 loadings
sort(loadings)

# flipping the sign (can be done, after looking into the observations; widely used method in PCA)
pca_data$wealth_index <- -1 * pca_result$x[, 1]

# checking correlation with high value variables and low value variable
cor(pca_data$wealth_index, pca_data$computer, use = "complete.obs")
cor(pca_data$wealth_index, pca_data$fridge, use = "complete.obs")
cor(pca_data$wealth_index, pca_data$mobile, use = "complete.obs")
cor(pca_data$wealth_index, pca_data$crowding_z, use = "complete.obs")  # should be negative


# checking co-relation with multiple high and low value variables at once
pca_data %>%
  select(wealth_index, computer, fridge, mobile, car, cook_firewood, crowding_z) %>%
  correlate() %>%
  focus(wealth_index) %>%
  arrange(desc(wealth_index))


# checking co-relations visually
ggpairs(pca_data %>%
          select(wealth_index, computer, fridge, mobile, car, cook_firewood, crowding_z))

finaldata <- pca_data %>%
  select(psu_number, hh_number, wealth_index)

View(finaldata)

write_dta(finaldata, "wealth_index.dta")
