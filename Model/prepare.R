# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())

library(tidyverse)
library(stargazer)
library(Hmisc)

# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# set data dir, load theme and functions
source("/home/ozzy/Documents/CEU/da_case_studies/ch00-tech-prep/theme_bg.R")
source("/home/ozzy/Documents/CEU/da_case_studies/ch00-tech-prep/da_helper_functions.R")

# data used
#source("set-data-directory.R") #data_dir must be first defined #
d_dir <- "/home/ozzy/Documents/CEU/DA3/Assignment 1"
data_in <- paste(d_dir,"Data","clean/", sep = "/")
data_out <- data_in
output <- paste0(d_dir,"Output/")

options(digits = 3)


#-------------------------------------------------------
# Import data
data <- read_csv(paste(data_in,"airbnb_barcelona_cleaned.csv", sep = ""))


#cleaning/selecting amenities
data <- data %>% mutate(
  a_balcony = as.integer(data$`Balcony`|data$`Patio or balcony`),
  a_garden = as.integer(data$`Shared fenced garden or backyard`|data$`Garden`|data$`Garden or backyard`),
  a_fireplace = as.integer(data$`Indoor fireplace`|data$`Fireplace guards`),
  a_outdoors = as.integer(data$`Outdoor dining area`| data$`Outdoor dining area_1`|data$`Outdoor furniture`|data$`Outdoor seating`|data$`Outdoor shower`),
  a_pool = as.integer(data$`Pool`|data$`Pool_1`|data$`Pool cover`|data$`Pool table`|data$`Pool toys`),
  a_breakfast = as.integer(data$Breakfast|data$Breakfast_1|data$`Breakfast buffet available   per person per day`|data$`Complimentary breakfast buffet`),
  a_air_conditioning = as.integer(data$`Portable air conditioning`|data$`Air conditioning`|data$`Air conditioning_1`|data$`Central air conditioning`),
  a_parking = as.integer(data$`Free parking garage on premises`|data$`Free parking garage on premises  1 space`| data$`Free parking on premises`| data$`Free parking on premises_1`|data$`Free driveway parking on premises  1 space`|data$`Free carport on premises`|data$`Free street parking`),
  a_working_space = as.integer(data$Office| data$`Dedicated workspace`|data$`Dedicated workspace: desk`| data$`Dedicated workspace: desk and table`| data$`Dedicated workspace: table`),
  a_child_friendly = as.integer(data$Crib|data$`Baby bath`|data$`Baby equipment`|data$`Baby monitor`|data$`Baby safety gates`|data$`Childrens dinnerware`|data$`Childrens books and toys`|data$`Childrens books and toys for ages 5-10 years old and 10+ years old`),
  a_gym = as.integer(data$`Fitness center`|data$Gym),
  a_gaming = as.integer(data$`Game console`|data$`Game room`),
  a_kitchen = as.integer(data$`Full kitchen`|data$`Kitchen`|data$`Kitchenette`),
  a_pets = as.integer(data$`Pets allowed`| data$`Pets allowed_1`)
  )


# keep if property type is Apartment, House or Townhouse
table(data$property_type) %>% sort()
data <- data %>%
  filter(property_type %in% c("Entire apartment", "Private room in apartment", "Entire serviced apartment",
                              "Entire loft", "Private room in condominium", "Entire condominium", 
                              "Private room in loft", "Shared room in apartment"))

table(data$room_type)
# rename Townhouse to House
data$property_type <- "Apartment"


#Room type as factor
table(data$property_type)
table(data$room_type)


#neighbourhood as factor
table(data$neighbourhood_group_cleansed)



#---------------------------------------------------------------------------------------------------------------------------


#create days since first review
data <- data %>%
  mutate(
    n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                as.Date(first_review ,format="%Y-%m-%d")))
# create dummy vars
data <- data %>% select(-(53:413))


# with price info only
data <- data %>%
  drop_na(price)

data <- data %>% select(-1, -9)
write_csv(data, paste0(data_out, "airbnb_barcelona_workfile.csv"))
