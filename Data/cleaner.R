### Barcelona Airbnb Listing Cleaner

# clearing the memory first
rm(list=ls())

# CHANGE TO YOUR WORKING DIRECTORY
dir<-"/home/ozzy/Documents/CEU/DA3/Assignment 1/Data"

#location folders
data_in  <- paste0(dir,"/raw/")
data_out <- paste0(dir,"/clean/")

library(tidyverse)

# step zero, dropping some columns
# can be skipped. Data can be loaded directly if available
data<-read.csv(paste0(data_in,"listings.csv"))
drops <- c("host_thumbnail_url","host_picture_url","listing_url","thumbnail_url","medium_url","picture_url","xl_picture_url","host_url","last_scraped","description", "experiences_offered", "neighborhood_overview", "notes", "transit", "access", "interaction", "house_rules", "host_about", "host_response_time", "name", "summary", "space", "host_location")
data<-data[ , !(names(data) %in% drops)]
write.csv(data,file=paste0(data_in,"airbnb_barcelona_listing.csv"))

#####################################

# opening dataset
df<-read.csv(paste0(data_in,"airbnb_barcelona_listing.csv"),
             sep=",",header = TRUE, stringsAsFactors = FALSE)

#####################
#formatting columns

#remove percentage signs
for (perc in c("host_response_rate","host_acceptance_rate")){
  df[[perc]]<-gsub("%","",as.character(df[[perc]]))
}

#remove dollar signs from price variable
df$price <- gsub("\\$", "", df$price)


#format binary variables
for (binary in c("host_is_superhost","host_has_profile_pic","host_identity_verified",
                 "instant_bookable", "has_availability")){
  df[[binary]][df[[binary]]=="f"] <- 0
  df[[binary]][df[[binary]]=="t"] <- 1
}
df <- df %>% select(-(c(1, 3, 4, 5, 10, 19, 20, 59, 60, 61, 62)))
#amenities
df$amenities<-gsub("\\[","",df$amenities)
df$amenities<-gsub("\\]","",df$amenities)
df$amenities<-gsub('\\"',"",df$amenities)
df$amenities<-gsub('\\\\u2013',"",df$amenities)
df$amenities<-gsub('\\\\u00a0',"",df$amenities)
df$amenities<-gsub('\\\\u2014',"",df$amenities)
df$amenities<-gsub('\\\\u2019',"",df$amenities)
df$amenities<-gsub('\\\\u2019s',"",df$amenities)
df$amenities<-gsub('\\\\u2019n',"",df$amenities)
df$amenities<-gsub('\\\\u20ac23/',"",df$amenities)
df$amenities<-gsub('\\\\u20ac20/',"",df$amenities)
df$amenities<-gsub('\\\\u20ac25/',"",df$amenities)
df$amenities<-gsub('\\\\u20ac12',"",df$amenities)
df$amenities<-gsub('\\\\u20ac4',"",df$amenities)
df$amenities<-gsub('\\\\u20ac19',"",df$amenities)

df$amenities<-as.list(strsplit(df$amenities, ","))

#define levels and dummies 
levs <- levels(factor(unlist(df$amenities)))
df<-cbind(df,as.data.frame(do.call(rbind, lapply(lapply(df$amenities, factor, levs), table))))

drops <- c("amenities")
df<-df[ , !(names(df) %in% drops)]

#write csv
write.csv(df,file=paste0(data_out,"airbnb_barcelona_cleaned.csv"))
