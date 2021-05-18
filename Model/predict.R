# ------------------------------------------------------------------------------------------------------
#### SET UP
# It is advised to start a new session for every case study
# CLEAR MEMORY
rm(list=ls())


# Descriptive statistics and regressions
library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(stargazer)
library(xtable)
library(directlabels)
library(knitr)
library(cowplot)
library(modelsummary)

# option A: open material as project
# option B: set working directory for da_case_studies
#           example: setwd("C:/Users/bekes.gabor/Documents/github/da_case_studies/")

# set data dir, load theme and functions
source("/home/ozzy/Documents/CEU/da_case_studies/ch00-tech-prep/theme_bg.R")
source("/home/ozzy/Documents/CEU/da_case_studies/ch00-tech-prep/da_helper_functions.R")

d_dir <- "/home/ozzy/Documents/CEU/DA3/Assignment 1"
data_in <- paste(d_dir,"Data","clean/", sep = "/")
data_out <- data_in
output <- paste0(d_dir,"Output/")


options(digits = 3)

#############
# Load data #
#############

# Used area
data <-
  read_csv("/home/ozzy/Documents/CEU/DA3/Assignment 1/Data/clean/airbnb_barcelona_workfile.csv")


######################
# Quick look at data #
######################
glimpse(data)
skim(data)

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# what to do with missing values?
# 1. drop if no target (already did)
data <- data %>%
  drop_na(price)



# 2. imput when few, not that important
data <- data %>%
  mutate(
    beds = ifelse(is.na(beds), accommodates, beds),
    bedrooms = ifelse(is.na(bedrooms), 1, bedrooms)
  )


to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]


data <- data %>% select(-c(license, review_scores_location, calendar_updated, 
                           neighbourhood, calendar_updated, first_review,
                           last_review, license, n_days_since))
data <- data %>% mutate(
  bathrooms_text = ifelse(is.na(bathrooms_text), 1, bathrooms_text)
)

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

data <- data %>% mutate(
  bathroom_digits = gsub("[[:digit:]]", "", bathrooms_text),
  bathroom_chars = gsub("[aA-zZ]", "", bathrooms_text)
)
data <- data %>% mutate(
  bathrooms_type = ifelse(bathroom_digits %in% c(" bath", " baths", " private bath", ". baths", "Half-bath", "Private half-bath", ""), "Private", "Shared" )
)
data <- data %>% mutate( bathrooms_count = floor(as.numeric(bathroom_chars)))
data <- data %>% mutate( bathrooms_count = ifelse(is.na(bathrooms_count), 1, bathrooms_count))

data <- data %>% select(-bathrooms, -host_since)

data <- data %>% mutate(
  host_response_rate = ifelse(is.na(host_response_rate), 0, host_response_rate),
  host_acceptance_rate = ifelse(is.na(host_acceptance_rate), 0, host_acceptance_rate),
  host_is_superhost = ifelse(is.na(host_is_superhost), 0, host_is_superhost),
  host_listings_count = ifelse(is.na(host_listings_count), 1, host_listings_count),
  host_has_profile_pic = ifelse(is.na(host_has_profile_pic), 0, host_has_profile_pic),
  host_identity_verified = ifelse(is.na(host_identity_verified), 0, host_identity_verified),
  host_total_listings_count = ifelse(is.na(host_total_listings_count), 1, host_total_listings_count)
  
)

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]


# 4. Replace missing variables re reviews with zero, when no review + add flags
data <- data %>%
  mutate(
    flag_review_scores_rating=ifelse(is.na(review_scores_rating),1, 0),
    review_scores_rating =  ifelse(is.na(review_scores_rating), median(review_scores_rating, na.rm = T), review_scores_rating),
      )


to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

data <- data %>% select(-c(review_scores_accuracy, review_scores_cleanliness, review_scores_checkin, 
                           review_scores_communication, review_scores_value, reviews_per_month))
# Look at data
summary(data$price)


data <- data %>% filter(price != 0)
###################################
# Business logic- define our prediction problem
###################################

# Decision
# Size, we need a normal apartment, 1-7persons
data <- data %>%
  filter(accommodates < 7
  )
data <- data %>%
  filter(accommodates > 1
  )


# that's gonna be our sample
skimr::skim(data)


# save workfile
# write.csv(data, paste0(data_out, "airbnb_barcelona_work_to_predict.csv"), row.names = F)


#####################################
# Look at some descriptive statistics
#####################################

#How is the average price changing in my district by `property_type`, `room_type` and the `bed_type`?
data %>%
  group_by(property_type, room_type) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE)) 

data %>%
  group_by(neighbourhood_group_cleansed, property_type, room_type) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE)) %>% arrange(-mean_price)

data <- data %>% select(-bathroom_digits, -bathroom_chars, -host_response_rate, -host_acceptance_rate)

Hmisc::describe(data$price)

datau <- data %>% filter(price <= 200)
# Distribution of price by type below 400

# Histograms
# price
g1a <- ggplot(data=datau, aes(x=price)) +
  geom_histogram_da(type="percent", binwidth = 10) +
  #geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 10, boundary=0,
  #               color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F,  na.rm=TRUE) +
  #  coord_cartesian(xlim = c(0, 400)) +
  labs(x = "Price (US dollars)",y = "Percent")+
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.16), breaks = seq(0, 0.16, by = 0.02), labels = scales::percent_format(1)) +
  scale_x_continuous(expand = c(0.00,0.00),limits=c(0,220), breaks = seq(0,220, 20)) +
  theme_bg()  
g1a

# lnprice
data$ln_price <- log(data$price)
datau$ln_price <- log(datau$price)
g1b<- ggplot(data=datau, aes(x=ln_price)) +
  geom_histogram_da(type="percent", binwidth = 0.2) +
  #  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.18,
  #               color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F,  na.rm=TRUE) +
  coord_cartesian(xlim = c(2, 5.5)) +
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.02), labels = scales::percent_format(5L)) +
  scale_x_continuous(expand = c(0.00,0.00),limits = c(0, 6), breaks = seq(0.2, 5.5, 0.2)) +
  labs(x = "ln(price, US dollars)",y = "Percent")+
  theme_bg() 
g1b



## Boxplot of price by room type
g2 <- ggplot(data = datau, aes(x = room_type, y = price)) +
  stat_boxplot(aes(group = room_type), geom = "errorbar", width = 0.3,
               color = c(color[2],color[1], color[3]), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = room_type),
               color = c(color[2],color[1], color[3]), fill = c(color[2],color[1], color[3]),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,200), breaks = seq(0,200,50)) +
  labs(x = "Room type",y = "Price (US dollars)")+
  theme_bg()
g2

# Boxplot
g3 <- ggplot(datau, aes(x = factor(accommodates), y = price,
                        fill = factor(room_type), color=factor(room_type))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c(color[2],color[1], color[3])) +
  scale_fill_manual(name="",
                    values=c(color[2],color[1], color[3])) +
  labs(x = "Accomodates (Persons)",y = "Price (US dollars)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 200), breaks = seq(0,200, 50))+
  theme_bg() +
  theme(legend.position = c(0.3,0.8)        )
g3

g4 <- ggplot(datau, aes(x = factor(neighbourhood_group_cleansed), y = price,
                        fill = factor(room_type), color=factor(room_type))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c(color[2],color[1], color[3])) +
  scale_fill_manual(name="",
                    values=c(color[2],color[1], color[3])) +
  labs(x = "Neighbourhoods",y = "Price (US dollars)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 200), breaks = seq(0,200, 50))+
  theme_bg() +
  theme(legend.position = c(0.3,0.8)        )
g4



########################################
# PART II.
########################################


#####################
# Setting up models #
#####################

# Basic Variables
basic_var  <- c("accommodates", "bedrooms", "room_type", "bathrooms_type","bathrooms_count", "neighbourhood_group_cleansed" )
Hmisc::describe(data$host_total_listings_count)
data <- data %>% mutate(host_professional = ifelse(host_total_listings_count > 3, "Professional", "Individual"))
host_var <- c("host_is_superhost", "host_has_profile_pic", "host_identity_verified", "host_professional")
Hmisc::describe(data$availability_90)
Hmisc::describe(data$availability_365)
data <- data %>% mutate(seeked = ifelse(availability_365 > 300, 0, 1))
Hmisc::describe(data$number_of_reviews)
data <- data %>% mutate(is_reviewed = ifelse(number_of_reviews > 0, 1, 0))
reviews <- c("seeked", "number_of_reviews", "review_scores_rating", "is_reviewed")
amenities <- c("a_balcony", "a_garden", "a_fireplace", "a_outdoors", "a_pool", "a_breakfast", "a_air_conditioning",
               "a_parking", "a_working_space", "a_child_friendly", "a_gym", "a_gaming", "a_kitchen",
               "a_pets")

#################################################
# Look for interactions
data <- data %>% filter(room_type != "Shared room")

Hmisc::describe(data$price)

#Look up room type interactions
p1 <- price_diff_by_variables2(data, "room_type", "a_child_friendly", "Room Type", "Child Friendly")
p2 <- price_diff_by_variables2(data, "room_type", "a_pets", "Room Type", "Pets")
p3 <- price_diff_by_variables2(data, "bathrooms_type", "seeked", "Bathroom Type", "Availability Last Year")
p4 <- price_diff_by_variables2(data, "room_type", "host_professional", "Room Type", "Is Host Has Multiple Listings")
p5 <- price_diff_by_variables2(data, "host_has_profile_pic", "is_reviewed", "Host Has Profile Pic", "Is Reviewed")
p6 <- price_diff_by_variables2(data, "a_pool", "a_outdoors", "Pool Facilities", "Outdoor Facilities")

g_interactions <- plot_grid(p1, p2, p3, p4, p5, p6, nrow=3, ncol=2)
g_interactions



# dummies suggested by graphs
X1  <- c("bathrooms_type*seeked",  "room_type*a_child_friendly", "a_pool*a_outdoors", "host_has_profile_pic*is_reviewed")


# Create models in levels models: 1-8
modellev1 <- " ~ accommodates"
modellev2 <- paste0(" ~ ",paste(basic_var,collapse = " + "))
modellev3 <- paste0(" ~ ",paste(c(basic_var, "neighbourhood_group_cleansed",reviews),collapse = " + "))
modellev4 <- paste0(" ~ ",paste(c(basic_var,"neighbourhood_group_cleansed",reviews, host_var),collapse = " + "))
modellev5 <- paste0(" ~ ",paste(c(basic_var,"neighbourhood_group_cleansed",reviews, host_var,X1),collapse = " + "))
modellev6 <- paste0(" ~ ",paste(c(basic_var,"neighbourhood_group_cleansed",reviews, host_var,X1,amenities),collapse = " + "))

#################################
# Separate hold-out set #
#################################

# create a holdout set (20% of observations)
smp_size <- floor(0.2 * nrow(data))

# Set the random number generator: It will make results reproducable
set.seed(12345678)

# create ids:
# 1) seq_len: generate regular sequences
# 2) sample: select random rows from a table
holdout_ids <- sample(seq_len(nrow(data)), size = smp_size)
data$holdout <- 0
data$holdout[holdout_ids] <- 1

#Hold-out set Set
data_holdout <- data %>% filter(holdout == 1)

#Working data set
data_work <- data %>% filter(holdout == 0)


##############################
#      cross validation      #
##############################

## N = 5
n_folds=5
# Create the folds
set.seed(123456789)

folds_i <- sample(rep(1:n_folds, length.out = nrow(data_work) ))
# Create results
model_results_cv <- list()


for (i in (1:6)){
  model_name <-  paste0("modellev",i)
  model_pretty_name <- paste0("(",i,")")
  
  yvar <- "ln_price"
  xvars <- eval(parse(text = model_name))
  formula <- formula(paste0(yvar,xvars))
  
  # Initialize values
  rmse_train <- c()
  rmse_test <- c()
  
  model_work_data <- lm(formula,data = data_work)
  BIC <- BIC(model_work_data)
  nvars <- model_work_data$rank -1
  r2 <- summary(model_work_data)$r.squared
  
  # Do the k-fold estimation
  for (k in 1:n_folds) {
    test_i <- which(folds_i == k)
    # Train sample: all except test_i
    data_train <- data_work[-test_i, ]
    # Test sample
    data_test <- data_work[test_i, ]
    # Estimation and prediction
    model <- lm(formula,data = data_train)
    prediction_train <- predict(model, newdata = data_train)
    prediction_test <- predict(model, newdata = data_test)
    
    # Criteria evaluation
    rmse_train[k] <- mse_lev(prediction_train, data_train[,yvar] %>% pull)**(1/2)
    rmse_test[k] <- mse_lev(prediction_test, data_test[,yvar] %>% pull)**(1/2)
    
  }
  
  model_results_cv[[model_name]] <- list(yvar=yvar,xvars=xvars,formula=formula,model_work_data=model_work_data,
                                         rmse_train = rmse_train,rmse_test = rmse_test,BIC = BIC,
                                         model_name = model_pretty_name, nvars = nvars, r2 = r2)
}

model <- lm(formula,data = data_train)
prediction_train <- predict(model, newdata = data_train)
prediction_test <- predict(model, newdata = data_test)

#skim(data_train$ln_days_since)

t1 <- imap(model_results_cv,  ~{
  as.data.frame(.x[c("rmse_test", "rmse_train")]) %>%
    dplyr::summarise_all(.funs = mean) %>%
    mutate("model_name" = .y , "model_pretty_name" = .x[["model_name"]] ,
           "nvars" = .x[["nvars"]], "r2" = .x[["r2"]], "BIC" = .x[["BIC"]])
}) %>%
  bind_rows()
t1
column_names <- c("Model", "N predictors", "R-squared", "BIC", "Training RMSE",
                  "Test RMSE")

# Nice table produced and saved as .tex without \beign{table}
# -R2, BIC on full work data-n.
# -In sample rmse: average on training data; avg test : average on test data

t14_2 <- t1 %>%
  select("model_pretty_name", "nvars", "r2" , "BIC", "rmse_train", "rmse_test")
colnames(t14_2) <- column_names
print(xtable(t14_2, type = "latex", digits=c(0,0,0,2,0,2,2)), file = paste0(output, "ch14_table_fit_level.tex"),
      include.rownames=FALSE, booktabs=TRUE, floating = FALSE)



# RMSE training vs test graph
t1_levels <- t1 %>%
  dplyr::select("nvars", "rmse_train", "rmse_test") %>%
  gather(var,value, rmse_train:rmse_test) %>%
  mutate(nvars2=nvars+1) %>%
  mutate(var = factor(var, levels = c("rmse_train", "rmse_test"),
                      labels = c("RMSE Training","RMSE Test")))

model_result_plot_levels <- ggplot(data = t1_levels,
                                   aes(x = factor(nvars2), y = value, color=factor(var), group = var)) +
  geom_line(size=1,show.legend=FALSE, na.rm = TRUE) +
  scale_color_manual(name="",
                     values=c(color[2],color[1])) +
  scale_y_continuous(name = "RMSE", limits = c(0.5, 0.65), breaks = seq(0.5,0.65, 0.075)) +
  scale_x_discrete( name = "Number of coefficients", expand=c(0.01, 0.01)) +
  geom_dl(aes(label = var),  method = list("last.points", dl.trans(x=x-1), cex=0.4)) +
  #scale_colour_discrete(guide = 'none') +
  theme_bg()
model_result_plot_levels
#save_fig("ch14-figure-7-airbnb-model-result-levels", output, "small")


#modellev5 <- paste0(" ~ ",paste(c(basic_var,"neighbourhood_group_cleansed",reviews, host_var,X1),collapse = " + "))
#modellev6 <- paste0(" ~ ",paste(c(basic_var,"neighbourhood_group_cleansed",reviews, host_var,X1,amenities),collapse = " + "))

#################################
#           LASSO               #
#################################

# take model 8 (and find observations where there is no missing data)may
#vars_model_7 <- c("ln_price", "neighbourhood_group_cleansed",reviews, host_var,X1)
vars_model_8 <- c("ln_price", "neighbourhood_group_cleansed",reviews, host_var,X1,amenities)

# Set lasso tuning parameters
train_control <- trainControl(method = "cv", number = n_folds)
tune_grid <- expand.grid("alpha" = c(1), "lambda" = seq(0.05, 1, by = 0.05))

# We use model 7 without the interactions so that it is easy to compare later to post lasso ols
formula <- formula(paste0("ln_price ~ ", paste(setdiff(vars_model_8, "price"), collapse = " + ")))

set.seed(1234)
lasso_model <- caret::train(formula,
                            data = data_work,
                            method = "glmnet",
                            preProcess = c("center", "scale"),
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            na.action=na.exclude)

print(lasso_model$bestTune$lambda)

lasso_coeffs <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `1`)  # the column has a name "1", to be renamed

print(lasso_coeffs)

lasso_coeffs_nz<-lasso_coeffs %>%
  filter(coefficient!=0)
print(nrow(lasso_coeffs_nz))

# Evaluate model. CV error:
lasso_cv_rmse <- lasso_model$results %>%
  filter(lambda == lasso_model$bestTune$lambda) %>%
  dplyr::select(RMSE)
print(lasso_cv_rmse[1, 1])

# Note: re book
# The textbook contains a somewhat different table and graph for train and test RMSE. 
# The ordering is the same but the numbers are not. This is because of some minor change in cleaing file. Sory. 


########################################
# PART III.
########################################



###################################################
# Diagnsotics #
###################################################
model2_level <- model_results_cv[["modellev2"]][["model_work_data"]]
model6_level <- model_results_cv[["modellev6"]][["model_work_data"]]
model4_level <- model_results_cv[["modellev4"]][["model_work_data"]]
model5_level <- model_results_cv[["modellev5"]][["model_work_data"]]


# look at holdout RMSE
model6_level_work_rmse <- mse_lev(predict(model6_level, newdata = data_work), data_work[,"ln_price"] %>% pull)**(1/2)
model6_level_holdout_rmse <- mse_lev(predict(model6_level, newdata = data_holdout), data_holdout[,"ln_price"] %>% pull)**(1/2)
model6_level_holdout_rmse

model2_level_work_rmse <- mse_lev(predict(model2_level, newdata = data_work), data_work[,"ln_price"] %>% pull)**(1/2)
model2_level_holdout_rmse <- mse_lev(predict(model2_level, newdata = data_holdout), data_holdout[,"ln_price"] %>% pull)**(1/2)
model2_level_holdout_rmse
model4_level_work_rmse <- mse_lev(predict(model4_level, newdata = data_work), data_work[,"ln_price"] %>% pull)**(1/2)
model4_level_holdout_rmse <- mse_lev(predict(model4_level, newdata = data_holdout), data_holdout[,"ln_price"] %>% pull)**(1/2)
model4_level_holdout_rmse
model5_level_work_rmse <- mse_lev(predict(model5_level, newdata = data_work), data_work[,"ln_price"] %>% pull)**(1/2)
model5_level_holdout_rmse <- mse_lev(predict(model5_level, newdata = data_holdout), data_holdout[,"ln_price"] %>% pull)**(1/2)
model5_level_holdout_rmse




###################################################
# FIGURES FOR FITTED VS ACTUAL OUTCOME VARIABLES #
###################################################

# Target variable
Ylev <- data_holdout[["ln_price"]]

meanY <-mean(Ylev)
sdY <- sd(Ylev)
meanY_m2SE <- meanY -1.96 * sdY
meanY_p2SE <- meanY + 1.96 * sdY
Y5p <- quantile(Ylev, 0.05, na.rm=TRUE)
Y95p <- quantile(Ylev, 0.95, na.rm=TRUE)

# Predicted values
predictionlev_holdout_pred <- as.data.frame(predict(model6_level, newdata = data_holdout, interval="predict")) %>%
  rename(pred_lwr = lwr, pred_upr = upr)
predictionlev_holdout_conf <- as.data.frame(predict(model6_level, newdata = data_holdout, interval="confidence")) %>%
  rename(conf_lwr = lwr, conf_upr = upr)

predictionlev_holdout <- cbind(data_holdout[,c("ln_price","accommodates")],
                               predictionlev_holdout_pred,
                               predictionlev_holdout_conf[,c("conf_lwr","conf_upr")])


# Create data frame with the real and predicted values
d <- data.frame(ylev=Ylev, predlev=predictionlev_holdout[,"fit"] )
# Check the differences
d$elev <- d$ylev - d$predlev

# Plot predicted vs price
level_vs_pred <- ggplot(data = d) +
  geom_point(aes(y=ylev, x=predlev), color = color[1], size = 1,
             shape = 16, alpha = 0.7, show.legend=FALSE, na.rm=TRUE) +
  #geom_smooth(aes(y=ylev, x=predlev), method="lm", color=color[2], se=F, size=0.8, na.rm=T)+
  geom_segment(aes(x = 2, y = 2, xend = 10, yend =10), size=0.5, color=color[2], linetype=2) +
  coord_cartesian(xlim = c(2, 10), ylim = c(2, 10)) +
  scale_x_continuous(expand = c(0.01,0.01),limits=c(2, 10), breaks=seq(2, 10, by=1)) +
  scale_y_continuous(expand = c(0.01,0.01),limits=c(2, 10), breaks=seq(2, 10, by=1)) +
  labs(y = "log(Price)", x = "Predicted log(price)") +
  theme_bg() 
level_vs_pred
save_fig("ch14-figure-8a-level-vs-pred", output, "small")


# Redo predicted values at 80% PI
predictionlev_holdout_pred <- as.data.frame(predict(model6_level, newdata = data_holdout, interval="predict", level=0.8)) %>%
  rename(pred_lwr = lwr, pred_upr = upr)
predictionlev_holdout_conf <- as.data.frame(predict(model6_level, newdata = data_holdout, interval="confidence", level=0.8)) %>%
  rename(conf_lwr = lwr, conf_upr = upr)

predictionlev_holdout <- cbind(data_holdout[,c("ln_price","accommodates")],
                               predictionlev_holdout_pred,
                               predictionlev_holdout_conf[,c("conf_lwr","conf_upr")])

summary(predictionlev_holdout_pred)

predictionlev_holdout_summary <-
  predictionlev_holdout %>%
  group_by(accommodates) %>%
  dplyr::summarise(fit = mean(fit, na.rm=TRUE), pred_lwr = mean(pred_lwr, na.rm=TRUE), pred_upr = mean(pred_upr, na.rm=TRUE),
                   conf_lwr = mean(conf_lwr, na.rm=TRUE), conf_upr = mean(conf_upr, na.rm=TRUE))

kable(x = predictionlev_holdout_summary, format = "latex", booktabs=TRUE,  digits = 3, row.names = FALSE,
      linesep = "", col.names = c("Accomodates","Prediction","Pred. interval lower",
                                  "Pred. interval upper","Conf.interval lower","Conf.interval upper")) %>%
  cat(.,file= paste0(output, "modellev6_holdout_summary.tex"))


F14_CI_n_accomodate <- ggplot(predictionlev_holdout_summary, aes(x=factor(accommodates))) +
  geom_bar(aes(y = fit ), stat="identity",  fill = color[1], alpha=0.7 ) +
  geom_errorbar(aes(ymin=pred_lwr, ymax=pred_upr, color = "Pred. interval"),width=.2) +
  #geom_errorbar(aes(ymin=conf_lwr, ymax=conf_upr, color = "Conf. interval"),width=.2) +
  scale_y_continuous(name = "Predicted price (US dollars)") +
  scale_x_discrete(name = "Accomodates (Persons)") +
  scale_color_manual(values=c(color[2], color[2])) +
  theme_bg() +
  theme(legend.title= element_blank(),legend.position="none")
F14_CI_n_accomodate
save_fig("ch14-figure-8b-ci-n-accomodate", output, "small")


