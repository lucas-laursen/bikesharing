
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggExtra)) install.packages("ggExtra", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom)", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(dplyr)
library(caret)
library(data.table)
library(magrittr)
library(lubridate)
library(ggExtra)
library(cowplot)
library(randomForest)
library(broom)

# Bike Sharing Dataset:
# https://archive.ics.uci.edu/ml/datasets/bike+sharing+dataset

#Downloading the data and putting it into a dataframe
dl <- tempfile()
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip", dl)
hour <- as.data.frame(read.csv(unzip(dl, "hour.csv")))

### Data exploration ###

#What variables are there and what data types are they?
str(hour)

#Are there missing values we should know about?
table(is.na(hour))

#Let's visualize each feature and choose some to focus on in our subsequent modeling

# Quick view
par(mfrow=c(2,6))
for(i in 3:14) {
  hist(hour[,i], main = names(hour)[i])
}

# More detail
# plot total counts per hour
cnt_dteday<-ggplot(hour,aes(x=dteday,y=cnt))+
  geom_point(alpha=0.07)+
  labs(x='Date', y= 'Usage per hour')
cnt_dteday

# plot total counts per month
cnt_mnth<-ggplot(hour,aes(x=mnth,y=cnt))+
  geom_col()+
  labs(x='Month', y= 'Usage')
cnt_mnth

# what about the difference between registered and casual users?

regcnt_mnth<-ggplot(hour,aes(x=mnth,y=registered))+
  geom_col()+
  labs(x='Month', y= 'Registered usage')
cascnt_mnth<-ggplot(hour,aes(x=mnth,y=casual))+
  geom_col()+
  labs(x='Month', y= 'Casual usage')
l_mnth <- list(regcnt_mnth, cascnt_mnth)
plot_grid(plotlist = l_mnth, nrow = 2)

# there's a small but noticeable difference, so we'll ask the same question of our other features

# plot separate registered count per weekday + casual count per weekday
regcnt_weekday<-ggplot(hour,aes(x=weekday,y=registered))+
  geom_col()+
  labs(x='Weekday', y= 'Registered Usage')
cascnt_weekday<-ggplot(hour,aes(x=weekday,y=casual))+
  geom_col()+
  labs(x='Weekday', y= 'Casual Usage')
l_weekday <- list(regcnt_weekday, cascnt_weekday)
plot_grid(plotlist = l_weekday, nrow = 2)

# plot counts per hour of day
regcnt_hour<-ggplot(hour,aes(x=hr,y=registered))+
  geom_col()+
  labs(x='Hour', y= 'Registered Usage')
cascnt_hour<-ggplot(hour,aes(x=hr,y=casual))+
  geom_col()+
  labs(x='Hour', y= 'Casual Usage')
l_weekday <- list(regcnt_hour, cascnt_hour)
plot_grid(plotlist = l_weekday, nrow = 2)

# plot counts per weather situation
regcnt_weather<-ggplot(hour,aes(x=weathersit,y=registered))+
  geom_col()+
  labs(x='Weather type: - 1: Mostly clear
- 2: Mist + Cloudy
- 3: Light precipitation
- 4: Heavy precipitation + Fog', y= 'Casual Usage')
cascnt_weather<-ggplot(hour,aes(x=weathersit,y=casual))+
  geom_col()+
  labs(x='Weather type: - 1: Mostly clear
- 2: Mist + Cloudy
- 3: Light precipitation
- 4: Heavy precipitation + Fog', y= 'Casual Usage')
l_weather <- list(regcnt_weather, cascnt_weather)
plot_grid(plotlist = l_weather, nrow = 2)

# plot count vs. atemp (note we are using this and ignoring the temp variable)
regcnt_atemp<-ggplot(hour,aes(x=atemp,y=registered))+
  geom_point(alpha=0.07)+
  labs(x='Normalized Adjusted Temperature.', y= 'Registered Usage per hour')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')
cascnt_atemp<-ggplot(hour,aes(x=atemp,y=casual))+
  geom_point(alpha=0.07)+
  labs(x='Normalized Adjusted Temperature.', y= 'Casual Usage per hour')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')
l_atemp <- list(regcnt_atemp, cascnt_atemp)
plot_grid(plotlist = l_atemp, nrow = 2)

# plot count vs. humidity
regcnt_hum<-ggplot(hour,aes(x=hum,y=registered))+
  geom_point(alpha=0.07)+
  labs(x='Humidity', y= 'Registered usage per hour')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')
cascnt_hum<-ggplot(hour,aes(x=hum,y=casual))+
  geom_point(alpha=0.07)+
  labs(x='Humidity', y= 'Casual usage per hour')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')
l_hum <- list(regcnt_hum, cascnt_hum)
plot_grid(plotlist = l_hum, nrow = 2)

# plot count vs. windspeed
regcnt_wind<-ggplot(hour,aes(x=windspeed,y=registered))+
  geom_point(alpha=0.07)+
  labs(x='Normalized windspeed', y= 'Registered usage per hour')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')
cascnt_wind<-ggplot(hour,aes(x=windspeed,y=casual))+
  geom_point(alpha=0.07)+
  labs(x='Normalized windspeed', y= 'Casual usage per hour')+
  geom_smooth(method='auto')+
  geom_smooth(method='lm',color= 'red')
l_wind <- list(regcnt_wind, cascnt_wind)
plot_grid(plotlist = l_wind, nrow = 2)

# So, to review, there are time dependencies:

l_time<-list(regcnt_mnth, cascnt_mnth, regcnt_weekday, cascnt_weekday, regcnt_hour, cascnt_hour)
plot_grid(plotlist = l_time, nrow = 3)

# and weather dependencies:

l_wx<-list(regcnt_weather, cascnt_weather, regcnt_atemp, cascnt_atemp, regcnt_hum, cascnt_hum, regcnt_wind, cascnt_wind)
plot_grid(plotlist = l_wx, nrow = 4)


# Looks like we'll want to build separate models for registered and causal users, though there are so many more casual users than registered users that it may not make a huge difference.
# We clearly need to take into account the hour of the day, the day of the week, and the day of the year .
# Those are cyclical.
# We'll also want to take weather into account, via the temp & humidity variables at least, because of their larger effects. Windspeed seems like less of a priority.


### Data wrangling ###

# First, discard things we don't plan to use, such as the index column and temp column
hour <- select(hour,-1, -11)

# Then prepare some of the things we will use. 
# Such as converting the dteday column into R-parsable dates...
hour$dteday <- as_datetime(hour$dteday)

# and extracting the day and week of the year from the dteday column and creating new columns with them
hour <- mutate(hour, day = yday(dteday))
hour <- mutate(hour, week = week(dteday))

# Next step is dividing our data into training and test sets. 
# Unlike the Movielens project, we need to do this one chronologically, since the data is time-dependent. 
# We will make a prediction for the last 10% of the time series based on training on the preceding 90%.

set.seed(1, sample.kind="Rounding") 
train_index <- c(1:round(nrow(hour)*.9))
train_set <- hour[train_index,]
test_set <- hour[-train_index,]

### Data analysis ###

# We can try a linear regression first.

#first define x and y data for regression. x is all the features minus the counts, y is the count
yTrain <- data.matrix(train_set[c(15)])
xTrain <- data.matrix(train_set[-c(13,14,15)])
#make a linear model for the count
fit <- lm(yTrain ~ xTrain, data = train_set)
#measure its mean squared residuals (MSR) as a measure of accuracy--the smaller the better
mean(residuals(fit)^2)

#Does modeling casual and registered users separately do any better?
yTrain <- data.matrix(train_set[c(13)]) #casual count is in column 13
xTrain <- data.matrix(train_set[-c(13,14,15)])
fit_lmcasual <- lm(yTrain ~ xTrain, data = train_set)
mean(residuals(fit_lmcasual)^2)

yTrain <- data.matrix(train_set[c(14)]) #registered count is in column 14
xTrain <- data.matrix(train_set[-c(13,14,15)])
fit_lmreg <- lm(yTrain ~ xTrain, data = train_set)
mean(residuals(fit_lmreg)^2)

# Both models are better than the overall count model and the casual prediction is an order of magnitude better than the registered prediction, which is weird because that dataset is much smaller.

# Now we'll try random forest 
set.seed(415)
fit_rf <- randomForest(train_set$cnt ~ dteday + season + yr + mnth + week + day + hr + holiday + workingday + weathersit + atemp + hum + windspeed, 
                       data = train_set, importance = TRUE, ntree = 300)
# Does it look like we are minimizing the error?
plot(fit_rf)
#Does seem to level off somewhere around 300 trees after a couple of iterations of trying different numbers. 
#Let's look at the results of the model
fit_rf
#MSR 2673

#Now let's check casual and registered users separately
set.seed(415)
fit_rf_cas <- randomForest(train_set$casual ~ season + yr + workingday + holiday + atemp + hum + windspeed + weathersit, 
                       data = train_set, importance = TRUE, ntree = 300)
plot(fit_rf_cas)
fit_rf_cas
#MSR: 810
fit_rf_reg <- randomForest(train_set$registered ~ season + yr + workingday + holiday + atemp + hum + windspeed + weathersit, 
                           data = train_set, importance = TRUE, ntree = 300)
plot(fit_rf_reg)
fit_rf_reg
#Mean Squared Residual (MSR) is 14,863, way worse than combined

# Now we'll have a look at the root mean square error (RMSE) for the three rF models

# Defining a function to calculate the root mean square error
RMSE <- function(true_cnt, predicted_cnt){sqrt(mean((true_cnt - predicted_cnt)^2))}

  ####SOMETHING IS WRONG WITH THE SIZE OF THESE VECTORS
  #I would have wanted to calculate RMSE for the linear model, too
  lmpredictions <-  predict(fit,newdata = test_set)
  lm_RMSE <- RMSE(lmpredictions, test_set$cnt)
  #Casual riders
  lm_caspredictions <-  predict(fit_lmcascasual,newdata = test_set)
  lm_cas_RMSE <- RMSE(lmf_caspredictions, test_set$casual)
  #Registered riders
  lm_regpredictions <- predict(fit_lm_reg,newdata = test_set)
  lm_reg_RMSE <- RMSE(lm_regpredictions, test_set$registered)
  #######################################################
  
#Overall count
rfpredictions <-  predict(fit_rf,newdata = test_set)
rf_RMSE <- RMSE(rfpredictions, test_set$cnt)
#Casual riders
rf_caspredictions <-  predict(fit_rf_cas,newdata = test_set)
rf_cas_RMSE <- RMSE(rf_caspredictions, test_set$casual)
#Registered riders
rf_regpredictions <- predict(fit_rf_reg,newdata = test_set)
rf_reg_RMSE <- RMSE(rf_regpredictions, test_set$registered)

results <- tibble(rf_RMSE, rf_cas_RMSE, rf_reg_RMSE)

#Based on lit search, could also try:
#decision tree, knn, conditional inference tree, boosted regression, neural network

# But for now let's examine feature importance
varImpPlot(fit_rf)
varImpPlot(fit_rf_cas)
varImpPlot(fit_rf_reg)







