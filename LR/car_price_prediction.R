#############################################################
# Linear Regression Assignment: Car Price Prediction Model
# Submitted by: Sharadananda Mondal
# Date of Submission: 23.07.2017
#############################################################
library(tidyr)
library(dplyr)
library(MASS) # stepAIC
library(car) # VIF 
library(ggplot2)

setwd('~/Desktop/pgdda/c3-pa-1/lr-assignment/intermediate/')
cars <- read.csv('../input/CarPrice_Assignment.csv')

str(cars)
#View(cars)
summary(cars)

colnames(cars) <- tolower(colnames(cars))
# check for duplicated rows
cars[which(duplicated(cars)), ] # no duplicated rows
# check for missing values
sum(is.na(cars)) # no missing values
# segregate carname into company and model
cars <- separate(cars, carname, c('company', 'model'), 
                 sep = '[[:blank:]]', extra = 'merge', fill = 'right')
# exclude model from analysis
cars <- cars[, -c(4)] 
# misspelt company names
table(cars$company)
### correcting misspelt car names
change_company <- function(name){
  changed_name <- name
  if(name=='maxda'){
    changed_name = 'mazda'
  } else if (name=='Nissan') {
    changed_name = 'nissan'
  } else if (name == 'porcshce') {
    changed_name = 'porsche'
  } else if (name == 'toyouta') {
    changed_name = 'toyota'
  } else if (name %in% c('vokswagen', 'vw')){
    changed_name = 'volkswagen'
  } else if (name=='alfa-romero'){
    changed_name = 'alfa-romeo'
  }
  return(changed_name)
}
cars$company <- sapply(cars$company, change_company)
table(cars$company) # company names corrected
#############################################
## outlier treatment  for numeric types ###
#############################################
# helper function to fix outliers 
fix_outliers <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
}
# fix outliers from numeric data fields of cars dataset
cars$wheelbase <- fix_outliers(cars$wheelbase)
cars$carlength <- fix_outliers(cars$carlength)
cars$carwidth <- fix_outliers(cars$carwidth)
cars$enginesize <- fix_outliers(cars$enginesize)
cars$stroke <- fix_outliers(cars$stroke)
cars$compressionratio <- fix_outliers(cars$compressionratio)
cars$horsepower <- fix_outliers(cars$horsepower)
cars$peakrpm <- fix_outliers(cars$peakrpm)
cars$citympg <- fix_outliers(cars$citympg)
cars$highwaympg <- fix_outliers(cars$highwaympg) 
## Clean and tidy number of doors
#cars1 <- cars
levels(cars$doornumber) <- c(4, 2)
cars$doornumber <- as.numeric(levels(cars$doornumber))[cars$doornumber]
## Clean and tidy number of cylinders
levels(cars$cylindernumber) <- c(8, 5, 4, 6, 3, 12, 2)
cars$cylindernumber <- as.numeric(levels(cars$cylindernumber))[cars$cylindernumber]
#################################
## derived metrics  ###
#################################
cars$company <- as.factor(cars$company)
# company origin: categorisation
# European, American, Asian
european <- c('alfa-romeo','audi','bmw','jaguar','peugeot','porsche','renault','saab','volkswagen','volvo')
asian <- c('honda','isuzu','mazda','mitsubishi','nissan','subaru','toyota')
us <- c('buick','chevrolet','dodge','mercury','plymouth')
# # exclude 
cars$origin <- ifelse(cars$company %in% european, 'european', 
                        ifelse(cars$company %in% us, 'us','asian'))
cars$origin <- as.factor(cars$origin)
table(cars$origin)

#################################
## dummy variable creation  ###
#################################
summary(cars[which(sapply(cars, class)=='factor')])
# we have 8 factors types for which we need dummy variables
### 2-lvl: fueltype, aspiration, enginelocation; ###
cols_2 <- c('fueltype', 'aspiration', 'enginelocation')
# helper function for encoding 2 level types giving weightage to popular level type
encode_cols <- function(x){ # popular level types: gas, turbo, front
  if(table(x)[1] > table(x)[2]){
    levels(x) <- c(1,0)  
  } else {
    levels(x) <- c(0,1)
  }
  x <- as.numeric(levels(x))[x]
}
cars[cols_2] <- sapply(cars[cols_2], encode_cols)
sapply(cars[cols_2],table)
# 3-lvl: company, drivewheel
cars1 <- cars
# factor columns
factor_cols <- which(sapply(cars1, is.factor))
## outlier treatment for factor variables
sapply(cars1[,factor_cols], function(x){round(table(x)/nrow(cars1)*100, 2)})
# we should combine obsearvations < 5% together
#cars1$company <- NULL # Since car origin is derived from car company 
fact_cols <- c('carbody', 'enginetype', 'fuelsystem') ## MISSED STEPS
cars1[,fact_cols] <- sapply(cars1[,fact_cols], as.character) ## MISSED STEPS
cars1$fuelsystem <- ifelse(cars1$fuelsystem %in% c('spdi','4bbl','mfi','spfi'),
                           'others', cars1$fuelsystem)
cars1$enginetype <- ifelse(cars1$enginetype %in% c('dohcv','rotor'), 
                           'others', cars1$enginetype)
cars1$carbody <- ifelse(cars1$carbody %in% c('convertible','hardtop'), 
                           'others', cars1$carbody)
cars1[,fact_cols] <- sapply(cars1[,fact_cols], as.factor) ## MISSED STEPS
#sapply(cars1[,factor_cols], function(x){round(table(x)/nrow(cars1)*100, 2)})
###########################################################################
dummy_drivewheel <- model.matrix(~drivewheel, data=cars1)
dummy_drivewheel <- dummy_drivewheel[,-1]
cars1 <- cbind(cars1[,-which(colnames(cars1)=='drivewheel')],dummy_drivewheel)

dummy_origin <- model.matrix(~origin, data=cars1)
dummy_origin <- dummy_origin[,-1]
cars1 <- cbind(cars1[,-which(colnames(cars1)=='origin')],dummy_origin)
# 5-lvl: carbody
dummy_carbody <- model.matrix(~carbody, data=cars1)
dummy_carbody <- dummy_carbody[,-1]
cars1 <- cbind(cars1[,-which(colnames(cars1)=='carbody')],dummy_carbody)

# 7-lvl: fuelsystem, enginetype
# dummy variable creation
dummy_fuelsystem <- model.matrix(~fuelsystem, data=cars1)
dummy_fuelsystem <- dummy_fuelsystem[,-1]
cars1 <- cbind(cars1[,-which(colnames(cars1)=='fuelsystem')],dummy_fuelsystem)
## ENGINE TYPE ##
dummy_enginetype <- model.matrix(~enginetype, data=cars1)
dummy_enginetype <- dummy_enginetype[,-1]
cars1 <- cbind(cars1[,-which(colnames(cars1)=='enginetype')],dummy_enginetype)

# company
dummy_company <- model.matrix(~company, data=cars1)
dummy_company <- dummy_company[,-1]
cars1 <- cbind(cars1[,-which(colnames(cars1)=='company')],dummy_company)

##########################
## Data Modeling ##
########################
#cors <- cor(cars1)
#write.csv(cors, 'cors.csv')
# reproducibility 
set.seed(12345)
# population size
pop_size <- nrow(cars1)
# sample size
ntrain <- sample(1:pop_size, 0.7 * pop_size)
# creating training and testing data sets
train_id <- cars1$car_id[ntrain]
test_id <- cars1$car_id[-ntrain]
cars1 <- cars1[,-which(colnames(cars1)=='car_id')]
train <- cars1[ntrain,]
test <- cars1[-ntrain,]
# model building: model1 - consisting of all variables
model <- lm(price ~ ., data = train) #R-squared:  0.8798
summary(model)
## Using StepAIC to select important features ##
step <- stepAIC(model, direction = 'both')
step
## creating second model based on stepAIC selection
model2 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
              curbweight + cylindernumber + drivewheelfwd + carbodyothers + 
              carbodywagon + fuelsystem2bbl + fuelsystemmpfi + enginetypeohc + 
              enginetypeohcf + enginetypeothers + companyaudi + companybmw + 
              companybuick + companyhonda + companyjaguar + companymercury + 
              companyporsche + companyvolvo, data = train) #R-squared:  0.9441
summary(model2)
## check for multicollinearity an alternative to correlation analysis
vif(model2)
# remove companymercury: it is not significant p-value=0.167642    > 0.05
model3 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
               curbweight + cylindernumber + drivewheelfwd + carbodyothers + 
               carbodywagon + fuelsystem2bbl + fuelsystemmpfi + enginetypeohc + 
               enginetypeohcf + enginetypeothers + companyaudi + companybmw + 
               companybuick + companyhonda + companyjaguar + 
               companyporsche + companyvolvo, data = train) #0.9437
summary(model3)

# remove carbodywagon: p-value=0.110566 > 0.05
model4 <- lm(formula = price ~ symboling + aspiration + enginelocation + 
               curbweight + cylindernumber + drivewheelfwd + carbodyothers + 
               fuelsystem2bbl + fuelsystemmpfi + enginetypeohc + 
               enginetypeohcf + enginetypeothers + companyaudi + companybmw + 
               companybuick + companyhonda + companyjaguar + 
               companyporsche + companyvolvo, data = train)  #0.943
summary(model4)
# remove fuelsystem2bbl: p-value=0.065 > 0.05
model5 <-lm(formula = price ~ symboling + aspiration + enginelocation + 
              curbweight + cylindernumber + drivewheelfwd + carbodyothers + 
              fuelsystemmpfi + enginetypeohc + 
              enginetypeohcf + enginetypeothers + companyaudi + companybmw + 
              companybuick + companyhonda + companyjaguar + 
              companyporsche + companyvolvo, data = train) #0.9419
summary(model5)

# remove companyhonda:  p-value=0.15 > 0.05
model6 <-lm(formula = price ~ symboling + aspiration + enginelocation + 
              curbweight + cylindernumber + drivewheelfwd + carbodyothers + 
              fuelsystemmpfi + enginetypeohc + 
              enginetypeohcf + enginetypeothers + companyaudi + companybmw + 
              companybuick + companyjaguar + 
              companyporsche + companyvolvo, data = train) #0.9414
summary(model6)
vif(model6)

# remove aspiration:  p-value=0.063 > 0.05
model7 <-lm(formula = price ~ symboling + enginelocation + 
              curbweight + cylindernumber + drivewheelfwd + carbodyothers + 
              fuelsystemmpfi + enginetypeohc + 
              enginetypeohcf + enginetypeothers + companyaudi + companybmw + 
              companybuick + companyjaguar + 
              companyporsche + companyvolvo, data = train) #0.9402
summary(model7)
vif(model7)

# remove carbodyothers:  p-value=0.08 > 0.05
model8 <-lm(formula = price ~ symboling + enginelocation + 
              curbweight + cylindernumber + drivewheelfwd + fuelsystemmpfi + enginetypeohc + 
              enginetypeohcf + enginetypeothers + companyaudi + companybmw + 
              companybuick + companyjaguar + 
              companyporsche + companyvolvo, data = train) #0.9393 , not much affected
summary(model8)
vif(model8)

# remove enginetypeohcf:  p-value=0.045 vif:2.127
model9 <-lm(formula = price ~ symboling + enginelocation + 
              curbweight + cylindernumber + drivewheelfwd + fuelsystemmpfi + enginetypeohc + 
              enginetypeothers + companyaudi + companybmw + companybuick + companyjaguar + 
              companyporsche + companyvolvo, data = train) #0.9378 , not much affected
summary(model9)
vif(model9)

# remove enginetypeohc:  p-value=0.22
model10 <-lm(formula = price ~ symboling + enginelocation + 
              curbweight + cylindernumber + drivewheelfwd + fuelsystemmpfi + 
              enginetypeothers + companyaudi + companybmw + companybuick + companyjaguar + 
              companyporsche + companyvolvo, data = train) #0.9376 , not much affected
summary(model10)
vif(model10)

# remove drivewheelfwd:  p-value=0.11; vif:2.15
model11 <-lm(formula = price ~ symboling + enginelocation + 
               curbweight + cylindernumber + fuelsystemmpfi + 
               enginetypeothers + companyaudi + companybmw + companybuick + companyjaguar + 
               companyporsche + companyvolvo, data = train) #0.9368 , not much affected
summary(model11)
vif(model11)

# remove fuelsystemmpfi:  p-value=0.01 >0.001; vif:1.61
model12 <-lm(formula = price ~ symboling + enginelocation + curbweight + cylindernumber + 
               enginetypeothers + companyaudi + companybmw + companybuick + companyjaguar + 
               companyporsche + companyvolvo, data = train) # 0.934 
summary(model12)
vif(model12)

# remove symboling:  p-value=0.0013 >0.001; vif:1.61
model13 <-lm(formula = price ~ enginelocation + curbweight + cylindernumber + 
               enginetypeothers + companyaudi + companybmw + companybuick + companyjaguar + 
               companyporsche + companyvolvo, data = train) # 0.934 
summary(model13)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6839.5  -964.6  -115.9   865.4  8453.0 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        1007.234   2367.886   0.425 0.671258    
# enginelocation   -12521.400   2080.416  -6.019 1.63e-08 ***
#   curbweight            7.181      0.505  14.218  < 2e-16 ***
#   cylindernumber     1039.794    238.094   4.367 2.52e-05 ***
#   enginetypeothers   5077.795   1218.205   4.168 5.52e-05 ***
#   companyaudi        4477.550   1145.564   3.909 0.000148 ***
#   companybmw         8645.515    957.527   9.029 1.82e-15 ***
#   companybuick      13466.898   1244.674  10.820  < 2e-16 ***
#   companyjaguar      8877.167   1625.019   5.463 2.25e-07 ***
#   companyporsche     7386.969   1747.788   4.226 4.40e-05 ***
#   companyvolvo       3320.271    900.400   3.688 0.000330 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2178 on 132 degrees of freedom
# Multiple R-squared:  0.9341,	Adjusted R-squared:  0.9292 
# F-statistic: 187.2 on 10 and 132 DF,  p-value: < 2.2e-16

vif(model13)
# enginelocation       curbweight   cylindernumber enginetypeothers      companyaudi 
# 2.678929         2.100164         2.354893         1.215981         1.075289 
# companybmw     companybuick    companyjaguar   companyporsche     companyvolvo 
# 1.110670         1.575330         1.634471         3.106259         1.137416 

################################################# 
## predicting the car price in test dataset  ##
#################################################
predicted_price <- predict(model13, test[,-which(colnames(test)=='price')])
test$test_price <- predicted_price

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared #0.85

# Plot - Actual vs Predicted Views Model9
# residual plot
residualPlot(model13)
# data frame for predicted values plotting
d <- data.frame(car_id=test_id, actual=test$price, predicted=test$test_price)
d$residuals <- d$actual - d$predicted
# plotting predicted and actual values
# size and color
ggplot(d, aes(car_id, actual)) +
  geom_smooth(method = 'lm', se = FALSE, color='lightgrey') +
  geom_segment(aes(xend=car_id, yend=predicted), alpha=0.2) +
  geom_point(aes(color=abs(residuals), size=abs(residuals))) + # alpha mapped to absolute residuals
  scale_color_continuous(low = 'black', high = 'red') + # color mapped here
  guides(color=F, size=F) + # colour legends removed 
  labs(x='car id', y='price', title='actual vs predicted price (hollow circle)') +
  geom_point(aes(y=predicted), shape=1) +
  theme_bw()
# different colors
d$cylindernumber <- test$cylindernumber
ggplot(d, aes(car_id, actual)) +
  geom_smooth(method = 'lm', se = FALSE, color='lightgrey') +
  geom_segment(aes(xend=car_id, yend=predicted), alpha=0.2) +
  geom_point(aes(color=abs(residuals), size=abs(residuals))) + # alpha mapped to absolute residuals
  scale_color_gradient2(low = 'blue', mid='white', high = 'red') + # color mapped here
  guides(color=F, size=F) + # colour legends removed 
  geom_point(aes(y=predicted), shape=1) +
  theme_bw() +
  labs(x='car id', y='price', title='actual vs predicted price (hollow circle) by cylinder number') +
  facet_grid(~cylindernumber, scales = 'free')

ggplot(d, aes(actual, residuals)) +geom_point() +geom_smooth()# as price increases residuals increases
####################
# Driver factors are:
  # 1. enginelocation
  # 2. curbweight
  # 3. cylindernumber
  # 4. enginetypeothers: 'dohcv','rotor'
  # 5. company audi *
  # 6. company bmw *
  # 7. company buick *
  # 8. company jaguar *
  # 9. company porsche *
  # 10.company volvo *
####################
