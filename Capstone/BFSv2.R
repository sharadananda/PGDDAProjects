# load libraries
library(ggplot2)
library(cowplot) # plot_grid
library(Information) # woe & IV analysis
library(caTools) # sample.split
library(caret) # confusionMatrix
library(dplyr)
library(glmnet) # Lasso regression

# set working directory
path <- '~/Desktop/pgdda/course/c10-CapstoneProject/intermediate/'
setwd(path)

# load data
demo <- read.csv('A:/UPGRAD/CP/Demographic data.csv', na.strings = '', dec = ".")
cibil <- read.csv('A:/UPGRAD/CP/Credit Bureau data.csv', na.strings = '', dec = '.')

str(demo)
str(cibil)

# sanitise column names
colnames(demo) <- c('application.id', 'age', 'gender', 'marital', 'dependents', 'income',
                    'education', 'profession', 'residence.type', 'duration.residence',
                    'duration.company', 'default')
colnames(cibil) <- c('application.id', 'DPD90.6m', 'DPD60.6m', 'DPD30.6m', 'DPD90.12m', 'DPD60.12m', 'DPD30.12m',
                     'cc.utilization', 'trades.6m', 'trades.12m', 'PL.trades.6m', 'PL.trades.12m', 'inquiries.6m',
                     'inquiries.12m', 'home.loan', 'outs.balance', 'total.no.trades', 'auto.loan', 'default')
## EDA
# missing cc.utilization + all other variable in credit bureau data zero. In this case
# applicant does not have any other credit card so, we will put cc.utilization to be 0.
# For all other cases of missing cc.utilization applicants are having a credit card so cc.utilization may or may
# not be zero. also, there is a chance of data not been captured for these applicants.
df <- dplyr::filter(cibil, is.na(cc.utilization) & outs.balance==0 & total.no.trades ==0 & auto.loan ==0 & 
                      home.loan==0 & trades.6m==0 & trades.12m ==0 & PL.trades.6m ==0 & PL.trades.12m==0 &
                      DPD90.6m==0 & DPD60.6m==0 & DPD30.6m==0 & DPD90.12m==0 & DPD60.12m==0 & DPD30.12m==0 & 
                      inquiries.6m==0 & inquiries.12m==0)
cibil[cibil$application.id %in% df$application.id, ]$cc.utilization <- 0 # applicants w/o any other credit card
summary(cibil$cc.utilization)

# check for duplicated application ids
# It is likely that the duplicates are actually data for two different customers
# So, assigning different application ids to the duplicated ids
# de duplication: new application ids to duplicated ids
DeDuplicate <- function(x){
  # De duplicate application ids by creating new application ids for duplicates
  # Args:
  #   x: data frame with duplicated application ids
  # Returns:
  #   de duplicated data frame
  dup.id.count <- sum(duplicated(x$application.id))
  dup.id <- unique(x[which(duplicated(x$application.id)), 1])
  #print(paste(dup.id.count, dup.id, sep = '-'))
  if(dup.id.count >=1){
    for(i in 1:dup.id.count){
      df <- x[x$application.id == dup.id[i],]
      dups <- rownames(df)
      #print(paste('-->', dups, sep = '-'))
      if(nrow(df[which(df$application.id==dup.id[i]+1), ])==0 & length(dups)>1){
        x[dups[2],]$application.id <- dup.id[i]+1
      }
    }
  }
  return(x)
}

demo <- DeDuplicate(demo)
cibil <- DeDuplicate(cibil)

# ------------------------------------------------------------#
 ## Preliminary Modeling: Using single demographic data set ##
# -------------------------------------------------------------#
## missing values
sort(sapply(demo, function(x) sum(is.na(x))), decreasing = T)
rejected.pop <- demo[which(is.na(demo$default)),]
demo <- demo[which(!is.na(demo$default)), ]
sort(round(colMeans(is.na(demo))*100,4), decreasing = T) 
# since missing values amount to less than 0.5% of total observations. we omit them
demo <- na.omit(demo)

str(demo)
summary(sapply(demo, unique))
demo$dependents <- as.factor(demo$dependents)

## outlier treatment
boxplot(demo[,c(2,6,10:11)], horizontal = T)
# clearly we have outliers for age and duration in current company fields
# also both age & income have negative values
plot(demo$age)
length(demo[which(demo$age<=0), ]$age) # 1 -ve value & 19 zero values
length(demo[which(demo$income<0), ]$income ) # 81 -ve values
# we remove invalid values from age and income fields
demo <- demo[which(demo$age>0 & demo$income>=0),]

## EDA ##
# Age #
summary(demo$age)
ggplot(demo, aes(age))+geom_histogram(binwidth = 1)
quantile(demo$age, seq(0,1,0.01))
table(demo$age) # 0-26; 27-35; 36-56; 57-65
# binning the age field and stored it in new age.grp field
demo$age.grp <- as.factor(cut(demo$age, breaks = c(10, 35, 45, 56, 70)))
table(demo$age.grp)

# Helper Function for plotting default rates of categorical variables
PlotDefaultRate <- function(x, y, ylimit=NULL){
  # Aggregates data by categorical variable. Next create bar plot using ggplot.
  # Args:
  #   x: Categorical Variable
  #   y: Name of Categorical Variable in string format
  # Returns:
  #   Nothing. Just plots bar chart for the aggregated data
  agg.default <- aggregate(default~x, demo, mean)
  count <- data.frame(table(x))
  count <- count[,-1]
  agg.default <- cbind(agg.default, count)
  
  colnames(agg.default) <- c(y, "default.rate","applicants")
  agg.default[, 2] <- format(round(agg.default[, 2], 2))
  # Bar plot
  ggplot(agg.default, aes(agg.default[, 1], count, label = default.rate)) + 
    geom_bar(stat = 'identity') + 
    labs(y = 'applicants', x = '', title = paste(y, ' by applicants') ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    geom_text(size = 2.5, vjust = -0.5)+coord_cartesian(ylim = ylimit)
}
PlotDefaultRate(demo$age.grp, 'Age') # Age doest not seem to have direct impact on defaults
PlotDefaultRate(demo$age, 'Age') # Applicant Aged 20 have more tendency to default

# Income #
PlotDefaultRate(demo$income, 'Income')
table(demo$income)
ggplot(demo, aes(income))+geom_histogram(binwidth = 1)
demo$income.grp <- cut(demo$income, breaks = c(0,4.5,15,25,35,45,65))
table(demo$income.grp)
summary(demo$income.grp)
demo[which(is.na(demo$income.grp)),]$income.grp <- as.factor('(0,4.5]')
PlotDefaultRate(demo$income.grp, 'Income Group') # 8>=income>4 more likely to default

# Duration in current company # 
plot(demo$duration.company)
boxplot(demo$duration.company, horizontal = T)
# outlier treatment
quantile(demo$duration.company, seq(0,1,0.01))
summary(demo$duration.company)
table(demo$duration.company)
demo$duration.company[demo$duration.company>75] <- 75
# plot duration in current company after outlier treatment
ggplot(demo, aes(duration.company))+geom_histogram(binwidth = 1)
PlotDefaultRate(demo$duration.company, 'Duration in current company')
demo$duration.company.grp <- cut(demo$duration.company, breaks = c(0, 5, 25, 45,65,80))
PlotDefaultRate(demo$duration.company.grp, 'Duration in current company group')
table(demo$duration.company)
table(demo$duration.company.grp) # applicants in current company <=21 months or >63 months are more likely to default
summary(demo$duration.company.grp)

# Duration in current residence
plot(demo$duration.residence)
ggplot(demo, aes(duration.residence))+geom_histogram(binwidth = 1)
ggplot(demo, aes(log(duration.residence)))+geom_histogram(binwidth = 1)
boxplot(demo$duration.residence, horizontal = T)
table(demo$duration.residence)
PlotDefaultRate(demo$duration.residence, 'Duration in current residence')

ggplot(demo[which(demo$duration.residence>6),], aes(duration.residence))+
  geom_histogram(binwidth = 1)
# plot all categorical variables
cat.vars.index <- sapply(demo, function(x) class(x)=='factor')
cat.vars <- names(demo[, cat.vars.index])
plotlist <- list()
for(i in 1:length(cat.vars)){
  plotlist[[i]] <- PlotDefaultRate(demo[,cat.vars[i]],cat.vars[i], ylimit = c(0,65000)) 
}

cowplot::plot_grid(plotlist = plotlist, nrow = 3, ncol = 3)

## Variable Transformation using WOE & IV
str(demo)
dumies <- data.frame(sapply(demo[, cat.vars],
                     function(x) data.frame(model.matrix(~x-1,data =demo[,cat.vars]))[,-1]))
demo2 <- cbind(demo[!colnames(demo) %in% cat.vars], dumies)
# demo.fact <- demo[, cat.vars]
# colnames(demo.fact)
# dummies<- data.frame(sapply(demo.fact[,1:6], 
#                    function(x) data.frame(model.matrix(~x-1,data =demo.fact[,1:6]))[,-1]))
# demo1 <- cbind(demo[!colnames(demo) %in% c(colnames(demo.fact))], dummies)
# create information value tables
iv <- create_infotables(data=demo2[,-1], y = 'default', bins = 10, parallel = F)
iv$Summary$IV <- round(iv$Summary$IV * 100, 2)
iv
(vars.list <- iv$Summary[iv$Summary$IV > 2, ])

demo2 <- subset(demo2, select = c(vars.list$Variable, 'default'))
str(demo2)
boxplot(demo2[,-4], horizontal = T)
## WOE transformation ##
# Helper function for woe transformation
WOE_Transform <- function(x, iv) {
  # Aggregates data by categorical variable. Next create bar plot using ggplot.
  # Args:
  #   x: data frame with variables for woe transformation
  #   iv: Information Value Table
  # Returns:
  #   data frame with features woe transformed
  iv.vars <- names(x[,-which(names(x)=='default')])
  for(c in iv.vars){
    iv.tbl <- iv$Tables[[c]]
    for(i in 1:nrow(iv.tbl)){
      splt <- strsplit(iv.tbl[i,1], split = ",")
      lstr <- splt[[1]][1]
      ustr <- splt[[1]][2]
      ll <- as.integer(substr(lstr,2,nchar(lstr)))
      ul <- as.integer(substr(ustr,1,nchar(ustr)-1))
      x[which(x[,c]>=ll & x[,c] <= ul),][,c]<- iv.tbl['WOE'][[1]][i]
    }
  }
  return(x)
}

demo2 <- WOE_Transform(demo2, iv)

## Model building: Logistic Regression ##
str(demo2)

set.seed(123)
ntrain <- sample.split(demo2$default, SplitRatio = 0.70)

train <- demo2[ntrain, ]
test <- demo2[!ntrain, ]

CalcPct <- function(x){
  tbl <- table(x)  
  cbind(count = tbl, perc = round(prop.table(tbl)*100,2))  
}
CalcPct(demo2$default)
CalcPct(train$default)
CalcPct(test$default)

# Model Building
model <- glm(default ~ ., family = "binomial", data = train) 
summary(model)
vif(model)
# predictions
test.pred.default <- predict(model, newdata = test[, -4], type = 'response')
test.actual.default <- as.factor(ifelse(test$default==1, 'yes', 'no'))
summary(test.pred.default)
# Model Evaluation:
# At cut off of 0.04
predicted.response <- factor(ifelse(test.pred.default >= 0.039, 'yes', 'no'))
(conf <- confusionMatrix(predicted.response, test.actual.default, positive = "yes"))
# Accuracy: 53.89.32%, Sensitivity: 60.431%, Specificity: 53.597% # WOE Transformation

####################################
### KS -statistic - Test Data ###
library(ROCR)

test_cutoff_default <-
  ifelse(predicted.response == "yes", 1, 0)
test_actual_default <-
  ifelse(test.actual.default == "yes", 1, 0)

#on testing  data
pred_object_test <-
  prediction(test_cutoff_default, test_actual_default)

performance_measures_test <-
  performance(pred_object_test, "tpr", "fpr") # checking true positive, false positives

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] -
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


#ROC Curve (tpr vs fpr)

plot(performance_measures_test, col ='red')
abline(a=0, b=1)


#Area under Roc curve
auc<- performance(pred_object_test,"auc")
AUC <- auc@y.values[[1]]
AUC

# GINI INDEX
gini <- 2*AUC - 1
gini

# Helper function to calculate accuracy matrix for a specific cut off value
CalcEvalMatrix <- function(x) {
  # Returns sensitiviy, specificity, accuracy of the model at various cut offs as a matrix
  # Args:
  #   x: cut off value
  # Returns:
  #   matrix of accuracy, sensitivity, specificity values 
  predicted.response <- as.factor(ifelse(test.pred.default >= x, "yes", "no"))
  conf <- confusionMatrix(predicted.response, test.actual.default, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  eval.matrix <- t(as.matrix(c(sens, spec, acc))) 
  colnames(eval.matrix) <- c("sensitivity", "specificity", "accuracy")
  return(eval.matrix)
}

# Helper function to get optimum accuracy
GetOptimumAcc <- function(){
  ## We loop through cutoffs from 0.01 to 0.99 for optimal value
  # initiating cut off values from 0.01 to 0.99 for creating 100x4 matrix
  cutoff.vals <- seq(0.01, 0.99, length.out = 100)
  model.eval.matrix <- matrix(0, nrow = 100, ncol = 3)
  for(i in 1:100){
    model.eval.matrix[i, ] <- CalcEvalMatrix(cutoff.vals[i])
  }
  # plotting cut off values
  plot(cutoff.vals, model.eval.matrix[, 1], xlab="Cutoff", ylab="Value", 
       cex.lab=1.5, cex.axis=1.5, ylim=c(0,1), type="l", lwd=2, axes=FALSE, col=2)
  axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
  lines(cutoff.vals,model.eval.matrix[,2], col="darkgreen", lwd=2)
  lines(cutoff.vals,model.eval.matrix[,3], col=4, lwd=2)
  box()
  legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),
         c("Sensitivity","Specificity","Accuracy"))
  d <- sort(abs(model.eval.matrix[,1]-model.eval.matrix[,2]))[1:5] # sensitivity-specificity
  # cut off value for final model
  cutoff <- cutoff.vals[which(abs(model.eval.matrix[,1]-model.eval.matrix[,2])<= d[1])] 
  predicted.response <- factor(ifelse(test.pred.default >= cutoff, "yes", "no"))
  
  conf.final <- confusionMatrix(predicted.response, test.actual.default, positive = "yes")
  # acc <- conf.final$overall[1]  
  # sens <- conf.final$byClass[1] 
  # spec <- conf.final$byClass[2] 
  # 
  # acc.list <- list()
  # acc.list$acc <- acc; acc.list$sens <- sens; acc.list$spec <- spec; 
  # acc.list$conf.mtrx <- conf.final
  # return(acc.list)
  return(conf.final)
  #return(cutoff)
} 

(opt.conf.matrix <- GetOptimumAcc())

# ---------------------------------------------------------------------#
## Model Building: Combined data--> demographic & credit bureau data ##
# ---------------------------------------------------------------------#
# check for application ids in demographic and credit bureau data sets which are not common
length(dplyr::setdiff(demo$application.id, cibil$application.id)) # 0
# this implies all application ids in two data sets are common, 
# merging the two data sets by application.id and default
credx <- merge(demo, cibil, by = c('application.id', 'default'))
# missing values
sort(sapply(credx, function(x) sum(is.na(x))), decreasing = T)
(missing.perc <- sort(sapply(credx, function(x) sum(is.na(x))/nrow(credx) * 100), decreasing = T))
plot(missing.perc, xlab='feature count', ylab='perc. missing values')
# since missing values are less than 0.7% we will omit these
credx <- na.omit(credx)
# checking for categorical values
summary(sapply(credx, unique)) # clearly home.loan & auto.loan are categorical variables
credx$home.loan <- as.factor(credx$home.loan)
credx$auto.loan <- as.factor(credx$auto.loan)
# proportion of target variables
CalcPct(credx$default)
#   count  perc
# 0 66243 95.77
# 1  2923 04.23
cat.vars.index2 <- sapply(credx, function(x) class(x)=='factor')
cat.vars2 <- names(credx[, cat.vars.index2])
dummies2<- data.frame(sapply(credx[,cat.vars2[-c(7:9)]], 
                     function(x) data.frame(model.matrix(~x-1, data =credx[,cat.vars2[-c(7:9)]]))[,-1]))
credx2 <- cbind(credx[!colnames(credx) %in% cat.vars2], dummies2)

iv <- create_infotables(data=credx2[,-1], y = 'default', parallel = F)
iv$Summary$IV <- round(iv$Summary$IV * 100, 2)
iv
(vars.list <- iv$Summary[iv$Summary$IV > 2, ])
credx_woe <- subset(credx2, select = c(vars.list$Variable, 'default'))
str(credx_woe)
credx_woe <- WOE_Transform(credx_woe, iv)
# --------------------------------------#
## Data Modeling: Lasso Regression ##
# --------------------------------------#
str(credx_woe)

x <- model.matrix(default ~ ., data=credx_woe)[,-1]
y <- credx_woe$default

set.seed(123)
ntrain <- sample(1:nrow(x), 0.7*nrow(x))
ntest <- seq(1:nrow(x))[-ntrain]

y.test <- y[ntest]

cv.out <- cv.glmnet(x[ntrain,],y[ntrain],alpha=1)
plot(cv.out)
# Optimal lamda store it into "minlamda_lasso" object
(minlamda_lasso <-cv.out$lambda.min) # 0.002607546 0.0004037206

lasso.mod <- glmnet(x[ntrain,],y[ntrain],alpha=1,lambda = minlamda_lasso)
lasso.pred <- predict(lasso.mod, s= minlamda_lasso, newx=x[ntest,])

# MSE 
mean((lasso.pred-y.test)^2) # 0.03971291
# All the coefficents from the model at optimal lamda, s=403.4
(lasso.coef <- predict(lasso.mod,type="coefficients",s=minlamda_lasso))

# Non zero coefficients in final model
drivers <- as.data.frame(as.matrix(lasso.coef))
drivers$driver.var <- rownames(drivers)
rownames(drivers) <- NULL
names(drivers)[[1]] <- 'lasso.coeff'
#lasso.coef[lasso.coef!=0]
drivers[which(drivers$lasso.coeff!=0),][-1,] # driver variables
#     lasso.coeff       driver.var
# 2   0.014537119   cc.utilization
# 3   0.004097623       trades.12m
# 5   0.007800338    inquiries.12m
# 6   0.002885548     outs.balance
# 7   0.016620851         DPD30.6m
# 10  0.007580654        DPD90.12m
# 12  0.001555152     inquiries.6m
# 13  0.006746015        DPD30.12m
# 16 -0.007672748         DPD90.6m
# 18  0.006848676           income
# 19  0.007286223 duration.company
test.pred.default <- lasso.pred[,1]
test.actual.default <- as.factor(ifelse(y[ntest]==1, 'yes', 'no'))
summary(test.pred.default)
# Model Evaluation:
# At cut off of 0.04
predicted.response <- factor(ifelse(test.pred.default >= 0.04, 'yes', 'no'))
(conf <- confusionMatrix(predicted.response, test.actual.default, positive = "yes"))
# Accuracy : 0.5416 Sensitivity : 0.73913 Specificity : 0.53295 
(opt.conf.matrix <- GetOptimumAcc())
# Optimum accuracy values
# Accuracy : 0.6368 Sensitivity : 0.61785 Specificity : 0.63760 
### KS -statistic - Test Data ###
library(ROCR)

test_cutoff_default <-
  ifelse(predicted.response == "yes", 1, 0)
test_actual_default <-
  ifelse(test.actual.default == "yes", 1, 0)

#on testing  data
pred_object_test <-
  prediction(test_cutoff_default, test_actual_default)

performance_measures_test <-
  performance(pred_object_test, "tpr", "fpr") # checking true positive, false positives

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] -
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


#ROC Curve (tpr vs fpr)
plot(performance_measures_test, col ='red')
abline(a=0, b=1)


#Area under Roc curve
auc<- performance(pred_object_test,"auc")
AUC <- auc@y.values[[1]]
AUC

# GINI INDEX
gini <- 2*AUC - 1
gini



app.scorecard <- data.frame(p.good=test.pred.default, 
                            odds.good = test.pred.default/(1-test.pred.default),
                            ln.odds = log(test.pred.default/(1-test.pred.default)))
app.scorecard$score <- round(250.26 + (-61.8* app.scorecard$ln.odds))
boxplot(app.scorecard$score, horizontal = T)
summary(app.scorecard$score)

# --------------------------------------#
## Data Modeling: Random Forest ##
# --------------------------------------#
library(randomForest)
set.seed(123)

train <- credx[ntrain, ]
test <- credx[-ntrain, ]
# Adding business rule that applicants having 90 days DPD in past 12 months are as good as a defaulter
train$default <- ifelse(train$DPD90.12m>=1, 1, train$default)
test$default <- ifelse(test$DPD90.12m>=1, 1, test$default)

CalcPct(credx$default)
CalcPct(train$default)
CalcPct(test$default)

train$default <- as.factor(ifelse(train$default==1, 'yes', 'no'))
test$default <- as.factor(ifelse(test$default==1, 'yes', 'no'))

system.time(rf.model <- randomForest(default ~ ., data = train, ntree = 1000, nodesize = 10))

rf.predict <- predict(rf.model, test[,-2])
confusionMatrix(test$default, rf.predict)
varImpPlot(rf.model)
# Accuracy: 97.89%; Sensitivity: 97.07%; Specificity: 100%
#               Reference
# Prediction    no   yes
#       no  14531     0
#       yes   438  5781


### KS -statistic - Test Data ###
library(ROCR)

test_cutoff_default <-
  ifelse(rf.predict == "yes", 1, 0)
test_actual_default <-
  ifelse(test$default == "yes", 1, 0)

#on testing  data
pred_object_test <-
  prediction(test_cutoff_default, test_actual_default)

performance_measures_test <-
  performance(pred_object_test, "tpr", "fpr") # checking true positive, false positives

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] -
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)


#ROC Curve (tpr vs fpr)
plot(performance_measures_test, col ='red')
abline(a=0, b=1)


#Area under Roc curve
auc<- performance(pred_object_test,"auc")
AUC <- auc@y.values[[1]]
AUC

# GINI INDEX
gini <- 2*AUC - 1
gini

# --------------------------------------#
## Data Modeling: XGBoost Model ##
# --------------------------------------#
library(xgboost)
library(Matrix)

set.seed(123)
# ntrain <- sample.split(credx_woe$default, SplitRatio = 0.60)
# 
# train <- credx_woe[ntrain, ]
# testcv <- credx_woe[!ntrain, ]
ntrain <- sample.split(credx2$default, SplitRatio = 0.60)

train <- credx2[ntrain, ]
testcv <- credx2[!ntrain, ]

# Adding business rule that applicants having 90 days DPD in past 12 months are as good as a defaulter
train$default[which(train$DPD90.12m>=1)] <- 1
testcv$default[which(testcv$DPD90.12m>=1)] <- 1

ntest <- sample.split(testcv$default, SplitRatio = 0.5)

test <- testcv[ntest, ]
cv <- testcv[!ntest, ]

# train <- credx_woe[ntrain, ]
# test <- credx_woe[-ntrain, ]

CalcPct(credx$default)
CalcPct(train$default)
CalcPct(test$default)
CalcPct(cv$default)

# convert data frames as matrix#
train <- Matrix(as.matrix(train), sparse = TRUE)
test <- Matrix(as.matrix(test), sparse = TRUE)
cv <- Matrix(as.matrix(cv), sparse = TRUE)

# Create XGB Matrices
# train.xgb <- xgb.DMatrix(data = train[,-19], label = train[,19])
# test.xgb <- xgb.DMatrix(data = test[,-19], label = test[,19])
# cv.xgb <- xgb.DMatrix(data = cv[,-19], label = cv[,19])
train.xgb <- xgb.DMatrix(data = train[,-2], label = train[,2])
test.xgb <- xgb.DMatrix(data = test[,-2], label = test[,2])
cv.xgb <- xgb.DMatrix(data = cv[,-2], label = cv[,2])

# watchlist
watchlist <- list(train  = train.xgb, cv = cv.xgb)

# set parameters:
parameters <- list(
  # General Parameters
  booster            = "gbtree",          
  silent             = 0,                 
  # Booster Parameters
  eta                = 0.3,               
  gamma              = 0,                 
  max_depth          = 6,                 
  min_child_weight   = 1,                 
  subsample          = 1,                 
  colsample_bytree   = 1,                 
  colsample_bylevel  = 1,                 
  lambda             = 1,                 
  alpha              = 0,                 
  # Task Parameters
  objective          = "binary:logistic",   
  eval_metric        = "auc",
  seed               = 123               
)
## Model training
# Original
xgb.model <- xgb.train(parameters, train.xgb, nrounds = 25, watchlist)
#Plot:
library(reshape2)
library(pROC)
melted <- melt(xgb.model$evaluation_log, id.vars="iter")
ggplot(data=melted, aes(x=iter, y=value, group=variable, color = variable)) + geom_line()

# Original
xgb.predict <- predict(xgb.model, test.xgb)
summary(xgb.predict)
#roc <- roc(test[,19], xgb.predict)
roc <- roc(test[,2], xgb.predict)
xgb.predictboolean <- ifelse(xgb.predict >= 0.04,1,0)
#xgb.cm <- confusionMatrix(Matrix(xgb.predictboolean), test[,19])
xgb.cm <- confusionMatrix(xgb.predictboolean, test[,2])
xgb.cm$table
#             Reference
# Prediction    0    1
#           0 7502  196    7412 166
#           1 5746  388    2298 3956
print(paste("AUC of XGBoost is:", roc$auc)) #0.65369830959235
print(paste("F1 of XGBoost is:", xgb.cm$byClass["F1"])) # 0.716318151437029
xgb.cm$byClass

test.pred.default <- xgb.predict
#test.actual.default <- as.factor(ifelse(test[,19]==1, 'yes', 'no'))
test.actual.default <- as.factor(ifelse(test[,2]==1, 'yes', 'no'))
summary(test.pred.default)
predicted.response <- factor(ifelse(test.pred.default >= 0.3, 'yes', 'no'))
GetOptimumAcc()
#             Reference
# Prediction   no  yes
        # no  7451  195 9089 247
        # yes 5797  389 621  3875
