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
demo <- read.csv('../input/Demographic data.csv', na.strings = '', dec = ".")
cibil <- read.csv('../input/Credit Bureau data.csv', na.strings = '', dec = '.')

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
# predictions
test.pred.default <- predict(model, newdata = test[, -4], type = 'response')
test.actual.default <- as.factor(ifelse(test$default==1, 'yes', 'no'))
summary(test.pred.default)
# Model Evaluation:
# At cut off of 0.04
predicted.response <- factor(ifelse(test.pred.default >= 0.04, 'yes', 'no'))
(conf <- confusionMatrix(predicted.response, test.actual.default, positive = "yes"))
# Accuracy: 53.89%, Sensitivity: 60.431%, Specificity: 53.597% # WOE Transformation

# Helper function to calculate accuracy matrix for a specific cut off value
CalcEvalMatrix <- function(x, y, z) {
  # Returns sensitiviy, specificity, accuracy of the model at various cut offs as a matrix
  # Args:
  #   x: cut off value
  #   y: predicted label prob. from test data
  #   z: actual label from test data
  # Returns:
  #   matrix of accuracy, sensitivity, specificity values 
  
  predicted.response <- as.factor(ifelse(y >= x, "yes", "no"))
  conf <- confusionMatrix(predicted.response, z, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  eval.matrix <- t(as.matrix(c(sens, spec, acc))) 
  colnames(eval.matrix) <- c("sensitivity", "specificity", "accuracy")
  
  return(eval.matrix)
}

# Helper function to get optimum accuracy
GetOptimumAcc <- function(x, y){
  # This function identifies the optimum cut off value
  # Args:
  #   x: predicted label prob. from test data
  #   y: actual label from test data
  # Returns:
  #   a list of optimized confusion matrix & final cut off value
  
  ## We loop through cutoffs from 0.01 to 0.99 for optimal value
  # initiating cut off values from 0.01 to 0.99 for creating 100x4 matrix
  cutoff.vals <- seq(0.01, 0.99, length.out = 100)
  model.eval.matrix <- matrix(0, nrow = 100, ncol = 3)
  for(i in 1:100){
    model.eval.matrix[i, ] <- CalcEvalMatrix(cutoff.vals[i], x, y)
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
  predicted.response <- factor(ifelse(x >= cutoff, "yes", "no"))
  
  conf.final <- confusionMatrix(predicted.response, y, positive = "yes")
  
  return(list(opt.cmttx=conf.final, opt.cutoff = cutoff))
} 
# optimised evaluation matrix
(opt.vals <- GetOptimumAcc(x=test.pred.default, y=test.actual.default))
# Accuracy: 53.23%, Sensitivity: 61.111%, Specificity: 52.882% # WOE Transformation optimised

# ---------------------------------------------------------------------#
## Model Building: Combined data--> demographic & credit bureau data ##
# ---------------------------------------------------------------------#
# check for application ids in demographic and credit bureau data sets which are not common
length(dplyr::setdiff(demo$application.id, cibil$application.id)) # 0
# this implies all application ids in two data sets are common, 
# merging the two data sets by application.id and default
credx <- merge(demo, cibil, by = c('application.id', 'default'))
#credx <- merge(demo, cibil, by = 'application.id')

# Adding business rule that applicants having 90 days DPD in past 12 months are as good as a defaulter
table(credx[which(credx$DPD90.12m>=1),]$default)
# 0       1 
# 17887  1436 
credx[which(credx$DPD90.12m>=1),]$default <- 1
credx <- credx[, - which(names(credx)=='DPD90.12m')] # remove DPD90.12m to ensure model independece on this static feature

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
(minlamda_lasso <-cv.out$lambda.min) # 0.002607546 0.0004037206 0.0005894762 0.00153329

lasso.mod <- glmnet(x[ntrain,],y[ntrain],alpha=1,lambda = minlamda_lasso)
lasso.pred <- predict(lasso.mod, s= minlamda_lasso, newx=x[ntest,])

# MSE 
mean((lasso.pred-y.test)^2) # 0.03971291 0.03194379 0.0579637
# All the coefficents from the model at minimal lamda
(lasso.coef <- predict(lasso.mod,type="coefficients",s=minlamda_lasso))

# Non zero coefficients in final model
drivers <- as.data.frame(as.matrix(lasso.coef))
drivers$driver.var <- rownames(drivers)
rownames(drivers) <- NULL
names(drivers)[[1]] <- 'lasso.coeff'
#lasso.coef[lasso.coef!=0]
drivers[which(drivers$lasso.coeff!=0),][-1,] # driver variables
#     lasso.coeff       driver.var
# 2  0.040087612           DPD30.6m
# 3  0.008626311           DPD60.6m
# 4  0.012659121          DPD60.12m
# 5  0.002592696          DPD30.12m
# 6  0.029890585     cc.utilization
# 7  0.008153529         trades.12m
# 9  0.007179111      inquiries.12m
# 10 0.005869477    total.no.trades
# 11 0.001124064       outs.balance
# 15 0.359717327           DPD90.6m
# 16 0.013652563 duration.residence
# 17 0.003629720             income
# 18 0.010153429   duration.company
# 19 0.001416536          home.loan

test.pred.default <- lasso.pred[,1]
test.pred.default <- ifelse(test.pred.default>=1, 0.999999999, ifelse(test.pred.default<=0, 0.0000001, test.pred.default))
test.actual.default <- as.factor(ifelse(y[ntest]==1, 'yes', 'no'))
summary(test.pred.default)
# Model Evaluation:
# At cut off of 0.3
predicted.response <- factor(ifelse(test.pred.default >= 0.3, 'yes', 'no'))
(conf <- confusionMatrix(predicted.response, test.actual.default, positive = "yes"))
#             Reference
# Prediction  no   yes
#        no  13855  1023
#       yes   676  5196
# Accuracy : 0.9181 0.7735  Sensitivity : 0.8355 0.70127 Specificity : 0.9535 0.77785
(opt.vals <- GetOptimumAcc(x=test.pred.default, y=test.actual.default))
# Optimum accuracy values
#             Reference
# Prediction  no   yes
#        no  12970   608
#       yes   1561  5611
# Accuracy : 0.8955 0.7079 Sensitivity : 0.9022 0.76586  Specificity : 0.8926 0.70439 

# --------------------------------------#
## Data Modeling: XGBoost Model ##
# --------------------------------------#
library(xgboost)
library(Matrix)

set.seed(123)

ntrain <- sample.split(credx2$default, SplitRatio = 0.60)

train <- credx2[ntrain, ]
testcv <- credx2[!ntrain, ]

ntest <- sample.split(testcv$default, SplitRatio = 0.5)

test <- testcv[ntest, ]
cv <- testcv[!ntest, ]

CalcPct(credx$default)
CalcPct(train$default)
CalcPct(test$default)
CalcPct(cv$default)

# convert data frames as matrix#
train.mtrx <- Matrix(as.matrix(train), sparse = TRUE)
test.mtrx <- Matrix(as.matrix(test), sparse = TRUE)
cv <- Matrix(as.matrix(cv), sparse = TRUE)

# Create XGB Matrices
train.xgb <- xgb.DMatrix(data = train.mtrx[,-2], label = train.mtrx[,2])
test.xgb <- xgb.DMatrix(data = test.mtrx[,-2], label = test.mtrx[,2])
cv.xgb <- xgb.DMatrix(data = cv[,-2], label = cv[,2])

# watchlist
watchlist <- list(train  = train.xgb, cv = cv.xgb)

# set parameters:
params <- list (
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
xgb.model <- xgb.train(params, train.xgb, nrounds = 25, watchlist)
#Plot:
library(reshape2)
library(pROC)
melted <- melt(xgb.model$evaluation_log, id.vars="iter")
ggplot(data=melted, aes(x=iter, y=value, group=variable, color = variable)) + geom_line()

# Original
xgb.predict <- predict(xgb.model, test.xgb)
summary(xgb.predict)

roc <- roc(test[,2], xgb.predict)
xgb.predictboolean <- ifelse(xgb.predict >= 0.05,1,0)

xgb.cm <- confusionMatrix(xgb.predictboolean, test.mtrx[,2])
xgb.cm$table
#             Reference
# Prediction    0    1
#           0 7103  176
#           1 2574 3980
print(paste("AUC of XGBoost is:", roc$auc)) # 0.96
print(paste("F1 of XGBoost is:", xgb.cm$byClass["F1"])) # 0.83
xgb.cm$byClass

test.pred.default <- xgb.predict
test.actual.default <- as.factor(ifelse(test.mtrx[,2]==1, 'yes', 'no'))
summary(test.pred.default)
(opt.vals.xgb <- GetOptimumAcc(x=test.pred.default, y=test.actual.default))
#             Reference
# Prediction   no  yes
        # no  8673  423
        # yes 1004 3733
# $opt.cutoff
# [1] 0.2178788
predicted.response <- factor(ifelse(test.pred.default >= opt.vals.xgb$opt.cutoff, "yes", "no"))

# ----------------------------------------------- #
 ## Lift & Gain Chart Plots ##
# ----------------------------------------------- #
## Created predictions data frame for analysis
model.df <- data.frame(prospectID = test$application.id,
                       actual.response = test.actual.default,
                       predicted.response = predicted.response,
                       predicted.probability = xgb.predict,
                       cc.utilization = test$cc.utilization)

# helper function to calculate gains
CalcGain <- function(labels , predicted.probability, groups=10) {
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted.probability)) predicted.probability <- as.integer(as.character(predicted.probability))
  helper = data.frame(cbind(labels , predicted.probability))
  helper[,"Bucket"] = ntile(-helper[,"predicted.probability"], groups)
  gaintable = helper %>% group_by(Bucket)  %>%
    summarise_at(vars(labels ), funs(Total = n(),
                                     Default=sum(., na.rm = TRUE))) %>%
    mutate(CumDefault = cumsum(Default),
           Gain=CumDefault/sum(Default)*100,
           Cumlift=Gain/(Bucket*(100/groups)))
  return(gaintable)
}

# Create a Table of cumulative gain and lift 
model.df$response <- as.factor(ifelse(model.df$actual.response=="yes",1,0))
LG <- CalcGain(model.df$response, model.df$predicted.probability, groups = 10)
# Gain Chart 
plot(LG$Bucket,LG$Gain,col="red",type="l",main="Gain Chart",
     xlab="decile",ylab = "default rate")
# Lift Chart 
plot(LG$Bucket,LG$Cumlift,col="red",type="l",main="Lift Chart",
     xlab="decile",ylab = "lift")

View(LG)
write.csv(LG, 'lift_gain.csv', row.names = F)
# --------------------------------------#
# Application Scorecard: Calculations 
# --------------------------------------#
# We have the following two simultaneous equations -->
# 1> Score = Offset + Factor * ln(odds) -- (1)
# 2> Score + pdo = Offset + Factor * ln(2 * odds) -- (2)
# By solving (1) & (2), Factor = pdo / ln(2)
# & Offset = Score - Factor * ln (odds)
# In our case the requirement is 10:1 odds at 400 points and the odds double at every 20 points,
# So, pdo = 20 & Factor = 20/ln(2) = 28.8539; Offset = 400 - 28.8539 * ln (10) = 333.5614
# Substituting these values in (1) we get, 
# Score = 333.5614 + 28.8539 * ln(odds) --- (A)
# --------------------------------------#
app.scorecard <- data.frame(p.good=test.pred.default, 
                            odds.good = test.pred.default/(1-test.pred.default),
                            ln.odds = log(test.pred.default/(1-test.pred.default)))
app.scorecard$score <- round(333.5614 + (28.8539 * app.scorecard$ln.odds))
boxplot(app.scorecard$score, horizontal = T)
summary(app.scorecard$score) # 181--> 565

# -----------------------------------------------------------------#
 # Missed Business Calculation: using rejected application list #
# -----------------------------------------------------------------#
# check for application ids in rejected demo data and credit bureau data sets which are not common
length(dplyr::setdiff(rejected.pop$application.id, cibil$application.id)) # 0
# merging the two data sets by application.id and default
valid.dat <- merge(rejected.pop, cibil, by = c('application.id', 'default'))
# missing values
sort(sapply(valid.dat, function(x) sum(is.na(x))), decreasing = T)
# since these are rejected applicants by default they have been tagged as defaulters
valid.dat$default <- 1
# we omit 35-36 missing values
valid.dat <- na.omit(valid.dat)

summary(sapply(valid.dat, unique)) # clearly home.loan & auto.loan are categorical variables
valid.dat$home.loan <- as.factor(valid.dat$home.loan)
valid.dat$auto.loan <- as.factor(valid.dat$auto.loan)
valid.dat$dependents <- as.factor(valid.dat$dependents)

cat.vars.index3 <- sapply(valid.dat, function(x) class(x)=='factor')
cat.vars3 <- names(valid.dat[, cat.vars.index3])
dummies3<- data.frame(sapply(valid.dat[,cat.vars3], 
                             function(x) data.frame(model.matrix(~x-1, data =valid.dat[,cat.vars3]))[,-1]))
valid.dat2 <- cbind(valid.dat[!colnames(valid.dat) %in% cat.vars3], dummies3)

vtest <- Matrix(as.matrix(valid.dat2), sparse = TRUE)
vtest.xgb <- xgb.DMatrix(data = vtest[,-2], label = vtest[,2])

# predict
vtest.xgb.predict <- predict(xgb.model, vtest.xgb)
summary(vtest.xgb.predict)
boxplot(vtest.xgb.predict, horizontal = T)

vtest.pred <- ifelse(vtest.xgb.predict > 0.5, 1, 0)
table(vtest.pred)
# vtest.pred
# 0    1 
# 169 1219 
valid.dat2$predicted.defaults <- vtest.pred
sum(valid.dat2[which(valid.dat2$predicted.defaults==0),]$cc.utilization)
# [1] 8,393,000 # missed cc.utilization
sum(credx$cc.utilization) # 2,008,446,000 # total cc utilization
# Assumption: avg credit line - Rs. 1Lac/customer; avg. cc interest rate - 15% per year; 
# cc.utilization in 1000' Rs / customer.
# Using his model could have generated additional business of around Rs. 8,393,000 * .15 -> Rs. 1,258,950
# This accounts for about 0.04% of total earnings from cc interest

# -----------------------------------------------------------------***-----------------------------------------------------------------#
# -----------------------------------------------------------#
# ------------------------------#
# REF:
# 1. https://www.kaggle.com/bonovandoo/fraud-detection-with-smote-and-xgboost-in-r 
# 2. https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/
# 
