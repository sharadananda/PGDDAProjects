################################################################
## Course 3: PA-I, Project 1
## HR Analytics Case Study 
## Prepared by:
# 1. Sharadananda Mondal
## Submitted on: 20.08.2017
################################################################

### Install Packages if not already installed ###
# install.packages(MASS)
# install.packages(car)
# install.packages(caTools)
# install.packages(caret)
# install.packages(cowplot)
# install.packages(ggplot2)
# install.packages(corrplot) # correlation matrix plot
# install.packages(lubridate) # ymd_hms 
# install.packages(dplyr)
# install.packages(tidyr)
# install.packages(GGally)
# install.packages(ROCR)
# install.packages(Information) # IV table
# install.packages(InformationValue) # Gain Chart Plot
# install.packages(scales) # ggplot percent display

## load the necessary libraries
library(MASS)
library(car)
library(caTools)
library(caret)
library(cowplot)
library(ggplot2)
library(corrplot) # correlation matrix plot
library(lubridate) # ymd_hms 
library(dplyr)
library(tidyr)
library(GGally)
library(ROCR)
library(Information) # IV table
library(scales) # ggplot percent display
# set the working directory
setwd('~/Desktop/pgdda/c3-pa-1/hra-project/intermediate/')

# load the datasets
in.time <- read.csv('../input/in_time.csv', stringsAsFactors = F)
out.time <- read.csv('../input/out_time.csv', stringsAsFactors = F)
emp.survey <- read.csv('../input/employee_survey_data.csv', stringsAsFactors = F)
mgr.survey <- read.csv('../input/manager_survey_data.csv', stringsAsFactors = F)
general <- read.csv('../input/general_data.csv', stringsAsFactors = F)

# structure of the datasets
str(in.time) #4410 obs of 262 variables
str(out.time) #4410 obs of 262 variables
str(emp.survey) #4410 obs of 4 variables
str(mgr.survey) #4410 obs of 3 variables
str(general) # 4410 obs of 24 variables

# first we will check for the unique ids of the datasets
length(unique(in.time$X)) # 4410, so employeeID stored in 1st column is the unique ID
length(unique(out.time$X)) # 4410,  employeeID in the 1st column is the key
length(unique(emp.survey$EmployeeID)) # 4410, EmployeeID is the primary key
length(unique(mgr.survey$EmployeeID)) # 4410, EmployeeID is the primary key
length(unique(general$EmployeeID)) # 4410, EmployeeID is the primary key

# we will use clock-in & clock-out dataset for deriving new metrics
# 1. avg.hours: average working hours of employees; 
# 2. hours.worked.by.dayofweek
# 3. tot.leaves: leaves by employees
# 4. leaves.by.month # 12 fields
# 5. leaves.by.dayofweek # 5 fields
# 6. leaves.by.quarter # 4 fields
# Assumption: 
# 1. we will consider clock-in as presence (0) and no-clock-in as absence (1)
# 2. difference between clock-in & clock-out is considered as hours worked in a day
# 3. public holiday: day where no employees had clock-in data in in.time dataset

## derive metrics:leave pattern ##
leave <- as.data.frame(ifelse(is.na(in.time[,-1]), 1, 0))
# exclude public holidays from the leave df
leave <- leave[, which(colSums(leave)!=4410)]

# create emp.leaves df for leave pattern analysis
# total leaves calculation
emp.leaves <- data.frame(EmployeeID = in.time$X, TotLeaves = (rowSums(leave))) 

# dates.matrix df for leave analysis
dates.matrix <- data.frame(date=as.Date(gsub('X','', names(leave)), format = '%Y.%m.%d'))
dates.matrix$month <- months(dates.matrix$date, abbreviate = T)
dates.matrix$weekday <- weekdays(dates.matrix$date, abbreviate = T)
dates.matrix$quarter <- quarters(dates.matrix$date)
head(dates.matrix)
# leave count calculations
leave.calc.by <- c('weekday', 'month', 'quarter')
for(calc.by in leave.calc.by){
  colnames(leave) <- dates.matrix[, c(calc.by)]
  leaves.calc <- as.data.frame(sapply(unique(colnames(leave)), 
                                      function(x) rowSums(leave[, colnames(leave)==x, drop=F], na.rm = T)))
  emp.leaves <- cbind(emp.leaves, leaves.calc)
}
head(emp.leaves)
## correlation matrix
corrplot(cor(emp.leaves[,-1]))

###################################
##  working hours calculations  ##
###################################
identical(colnames(in.time$X), colnames(out.time$X)) # clock in & out datasets are similar
# exclude public holidays
in.time <- in.time[, which(sapply(in.time, function(x) sum(is.na(x))) !=4410)]
out.time <- out.time[, which(sapply(out.time, function(x) sum(is.na(x))) !=4410)]
in.time[,-1] <- sapply(in.time[,-1], ymd_hms) # character to datetime
out.time[,-1] <- sapply(out.time[,-1], ymd_hms) # character to datetime
work.hrs <- (out.time[,-1] - in.time[,-1])/3600 # total working hours calculation
# emp.hrs df to store employee working hours
# average working hours calculations
emp.hrs <- data.frame(EmployeeID=in.time$X, 
                      Avg.hrs = round(rowMeans(work.hrs, na.rm = T),2))
# working hours by day of week
colnames(work.hrs) <- paste(dates.matrix$weekday, '.hrs', sep = '')
hours.by.weekday <- as.data.frame(sapply(unique(colnames(work.hrs)), 
                                         function(x) round(rowSums(work.hrs[, colnames(work.hrs)==x, drop=F], 
                                                                   na.rm = T)/(365/7),2)))
emp.hrs <- cbind(emp.hrs, hours.by.weekday)
head(emp.hrs)

# remove features with single values 
general <- general[, -which(colnames(general) %in% c('EmployeeCount', 'Over18', 'StandardHours'))]
str(general)
# missing values
sum(is.na(general)) # 28 (NumCompaniesWorked:19; TotalWorkingYears:9)
# remove missing values # use WOE to impute missing values
#general <- na.omit(general)
# outlier treatment
sapply(general[,c(1,5:6,10,13:21)], function(x) boxplot.stats(x)$out)
# columns with outlier values: MonthlyIncome, NumCompaniesWorked, StockOptionLevel,
# TotalWorkingYears, TrainingTimesLastYear, YearsAtCompany, YearsSinceLastPromotion
# YearsWithCurrManager # These will be treated later using WOE analysis

###################################
##  Missing value treatment  ##
###################################
sum(is.na(emp.survey)) # 83 missing values 
sum(is.na(mgr.survey)) # 0 missing values
sum(is.na(general)) # 28 missing values

sapply(emp.survey, function(x) sum(is.na(x))) # (worklife:38, env:25, jobs:20)

# omit missing values
emp.survey <- na.omit(emp.survey)

############################
## master file creation  ##
############################
hr <- merge(general, emp.survey, by="EmployeeID", all = F)
hr <- merge(hr, mgr.survey, by = "EmployeeID", all = F)
hr <- merge(hr, emp.hrs, by = "EmployeeID", all = F)
hr <- merge(hr, emp.leaves, by = "EmployeeID", all = F)
str(hr) # master file 4300 obs of 57 variables

####################################
####  Data Preparation & EDA   ####
####################################
str(hr) # 4327 observations of 54 variables
sapply(hr[,-c(1:2,13, 27:54)], table)
# Age, distance from home, total working years, years at company, years since last
# promotion, years with curr manager, Avg.hrs, TotLeaves, Monthly income are continuous
# variables
## Attrition is the target variable ##
sum(duplicated(hr)) # de duplication not needed

## missing values treatment already completed
sapply(hr, function(x){sum(is.na(x))})

####################################################################################
##    Barcharts for categorical features    ##
####################################################################################
# make a copy of the combined data set
hr1 <- hr 
# for plotting need to change categorical data from numeric to text values
hr1$Education <- ifelse(hr1$Education==1, "Below College",
                        ifelse(hr1$Education==2, "College", 
                               ifelse(hr1$Education==3, "Bachelor",
                                      ifelse(hr1$Education==4, "Master", "Doctor"))))
hr1$EnvironmentSatisfaction <- ifelse(hr1$EnvironmentSatisfaction==1, "Low",
                                      ifelse(hr1$EnvironmentSatisfaction==2, "Medium", 
                                             ifelse(hr1$EnvironmentSatisfaction==3, "High", "Very High")))
hr1$JobInvolvement <- ifelse(hr1$JobInvolvement==1, "Low",
                             ifelse(hr1$JobInvolvement==2, "Medium", 
                                    ifelse(hr1$JobInvolvement==3, "High", "Very High")))
hr1$JobSatisfaction <- ifelse(hr1$JobSatisfaction==1, "Low",
                              ifelse(hr1$JobSatisfaction==2, "Medium", 
                                     ifelse(hr1$JobSatisfaction==3, "High", "Very High")))
hr1$PerformanceRating <- ifelse(hr1$PerformanceRating==1, "Low",
                                ifelse(hr1$PerformanceRating==2, "Good", 
                                       ifelse(hr1$PerformanceRating==3, "Excellent", "Outstanding")))
hr1$WorkLifeBalance <- ifelse(hr1$WorkLifeBalance==1, "Bad",
                              ifelse(hr1$WorkLifeBalance==2, "Good", 
                                     ifelse(hr1$WorkLifeBalance==3, "Better", "Best")))
## define the theme for the bar plots #
bar.theme<- theme(axis.text.x = element_text(size=8, angle = 37, vjust = 0.5),
                  axis.text.y = element_text(size=8),
                  plot.margin = unit(c(0,0,0,0), "mm"),
                  legend.spacing =unit(0, "mm"),
                  text = element_text(size=12),
                  legend.position="none",
                  axis.title.x = element_blank(),
                  plot.title = element_text(hjust = 0.5))

# helper function for multiple ggplots
plotDFCol <- function(df, col1, col2, col3){
  p <- ggplot(df, aes(x=col1, fill=col2)) + 
    geom_bar(aes(y = (..count..)/sum(..count..)), position = 'dodge') + 
    geom_text(aes(y = ((..count..)/sum(..count..)), 
                  label = scales::percent((..count..)/sum(..count..))), 
              stat = "count", vjust = -0.1,position=position_dodge(.9), size=2.5) + 
    scale_fill_manual(values=c("#0033FF", "#FF0000")) +
    scale_y_continuous(labels = scales::percent) +
    labs(x='', y='', title = col3)+
    bar.theme
  return(p)
}

#helper function for grid plots
plotGrid <- function(...){
  args <- list(...)
  n <- length(args)
  print(n)
  # check for factor fields
  if(args[["f1"]]== 1){
    args[["x1"]] <- factor(args[["x1"]])
  }
  if(args[["f2"]]== 1){
    args[["x2"]] <- factor(args[["x2"]])
  }
  if(args[["f3"]]== 1){
    args[["x3"]] <- factor(args[["x3"]])
  }
  if(args[["f4"]]== 1){
    args[["x4"]] <- factor(args[["x4"]])
  }
  
  # individual ggplots
  p1 <- plotDFCol(args[["df"]], args[["x1"]], args[["x0"]], args[["x7"]])
  p2 <- plotDFCol(args[["df"]], args[["x2"]], args[["x0"]], args[["x8"]])
  p3 <- plotDFCol(args[["df"]], args[["x3"]], args[["x0"]], args[["x9"]])
  p4 <- plotDFCol(args[["df"]], args[["x4"]], args[["x0"]], args[["x10"]])
  
  if(n == 20){
    if(args[["f5"]]== 1){
      args[["x5"]] <- factor(args[["x5"]])
    }
    if(args[["f6"]]== 1){
      args[["x6"]] <- factor(args[["x6"]])
    }
    p5 <- plotDFCol(args[["df"]], args[["x5"]], args[["x0"]], args[["x11"]])
    p6 <- plotDFCol(args[["df"]], args[["x6"]], args[["x0"]], args[["x12"]])
    # grid display of the six plots
    plot_grid(p1, p2, p3, p4, p5, p6, align = "h")
  } else {
    # grid display of the four plots
    plot_grid(p1, p2, p3, p4, align = "h")
  }
}

###############################
##    Grid Plot : I     ##
###############################
## define arguments for the first grid plot ##
arg.list1 <- list(df=hr1, x0 = hr1$Attrition, x1 = hr1$BusinessTravel, x2 = hr1$Department, x3 = hr1$MaritalStatus,
                  x4 = hr1$Education, x5 = hr1$EducationField, x6 = hr1$Gender)
arg.list2 <- list(x7 = 'BusinessTravel', x8 = 'Department', x9 = 'MaritalStatus',
                 x10 = 'Education', x11 = 'EducationField', x12 = 'Gender')
arg.list3 <- list(f1=0, f2=0, f3=0, f4=0, f5=0, f6=0)
# function call for grid plot and passing arguments as a vector of three lists
do.call(plotGrid, c(arg.list1, arg.list2, arg.list3))

# First grid plot analysis:
  # 1. Male employees are more prone to attrition than female. 
  # 2. R&D employees are more prone to attrition especially from Life Sciences & Medical education level. 
  # 3. These are also people who travel rarely and have master's and graduate degrees.
  # 4. Single employees are more prone to quitting almoste 4 times than divorced and 1.5 times Married ones.

###############################
##    Grid Plot : II     ##
###############################
arg.list4 <- list(df=hr1, x0 = hr1$Attrition, x1 = hr1$JobLevel, x2 = hr1$JobInvolvement, x3 = hr1$JobSatisfaction,
                  x4 = hr1$PerformanceRating, x5 = hr1$WorkLifeBalance, x6 = hr1$EnvironmentSatisfaction)
arg.list5 <- list(x7 = 'Job Level', x8 = 'Job Involvement', x9 = 'Job Satisfaction',
                  x10 = 'Performance Rating', x11 = 'Work Life Balance', x12 = 'Environment Satisfaction')
arg.list6 <- list(f1=0, f2=0, f3=0, f4=0, f5=0, f6=0)
# function call for grid plot and passing arguments as a vector of three lists
do.call(plotGrid, c(arg.list4, arg.list5, arg.list6))
# Second grid plot analysis:
  # 1. Junior or Fresher employees with high job involvement and no stock options are the ones most prone to quitting
  # 2. These are employees in their first company, fresh out of college and into employment
  # 3. Also, they are highly satisfied with the environment and have good work life balance

###############################
##    Grid Plot : III     ##
###############################
arg.list7 <- list(df=hr1, x0 = hr1$Attrition, x1 = hr1$NumCompaniesWorked, x2 = hr1$YearsWithCurrManager, 
                  x3 = hr1$YearsSinceLastPromotion, x4=hr1$PercentSalaryHike)
arg.list8 <- list(x7 = 'Number of Companies Worked', x8 = 'Years With Current Manager', 
                  x9 = 'Years Since Last Promotion', x10 = 'Percent Salary Hike')
arg.list9 <- list(f1=1, f2=0, f3=0, f4=0)
# function call for grid plot and passing arguments as a vector of three lists
do.call(plotGrid, c(arg.list7, arg.list8, arg.list9))

# Third grid plot analysis:
  # 1. Junior or Fresher employees having worked in just one another company and are with current manager for 0 to 1 years are most likely to quit.
  # 2. Employees who have recently been promoted this year or past year are most likely to quit
  # 3. employees with salary hike perc 11-14 are more prone to attrition

###############################
##    Grid Plot : IV     ##
###############################
arg.list10 <- list(df=hr1, x0 = hr1$Attrition, x1 = hr1$Age, x2 = hr1$TotalWorkingYears, 
                   x3 = hr1$YearsAtCompany, x4 = hr1$DistanceFromHome)
arg.list11 <- list(x7 = 'Age', x8 = 'Total Working Years', 
                   x9 = 'Years at Company', x10 = 'Distance from Home')
arg.list12 <- list(f1=1, f2=0, f3=0, f4=0)
# function call for grid plot and passing arguments as a vector of three lists
do.call(plotGrid, c(arg.list10, arg.list11, arg.list12))
# Fourth grid plot analysis:
  # 1. single employees in late 20s to early 30s who have worked for less than 2 years are more prone to attrition.
  # 2. These are employees who have started working by joining this company
  # 3. Research Scientists stay within 2 kms of labs. employees staying within 10 kms constitue most of the attrition population.

###############################
##    Grid Plot : V     ##
###############################
arg.list13 <- list(df=hr1, x0 = hr1$Attrition, x1 = hr1$TrainingTimesLastYear, x2 = hr1$JobRole, 
                   x3 = hr1$StockOptionLevel, x4 = hr1$TotLeaves)
arg.list14 <- list(x7 = 'Training Times Last Year', x8 = 'Job Role', 
                   x9 = 'Stock Option Level', x10 = 'Total Leaves')
arg.list15 <- list(f1=1, f2=0, f3=0, f4=0)
# function call for grid plot and passing arguments as a vector of three lists
do.call(plotGrid, c(arg.list13, arg.list14, arg.list15))
# Fifth grid plot analysis:
  # 1. Employees with 0 to 1 stock options are the ones most prone to quitting
  # 2. Also, employees without training in past 2 to 3 years have quitted more in numbers.
  # 3. Research Scientists, Sales Executive, Laboratory Technicians are more prone to quitting.
  # 4. No specific trend on total leaves and attritions
rm(hr1) # remove hr1 from workspace
##################################################
###   Plots for numeric variables   ###
##################################################
# Average hours scatter plot
ggplot(hr, aes(EmployeeID, Avg.hrs, color=Attrition))+
  geom_point(position = 'jitter', alpha=0.3)+
  labs(title='Average hours worked by Employee ID', 
       y='Average Hours Worked',alpha='', color='')+
  theme(axis.title.x = element_blank())

## binned average working hours plot
df_hrs <- hr[,c(1,3,27:32)]
df_hrs['Avg.hrs.cut'] <- cut(df_hrs$Avg.hrs, breaks = c(5.9, 7, 8, 9, 10, 11.05), 
                             labels = c('6-7', '7-8', '8-9', '9-10', '10-11'))
# attrition by average working hours plot
ggplot(df_hrs, aes(x=factor(Avg.hrs.cut), fill=Attrition)) + 
  geom_bar(position = 'dodge', aes(y = ((..count..)/sum(..count..)))) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.1, size=3, position=position_dodge(.9)) +
  scale_fill_manual(values=c("#0033FF", "#FF0000")) +
  scale_y_continuous(labels =  scales::percent) +
  labs(x='', y='', title = 'Average hours')+
  bar.theme

# Average working hours by weekday plot
df1 <- gather(df_hrs, day, hrs, -c(EmployeeID, Attrition, Avg.hrs, Avg.hrs.cut))
df2 <- df1 %>% 
  subset(select=-c(EmployeeID, Avg.hrs, Avg.hrs.cut)) %>% 
  group_by(day, Attrition) %>% 
  summarise(avg.hrs = round(mean(hrs),2)) %>% 
  arrange(desc(avg.hrs))
ggplot(df2, aes(day, avg.hrs, fill=Attrition)) +
  geom_text(data=df2, aes(label=avg.hrs),vjust= -0.2, 
            position=position_dodge(.9), size=4) +
  geom_bar(position = 'dodge', stat = 'identity') + 
  geom_hline(aes(yintercept = 7)) + 
  labs(title='Attrition by average working hours on weekday')+
  scale_fill_manual(values=c("#0033FF", "#FF0000")) +
  theme(axis.title.x = element_blank())
## Findings: Clearly employees averaging over 7 hours on weekdays have quit
# Leave pattern
df3 <- hr[c(1,3,34:54)]
df4 <- df3 %>% 
  gather(day, leave, Fri, Mon, Tue, Wed, Thu) %>% 
  subset(select=c(EmployeeID, Attrition, day, leave)) %>% 
  group_by(EmployeeID, day, Attrition) %>% 
  summarise(leave.count = sum(leave)) %>% 
  arrange(desc(leave.count))

ggplot(df4, aes(day, leave.count, fill=Attrition))+
  geom_bar(stat='identity', position='dodge') +
  scale_fill_manual(values=c("#0033FF", "#FF0000")) +
  labs(title='Attrition by average leaves on weekdays in a year',
       x='', y='leaves taken')+
  theme_gray()
# Findings: No specific patterns
## Monthly Income Plot (before outlier treatment) ##
box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")
# Boxplots of numeric variables relative to attrition
plot_grid(ggplot(hr, aes(x=Attrition,y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(hr, aes(x=Attrition,y=Avg.hrs, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)
################################################################
## Outlier Treatment ##
################################################################
##################################################
###   Wieght of Evidence (WOE) analysis   ###
##################################################
indices <- which(colnames(hr) %in% 
                   c('YearsWithCurrManager', 'MonthlyIncome', 'NumCompaniesWorked', 
                     'StockOptionLevel','TotalWorkingYears', 'TrainingTimesLastYear', 
                     'YearsAtCompany', 'YearsSinceLastPromotion','Attrition'))
hr_woe <- hr[, indices]
## converting target variable Attrition from No/Yes character to factorwith levels 0/1 ##
hr_woe$Attrition <- ifelse(hr_woe$Attrition=='Yes', 0, 1)
# create Information Value (IV) Table
iv <- create_infotables(hr_woe, y = "Attrition")
# summary of IV table
iv$Summary
# Variable          IV
# 4       TotalWorkingYears 0.372166000
# 6          YearsAtCompany 0.345413517
# 8    YearsWithCurrManager 0.302110632
# 2      NumCompaniesWorked 0.110977837
# 7 YearsSinceLastPromotion 0.051884250
# 1           MonthlyIncome 0.027811740
# 5   TrainingTimesLastYear 0.025088588
# 3        StockOptionLevel 0.003730808
## According to Siddiqui(2006): 
# IV > 0.3 => strong predictive power
# IV [0.1, 0.3] => medium predictive power, 
# IV < 0.1 => weak predictive power
## So, we will use WOE variable transformation for:
## 1. TotalWorkingYears 
hr_woe$TotalWorkingYears <- ifelse(is.na(hr_woe$TotalWorkingYears), iv$Tables[['TotalWorkingYears']]['WOE'][[1]][1], 
                                   ifelse(hr_woe$TotalWorkingYears >=0 & hr_woe$TotalWorkingYears<=2, iv$Tables[['TotalWorkingYears']]['WOE'][[1]][2], 
                                          ifelse(hr_woe$TotalWorkingYears >=3 & hr_woe$TotalWorkingYears<=4, iv$Tables[['TotalWorkingYears']]['WOE'][[1]][3],
                                                 ifelse(hr_woe$TotalWorkingYears==5, iv$Tables[['TotalWorkingYears']]['WOE'][[1]][4],
                                                        ifelse(hr_woe$TotalWorkingYears >=6 & hr_woe$TotalWorkingYears<=7, iv$Tables[['TotalWorkingYears']]['WOE'][[1]][5],
                                                               ifelse(hr_woe$TotalWorkingYears >=8 & hr_woe$TotalWorkingYears<=9, iv$Tables[['TotalWorkingYears']]['WOE'][[1]][6],
                                                                      ifelse(hr_woe$TotalWorkingYears >=10 & hr_woe$TotalWorkingYears<=12, iv$Tables[['TotalWorkingYears']]['WOE'][[1]][7],
                                                                             ifelse(hr_woe$TotalWorkingYears >=13 & hr_woe$TotalWorkingYears<=16, iv$Tables[['TotalWorkingYears']]['WOE'][[1]][8],
                                                                                    ifelse(hr_woe$TotalWorkingYears >=17 & hr_woe$TotalWorkingYears<=22, iv$Tables[['TotalWorkingYears']]['WOE'][[1]][9],
                                                                                           ifelse(hr_woe$TotalWorkingYears >=23 & hr_woe$TotalWorkingYears<=40, iv$Tables[['TotalWorkingYears']]['WOE'][[1]][10], NA))))))))))
## 2. YearsAtCompany 
hr_woe$YearsAtCompany <- ifelse(hr_woe$YearsAtCompany ==0, iv$Tables[['YearsAtCompany']]['WOE'][[1]][1], 
                                ifelse(hr_woe$YearsAtCompany ==1, iv$Tables[['YearsAtCompany']]['WOE'][[1]][2],
                                       ifelse(hr_woe$YearsAtCompany==2, iv$Tables[['YearsAtCompany']]['WOE'][[1]][3],
                                              ifelse(hr_woe$YearsAtCompany >=3 & hr_woe$YearsAtCompany<=4, iv$Tables[['YearsAtCompany']]['WOE'][[1]][4],
                                                     ifelse(hr_woe$YearsAtCompany >=5 & hr_woe$YearsAtCompany<=6, iv$Tables[['YearsAtCompany']]['WOE'][[1]][5],
                                                            ifelse(hr_woe$YearsAtCompany >=7 & hr_woe$YearsAtCompany<=8, iv$Tables[['YearsAtCompany']]['WOE'][[1]][6],
                                                                   ifelse(hr_woe$YearsAtCompany ==9, iv$Tables[['YearsAtCompany']]['WOE'][[1]][7],
                                                                          ifelse(hr_woe$YearsAtCompany >=10 & hr_woe$YearsAtCompany<=14, iv$Tables[['YearsAtCompany']]['WOE'][[1]][8],
                                                                                 ifelse(hr_woe$YearsAtCompany >=15 & hr_woe$YearsAtCompany<=40, iv$Tables[['YearsAtCompany']]['WOE'][[1]][9], NA)))))))))
## 3. YearsWithCurrManager & 
hr_woe$YearsWithCurrManager <- ifelse(hr_woe$YearsWithCurrManager ==0, iv$Tables[['YearsWithCurrManager']]['WOE'][[1]][1], 
                                      ifelse(hr_woe$YearsWithCurrManager ==1, iv$Tables[['YearsWithCurrManager']]['WOE'][[1]][2],
                                             ifelse(hr_woe$YearsWithCurrManager==2, iv$Tables[['YearsWithCurrManager']]['WOE'][[1]][3],
                                                    ifelse(hr_woe$YearsWithCurrManager ==3, iv$Tables[['YearsWithCurrManager']]['WOE'][[1]][4],
                                                           ifelse(hr_woe$YearsWithCurrManager >=4 & hr_woe$YearsWithCurrManager<=6, iv$Tables[['YearsWithCurrManager']]['WOE'][[1]][5],
                                                                  ifelse(hr_woe$YearsWithCurrManager >=7 & hr_woe$YearsWithCurrManager<=8, iv$Tables[['YearsWithCurrManager']]['WOE'][[1]][6],
                                                                         ifelse(hr_woe$YearsWithCurrManager >=9 & hr_woe$YearsWithCurrManager<=17, iv$Tables[['YearsWithCurrManager']]['WOE'][[1]][7], NA)))))))
#4. NumCompaniesWorked
hr_woe$NumCompaniesWorked <- ifelse(is.na(hr_woe$NumCompaniesWorked), iv$Tables[['NumCompaniesWorked']]['WOE'][[1]][1], 
                                    ifelse(hr_woe$NumCompaniesWorked ==0, iv$Tables[['NumCompaniesWorked']]['WOE'][[1]][2], 
                                           ifelse(hr_woe$NumCompaniesWorked ==1, iv$Tables[['NumCompaniesWorked']]['WOE'][[1]][3],
                                                  ifelse(hr_woe$NumCompaniesWorked==2, iv$Tables[['NumCompaniesWorked']]['WOE'][[1]][4],
                                                         ifelse(hr_woe$NumCompaniesWorked ==3, iv$Tables[['NumCompaniesWorked']]['WOE'][[1]][5],
                                                                ifelse(hr_woe$NumCompaniesWorked ==4, iv$Tables[['NumCompaniesWorked']]['WOE'][[1]][6],
                                                                       ifelse(hr_woe$NumCompaniesWorked >=5 & hr_woe$NumCompaniesWorked<=6, iv$Tables[['NumCompaniesWorked']]['WOE'][[1]][7],
                                                                              ifelse(hr_woe$NumCompaniesWorked >=7 & hr_woe$NumCompaniesWorked<=9, iv$Tables[['NumCompaniesWorked']]['WOE'][[1]][8], NA))))))))
hr$TotalWorkingYears <- hr_woe$TotalWorkingYears
hr$YearsAtCompany <- hr_woe$YearsAtCompany
hr$YearsWithCurrManager <- hr_woe$YearsWithCurrManager
hr$NumCompaniesWorked <- hr_woe$NumCompaniesWorked

# there are outliers in monthly income & Avg.hrs
# numerical dataset
hr_num <- hr[,-c(1, 3:5, 8:9, 11:12, 14, 17, 19, 21)] # excluding woe treated variables
sapply(hr_num, function(x) length(boxplot.stats(x)$out))
# we have outliers in the following features:
# 1.MonthlyIncome 2. StockOptionLevel 3. TrainingTimesLastYear
# 4. YearsSinceLastPromotion 5. PerformanceRating 6. Avg.hrs:Thu.hrs
hr_num$Attrition <- ifelse(hr$Attrition=='Yes',0,1)
##################################################################################################
## woe transformation for Avg.hrs to Thu.hrs as these variables have significant predictions  ##
##################################################################################################
iv2 <- create_infotables(hr_num, y="Attrition")
head(iv2$Summary)
# Variable        IV
# 15  Avg.hrs 0.3758377
# 19  Wed.hrs 0.3479009
# 17  Mon.hrs 0.3359566
# 16  Fri.hrs 0.3307226
# 20  Thu.hrs 0.3229072
# 18  Tue.hrs 0.3183382

## Avg.hrs ##
hr$Avg.hrs <- ifelse(hr$Avg.hrs >=5.95 & hr$Avg.hrs <=6.19, iv2$Tables[['Avg.hrs']]['WOE'][[1]][1], 
               ifelse(hr$Avg.hrs >=6.2 & hr$Avg.hrs<=6.5, iv2$Tables[['Avg.hrs']]['WOE'][[1]][2], 
        ifelse(hr$Avg.hrs >=6.51 & hr$Avg.hrs<=6.79, iv2$Tables[['Avg.hrs']]['WOE'][[1]][3],
          ifelse(hr$Avg.hrs>=6.8 & hr$Avg.hrs<=7.1, iv2$Tables[['Avg.hrs']]['WOE'][[1]][4],
            ifelse(hr$Avg.hrs >=7.11 & hr$Avg.hrs<=7.4, iv2$Tables[['Avg.hrs']]['WOE'][[1]][5],
         ifelse(hr$Avg.hrs >=7.41 & hr$Avg.hrs<=7.69, iv2$Tables[['Avg.hrs']]['WOE'][[1]][6],
          ifelse(hr$Avg.hrs >=7.7 & hr$Avg.hrs<=7.99, iv2$Tables[['Avg.hrs']]['WOE'][[1]][7],
            ifelse(hr$Avg.hrs >=8 & hr$Avg.hrs<=8.87, iv2$Tables[['Avg.hrs']]['WOE'][[1]][8],
             ifelse(hr$Avg.hrs >=8.88 & hr$Avg.hrs<=9.99, iv2$Tables[['Avg.hrs']]['WOE'][[1]][9],
 ifelse(hr$Avg.hrs >=10 & hr$Avg.hrs<=11.03, iv2$Tables[['Avg.hrs']]['WOE'][[1]][10], NA))))))))))

## Wed.hrs ##
hr$Wed.hrs <- ifelse(hr$Wed.hrs >=4.76 & hr$Wed.hrs <=5.6, iv2$Tables[['Wed.hrs']]['WOE'][[1]][1], 
    ifelse(hr$Wed.hrs >=5.61 & hr$Wed.hrs<=5.87, iv2$Tables[['Wed.hrs']]['WOE'][[1]][2], 
    ifelse(hr$Wed.hrs >=5.88 & hr$Wed.hrs<=6.17, iv2$Tables[['Wed.hrs']]['WOE'][[1]][3],
    ifelse(hr$Wed.hrs>=6.18 & hr$Wed.hrs<=6.45, iv2$Tables[['Wed.hrs']]['WOE'][[1]][4],
    ifelse(hr$Wed.hrs >=6.46 & hr$Wed.hrs<=6.73, iv2$Tables[['Wed.hrs']]['WOE'][[1]][5],
    ifelse(hr$Wed.hrs >=6.74 & hr$Wed.hrs<=7.01, iv2$Tables[['Wed.hrs']]['WOE'][[1]][6],
    ifelse(hr$Wed.hrs >=7.02 & hr$Wed.hrs<=7.35, iv2$Tables[['Wed.hrs']]['WOE'][[1]][7],
    ifelse(hr$Wed.hrs >=7.36 & hr$Wed.hrs<=8.1, iv2$Tables[['Wed.hrs']]['WOE'][[1]][8],
    ifelse(hr$Wed.hrs >=8.11 & hr$Wed.hrs<=9.14, iv2$Tables[['Wed.hrs']]['WOE'][[1]][9],
ifelse(hr$Wed.hrs >=9.15 & hr$Wed.hrs<=10.58, iv2$Tables[['Wed.hrs']]['WOE'][[1]][10], NA))))))))))

## Mon.hrs ##
hr$Mon.hrs <- ifelse(hr$Mon.hrs >=4.86 & hr$Mon.hrs <=5.59, iv2$Tables[['Mon.hrs']]['WOE'][[1]][1], 
    ifelse(hr$Mon.hrs >=5.6 & hr$Mon.hrs<=5.86, iv2$Tables[['Mon.hrs']]['WOE'][[1]][2], 
    ifelse(hr$Mon.hrs >=5.87 & hr$Mon.hrs<=6.16, iv2$Tables[['Mon.hrs']]['WOE'][[1]][3],
    ifelse(hr$Mon.hrs>=6.17 & hr$Mon.hrs<=6.44, iv2$Tables[['Mon.hrs']]['WOE'][[1]][4],
    ifelse(hr$Mon.hrs >=6.45 & hr$Mon.hrs<=6.72, iv2$Tables[['Mon.hrs']]['WOE'][[1]][5],
    ifelse(hr$Mon.hrs >=6.73 & hr$Mon.hrs<=7.02, iv2$Tables[['Mon.hrs']]['WOE'][[1]][6],
    ifelse(hr$Mon.hrs >=7.03 & hr$Mon.hrs<=7.34, iv2$Tables[['Mon.hrs']]['WOE'][[1]][7],
    ifelse(hr$Mon.hrs >=7.35 & hr$Mon.hrs<=8.07, iv2$Tables[['Mon.hrs']]['WOE'][[1]][8],
    ifelse(hr$Mon.hrs >=8.08 & hr$Mon.hrs<=9.19, iv2$Tables[['Mon.hrs']]['WOE'][[1]][9],
    ifelse(hr$Mon.hrs >=9.2 & hr$Mon.hrs<=10.62, iv2$Tables[['Mon.hrs']]['WOE'][[1]][10], NA))))))))))

## Fri.hrs ##
hr$Fri.hrs <- ifelse(hr$Fri.hrs >=4.56 & hr$Fri.hrs <=5.36, iv2$Tables[['Fri.hrs']]['WOE'][[1]][1], 
    ifelse(hr$Fri.hrs >=5.37 & hr$Fri.hrs<=5.63, iv2$Tables[['Fri.hrs']]['WOE'][[1]][2], 
    ifelse(hr$Fri.hrs >=5.64 & hr$Fri.hrs<=5.91, iv2$Tables[['Fri.hrs']]['WOE'][[1]][3],
    ifelse(hr$Fri.hrs >=5.92 & hr$Fri.hrs<=6.18, iv2$Tables[['Fri.hrs']]['WOE'][[1]][4],
    ifelse(hr$Fri.hrs >=6.19 & hr$Fri.hrs<=6.46, iv2$Tables[['Fri.hrs']]['WOE'][[1]][5],
    ifelse(hr$Fri.hrs >=6.47 & hr$Fri.hrs<=6.74, iv2$Tables[['Fri.hrs']]['WOE'][[1]][6],
    ifelse(hr$Fri.hrs >=6.75 & hr$Fri.hrs<=7.08, iv2$Tables[['Fri.hrs']]['WOE'][[1]][7],
    ifelse(hr$Fri.hrs >=7.09 & hr$Fri.hrs<=7.8, iv2$Tables[['Fri.hrs']]['WOE'][[1]][8],
    ifelse(hr$Fri.hrs >=7.81 & hr$Fri.hrs<=8.81, iv2$Tables[['Fri.hrs']]['WOE'][[1]][9],
    ifelse(hr$Fri.hrs >=8.82 & hr$Fri.hrs<=10.16, iv2$Tables[['Fri.hrs']]['WOE'][[1]][10], NA))))))))))

## Thu.hrs ##
hr$Thu.hrs <- ifelse(hr$Thu.hrs >=4.89 & hr$Thu.hrs <=5.6, iv2$Tables[['Thu.hrs']]['WOE'][[1]][1], 
    ifelse(hr$Thu.hrs >=5.61 & hr$Thu.hrs<=5.87, iv2$Tables[['Thu.hrs']]['WOE'][[1]][2], 
    ifelse(hr$Thu.hrs >=5.88 & hr$Thu.hrs<=6.15, iv2$Tables[['Thu.hrs']]['WOE'][[1]][3],
    ifelse(hr$Thu.hrs >=6.16 & hr$Thu.hrs<=6.44, iv2$Tables[['Thu.hrs']]['WOE'][[1]][4],
    ifelse(hr$Thu.hrs >=6.45 & hr$Thu.hrs<=6.71, iv2$Tables[['Thu.hrs']]['WOE'][[1]][5],
    ifelse(hr$Thu.hrs >=6.72 & hr$Thu.hrs<=7.01, iv2$Tables[['Thu.hrs']]['WOE'][[1]][6],
    ifelse(hr$Thu.hrs >=7.02 & hr$Thu.hrs<=7.36, iv2$Tables[['Thu.hrs']]['WOE'][[1]][7],
    ifelse(hr$Thu.hrs >=7.37 & hr$Thu.hrs<=8.13, iv2$Tables[['Thu.hrs']]['WOE'][[1]][8],
    ifelse(hr$Thu.hrs >=8.14 & hr$Thu.hrs<=9.12, iv2$Tables[['Thu.hrs']]['WOE'][[1]][9],
    ifelse(hr$Thu.hrs >=9.13 & hr$Thu.hrs<=10.6, iv2$Tables[['Thu.hrs']]['WOE'][[1]][10], NA))))))))))

## Tue.hrs ##
hr$Tue.hrs <- ifelse(hr$Tue.hrs >=4.78 & hr$Tue.hrs <=5.7, iv2$Tables[['Tue.hrs']]['WOE'][[1]][1], 
    ifelse(hr$Tue.hrs >=5.71 & hr$Tue.hrs<=5.97, iv2$Tables[['Tue.hrs']]['WOE'][[1]][2], 
    ifelse(hr$Tue.hrs >=5.98 & hr$Tue.hrs<=6.28, iv2$Tables[['Tue.hrs']]['WOE'][[1]][3],
    ifelse(hr$Tue.hrs >=6.29 & hr$Tue.hrs<=6.58, iv2$Tables[['Tue.hrs']]['WOE'][[1]][4],
    ifelse(hr$Tue.hrs >=6.59 & hr$Tue.hrs<=6.85, iv2$Tables[['Tue.hrs']]['WOE'][[1]][5],
    ifelse(hr$Tue.hrs >=6.86 & hr$Tue.hrs<=7.16, iv2$Tables[['Tue.hrs']]['WOE'][[1]][6],
    ifelse(hr$Tue.hrs >=7.17 & hr$Tue.hrs<=7.49, iv2$Tables[['Tue.hrs']]['WOE'][[1]][7],
    ifelse(hr$Tue.hrs >=7.5 & hr$Tue.hrs<=8.24, iv2$Tables[['Tue.hrs']]['WOE'][[1]][8],
    ifelse(hr$Tue.hrs >=8.25 & hr$Tue.hrs<=9.36, iv2$Tables[['Tue.hrs']]['WOE'][[1]][9],
    ifelse(hr$Tue.hrs >=9.37 & hr$Tue.hrs<=10.74, iv2$Tables[['Tue.hrs']]['WOE'][[1]][10], NA))))))))))
#####################################################
## Outlier treatment for: MonthlyIncome ##
#####################################################
## binning monthly income
hr$MonthlyIncome <- cut(hr$MonthlyIncome, breaks = c(10000, 40000, 70000, 100000, 150000, 200000), 
                        labels = c('10-40k','40-70k','70-100k','100-150k', '150-200k'))
#2. StockOptionLevel ## categorical variable with levels: 0,1,2,3
#3. TrainingTimesLastYear 4. YearsSinceLastPromotion 5. PerformanceRating ## all categoricals

# Monthly Income bar plot
ggplot(hr, aes(MonthlyIncome, fill=Attrition))+
  geom_bar(position = 'dodge', aes(y = ((..count..)/sum(..count..)))) + 
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.1, size=3, position=position_dodge(.9)) +
  scale_fill_manual(values=c("#0033FF", "#FF0000")) +
  scale_y_continuous(labels =  scales::percent) +
  labs(x='', fill='', y='', title='Attrition by Monthly Income') +
  bar.theme
# Findings: Employees earning less than 70,000 per month mostly quit and constitute ~75% of attrition population
  # Top earners and working little above standard working hours are more likely to attrition

corrplot(cor(hr[,-c(1,3:5,8:9,11:13)]), tl.cex = 0.5, mar = c(1,0,0,0)) #c(bottom, left, top, right)
# Findings: Performance Rating & PercentageSalaryHike is highly correlated so does yearswithchurnmanager
  # & yearsatcompany, Also totatl working years is directly correlated with age

####################################
##    Feature standardisation   ## 
###################################
# Normalising continuous features#
hr[,-c(1,3:5,8:9,11:14, 17, 19, 21, 27:32)] <- as.data.frame(scale(hr[,-c(1,3:5,8:9,11:14, 17, 19, 21, 27:32)]))

# converting target variable Attrition from No/Yes character to factorwith levels 0/1 
hr$Attrition<- ifelse(hr$Attrition=="Yes",1,0)

# Checking attrition rate of prospect customer
Attrition <- sum(hr$Attrition)/nrow(hr)
Attrition # 16.20% attrition rate which is near to 15% as mentioned in the case study

# creating a dataframe of categorical features
hr_chr<- hr[,c(4:5,8,11:13)]
# converting categorical attributes to factor
hr_fact<- data.frame(sapply(hr_chr, function(x) factor(x)))
str(hr_fact)
# creating dummy variables for factor attributes
dummies<- data.frame(sapply(hr_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =hr_fact))[,-1]))
# for gender male is 1 and female is 0
hr$Gender <- ifelse(hr$Gender=='Male', 1, 0)
# Final dataset
hr_final<- cbind(hr[,-c(1,4:5,8, 11:13)],dummies) # removed the ID field: EmployeeID
View(hr_final) # 4327 obs. of  70 variables

########################################################################
# splitting the data between train and test
set.seed(123) # for reproducibility

ntrain = sample.split(hr_final$Attrition, SplitRatio = 0.7) #splitting indices
train = hr_final[ntrain, ] # train data consisting of 70% of original obs.
test = hr_final[!ntrain, ] # test data consisting of 30% of original obs.

########################################################################
# Logistic Regression: 
#Initial model
model = glm(Attrition ~ ., data = train, family = "binomial")
summary(model) #AIC 2061.6....69 coeff..nullDev 2684.5 ...resDev 1933.6

# Stepwise selection
model2 <- stepAIC(model, direction="both")
summary(model2)
##################################################
## Removing Multicollinearity through VIF check #
#################################################
vif(model2)

# removing EducationField.xMarketing with p-value 0.158
model3 <- glm(formula = Attrition ~ Age + Gender + NumCompaniesWorked + 
            TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
            YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
            JobSatisfaction + WorkLifeBalance + Avg.hrs + Wed.hrs + TotLeaves + 
            Oct + Nov + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
            Department.xResearch...Development + Department.xSales + 
            EducationField.xOther + JobRole.xHuman.Resources + 
            JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
            JobRole.xSales.Representative + MaritalStatus.xMarried + 
            MaritalStatus.xSingle + MonthlyIncome.x40.70k, family = "binomial", 
          data = train)
summary(model3)
vif(model3)

# removing Gender with p-value 0.10
model4 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + Avg.hrs + Wed.hrs + TotLeaves + 
                Oct + Nov + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                EducationField.xOther + JobRole.xHuman.Resources + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Representative + MaritalStatus.xMarried + 
                MaritalStatus.xSingle + MonthlyIncome.x40.70k, family = "binomial", 
              data = train)
summary(model4)
vif(model4)

# removing JobRole.xSales.Representative with p-value 0.16
model5 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + Avg.hrs + Wed.hrs + TotLeaves + 
                Oct + Nov + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                EducationField.xOther + JobRole.xHuman.Resources + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xMarried + 
                MaritalStatus.xSingle + MonthlyIncome.x40.70k, family = "binomial", 
              data = train)
summary(model5)
vif(model5)

# removing Wed.hrs with vif 5.25
model6 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + Avg.hrs + TotLeaves + 
                Oct + Nov + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + Department.xSales + 
                EducationField.xOther + JobRole.xHuman.Resources + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xMarried + 
                MaritalStatus.xSingle + MonthlyIncome.x40.70k, family = "binomial", 
              data = train)
summary(model6)
vif(model6)

# removing Department.xSales with vif 4.69
model7 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + Avg.hrs + TotLeaves + 
                Oct + Nov + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development +  
                EducationField.xOther + JobRole.xHuman.Resources + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xMarried + 
                MaritalStatus.xSingle + MonthlyIncome.x40.70k, family = "binomial", 
              data = train)
summary(model7)
vif(model7)

# removing Department.xResearch...Development with p-value 0.42
model8 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + Avg.hrs + TotLeaves + 
                Oct + Nov + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                EducationField.xOther + JobRole.xHuman.Resources + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xMarried + 
                MaritalStatus.xSingle + MonthlyIncome.x40.70k, family = "binomial", 
              data = train)
summary(model8)
vif(model8)

# removing Nov with p-value 0.0418
model9 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + Avg.hrs + TotLeaves + 
                Oct + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                EducationField.xOther + JobRole.xHuman.Resources + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xMarried + 
                MaritalStatus.xSingle + MonthlyIncome.x40.70k, family = "binomial", 
              data = train)
summary(model9)
vif(model9)

# removing TotLeaves with p-value 0.18
model10 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                JobSatisfaction + WorkLifeBalance + Avg.hrs + 
                Oct + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                EducationField.xOther + JobRole.xHuman.Resources + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                MaritalStatus.xMarried + 
                MaritalStatus.xSingle + MonthlyIncome.x40.70k, family = "binomial", 
              data = train)
summary(model10)
vif(model10)

# removing Oct with p-value 0.08
model11 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + Avg.hrs + 
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                 EducationField.xOther + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + MonthlyIncome.x40.70k, family = "binomial", 
               data = train)
summary(model11)
vif(model11)

# removing BusinessTravel.xTravel_Rarely with vif 3.62
model12 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + Avg.hrs + 
                 BusinessTravel.xTravel_Frequently + 
                 EducationField.xOther + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + MonthlyIncome.x40.70k, family = "binomial", 
               data = train)
summary(model12)
vif(model12)

# removing JobRole.xHuman.Resources with p-value 0.026
model13 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + Avg.hrs + 
                 BusinessTravel.xTravel_Frequently + 
                 EducationField.xOther + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + MonthlyIncome.x40.70k, family = "binomial", 
               data = train)
summary(model13)
vif(model13)

# removing JobRole.xManager with p-value 0.034
model14 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + Avg.hrs + 
                 BusinessTravel.xTravel_Frequently + 
                 EducationField.xOther + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + MonthlyIncome.x40.70k, family = "binomial", 
               data = train)
summary(model14)
vif(model14)

# removing EducationField.xOther with p-value 0.019
model15 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + Avg.hrs + 
                 BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xMarried + 
                 MaritalStatus.xSingle + MonthlyIncome.x40.70k, family = "binomial", 
               data = train)
summary(model15)
vif(model15)

# removing MaritalStatus.xMarried with vif 2.1
model16 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + Avg.hrs + 
                 BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xSingle + MonthlyIncome.x40.70k, family = "binomial", 
               data = train)
summary(model16)
vif(model16)

# removing YearsAtCompany with vif 3.65
model17 <- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + Avg.hrs + 
                 BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xSingle + MonthlyIncome.x40.70k, family = "binomial", 
               data = train)
summary(model17)
vif(model17)

# removing Age with p-value 0.0123
model18 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + Avg.hrs + 
                 BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xSingle + MonthlyIncome.x40.70k, family = "binomial", 
               data = train)
summary(model18)
vif(model18)

# removing JobRole.xResearch.Director with p-value 0.005
model19 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + Avg.hrs + 
                 BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + MonthlyIncome.x40.70k, family = "binomial", 
               data = train)
summary(model19)

# removing TrainingTimesLastYear with p-value 0.0028
model20 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                 TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + Avg.hrs + 
                 BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle + MonthlyIncome.x40.70k, family = "binomial", 
               data = train)
summary(model20)

# removing MonthlyIncome.x40.70k with p-value 0.00268
model21 <- glm(formula = Attrition ~ NumCompaniesWorked + 
                 TotalWorkingYears + 
                 YearsSinceLastPromotion + YearsWithCurrManager + EnvironmentSatisfaction + 
                 JobSatisfaction + WorkLifeBalance + Avg.hrs + 
                 BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle, family = "binomial", 
               data = train)
summary(model21)
########################################################################
# Final Model: with 11 significant variables in the model #
#######################################################################
final.model<- model21

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                       -2.24142    0.08905 -25.171  < 2e-16 ***
#   NumCompaniesWorked                -0.87385    0.18015  -4.851 1.23e-06 ***
#   TotalWorkingYears                 -0.72030    0.10376  -6.942 3.86e-12 ***
#   YearsSinceLastPromotion            0.33974    0.06577   5.165 2.40e-07 ***
#   YearsWithCurrManager              -1.00333    0.11807  -8.498  < 2e-16 ***
#   EnvironmentSatisfaction           -0.43428    0.05699  -7.620 2.54e-14 ***
#   JobSatisfaction                   -0.41483    0.05768  -7.191 6.41e-13 ***
#   WorkLifeBalance                   -0.22350    0.05585  -4.002 6.29e-05 ***
#   Avg.hrs                           -1.22828    0.09392 -13.078  < 2e-16 ***
#   BusinessTravel.xTravel_Frequently  0.86287    0.13062   6.606 3.95e-11 ***
#   JobRole.xManufacturing.Director   -0.81632    0.22370  -3.649 0.000263 ***
#   MaritalStatus.xSingle              0.93016    0.11568   8.041 8.90e-16 ***

###########################
### Model Evaluation  ###
###########################
### Test Data ####
#predicted probabilities of attrition '1' for test data
test.pred = predict(final.model, type = "response", 
                    newdata = test)
summary(test.pred) # summary of the predicted values

test$prob <- test.pred
View(test)
# Let's use the probability cutoff of 50%.
test.pred.attrition <- factor(ifelse(test.pred >= 0.50, "Yes", "No"))
test.actual.attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))

table(test.actual.attrition, test.pred.attrition) # 86% accuracy & 28% sensitivity

############################################
# when the probability cutoff of is 40%. ##
############################################
test.pred.attrition <- factor(ifelse(test.pred >= 0.40, "Yes", "No"))
test.conf <- confusionMatrix(test.pred.attrition, test.actual.attrition, positive = "Yes")
test.conf # very low sensitivity at 35% and low PPV (precision) at 57% 
# although 85% accuracy; 95% specificity
# sensitivity: perc of yeses correctly predicted; 
# specificity: perc of nos correctly predicted

#############################
## Optimal cut off value ##
#############################
# Let's find out the optimal probalility cutoff 
# helper function to seperate model performance data from confusion matrix
populateCMdata <- function(cutoff)  {
  predicted.attrition <- factor(ifelse(test.pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted.attrition, test.actual.attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) #transpose the matrix
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix 
# of 100 X 3.
# Summary of test probability
summary(test.pred)

cutoff.data = seq(.01,.80,length=100)
cmdata = matrix(0,100,3) # matrix to hold model performance data

for(i in 1:100){
  cmdata[i,] = populateCMdata(cutoff.data[i])
} 
(cutoff <- cutoff.data[which(abs(cmdata[,1]-cmdata[,2]) < 0.01)])
plot(cutoff.data, cmdata[,1], xlab="Cutoff", ylab="Value", cex.lab=1,
     cex.axis=1, ylim=c(0,1), type="l", lwd=2, axes=FALSE, col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(cutoff.data,cmdata[,2],col="darkgreen",lwd=2)
lines(cutoff.data,cmdata[,3],col=4,lwd=2)
box()
legend(0.10,0.25,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),
       c("Sensitivity","Specificity","Accuracy"))
text(0.24, 0.3, labels=sprintf("cutoff value: %0.7f", cutoff))

# Let's choose a cutoff value of 0.1536364 for final model
test.cutoff.attrition <- factor(ifelse(test.pred >= 0.1536364, "Yes", "No"))
conf.final <- confusionMatrix(test.cutoff.attrition, test.actual.attrition, positive = "Yes")
acc <- conf.final$overall[1]
sens <- conf.final$byClass[1]
spec <- conf.final$byClass[2]
acc # 78%
sens # 78%
spec # 78%
View(test)

########################################
### KS -statistic - Test Data ######
########################################
test.cutoff.attrition <- ifelse(test.cutoff.attrition=="Yes",1,0)
test.actual.attrition <- ifelse(test.actual.attrition=="Yes",1,0)
#on testing  data
pred.object.test<- prediction(test.cutoff.attrition, test.actual.attrition)
performance.measures.test<- performance(pred.object.test, measure = "tpr", x.measure = "fpr")

################################################################################
### Plot Receiver Operating Characteristics (ROC) Curve: AUC calculation ######
################################################################################
plot(performance.measures.test, type = "b", col = "red", lwd=1.5,
     main = "ROC Curve",
     ylab = "Sensitivity:TPR", 
     xlab = "(1 - Specificity):FPR")
abline(0,1, lty = 8, col = "grey", untf = T)
auc<-performance(pred.object.test,"auc")
auc.value <- unlist(auc@y.values)
text(0.8, 0.23, labels=sprintf("AUC: %0.3f", auc.value))

##
ks.table.test <- attr(performance.measures.test, "y.values")[[1]] - 
  (attr(performance.measures.test, "x.values")[[1]])
max(ks.table.test) # 0.5688

##############################
## Lift & Gain Chart ##
#############################
# helper function to calculate gain & lift
calcLift <- function(labels , predicted.prob, groups=10) {
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted.prob)) predicted.prob <- as.integer(as.character(predicted.prob))
  helper = data.frame(cbind(labels , predicted.prob))
  helper[,"bucket"] = ntile(-helper[,"predicted.prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}
attrition.decile  <-  calcLift(test.actual.attrition, test.pred, groups = 10)

######## plot the lift chart #######
plot(attrition.decile$Cumlift, type="l", lwd=2, col="red",# lty=4,
     xlim = c(0,10),
     ylim = c(0,4),
     main = "Lift Chart",
     xlab = "Decile",
     ylab = "Lift")
abline(h=1, col="brown")
axis(1, 1:10)
abline(h=0:10, v=0:10, lty=3)

######### Plot Gain Chart #########
library(InformationValue)
ks_plot(test.actual.attrition, test.cutoff.attrition) # Gain chart plot
#######################################################################################
# ************************* ********************************* 
