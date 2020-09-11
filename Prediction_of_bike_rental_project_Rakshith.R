#Clean the environment
rm(list=ls())

#Set Working Directory
setwd("C:/Users/User/Desktop/edwisor/New folder")

#get Working directory
getwd()

#Load the librarires
libraries = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')


install.packages(libraries)
lapply(X = libraries,require, character.only = TRUE)
rm(libraries)

#load Bike rental data in R

df_day= read.csv(file = "day.csv", header = T)

########################################EXPLORATARY DATA ANALYSIS########################################
# Summarizing  data

#Class of data
class(df_day)

#Verify first five rows of data

head(df_day)

#Dimensions of data
dim(df_day)

#Column names
names(df_day)

#Structure of variables
str(df_day)

#Verify  summary of data
summary(df_day)

##Dropping the variables which are not necessary for our model

#variable "instant" can be dropped as it simply represents the index number
# casual and registered variables can be removed, as these two sums to dependent variable count and which is what we need to predict
# Variable "dteday" can be ignored as the output is not based on time series analysis

df_day = subset(df_day, select = -c(instant, dteday, casual, registered))

#Dimensions of data after dropping
dim(df_day)
names(df_day)

#Classifying into numeric and categorical variables and saving those in a specific array

numeric_var = c('temp', 'atemp', 'hum', 'windspeed', 'cnt')

categorical_var = c('season', 'yr', 'mnth', 'holiday', 'weekday', 'workingday', 'weathersit')

####Missing Value analysis
summary(is.na(df_day))
sum(is.na(df_day))

#No missing values in dataset

####Outlier Analysis

df = df_day
df_day = df

# #Check for outliers in data using boxplot method
library(ggplot2)

for (i in 1:length(numeric_var))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (numeric_var[i]), x = "cnt"), data = subset(df_day))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "blue" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=numeric_var[i],x="count")+
           ggtitle(paste("Box plot of count versus",numeric_var[i])))
}

## Plotting of the plots together
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5, ncol=2) 

#outliers are found in windspeed and humidity variables.

#To replace outliers with NA

for(i in numeric_var){
  print(i)
  outlier_number = df_day[,i][df_day[,i] %in% boxplot.stats(df_day[,i])$out]
  print(length(outlier_number))
  df_day[,i][df_day[,i] %in% outlier_number] = NA
}

sum(is.na(df_day))


#Impute the NA values with KNN

library(DMwR)
library(rpart)

df_day = knnImputation(df_day, k = 5)

sum(is.na(df_day))

#### Data Understanding 
# In order to plot some graphs,install few libraries

library(ggplot2)
library(gplots)
library(scales)
library(psych)


# Barplot with x axis being season and y axis being count
ggplot(df_day, aes(x = 'season', y= 'cnt')) +
  geom_bar(stat = "identity", fill = "blue")+
  labs(title = "Count of bikes rented wrt season", x = "Seasons", y = "cnt")+ 
  theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(face = "bold"))
#We can observe from the cat plots that In Season 2, 3 and 4 has the highest count compared to season 1

# Barplot with x axis being year and y axis being count
ggplot(df_day, aes(x = df_day$yr, y = df_day$cnt))+
  geom_bar(stat = "identity", fill = "red")+
  labs(title = "Count of bikes rented wrt year", x = "yr", y = "cnt")+ 
  theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(face = "bold"))
#In Year 1 has high count than 0 (0= 2011, 1= 2012)


# Barplot with x axis being weekday and y axis being count
ggplot(df_day, aes(x = df_day$weekday, y = df_day$cnt))+
  geom_bar(stat = "identity", fill = "navyblue")+
  labs(title = "Count of bikes rented wrt days", x = "Days of week", y = "count")+ 
  theme(panel.background = element_rect("white"))+
  theme(plot.title = element_text(face = "bold"))
#In weekdays, 0 and 6 has the highest count

####Feature Selection 
df2 = df_day
df_day = df2

# Correlation Analysis to find varaibles which can be excluded
library(corrgram)

corrgram(df_day[,numeric_var],order=FALSE,upper.panel = panel.pie,
         text.panel = panel.txt,
         main= "Correlation Analysis with numeric variables")
#It is found that temperature and atemp are highly correlated with each other.

#Anova test to find varaibles which can be dropped
for(i in categorical_var){
  print(i)
  Anova_test_result = summary(aov(formula = cnt~df_day[,i],df_day))
  print(Anova_test_result)
}
#It is found that holiday, weekday and workingday has p value > 0.05, by which, we accept null hypothesis.

#Dimension Reduction. Drop atemp, holiday, weekday and working day
df_day = subset(df_day, select=-c(atemp,holiday,weekday,workingday))

####Feature Scaling
#Final Variables- the cleaned data
numeric_var = c("temp","hum","windspeed","cnt")
catergorical_var = c("season", "yr", "mnth", "weathersit")

# To check whether the variables are normally distributed 
# Skewness test

library(propagate)

for(i in numeric_var){
  print(i)
  skew = skewness(df_day[,i])
  print(skew)
}
# We can observe that dataset is approximately symmetric.


# Summary of the variables to check normality

for(i in numeric_var){
  print(summary(df_day[,i]))
}

#Data is found to be normalized, scaling not required

# visualizing normality check 

hist(df_day$hum, col="Blue", xlab="Humidity", ylab="Frequency",
     main="Humidity Distribution")

hist(df_day$temp, col="Navyblue", xlab="Temperature", ylab="Frequency",
     main="Temperature Distribution")

hist(df_day$windspeed,col="Dark green",xlab="Windspeed",ylab="Frequency",
     main="Windspeed Distribution")

#Distribution is approximately symmetric

####################MODELING ############################


library(DataCombine)
rmExcept("df_day")


df3 = df_day
df_day = df3

##define the Error Metrics.

#MAPE

MAPE = function(y,y1){
  mean(abs((y-y1)/y))*100
}


#R Square 

Rsquare = function(y,y1){
  cor(y,y1)^2
}


####creation of dummies

categorical_var = c("season","yr","mnth","weathersit")

library(dummies)

df_day = dummy.data.frame(df_day, categorical_var)

#Divide the data into train and test

set.seed(123)
train_index = sample(1:nrow(df_day),0.8*nrow(df_day))
train= df_day[train_index,]
test= df_day[-train_index,]


####check multicollinearity 

numeric_var = c("temp","hum","windspeed", "cnt")

numeric_var2 = df_day[,numeric_var]

library(usdm)

vifcor(numeric_var2, th = 0.7)

#No collinearity problem observed.


#########################DECISION TREE ##################################

library(rpart)

DTModel = rpart(cnt~., train, method = "anova" , minsplit=5)

# Prediction

DTTest = predict(DTModel, test[-25])

#summary
summary(DTModel)

#MAPE value

DTMape_Test = MAPE(test[,25], DTTest)
DTMape_Test   #26.42


#R Square value

DT_RSquare = Rsquare(test[,25], DTTest)
DT_RSquare  #0.7612


#######################RANDOM FOREST#############################

library(randomForest)
set.seed(123)

RFModel = randomForest(cnt~., train, ntree = 500, importance = TRUE)

# Prediction

RFTest = predict(RFModel, test[-25])


# MAPE Value

RFMape_Test = MAPE(test[,25], RFTest)
RFMape_Test  #  19.32

#R Square value

RF_RSquare = Rsquare(test[,25], RFTest)
RF_RSquare   # 0.8685

######################LINEAR REGRESSION#########################

LRModel = lm(cnt~., train)
#Summary
summary(LRModel)


# Predictions on test values

LRTest = predict(LRModel, test[-25])


#MAPE Value

LRMape_Test = MAPE(test[,25], LRTest)
LRMape_Test #  21.56


#R Square Value

LR_RSquare = Rsquare(test[,25], LRTest)
LR_RSquare  #  0.8191


########################Model Selection & Evaluation ########################

print("MAPE Values")
print(DTMape_Test)
print(RFMape_Test)
print(LRMape_Test)

print("Accuracy")
print(100 - DTMape_Test)
print(100 - RFMape_Test)
print(100 - LRMape_Test)


print("R-Square Values")
print(DT_RSquare)
print(RF_RSquare)
print(LR_RSquare)


