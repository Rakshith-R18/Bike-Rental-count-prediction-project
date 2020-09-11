#Clean the environment
rm(list = ls())

#Set working directory
setwd("C:/Users/User/Desktop/edwisor/New folder")

#get Working directory
getwd()

#Load the librarires
libraries = c("plyr","dplyr", "ggplot2","rpart","dplyr","DMwR","randomForest","usdm","corrgram","DataCombine")
lapply(X = libraries,require, character.only = TRUE)

#load Bike rental data in R

df_day= read.csv(file = "day.csv", header = T)

########################################EXPLORATARY DATA ANALYSIS########################################
# Summarizing  data 

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

################################FEATURE ENGINEERING#################################
#Creating columns
df_day$actual_temp <- df_day$temp*39
df_day$actual_feel_temp <- df_day$atemp*50
df_day$actual_windspeed <- df_day$windspeed*67
df_day$actual_humidity = df_day$hum * 100

df_day$actual_holiday = factor(x = df_day$holiday, levels = c(0,1), labels = c("Working day","Holiday"))
df_day$actual_year = factor(x = df_day$yr, levels = c(0,1), labels = c("2011","2012"))
df_day$actual_season = factor(x = df_day$season, levels = c(1,2,3,4), labels = c("Spring","Summer","Fall","Winter"))
df_day$actual_weathersit = factor(x = df_day$weathersit, levels = c(1,2,3,4), 
                               labels = c("Clear","Cloudy/mist","Rain/Snow/fog","Heavy rain/snow/fog"))

df_day$weathersit = as.factor(df_day$weathersit)
df_day$season = as.factor(df_day$season)
df_day$dteday = as.character(df_day$dteday)
df_day$mnth = as.factor(df_day$mnth)
df_day$weekday = as.factor(as.character(df_day$weekday))
df_day$workingday = as.factor(as.character(df_day$workingday))
df_day$yr = as.factor(df_day$yr)
df_day$holiday = as.factor(df_day$holiday)
head(df_day)

########################################MISSING VALUES ANALYSIS########################################
missing_values = sapply(df_day, function(x){sum(is.na(x))})
missing_values
#There are no missing values

############################EXPLORING USING GRAPHS##############################
#Check the distribution of categorical variables using bar graph
bar1 = ggplot(data = df_day, aes(x = actual_season)) + geom_bar() + ggtitle("Count of the Season")
bar2 = ggplot(data = df_day, aes(x = actual_weathersit)) + geom_bar() + ggtitle("Count of the Weather")
bar3 = ggplot(data = df_day, aes(x = actual_holiday)) + geom_bar() + ggtitle("Count of the Holiday")
bar4 = ggplot(data = df_day, aes(x = workingday)) + geom_bar() + ggtitle("Count of the Working day")


gridExtra::grid.arrange(bar1,bar2,bar3,bar4,ncol=2)

#Check the distribution of numerical data using histogram
hist1 = ggplot(data = df_day, aes(x =actual_temp)) + ggtitle("Distribution of Temperature") + geom_histogram(bins = 25)
hist2 = ggplot(data = df_day, aes(x =actual_humidity)) + ggtitle("Distribution of Humidity") + geom_histogram(bins = 25)
hist3 = ggplot(data = df_day, aes(x =actual_feel_temp)) + ggtitle("Distribution of Feel Temperature") + geom_histogram(bins = 25)
hist4 = ggplot(data = df_day, aes(x =actual_windspeed)) + ggtitle("Distribution of Windspeed") + geom_histogram(bins = 25)


gridExtra::grid.arrange(hist1,hist2,hist3,hist4,ncol=2)

#Check the distribution of numerical data using scatterplot
scat1 = ggplot(data = df_day, aes(x =actual_temp, y = cnt)) + ggtitle("Distribution of Temperature") + geom_point() + xlab("Temperature") + ylab("Bike COunt")
scat2 = ggplot(data = df_day, aes(x =actual_humidity, y = cnt)) + ggtitle("Distribution of Humidity") + geom_point(color="green") + xlab("Humidity") + ylab("Bike COunt")
scat3 = ggplot(data = df_day, aes(x =actual_feel_temp, y = cnt)) + ggtitle("Distribution of Feel Temperature") + geom_point() + xlab("Feel Temperature") + ylab("Bike COunt")
scat4 = ggplot(data = df_day, aes(x =actual_windspeed, y = cnt)) + ggtitle("Distribution of Windspeed") + geom_point(color="green") + xlab("Windspeed") + ylab("Bike COunt")

gridExtra::grid.arrange(scat1,scat2,scat3,scat4,ncol=2)

#Check for outliers in data using boxplot method
cnames = colnames(df_day[,c("actual_temp","actual_feel_temp","actual_windspeed","actual_humidity")])
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = cnames[i]), data = df_day)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot for",cnames[i])))
}
gridExtra::grid.arrange(gn1,gn3,gn2,gn4,ncol=2)

#Remove outliers in Windspeed
val = df_day[,19][df_day[,19] %in% boxplot.stats(df_day[,19])$out]
df_day = df_day[which(!df_day[,19] %in% val),]

#Check for multicollinearity using VIF
df = df_day[,c("instant","temp","atemp","hum","windspeed")]
vifcor(df)

#Check for collinearity using corelation graph
corrgram(df_day, order = F, upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Remove the unwanted variables

df_day <- subset(df_day, select = -c(holiday,instant,dteday,atemp,casual,registered,actual_temp,actual_feel_temp,actual_windspeed,
                               actual_humidity,actual_season,actual_year,actual_holiday,actual_weathersit))

rmExcept(keepers = "df_day")
########################################DECISION TREE########################################

#Divide the data into train and test
set.seed(123)
train_index = sample(1:nrow(df_day), 0.8 * nrow(df_day))
train = df_day[train_index,]
test = df_day[-train_index,]

#rpart for regression
dt_model = rpart(cnt ~ ., data = train, method = "anova")

#Predict the test cases
dt_predictions = predict(dt_model, test[,-10])

#Create dataframe for actual and predicted values
df = data.frame("actual"=test[,10], "pred"=dt_predictions)
head(df)

#calculate MAPE
regr.eval(trues = test[,10], preds = dt_predictions, stats = c("mae","mse","rmse","mape"))

#calculate MAPE
MAPE = function(actual, pred)
  {
  print(mean(abs((actual - pred)/actual)) * 100)
}
MAPE(test[,10], dt_predictions)

########################################RANDOM FOREST########################################

#Train the data using random forest
rf_model = randomForest(cnt~., data = train, ntree = 500)

#Predict the test cases
rf_predictions = predict(rf_model, test[,-10])

#Create dataframe for actual and predicted values
df = cbind(df,rf_predictions)
head(df)

#Calculate MAPE
regr.eval(trues = test[,10], preds = rf_predictions, stats = c("mae","mse","rmse","mape"))
MAPE(test[,10], rf_predictions)

########################################LINEAR REGRESSION########################################


#Train the data using linear regression
lr_model = lm(formula = cnt~., data = train)

#Check the summary of the model
summary(lr_model)

#Predict the test cases
lr_predictions = predict(lr_model, test[,-10])

#Create dataframe for actual and predicted values
df = cbind(df,lr_predictions)
head(df)

#Calculate MAPE
regr.eval(trues = test[,10], preds = lr_predictions, stats = c("mae","mse","rmse","mape"))
MAPE(test[,10], lr_predictions)

#Plot a graph for actual vs predicted values
plot(test$cnt,type="l",lty=2,col="green")
lines(lr_predictions,col="blue")

#Predict a sample data
predict(lr_model,test[1,])
