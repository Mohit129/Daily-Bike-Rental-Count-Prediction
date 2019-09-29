# cleaning the R environment
rm(list = ls())

# Set working directory and load data

setwd("F:/Edwisor python codes")
day = read.csv("day.csv", header = T)


# Check the data and datatypes
View(day)

# datset contains 731 obs. and 16 variables

# now we will check the datatypes of all variables
str(day)


# here we clearly know that variables season, yr, mnth, holiday, weekday, workingday and weathersit are factor(category) not integer
# so we need to convert their datatypes before EDA process

day$season = as.factor(day$season)
day$yr = as.factor(day$yr)
day$mnth = as.factor(day$mnth)
day$holiday = as.factor(day$holiday)
day$workingday = as.factor(day$workingday)
day$weekday = as.factor(day$weekday)
day$weathersit = as.factor(day$weathersit)

####################### Exploratory data analysis  #####################

# here we are given yr and month variable but date variable is missing to analyse trends
# so we will etract date variable from dteday column


day$dteday = as.Date(day$dteday, format= "20%y-%m-%d")
day$date = as.integer(gsub("(.*)[-]", "", day$dteday))
day$date = as.factor(day$date)
# once we got date now instant and dteday column are not useful so we can remove them

day$instant = day$dteday = NULL

# before proceeding for visualizations we need to look for missing values in our dataset and do the appropriate pre processing


# Missing value Analysis

missing_values = sapply(day, function(x){sum(is.na(x))})
missing_values

# NO missing values present in any of the variables

# Data Visualization by uing boxplot, histograms and bar graph to better understand data

# box plot for numerical variables to check distribution and detect outliers
install.packages("ggplot2")
library(ggplot2)
cnames = colnames(day[, c("temp", "atemp", "hum", "windspeed", "casual", "registered", "cnt")])
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = cnames[i]), data = day) + 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot for",cnames[i])))
}
gridExtra::grid.arrange(gn1, gn2, gn3, gn4, gn5, gn6, gn7, ncol=4)


# Outlier analysis
# outliers are present in data windspeed and humidity and casual users
# for now we wont do anything with outliers and further explore data, before model building will take call

# Here our final output is continous variable so this is a regression problem
# for regression algorithm to work data must be normally distributed and not skewed


# will check for our continous variable how they are distributed


hist1 = ggplot(data = day, aes(x =temp)) + ggtitle("Distribution of Temperature") + geom_histogram(bins = 20)
hist2 = ggplot(data = day, aes(x =hum)) + ggtitle("Distribution of Humidity") + geom_histogram(bins = 20)
hist3 = ggplot(data = day, aes(x =atemp)) + ggtitle("Distribution of Feel Temperature") + geom_histogram(bins = 20)
hist4 = ggplot(data = day, aes(x =windspeed)) + ggtitle("Distribution of Windspeed") + geom_histogram(bins = 20)
hist5 = ggplot(data = day, aes(x =registered)) + ggtitle("Distribution of Registered user") + geom_histogram(bins = 20)
hist6 = ggplot(data = day, aes(x =casual)) + ggtitle("Distribution of casual user") + geom_histogram(bins = 20)
hist7 = ggplot(data = day, aes(x =cnt)) + ggtitle("Distribution of final cnt") + geom_histogram(bins = 20)

gridExtra::grid.arrange(hist1,hist2,hist3,hist4, hist5, hist6, hist7, ncol=3)

# Some important points to note from here
# 1. Distribution of humidity and windspeed is not exactly normal dostributed
# 2. Registered user count is normally distributed but casual users is left skewed
# 3. Final cnt variable is almost normally distributed

# now we will see distribution of categorical variabel with respect to continous variable
bar1 = ggplot(aes_string(y = "cnt", x = "season"), data = day) + geom_boxplot(fill = 'red', outlier.color = 'blue', outlier.shape = 18, outlier.size = 1)
bar2 = ggplot(aes_string(y = "cnt", x = "yr"), data = day) + geom_boxplot(fill='red', outlier.color = 'blue', outlier.shape = 18, outlier.size = 1)
bar3 = ggplot(aes_string(y = "cnt", x = "mnth"), data = day) + geom_boxplot(fill='red', outlier.color = 'blue', outlier.shape = 18, outlier.size = 1)
bar4 = ggplot(aes_string(y = "cnt", x = "holiday", fill= "yr"), data = day) + geom_boxplot(outlier.color = 'blue', outlier.shape = 18, outlier.size = 1)
bar5 = ggplot(aes_string(y = "cnt", x = "weekday"), data = day) + geom_boxplot(fill='red', outlier.color = 'blue', outlier.shape = 18, outlier.size = 1)
bar6 = ggplot(aes_string(y = "cnt", x = "workingday"), data = day) + geom_boxplot(fill='red', outlier.color = 'blue', outlier.shape = 18, outlier.size = 1)
bar7 = ggplot(aes_string(y = "cnt", x = "weathersit"), data = day) + geom_boxplot(fill='red', outlier.color = 'blue', outlier.shape = 18, outlier.size = 1)
bar8 = ggplot(aes_string(y = "cnt", x = "date"), data = day) + geom_boxplot(fill='red', outlier.color = 'blue', outlier.shape = 18, outlier.size = 1)

gridExtra::grid.arrange(bar1,bar2,bar3,bar4, bar5, bar6, bar7, bar8, ncol=4)

# some of the analysis drawn from this plots are
# 1. Weather situation 1 has a higher count which reflects people like to ride a bicycle in clear weather
# 2. In fall and Summer season more people ride a bicycle and it drops in Spring season
# 3. From month 5 to month 10 there is high demand it fits well with season analysis
# 4. There no such trend observed in the day variable it is almost constant
# 5. From the year 2011 to 2012 the average bicycle count has almost doubled, the company is growing
# 6. We could drop the variable date from the dataset as they are not exhibiting any trend
# 7. On weekends/holiday the count is low because registered user doesn't go to the office and casual users are using it more as compared to the registered user



# Feature selection/Correlation Analysis
install.packages("corrgram")
library(corrgram)
corrgram(day[,c("temp", "atemp", "hum", "windspeed", "casual", "registered", "cnt")], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot for Continous Variable")

# From here we can say that temp and atemp are highly correlated
# cnt and registered user are also highly correlated, which is true also becuase with passage of time registered user base will increase and so casual user will represent much less in total count
# We can remove temp because atemp is feeled temperature which is important from human point of view, so remove temp.

# Feature Selection

# 1. We will remove "temp" variable as it is highly correlated with "atemp"
# 2. We will also remove "date" variable as it si not exhibiting any trend and is of no use
# 3. We will also remove Registered and casual user because we are interested in final count and registered suer base will increase over time

day$temp = day$date = day$casual = day$registered = NULL

##################### Model Building and Evaluation ###########################

# Here we will use ensemble technique to build our regression model
# Past research indicates that ensemble techniques is most suitable for this type of problem
# Decision tree is weak learner whereas Random Forest is collection of all weak learners to from strong learning algorithm
# So we will use RandomForest Regressor under ensemble technique to build model
# here we will predict both casual and registered user separately and then add it up to get final count


install.packages("rpart")
library("rpart")
set.seed(123)
# splitting dataset into train and test

train_index = sample(1:nrow(day), 0.80 * nrow(day))
train = day[train_index,]
test = day[-train_index,]


# installing random Forest and running it
install.packages("randomForest")
library(randomForest)
set.seed(123)
#Train the data using random forest
rf_model = randomForest(cnt~., data = train, ntree = 500)

#Predict the test cases
rf_predictions = predict(rf_model, test[,-11])

#calculate MAPE
MAPE = function(actual, pred){
  print(mean(abs((actual - pred)/actual)) * 100)
}
MAPE(test[,11], rf_predictions)
# So here by splitting the data into 80-20 and trees 500 we are gettign accuracy of 85.7%
# This ends our project here

