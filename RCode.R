library(readr)
cars <- read.csv("cars.csv")
View(cars)
attributes(cars)#list the attribute
summary(cars) #prints min, max, median & quartile
str(cars) #displays the structure of the dataset
names(cars) #names of the attributes
cars$`name of car`
head(cars,4) # prints the head
cars[1:4,]
# dbl stands for double class.
# A double-precision floating point number. 

#*******EDA**********
#bins (0,10], (10,20], (20,25]
hist(cars$speed.of.car, breaks=c(0,10,20,25))
hist(cars$speed.of.car)
hist(cars$distance.of.car)
#to get the density graph
hist(cars$speed.of.car,prob=T)

stem(cars$speed.of.car)
#boxplot
boxplot(cars$speed.of.car, cars$distance.of.car,varwidth=T)
par(mfrow = c(1,2)) # divide graph area in 2 columns
boxplot(cars$speed.of.car, main="Speed",
        sub = paste("Outlier rows:",boxplot.stats(cars$speed.of.car)$out))

boxplot(cars$distance.of.car, main="Distance",
        sub = paste("Outlier rows:",boxplot.stats(cars$distance.of.car)$out))
par(mfrow = c(1,1))
#Normal Quantile Plot- is a way to see if your data is normally distributed
qqnorm(cars$speed.of.car)
# Bar charts are different from histograms.
# Histograms are for numeric data whereas
# bar charts and pie charts are for counts
# of levels of a factor.  
# Usually, the counts must be tabulated first with the table function.

counts = table(cars$name.of.car)
barplot(counts)
pie(counts)

#scatter plot
plot(cars$speed.of.car, cars$distance.of.car)
plot(cars$speed.of.car~ cars$distance.of.car)

#to plot a line in the scatter plot
xymodel = lm(cars$speed.of.car~ cars$distance.of.car,data = cars)
plot(cars$speed.of.car~ cars$distance.of.car)
abline(coef(xymodel))
scatter.smooth(x=cars$speed.of.car, y=cars$distance.of.car, main="Dist ~ Speed")


#multi scatter plot
plot(cars)

#3d scatter plot
# install.packages("Rcmdr")
# library(Rcmdr)

#density 
library(e1071)
plot(density(cars$speed.of.car), main="Speed",
     ylab = "frequency", sub=paste("Skewness", round(e1071::skewness(cars$speed.of.car),2)))
polygon(density(cars$speed.of.car), col="red")
plot(density(cars$distance.of.car,main="Distance"),
     ylab = "Frequency",sub=paste("skewness:",round(e1071::skewness(cars$distance.of.car),2)))
polygon(density(cars$distance.of.car),col = "red")

#correlation
cor(cars$speed.of.car, cars$distance.of.car)

#***preprocessing***
#changing name
names(cars)<-c("name","speed","distance")
summary(cars)
str(cars)
#missing value
is.na(cars)# gives true id n/a
table(is.na(cars)) #gives the list 
sapply(cars, function(cars)sum(is.na(cars)))
#remove missing
na.omit(cars$name)#Drops any rows with missing values and omits them forever.
na.exclude(cars$speed)#Drops any rows with missing values, but keeps track of where they were.
cars$distance[is.na(cars$distance)]<-mean(cars$distance,na.rm = TRUE)
#.na.rm literally means NA remove.It tells weather or not to remove

#duplicate
library(tidyverse)
table(duplicated(cars)) #gives you no:of duplicate
unique(cars)
#remove duplicate based on all columns
cars%>% distinct()

#*******Modelling*********
#*Creating test & train
#*seed is a number that is choosen as a starting point
#*to create sequence of random numbers
set.seed(123)
#The nrow R function returns the number of rows that are present in a data frame or matrix.
trainSize <- round(nrow(cars)*0.7)
testSize <- nrow(cars) - trainSize
trainSize
testSize
#Sample function in R, 
#generates a sample of the specified size from the data set
#The seq_len(number) function creates a 
#sequence that starts at 1 and with steps of 1 finishes at the number value

seq_len(nrow(cars))
training_indices <- sample(seq_len(nrow(cars)),size = trainSize)
training_indices
-training_indices
trainSet <- cars[training_indices,]
testSet <- cars[-training_indices,]
cars[-1,] #-ve number here means selecting all other than numer

#build a model
linearMod <- lm(distance ~ speed, data = trainSet)
print(linearMod)
#**Now that we have built the 
#*linear model, we also have established the relationship 
#*between the predictor and response in the form of a mathematical formula for 
#*Distance (dist) as a function for speed. For the above output, 
#*you can notice the ‘Coefficients’ part having two 
#*components: Intercept: -17.579, speed: 3.932 These are also called 
#*the beta coefficients. In other words,
#*dist = Intercept + (β ∗ speed)

summary(linearMod)

modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["speed", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["speed", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- linearMod$fstatistic[1]  # fstatistic
f_statistic
modelSummary$fstatistic #higher the better

f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)
model_p

AIC(linearMod) #lower the better
BIC(linearMod)

#****predictions**
Distance_pred <-predict(linearMod,testSet)
Distance_pred

actuals_preds <-data.frame(cbind(actuals = testSet$distance,
                                 predictions = Distance_pred))
correlation_accuracy <- cor(actuals_preds)
correlation_accuracy
head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min)/apply(actuals_preds, 1, max))
#mean absolute percentage deviation
mape <- mean(abs(actuals_preds$predictions -actuals_preds$actuals)/actuals_preds$actuals)

#*****Kfold***** 
install.packages("DAAG")
library(DAAG)
# cvResults <- suppressWarnings(CVlm(data = cars, form.lm= distance ~ speed,
#                                    m=3,dots=FALSE, seed=29,
#                                    legend.pos="topleft",  printit=FALSE,
#                                    main="Small symbols are predicted values while bigger ones are actuals."));  # performs the CV
# 
# attr(cvResults, 'ms')
cvResults <- CVlm(data = cars, form.lm=formula(distance ~ speed),
                  m=3,dots=FALSE, seed=29,plotit = c("Observed","Residual"),
                  legend.pos="topleft",  printit=FALSE,
                  main="Small symbols are predicted values while bigger ones are actuals.");  # performs the CV
View(cvResults)

#install.packages("caret")
library(caret)
#define a train control
set.seed(123)
train.control <- trainControl(method = "repeatedcv",
                              number = 10, repeats = 3)
#train the model
model <- train(distance ~ speed, data = cars,
               method = "lm",
               trControl = train.control)
print(model)

x = summary(model)
