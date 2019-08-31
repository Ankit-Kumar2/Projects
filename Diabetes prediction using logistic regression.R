# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

install.packages("ggplot2")
library(ggplot2) # Data visualization
install.packages("readr")
library(readr) # CSV file I/O, e.g. the read_csv function

# Setting working directory 

setwd("C:/Users/DELL/Desktop/Linear Regression Project/Logistic Regression with R")
# Any results you write to the current directory are saved as output.

install.packages("ggcorrplot")
install.packages("ROCR")

db = read.csv('C:/Users/DELL/Desktop/Linear Regression Project/Logistic Regression with R/diabetes.csv', header=TRUE)
str(db)
head(db)
tail(db)
summary(db) # Dont see any outliers in the data columns 

# Create Age Category column
db$Age_Cat <- ifelse(db$Age < 21, "<21", 
                     ifelse((db$Age>=21) & (db$Age<=25), "21-25", 
                            ifelse((db$Age>25) & (db$Age<=30), "25-30",
                                   ifelse((db$Age>30) & (db$Age<=35), "30-35",
                                          ifelse((db$Age>35) & (db$Age<=40), "35-40",
                                                 ifelse((db$Age>40) & (db$Age<=50), "40-50",
                                                        ifelse((db$Age>50) & (db$Age<=60), "50-60",">60")))))))

db$Age_Cat <- factor(db$Age_Cat, levels = c('<21','21-25','25-30','30-35','35-40','40-50','50-60','>60'))
table(db$Age_Cat)

ggplot(aes(x = Age), data=db) +
  geom_histogram(binwidth=1, color='black', fill = "#F79420") +
  scale_x_continuous(limits=c(20,90), breaks=seq(20,90,5)) +
  xlab("Age") +
  ylab("No of people by age")


# Most of the subjects are in the age bracket of 21-30 

# Bar plot by Age_Cat

# Barplot by Age_Cat
library(ggplot2)
ggplot(aes(x = Age_Cat), data = db) +
  geom_bar(fill='steelblue')

library(ggplot2)
ggplot(aes(x=Age_Cat, y = BMI), data = db) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0,70))

# No clear trend between Age and BMI variables 

by(db$BMI, db$Age_Cat, summary)
# Compute correlation matrix
db_cor <- round(cor(db[1:8]),1)
db_cor

library(ggcorrplot)
ggcorrplot(db_cor)

# No high correlation observed among the variables

# Split dataset into train and test sets
require(caTools)
set.seed(3)
sample = sample.split(db$Outcome, SplitRatio=0.75)
train = subset(db, sample==TRUE)
test = subset(db, sample==FALSE)

# Check if the distribution of age is same as the original dataset 

table(train$Age_Cat)

# Baseline Model 
table(db$Outcome)

baseline <- round(500/nrow(db),2)
baseline

# Do not select a model whose accuracy is lower than the baseline model. In this case, it is 0.65

# Fit Model using all independent variables 

AllVar <- glm(Outcome ~ Glucose + BMI + BloodPressure + DiabetesPedigreeFunction 
              + Age_Cat30-35 + Age_Cat40-50, data = train, family = binomial)
summary(AllVar)

PredictTrain <- predict(AllVar, type = "response")
summary(PredictTrain)

tapply(PredictTrain, train$Outcome, mean)

# Confusion Matrix
# Build confusion matrix with a threshold value of 0.5

threshold_0.5 <- table(train$Outcome, PredictTrain > 0.5)
threshold_0.5

# Accuracy
accuracy_0.5 <- round(sum(diag(threshold_0.5))/sum(threshold_0.5),2)
sprintf("Accuracy is %s",accuracy_0.5)

# Mis-classification error rate
MC_0.5 <- 1-accuracy_0.5
sprintf("Mis-classification error is %s",MC_0.5)

sensitivity0.5 <- round(118/(83+118),2)
specificity0.5 <- round(333/(333+42),2)
sprintf("Sensitivity at 0.5 threshold: %s", sensitivity0.5)
sprintf("Specificity at 0.5 threshold: %s", specificity0.5)

# Build confusion matrix with a threshold value of 0.7

threshold_0.7 <- table(train$Outcome, PredictTrain > 0.7)
threshold_0.7

# Accuracy
accuracy_0.7 <- round(sum(diag(threshold_0.7))/sum(threshold_0.7),2)
sprintf('Accuracy is %s', accuracy_0.7)

# Mis-classification error rate
MC_0.7 <- 1-accuracy_0.7
sprintf("Mis-classification error is %s",MC_0.7)

sensitivity0.7 <- round(78/(123+78),2)
specificity0.7 <- round(359/(359+16),2)
sprintf("Sensitivity at 0.7 threshold: %s", sensitivity0.7)
sprintf("Specificity at 0.7 threshold: %s", specificity0.7)

# Build confusion matrix with a threshold value of 0.2

threshold_0.2 <- table(train$Outcome, PredictTrain > 0.2)
threshold_0.2

# Accuracy
accuracy_0.2 <- round(sum(diag(threshold_0.2))/sum(threshold_0.2),2)
sprintf("Accuracy is %s", accuracy_0.2)

# Mis-classification error rate
MC_0.2 <- 1-accuracy_0.2
sprintf("Mis-classification error is %s",MC_0.2)

sensitivity0.2 <- round(180/(21+180),2)
specificity0.2 <- round(215/(215+160),2)
sprintf("Sensitivity at 0.2 threshold: %s",sensitivity0.2)
sprintf("Specificity at 0.2 threshold: %s",specificity0.2)

# Generate ROC Curves

library(ROCR)

ROCRpred = prediction(PredictTrain, train$Outcome)
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Adding threshold labels
plot(ROCRperf, colorize=TRUE, print.cutoffs.at = seq(0,1,0.1), text.adj = c(-0.2, 1.7))
abline(a=0, b=1)

auc_train <- round(as.numeric(performance(ROCRpred, "auc")@y.values),2)
legend(.8, .2, auc_train, title = "AUC", cex=1)

# Making Predictions on the test data 

PredictTest <- predict(AllVar, type = "response", newdata = test)

# Convert probabilities to values using the below

## Based on ROC curve above, selected a threshold of 0.5
test_tab <- table(test$Outcome, PredictTest > 0.5)
test_tab

accuracy_test <- round(sum(diag(test_tab))/sum(test_tab),2)
sprintf("Accuracy on test set is %s", accuracy_test)

# Compute test set AUC

ROCRPredTest = prediction(PredictTest, test$Outcome)
auc = round(as.numeric(performance(ROCRPredTest, "auc")@y.values),2)
auc