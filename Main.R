############ Importing the required libraries
library(dplyr)
library(ggplot2) #for the exploratory data analysis

library(ROCR) # ROCR model validation
#Decision Tree Libraries
library(irr)
library(rpart)
library(caret)
#Tree plotting
library(rattle)
library(rpart.plot)
library(RColorBrewer)

##Random Forest Libraries
library(randomForest)


setwd("D:\\titanic")

getwd()

TTrain<-read.csv("train.csv",header = T,sep = ",",na.strings = "")
TTest<-read.csv("test.csv",header = T,sep = ",",na.strings = "")

TTrain$Set<-"Train"
TTest$Set<-"TTest"

TTest$Survived<-NA

Full<-rbind(TTrain,TTest)


########### Exploratory data analysis
str(Full)
summary(Full)

#Survived having 418 missing values because it is from the Test dataset
#Age 263 missing values
#Cabin 1014 missing values
#Fare having 1 and embarked have 2 missing values

hist(Full$Age)
boxplot(Full$Age)
ggplot(data=TTrain,mapping=aes(x=Age,fill=Survived,alpha=0.2))+geom_density()+scale_x_continuous(limit=c(0,100))
quantile(Full$Age,na.rm = T)
median(Full$Age,na.rm = T)


plot(as.factor(TTrain$Sex),as.factor(TTrain$Survived))

plot(as.factor(TTrain$Pclass),as.factor(TTrain$Survived))

cor(TTrain$Fare,TTrain$Pclass)

histogram(Full$Fare)
ggplot(data=Full,mapping=aes(x=Fare))+geom_density()+scale_x_continuous(limit=c(0,100))

#Embarked might not be important variable as boarding point may not decide a people will die or not

############### Data Preparation

###### replacing Age with the median value
Full[is.na(Full$Age),]$Age<-28

Full$Sex_Male<-ifelse(Full$Sex=="male",1,0)
Full$Sex_Female<-ifelse(Full$Sex=="female",1,0)

Full$Pclass_1<-ifelse(Full$Pclass==1,1,0)
Full$Pclass_2<-ifelse(Full$Pclass==2,1,0)
Full$Pclass_3<-ifelse(Full$Pclass==3,1,0)

Full$Survived<-as.factor(Full$Survived)


#################### Spliting Train, Validation and Test DataSet
set.seed(100)
index<-sample(x = 1:nrow(TTrain),size = 0.8*nrow(TTrain),replace = F)
Train<-Full[Full$Set=="Train",]

Validate<-Train[-index,]
Train<-Train[index,]
Test<-Full[Full$Set=="TTest",]

#################### Applying Decision Tree Model for initial understanding of Parameters
mod<-rpart(Survived~Sex_Male+Sex_Female+Pclass_1+Pclass_2+Pclass_3+Age+Parch+SibSp,data=Train,control=rpart.control(cp=0.002,maxdepth=7),method="class",parms=list(split="gini"))

mod
#Visualization of Model
fancyRpartPlot(mod)
summary(mod)
printcp(mod)
plotcp(mod, minline = TRUE)

### Model Pruning
mod1<-prune(mod,cp= 0.02)

#### Model Accuracy on the Train Data Itself
actual<-Train$Survived
predicted<-predict(mod1,type = "class")
actual<-as.factor(actual)

#### Kappa Metric
kappa2(data.frame(actual,predicted))

#Confusion Matric
confusionMatrix(predicted,actual,positive="1")

#ROCR curve and AUC value
head(predicted)
head(as.numeric(predicted))
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)

head(actual)
head(as.numeric(actual))
actual<-as.numeric(actual)
actual<-ifelse(actual==2,1,0)

pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
unlist(auc@y.values)

################ Testing on the Validation DataSet

actual<-Validate$Survived
predicted<-predict(mod1,type = "class",newdata = Validate)
actual<-as.factor(actual)

#### Kappa Metric
kappa2(data.frame(actual,predicted))

#Confusion Matric
confusionMatrix(predicted,actual,positive="1")

#ROCR curve and AUC value
head(predicted)
head(as.numeric(predicted))
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)

head(actual)
head(as.numeric(actual))
actual<-as.numeric(actual)
actual<-ifelse(actual==2,1,0)

pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
unlist(auc@y.values)








########################### Using the Logistic Regression Model
######## Logistic regression


mod1<-glm(formula = Survived~Sex_Male+Sex_Female+Pclass_1+Pclass_2+Pclass_3+Age+Parch+SibSp, family = "binomial", data = Train)
mod1
summary(mod1)
#### Model Accuracy on the Train Data Itself
actual<-Train$Survived
pred<-predict(mod1,type="response")

table(Train$Survived)/nrow(Train)

predicted<-ifelse(pred>0.60,1,0)
actual<-as.factor(actual)
predicted<-as.factor(predicted)

#### Kappa Metric
kappa2(data.frame(actual,predicted))

#Confusion Matric
confusionMatrix(predicted,actual,positive="1")

#ROCR curve and AUC value
head(predicted)
head(as.numeric(predicted))
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)

head(actual)
head(as.numeric(actual))
actual<-as.numeric(actual)
actual<-ifelse(actual==2,1,0)

pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
unlist(auc@y.values)

################ Testing on the Validation DataSet

actual<-Validate$Survived
predicted<-predict(mod1,type = "response",newdata = Validate)

predicted<-ifelse(predicted>0.60,1,0)
actual<-as.factor(actual)
predicted<-as.factor(predicted)

#### Kappa Metric
kappa2(data.frame(actual,predicted))

#Confusion Matric
confusionMatrix(predicted,actual,positive="1")

#ROCR curve and AUC value
head(predicted)
head(as.numeric(predicted))
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)

head(actual)
head(as.numeric(actual))
actual<-as.numeric(actual)
actual<-ifelse(actual==2,1,0)

pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
unlist(auc@y.values)




########################################### Using Random Forest Algorithm

model1 <- randomForest(formula = Survived~Sex_Male+Sex_Female+Pclass_1+Pclass_2+Pclass_3+Age+Parch+SibSp,ntree = 500, data = Train, importance = TRUE)
model1
a=c()
i=5
for (i in 3:6) {
  model3 <- randomForest(formula = Survived~Sex_Male+Sex_Female+Pclass_1+Pclass_2+Pclass_3+Age,ntree = 500,mtry=i, data = Train, importance = TRUE)
  predValid <- predict(model3, Validate, type = "class")
  a[i-2] = mean(predValid == Validate$Survived)
}

a

plot(3:6,a)


######max accuracy at mtry=3
model1 <- randomForest(formula = Survived~Sex_Male+Sex_Female+Pclass_1+Pclass_2+Pclass_3+Age+Parch,ntree = 500,mtry=3, data = Train, importance = TRUE)
model1


#### Model Accuracy on the Train Data Itself
actual<-Train$Survived
predicted<-predict(model1,type = "class")
actual<-as.factor(actual)

#### Kappa Metric
kappa2(data.frame(actual,predicted))

#Confusion Matric
confusionMatrix(predicted,actual,positive="1")

#ROCR curve and AUC value
head(predicted)
head(as.numeric(predicted))
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)

head(actual)
head(as.numeric(actual))
actual<-as.numeric(actual)
actual<-ifelse(actual==2,1,0)

pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
unlist(auc@y.values)

################ Testing on the Validation DataSet

actual<-Validate$Survived
predicted<-predict(model1,type = "class",newdata = Validate)
actual<-as.factor(actual)

#### Kappa Metric
kappa2(data.frame(actual,predicted))

#Confusion Matric
confusionMatrix(predicted,actual,positive="1")

#ROCR curve and AUC value
head(predicted)
head(as.numeric(predicted))
predicted<-as.numeric(predicted)
predicted<-ifelse(predicted==2,1,0)

head(actual)
head(as.numeric(actual))
actual<-as.numeric(actual)
actual<-ifelse(actual==2,1,0)

pred<-prediction(actual,predicted)
perf<-performance(pred,"tpr","fpr")
plot(perf,col="red")
abline(0,1, lty = 8, col = "grey")

auc<-performance(pred,"auc")
unlist(auc@y.values)


