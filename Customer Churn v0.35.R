## ----setup, include=FALSE---------------------------
#Authors : Abhinav, Archit, Ayush, Matthew, Nir
knitr::opts_chunk$set(echo = TRUE)
#########################
##Cleaning Data and EDA##
#########################
#Install libraries if not installed, else load them-----------------------------
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
# usage
packages <- c("ggplot2", "ISLR", "DataExplorer", "RColorBrewer", "dplyr", "data.table","rpart","randomForest","xgboost","DescTools","Hmisc","ggcorrplot","MASS","tidyverse","caret","precrec","ROCR")
ipak(packages)

options(scipen=999)

#Set seed and working directory-------------------------------------------------
set.seed(100)
#setwd("C:/Users/archi/OneDrive/Documents/GitHub/STA380-69963/ML - Project 1")#--WD for Archit
setwd("~/Documents/GitHub/Bank-Customer-Churn-Prediction")#--WD for Abhinav
#setwd("C:\\Users\\nirra\\OneDrive\\Documents\\GitHub\\Bank-Customer-Churn-Prediction")#WD-- for nir


## ---------------------------------------------------
#Read data file
raw_data = fread('Churn_Modelling.csv')
#raw_data=Churn_Modelling

n = dim(raw_data)[1]
df = data.frame(raw_data)

#Understanding the structure of data
str(df)


## ---------------------------------------------------
#Checking if there are any null value in the dataframe
sapply(df,function(df) sum(is.na(df)))


## ---------------------------------------------------
#Checking Unique value counts in each columns
sapply(df, n_distinct)

#Exploring unique values for few variables
unique(df$NumOfProducts)
unique(df$HasCrCard)
unique(df$IsActiveMember)
unique(df$Exited)


## ---------------------------------------------------

## To make data frames easily readable we have 
## removed unnecessary data fields
df = subset(raw_data, select = -c(RowNumber,CustomerId,Surname))
df = data.frame(df)

#Coerce response variable to factor
df$Exited = factor(as.character(df$Exited),levels = c("0","1"))

#univariate EDA of continuous variables
#Plotting Histograms to understand the distributions
par(mfrow = c(3, 2))

hist(df$CreditScore,main=c("Credit Score"),xlab = c(""),col = "lightblue")
hist(df$Age,main=c("Age"),xlab = c(""),col = "lightblue")
hist(df$Tenure, breaks = 10,main=c("Tenure"),xlab = c(""),col = "lightblue")
hist(df$Balance, breaks = 12,main=c("Balance"),xlab = c(""),col = "lightblue")
hist(df$EstimatedSalary,main=c("Estimated Salary"),xlab = c(""),col = "lightblue")

#Reset par for future plots
par(mfrow=c(1,1))

## ---------------------------------------------------
#univariate EDA of categorical variables
#Plotting Bar Charts using ggplot

#Custom theme for background, font and other aesthetics
theme_custom <- function () { 
    theme_bw(base_size=12, base_family="Avenir") %+replace% 
        theme(
            panel.background  = element_blank(),
            plot.background = element_rect(fill="gray96", colour=NA), 
            legend.background = element_rect(fill="transparent", colour=NA),
            legend.key = element_rect(fill="transparent", colour=NA)
        )
}


#-----------

ggplot(df, aes(x = factor(Gender))) + geom_bar(fill="skyblue2",alpha=0.65) +xlab("Gender")+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black")+theme_custom()

ggplot(df, aes(x = factor(Geography))) + geom_bar(fill="skyblue2",alpha=0.65) +xlab("Geography")+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black")+theme_custom()

ggplot(df, aes(x = factor(NumOfProducts))) + geom_bar(fill="skyblue2",alpha=0.65)+xlab("Number of Products")+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black")+theme_custom()

ggplot(df, aes(x = factor(HasCrCard))) + geom_bar(fill="skyblue2",alpha=0.65) +xlab("Has Credit Card")+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black")+theme_custom()

ggplot(df, aes(x = factor(IsActiveMember)))+ geom_bar(fill="skyblue2",alpha=0.65)+xlab("Active membership")+
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black")+theme_custom()

ggplot(df, aes(x = Exited)) + geom_bar(fill="skyblue2",alpha=0.65) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black")+
  labs( x = "Exit Status")+theme_custom()



## ---------------------------------------------------
#Customer Churn by Region
#table1 <- table(df$Exited, df$Geography, dnn=c("Exit Count", "Geography")) 
#barplot(table1, ylab="Frequency", xlab="Geography", main="Comparing Exit Status across countries\n", 
#        col=c("turquoise4", "turquoise2" ), beside=TRUE, width=.2)
#legend("right", title="Exited", legend= sort(unique(df$Exited)),
#       fill =c("turquoise4", "turquoise2" ), box.lty=0)



## ---------------------------------------------------
## Geography vs Gender
cols <- c("Gender","Geography","NumOfProducts","HasCrCard","IsActiveMember","Exited" )
df[cols] <- lapply(df[cols], function(x) as.factor(as.character(x)))

g <- ggplot(df, aes(x = Geography)) +geom_bar(aes(fill = Gender),position="dodge")+geom_text(aes(label = ..count..),position = "identity", stat = "count", vjust =5 , colour = "black")+scale_fill_manual(values=c('#999999','#E69F00'))+theme_custom()
plotly::ggplotly(g+ggtitle("Gender Distribution by Geography")) #Plotly objects seen in Viewer tab or browser





## ---------------------------------------------------
## EDA of Churn by demographics 
## Customer Churn by Geography
plotly::ggplotly(ggplot(df, aes(x = Geography, fill = Exited)) +
  geom_bar(position="dodge") +
  geom_text(aes(label = ..count..),
            stat = "Count",position = position_dodge(0.8),
            vjust = 1.5, hjust = 0.5,
            colour = "black") +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  labs(title = "Churn by Geography")+theme_custom())


## ---------------------------------------------------
##Trying to plot in percent---WIP
## Customer Churn by Gender
plotly::ggplotly(ggplot(df, aes(x = Gender,y = (..count..)/sum(..count..), fill = Exited)) +
  geom_bar(position="dodge") +
  geom_text(aes(label =scales::percent( (..count..)/sum(..count..)) ,y = (..count..)/sum(..count..)),
            stat = "Count",position = position_dodge(0.8),
            vjust = 1.5, hjust = 0.9,
            colour = "black") +
  scale_fill_manual(values=c('#999999','#E69F00'))+
  labs(title = "Churn by Gender")+theme_custom())



## ---------------------------------------------------
#Density Plots
plotly::ggplotly(ggplot(df, aes(x=Age)) + 
  geom_density(fill="grey",alpha=0.65)+theme_custom())

plotly::ggplotly(ggplot(df, aes(x=Age,fill=Exited),size=1.3) + 
  geom_density(alpha=0.65)+
  labs(title = "Density Plot: Age")+theme_custom())

#ggplot(df, aes(x=NumOfProducts)) +geom_density()+scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+labs(title = "Density Plot: #Products")

#ggplot(df, aes(x=NumOfProducts, color=Exited)) + geom_density()+scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+labs(title = "Density Plot: #Products")


## ----include=FALSE----------------------------------
#df$Exited <- as.factor(df$Exited)
#ggplot(df, aes(x=NumOfProducts, color=Exited)) +
#  geom_density()+
#  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
#  labs(title = "Density Plot: #Products")+xlab("Number of Products") + theme_custom()


## ---------------------------------------------------
#Correlation Analysis

#str(df)
cols_num <- c("CreditScore","Age","Tenure","NumOfProducts","Balance","EstimatedSalary" )
df[cols_num] <- lapply(df[cols_num], function(x) as.numeric(x))
corr <- round(cor(df[cols_num]), 2)
#ggcorrplot(
#  corr,
#  hc.order = TRUE,
#  type = "lower",
#  outline.color = "white",
#  ggtheme = ggplot2::theme_gray,
#  colors = c("#6D9EC1", "white", "#E46726")
#)
plotly::ggplotly(ggcorrplot(corr,outline.color = "black",colors = c("salmon","white","skyblue2"),lab = T)+ggtitle("Correlation plot\n"))

suppressWarnings(ggpairs(df[c(cols_num,"Exited")],progress = F ,aes(alpha=0.6))+theme_custom()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))



## ---------------------------------------------------
#Check impactables with simple logistic regression -- given we see no correlations and low dimensionality, we needn't use Ridge/Lasso
#Parametric Model - Fitting Generalized Linear Models

#AIC(8585)
model.glm <- glm(Exited~.,df,family = "binomial")
summary(model.glm)

#Demographics such as age, gender, geography and behavorial paramters such as Balance, #Products and active membership return significant betas



## ---------------------------------------------------
#Running stepwise - Choosing model by AIC in a Stepwise Algorithm
#AIC(8583)
step.model <- model.glm %>% stepAIC(trace = FALSE)
cols <- names(coef(step.model))
summary(step.model)

#Fewer characterstics such as Age, Credit score and Balance return significant coefficients. AIC reduces marginally.

## ---------------------------------------------------
#Setting up train validation test 
# Split the data into training and test set
set.seed(123)

training.samples <- df$Exited %>%  createDataPartition(p = 0.7,list = F)
train.data  <- df[training.samples, ]
test.data <- df[-training.samples, ]

# Define training control
train.control <- trainControl(## 10-fold repeated CV
                           method = "repeatedcv",
                           number = 10,repeats = 10)
# Train the model
#train.data <- train.data[, colnames(train.data) %in% c(cols,"Exited")]
#test.data <- test.data[, colnames(test.data) %in% c(cols,"Exited")]

set.seed(1) #Model 1
cv_logit <- train(data = train.data, Exited~.,
                 method = "glm", 
                 family = "binomial",
                 trControl = train.control)
summary(cv_logit)

PredTrain = predict(cv_logit, newdata=train.data[,!colnames(train.data) == "Exited"], type="prob")
table(train.data$Exited, PredTrain[,2] > 0.5)



## ----fig.width=6.5, fig.height=5.5------------------

#Train Error Metrics
precrec_obj <- evalmod(scores = PredTrain[,2], labels = train.data$Exited)
autoplot(precrec_obj)
#precrec_obj <- evalmod(scores = PredTrain[,2], labels = train.data$Exited,mode="basic")
#autoplot(precrec_obj)

confusionMatrix(as.factor(as.numeric(PredTrain[,2]>0.5)),train.data$Exited)
logit_pred_train <- prediction(PredTrain[,2],train.data$Exited)
#logit_roc_train <- performance(logit_pred_train,"tpr","fpr")
#plot(logit_roc_train)
auc_logit_train <- performance(logit_pred_train,"auc")
auc_logit_train@y.values

#Test Error Metrics
PredTest = predict(cv_logit, newdata=test.data[,!colnames(test.data) == "Exited"], type="prob")
table(test.data$Exited, PredTest[,2] > 0.5)
precrec_obj_test <- evalmod(scores = PredTest[,2], labels = test.data$Exited)
autoplot(precrec_obj_test)

confusionMatrix(as.factor(as.numeric(PredTest[,2]>0.5)),test.data$Exited)
logit_pred_test <- prediction(PredTest[,2],test.data$Exited)
logit_roc_test <- performance(logit_pred_test,"tpr","fpr")
logit_pr_test <- performance(logit_pred_test,"rec","prec")
pr_df= data.frame(logit_roc_test2@x.values,logit_roc_test2@y.values)
names(pr_df) <- c("Precision", "Recall")
pr_df$F1 = 2*pr_df$Precision * pr_df$Recall / (pr_df$Precision + pr_df$Recall)
#Ideal cut-off = 0.2168
logit_pr_test@alpha.values[[1]][which.max(pr_df$F1)]

#plot(logit_roc_train)
auc_logit_test <- performance(logit_pred_test,"auc")
auc_logit_test@y.values #AUC 0.7796



## ---------------------------------------------------
#Additionally we explored multiple versions/combinations of logistic regression to study if there were anymore intersting patterns
#Logistic Regression
library(tidyverse)
library(mosaic)
library(rms) #(lrm)

#Read CSV
bank <- read.csv("Churn_Modelling.csv", stringsAsFactors = FALSE)
names(bank)

#NumOfProducts_C (AIC 8718.8)
tally(~NumOfProducts, data=bank)

bank <- bank %>%
  mutate(NumOfProducts_C = factor(NumOfProducts, labels=c("1", "2", "3", "4")))

Products_C <- glm(Exited ~ NumOfProducts_C, data=bank, family="binomial")
summary(Products_C)

#Products (AIC 10090)
Products <- glm(Exited ~ NumOfProducts, data=bank, family="binomial")
summary(Products)

#Age (AIC 9355.1)
Ag <- glm(Exited ~ Age, data=bank, family="binomial")
summary(Ag)

#Geography (AIC 9835.4)
Geo <- glm(Exited ~ Geography, data=bank, family="binomial")
summary(Geo)

#Activity (AIC 9868)
Active <- glm(Exited ~ IsActiveMember, data=bank, family="binomial")
summary(Active)

#Balance (AIC 9971)
Bal <- glm(Exited ~ Balance, data=bank, family="binomial")
summary(Bal)

#Gender (AIC 10001)
Gen <- glm(Exited ~ Gender, data=bank, family="binomial")
summary(Gen)

#Credit Score (AIC 10106)
Credit <- glm(Exited ~ CreditScore, data=bank, family="binomial")
summary(Credit)

#Tenure (AIC 10112)
Ten <- glm(Exited ~ Tenure, data=bank, family="binomial")
summary(Ten)

#EstimatedSalary (AIC 10112)
Salary <- glm(Exited ~ EstimatedSalary, data=bank, family="binomial")
summary(Salary)

#HasCrCard (AIC 10113)
Card <- glm(Exited ~ HasCrCard, data=bank, family="binomial")
summary(Card)

#Model 1 (AIC 8099.2)
Mod1 <- glm(Exited ~ NumOfProducts_C + Age, data=bank, family="binomial")
summary(Mod1)

#Model 2 (AIC 7879.8)
Mod2 <- glm(Exited ~ NumOfProducts_C + Age + Geography, data=bank, family="binomial")
summary(Mod2)

#Model 3 (AIC 7537.1 )
Mod3 <- glm(Exited ~ NumOfProducts_C + Age + Geography + IsActiveMember, data=bank, family="binomial")
summary(Mod3)

#Model 4 (AIC 7536.7 ) (Balance not significant)
Mod4 <- glm(Exited ~ NumOfProducts_C + Age + Geography + IsActiveMember + Balance, data=bank, family="binomial")
summary(Mod4)

#Model 5 (AIC 7458.8)
Mod5 <- glm(Exited ~ NumOfProducts_C + Age + Geography + IsActiveMember + Gender, data=bank, family="binomial")
summary(Mod5)

#Model 6 (AIC 7455.6)
Mod6 <- glm(Exited ~ NumOfProducts_C + Age + Geography + IsActiveMember + Gender + CreditScore, data=bank, family="binomial")
summary(Mod6)

#Model 7 (AIC 7453.7)
Mod7 <- glm(Exited ~ NumOfProducts_C + Age + Geography + IsActiveMember + Gender + CreditScore + Tenure, data=bank, family="binomial")
summary(Mod7)

#Model 8 (AIC 7455)
Mod8 <- glm(Exited ~ NumOfProducts_C + Age + Geography + IsActiveMember + Gender + CreditScore + Tenure + EstimatedSalary, data=bank, family="binomial")
summary(Mod8)

#Model 9 (AIC 7454.9)
Mod9 <- glm(Exited ~ NumOfProducts_C + Age + Geography + IsActiveMember + Gender + CreditScore + Tenure + HasCrCard, data=bank, family="binomial")
summary(Mod9)

#Final (Model 7)
Full <- glm(Exited ~ NumOfProducts_C + Age + Geography + IsActiveMember + Gender + CreditScore + Tenure, data=bank, family="binomial")
summary(Full)

#Model returning better AICs of all tried combinations
set.seed(2) #Model 2
Final <- lrm(Exited ~ NumOfProducts_C + Age + Geography + IsActiveMember + Gender + CreditScore + Tenure, x=TRUE, y=TRUE, data=bank)
Final

signif(exp(Final$coef), digits=3)


#Resampling Validation of a Fitted Model's Indexes of Fit -- provides bias-corrected indexes 
validFull <- validate(Final, method="boot", B=100)
validFull

#Using bootstrapping to get bias-corrected (overfitting- corrected) estimates of predicted vs. observed values based on subsetting predictions on nonparametric smoothers
calibFull <- calibrate(Final, method="boot", B=100)

par(bg="white", las=1)
plot(calibFull, las=1)

#ROC is C-index under Final stats = 0.836 on overall data
#We found that creating Number of products as a factor reduced our model AIC from ~8k range to mid-7k range.

train.data2 = copy(train.data)
test.data2 = copy(test.data)
train.data2$NumOfProducts_C = factor(train.data2$NumOfProducts, labels=c("1", "2", "3", "4"))
test.data2$NumOfProducts_C = factor(test.data2$NumOfProducts, labels=c("1", "2", "3", "4"))

#Test Error Metrics
PredTest = predict(Final, newdata=test.data2[,!colnames(test.data2) == "Exited"], type="fitted.ind")
table(test.data2$Exited, PredTest > 0.5)
precrec_obj_test <- evalmod(scores = PredTest, labels = test.data2$Exited)
autoplot(precrec_obj_test)

confusionMatrix(as.factor(as.numeric(PredTest>0.5)),test.data2$Exited)
logit_pred_test <- prediction(PredTest,test.data2$Exited)
logit_roc_test <- performance(logit_pred_test,"tpr","fpr")
logit_pr_test <- performance(logit_pred_test,"rec","prec")
pr_df= data.frame(logit_roc_test2@x.values,logit_roc_test2@y.values)
names(pr_df) <- c("Precision", "Recall")
pr_df$F1 = 2*pr_df$Precision * pr_df$Recall / (pr_df$Precision + pr_df$Recall)
#Ideal cut-off = 0.2127
logit_pr_test@alpha.values[[1]][which.max(pr_df$F1)]

#plot(logit_roc_train)
auc_logit_test <- performance(logit_pred_test,"auc")
auc_logit_test@y.values
#0.8414 on test sample

## ---------------------------------------------------
#We now try a non-parametric form of model

library(tree)

# train = sample(1:nrow(df),7000)
# churn.test=df[-train,]
# Exited.test=df$Exited[-train]
# df.test=df[-train,"Exited"]
# tree.churn = tree(Exited~.,df,subset=train)
# tree.pred = predict(tree.churn,churn.test,type="class")
# table(tree.pred,Exited.test)
# plot(tree.churn)
# text(tree.churn,pretty=0)
# library(randomForest)
# set.seed(100)
# rf.churn = randomForest(Exited~.,data=df,subset=train,importance=T,mtry=3,ntree=25)
# yhat.rf= predict(rf.churn,newdata=churn.test,type="response")
# mean((yhat.rf-df.test)^2)




library(tree)
library(randomForest)
library(gbm)
df$Geography = as.factor(df$Geography)
df$Gender = as.factor((df$Gender))
#set.seed(100)
accuracy.df = data.frame(accuracy=numeric(3))
rownames(accuracy.df)=c("mtry=3","mtry=4","mtry=5")
#train = copy(train.data)
churn.train=copy(train.data)
churn.test=copy(test.data)
#mtry = 3
churn.rf1 = randomForest(Exited~.,data=churn.train,ntree=100,proximity=T,importance=T)
table(predict(churn.rf1),churn.train$Exited)
churn.pred1 = predict(churn.rf1,newdata = churn.test)
OOSPred1 = table(churn.pred1,churn.test$Exited)
OOSPred1
#Left hand side column of 0,1 indicates prediction, Top row 0,1 indicates True label

#importance(churn.rf)

accuracy1=(sum(diag(OOSPred1)))/sum(OOSPred1)
accuracy.df[1,1]=accuracy1

#mtry = 4
set.seed(3) #Model 3

churn.rf2 = randomForest(Exited~.,data=churn.train,ntree=100,mtry = 4, proximity=T,importance=T)
churn.pred2 = predict(churn.rf2,newdata = churn.test)
OOSPred2 = table(churn.pred2,churn.test$Exited)
accuracy2=(sum(diag(OOSPred2)))/sum(OOSPred2)
accuracy.df[2,1]=accuracy2

#mtry = 5
set.seed(100)

churn.rf3 = randomForest(Exited~.,data=churn.train,ntree=100,mtry = 5, proximity=T,importance=T)
churn.pred3 = predict(churn.rf3,newdata = churn.test)
OOSPred3 = table(churn.pred3,churn.test$Exited)
accuracy3=(sum(diag(OOSPred3)))/sum(OOSPred3)
accuracy.df[3,1]=accuracy3
accuracy.df



#Summarize results from mtry=4
plot(tree(churn.rf2))
text(tree(churn.rf2))
OOSPred2
varImpPlot(churn.rf2)

#Test Error Metrics
PredTest_rf = predict(churn.rf2, newdata=test.data[,!colnames(test.data) == "Exited"], type="prob")
table(test.data$Exited, PredTest_rf[,2] > 0.5)
precrec_obj_test <- evalmod(scores = PredTest_rf[,2], labels = test.data$Exited)
autoplot(precrec_obj_test)

confusionMatrix(as.factor(as.numeric(PredTest_rf[,2]>0.5)),test.data$Exited)
rf_pred_test <- prediction(PredTest_rf[,2],test.data$Exited)
rf_roc_test <- performance(rf_pred_test,"tpr","fpr")
rf_pr_test <- performance(rf_pred_test,"prec","rec")
#plot(logit_roc_train)
auc_rf_test <- performance(rf_pred_test,"auc")
auc_rf_test@y.values
#Test AUC = 0.8612

pr_df= data.frame(rf_pr_test@x.values,rf_pr_test@y.values)
names(pr_df) <- c("Precision", "Recall")
pr_df$F1 = 2*pr_df$Precision * pr_df$Recall / (pr_df$Precision + pr_df$Recall)
#Ideal cut-off = 0.39
rf_pr_test@alpha.values[[1]][which.max(pr_df$F1)]



## ---------------------------------------------------

# Gradient Boosting Machines (GBM)

#df$Geography <- as.factor(df$Geography)
#df$Gender <- as.factor(df$Gender)
#df$HasCrCard <- as.factor(df$HasCrCard)
#df$IsActiveMember <- as.factor(df$IsActiveMember)


#set.seed(234)
#split_data <- createDataPartition(df$Exited, p=0.7, list = FALSE)
train_data <- copy(train.data)
test_data <- copy(test.data)

# GBM model without cross validation ; Important features: Age, Balance, NumOfProducts, EstimatedSalary

gbmmodel <- gbm(as.character(Exited) ~.,distribution="bernoulli",data=train_data,n.trees=1000,interaction.depth = 3)
summary(gbmmodel)
print(gbmmodel)

# GBM model without cross validation ; Important features: Age, Balance, NumOfProducts, EstimatedSalary, CreditScore
# With shrinkage (learning rate) the computation time increases and so does the number of trees. 
# To counter the same we use a simple tree as a base model and interaction depth i.e. depth of trees as 3 to make the model less complex.

set.seed(4) #Model 4
gbmmodel1 <- gbm(as.character(Exited) ~.,
                 distribution="bernoulli",
                 data=train_data,
                 n.trees=1000,
                 interaction.depth = 3,
                 #shrinkage = 0.001,
                 cv.folds = 3)

summary(gbmmodel1)
print(gbmmodel1)

#rmse = sqrt(min(gbmmodel1$cv.error))
#rmse

optm_cv <- gbm.perf(gbmmodel1,method="cv")
optm_oob <- gbm.perf(gbmmodel1,method="OOB")

print(optm_cv)
print(optm_oob)


predictions <- predict(object = gbmmodel1,
                       newdata = test_data,
                       n.trees = optm_cv,
                       type = "response")


# Selecting cutoff probability of churn as 0.3.

binarypredictions <- as.factor(ifelse(predictions>0.3,1,0))
test_data$Exited <- as.factor(test_data$Exited)
confusionMatrix(binarypredictions,test_data$Exited)

gbm_pred_test <- prediction(predictions,test_data$Exited)
gbm_roc_testing <- performance(gbm_pred_test,"tpr","fpr")
plot(gbm_roc_testing)


# AUC is 0.8721 indicating a good separation between the two classes of churn

auc_temp <- performance(gbm_pred_test,"auc")
gbm_auc_testing <- as.numeric(auc_temp@y.values)
gbm_auc_testing


gbm_pr_testing <- performance(gbm_pred_test,"prec","rec")
#plot(logit_roc_train)


pr_df= data.frame(gbm_pr_testing@x.values,gbm_pr_testing@y.values)
names(pr_df) <- c("Precision", "Recall")
pr_df$F1 = 2*pr_df$Precision * pr_df$Recall / (pr_df$Precision + pr_df$Recall)
#Ideal cut-off = 0.244
gbm_pr_testing@alpha.values[[1]][which.max(pr_df$F1)]


