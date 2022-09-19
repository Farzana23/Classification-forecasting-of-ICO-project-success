#Packages
library(dplyr)
library(VIM)
library(mice)
library(ggplot2)
library(ggpubr)
library(corrgram)
library(naniar)
library(gdata)
library(mice)
library(tidyverse)
library(lubridate)
library(class) 
library(gmodels) 
library(fastDummies)
library(readxl)
library(qdapTools)
library(janitor)
library(gdata)
library(mlbench)
library(caret)
library(cowplot)
library(pROC)
library(ROCR)
library(C50)
library(kernlab)


setwd("~/Documents/Farzana UoL/Semester 2/Machine Learning in Practice/Coursework/R Files/CSVs")
data1<-read.csv("ICO Minerva.csv", encoding = 'UTF-8')
data2<-read.csv("Full dataset with all variables.csv", encoding = "UTF-8")
data2<-data2%>%select(ID, bitcoin.price,continent_region)
data3<-read.csv("Categories variable.csv", encoding = 'UTF-8')
data<- merge(data1,data2, by="ID")
data<-merge(data,data3, by="ID")
summary(data)
data<-data[-8]

data$enddate<-as.Date(data$enddate, format ="%d/%m/%Y") 
data$startdate<-as.Date(data$startdate, format ="%d/%m/%Y") 

data$duration<-as.numeric(difftime(data$enddate, data$startdate, units="days"))

##Data inspection 
str(data)

##Missing Data
sum(is.na(data))
complete.cases(data)
!complete.cases(data)
aggr(data,prop=FALSE,numbers=TRUE)

#Replacing unknown values with NA 
dataMD<- data %>% replace_with_na(replace = list(platform = "unknown", acceptingCurrencyNum= "unknown", continent_region = "#N/A"))
is.na(dataMD)
sum(is.na(dataMD)) 
sum(is.na(dataMD$platform)) 
sum(is.na(dataMD$acceptingCurrencyNum)) 
sum(is.na(data$continent_region)) 


#Imputing Missing Values
#acceptingCurrencyNum variable

data<- data %>% replace_with_na(replace = list(acceptingCurrencyNum= "unknown"))
data$acceptingCurrencyNum<-as.numeric(data$acceptingCurrencyNum)

corrgram(data)
hist(data$acceptingCurrencyNum) 
median(data$acceptingCurrencyNum, na.rm = TRUE) 
data$acceptingCurrencyNum[is.na(data$acceptingCurrencyNum)]<-median(data$acceptingCurrencyNum, na.rm = TRUE)
sum(is.na(data$acceptingCurrencyNum)) 

#continent_region variable 
data<- data %>% replace_with_na(replace = list(continent_region = "#N/A"))
sum(is.na(data$continent_region))
data<-na.omit(data)

##data type transformation

#conversrion of teamLinkedIn 
data$teamLinkedIn<-parse_number(data$teamLinkedIn)
data$teamLinkedIn<-(data$teamLinkedIn/100) 

#conversrion of teamPhotos
data$teamPhotos<-parse_number(data$teamPhotos)
data$teamPhotos<-(data$teamPhotos/100) 

#continent_region variable
data <- dummy_cols(data, select_columns = 'continent_region')
data<-data[-21]

#platform variable
data<-data%>%
  mutate(platform_Ethereum=ifelse(platform=='Ethereum',1,0),
         platform_Waves=ifelse(platform=='WAVES',1,0),
         platform_Others=ifelse((platform!='Ethereum'& platform!='WAVES'),1,0))
data<-data[-11]

#categories variable
data<-bind_cols(data%>%select(-categories.y),qdapTools::mtabulate(strsplit(data$categories.y,','))%>%
                  rename_all(function(x) paste0('wishlist_',x)))

data$wishlist_Others<-replace(data$wishlist_Others, data$wishlist_Others>1,1)
data$wishlist_Technology<-replace(data$wishlist_Technology, data$wishlist_Technology>1,1)
data$wishlist_MediaCom<-replace(data$wishlist_MediaCom, data$wishlist_MediaCom>1,1)
data$wishlist_Finance<-replace(data$wishlist_Finance, data$wishlist_Finance>1,1)
data$wishlist_Entertainment<-replace(data$wishlist_Entertainment, data$wishlist_Entertainment>1,1)                               

#Changing class label as factor
data$goal <- factor(data$goal,levels = c("Y", "N"), 
                    labels = c("YES", "NO"))


##Renaming columns 
data<-clean_names(data)
data<-data%>%
  rename(category_cryptocurrency = wishlist_cryptocurrency,
         category_entertainment= wishlist_entertainment,
         category_finance=wishlist_finance,
         category_infrastructure=wishlist_infrastructure,
         category_media_com=wishlist_media_com,
         category_others=wishlist_others,
         category_platform=wishlist_platform,
         category_technology=wishlist_technology,
         category_business_services= wishlist_business_services,
         category_smart_contract= wishlist_smart_contract)

sum(is.na(data))
sum(!complete.cases(data))


##Outliers
par(mfrow=c(2, 2))
boxplot(data$coin_num, main="Outliers in coin_num")$out
boxplot(data$team_size,main="Outliers in team_size")$out 
boxplot(data$team_photos,main="Outliers in team_photos")$out 
boxplot(data$duration,main="Outliers in duration")$out 

#teamSize 
data<-data[-which(data$team_size %in% boxplot.stats(data$team_size)$out),] #this is ok 

#duration 
data<-data[which(data$duration>=0),]  

#Removing unnecessary variables 
data<-data[-1]
data<-data[-2]
data<-data[-2]
data<-data[-4]

#Descriptive Statistics 
str(data)
summary(data, scientific=FALSE)
summary<-data.frame(summary(data))

#Distribution 
dataDist<-data[-34]
dataDist<-dataDist[-34]
my_plots <- lapply(names(dataDist), function(var_x){
  p <- 
    ggplot(dataDist) +
    aes_string(var_x)
  
  if(is.numeric(dataDist[[var_x]])) {
    p <- p + geom_density()
    
  } else {
    p <- p + geom_bar()
  } 
  
})

plot_grid(plotlist = my_plots)

#Importance of each features
set.seed(7)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(goal~., data=data, method="lvq", preProcess="scale", trControl=control)
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance)

#Correlation
dataC<-data
dataC <- dummy_cols(data, select_columns = 'goal')
dataC<-dataC[-1]
corrgram<-data.frame(corrgram(dataC))
cor(dataC)
cor<-data.frame(cor(dataC))

dataFinal<-data[-5]
dataFinal<-dataFinal[-5]
dataFinal<-dataFinal[-22]

write.table(dataFinal, file = "ML Coursework Final Dataset.csv", 
            row.names=F,sep = ",")

#_____________________________________________________________________________________________

##K-NN

data_knn<- read.csv("ML Coursework Final Dataset.csv", stringsAsFactors = TRUE)
str(data_knn)


table(data_knn$goal) 
round(prop.table(table(data_knn$goal)) * 100, digits = 1) 


#Normalization 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data_knn_norm <- as.data.frame(lapply(data_knn[2:31], normalize))
data_knn_norm$goal<-data_knn$goal

K = 10 
set.seed(0599)
folds <- createFolds(data_knn_norm$goal, k = K)
str(folds)  

accuracy_list_knn_k3<-as.numeric() 
sensitivity_list_knn_k3<-as.numeric() 
specificity_list_knn_k3<-as.numeric()
precision_list_knn_k3<-as.numeric()
recall_list_knn_k3<-as.numeric()
f1_list_knn_k3<-as.numeric()
appendedDf_knn_k3<-data.frame()

for(i in 1:K){ 
  knn_test <- data_knn_norm[folds[[i]],] 
  knn_train <- data_knn_norm[-folds[[i]],]
  
  set.seed(2311)
  knn_pred_k3<- knn(train = knn_train[,-31], test = knn_test[,-31], 
                    cl = knn_train$goal, k=29) 
  
  cm_knn_k3<- confusionMatrix(knn_pred_k3, knn_test$goal, positive = "YES") 
  
  accuracy_knn_k3 <- cm_knn_k3$overall['Accuracy']
  sensitivity_knn_k3 <- cm_knn_k3$byClass['Sensitivity'] 
  specificity_knn_k3 <- cm_knn_k3$byClass['Specificity']
  precision_knn_k3 <- cm_knn_k3$byClass['Precision'] 
  recall_knn_k3 <- cm_knn_k3$byClass['Recall']
  f1_knn_k3 <- cm_knn_k3$byClass['F1']
  
  accuracy_list_knn_k3<- append(accuracy_list_knn_k3,accuracy_knn_k3)
  sensitivity_list_knn_k3<- append(sensitivity_list_knn_k3,sensitivity_knn_k3)
  specificity_list_knn_k3<- append(specificity_list_knn_k3,specificity_knn_k3)
  precision_list_knn_k3<- append(precision_list_knn_k3,precision_knn_k3)
  recall_list_knn_k3<- append(recall_list_knn_k3,recall_knn_k3)
  f1_list_knn_k3<- append(f1_list_knn_k3,f1_knn_k3)
  
  predict_class_knn_prob_k3 <- predict(caret::knn3(knn_train[,-31], knn_train$goal, k = 29), knn_test[,-31])
  
  k3 <- data.frame(actual_type = knn_test[,31],
                   predict_type = knn_pred_k3,
                   prob_Yes = round(predict_class_knn_prob_k3 [ , 2], 5),
                   prob_No = round(predict_class_knn_prob_k3 [ , 1], 5))
  
  appendedDf_knn_k3 <- rbind(appendedDf_knn_k3, k3)
  
}

CrossTable(knn_pred_k3, knn_test$goal, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted default', 'actual default' ))


##AUC and ROC curve
pred_object_knn_k3 <- prediction(appendedDf_knn_k3$prob_Yes, appendedDf_knn_k3$actual_type) 
roc_knn_k3<- performance(pred_object_knn_k3, measure = "tpr", x.measure = "fpr")
plot(roc_knn_k3, main = "KNN_k3 ROC curve for ICO", col = "blue", lwd = 2) 
abline(a = 0, b = 1, lwd = 2, lty = 2,col = "red") 

auc_object_knn_k3 <- performance(pred_object_knn_k3, measure = "auc")
auc_knn_k3 <- auc_object_knn_k3@y.values[[1]]
auc_knn_k3 

#Measures accuracy
accuracy_list_knn_k3 
accuracy_average_knn_k3 <- mean(accuracy_list_knn_k3)
accuracy_average_knn_k3 
sd(accuracy_list_knn_k3) 

sensitivity_list_knn_k3
sensitivity_average_knn_k3 <- mean(sensitivity_list_knn_k3)
sensitivity_average_knn_k3 
sd(sensitivity_list_knn_k3) 

specificity_list_knn_k3
specificity_average_knn_k3 <- mean(specificity_list_knn_k3)
specificity_average_knn_k3 
sd(specificity_list_knn_k3) 

precision_list_knn_k3
precision_average_knn_k3 <- mean(precision_list_knn_k3)
precision_average_knn_k3 
sd(precision_list_knn_k3) 

recall_list_knn_k3
recall_average_knn_k3 <- mean(recall_list_knn_k3)
recall_average_knn_k3 
sd(recall_list_knn_k3) 

f1_list_knn_k3
f1_average_knn_k3 <- mean(f1_list_knn_k3)
f1_average_knn_k3 
sd(f1_list_knn_k3)

#___________________________________________________________________________________________

##Decision Tree 

data_DT1<- read.csv("ML Coursework Final Dataset.csv", stringsAsFactors = TRUE)


data_DT <- as.data.frame(lapply(data_DT1[2:31], normalize))
data_DT$goal<-data_DT1$goal

#Decision tree k fold cross validation
#____________________________________________________

K = 10 
set.seed(0599)
folds <- createFolds(data_DT$goal, k = K)
str(folds)  

accuracy_list_DT<-as.numeric() 
sensitivity_list_DT<-as.numeric() 
specificity_list_DT<-as.numeric()
precision_list_DT<-as.numeric()
recall_list_DT<-as.numeric()
f1_list_DT<-as.numeric()
appendedDf_DT<-data.frame()

for(i in 1:K){ 
  DT_test <- data_DT[folds[[i]],] 
  DT_train <- data_DT[-folds[[i]],]
  
  set.seed(2311)
  model_DT <- C5.0(select(DT_train, -goal), DT_train$goal) 
  
  predict_class_DT <- predict(model_DT, DT_test, type='class')
  cm_DT <- confusionMatrix(predict_class_DT, DT_test$goal, positive = "YES") 
  
  accuracy_DT <- cm_DT$overall['Accuracy']
  sensitivity_DT <- cm_DT$byClass['Sensitivity'] 
  specificity_DT <- cm_DT$byClass['Specificity']
  precision_DT <- cm_DT$byClass['Precision'] 
  recall_DT <- cm_DT$byClass['Recall']
  f1_DT <- cm_DT$byClass['F1']
  
  accuracy_list_DT<- append(accuracy_list_DT,accuracy_DT)
  sensitivity_list_DT<- append(sensitivity_list_DT,sensitivity_DT)
  specificity_list_DT<- append(specificity_list_DT,specificity_DT)
  precision_list_DT<- append(precision_list_DT,precision_DT)
  recall_list_DT<- append(recall_list_DT,recall_DT)
  f1_list_DT<- append(f1_list_DT,f1_DT)
  
  
  predict_class_DT_prob <- predict(model_DT, DT_test, type='prob')
  predict_class_list_DT <- data.frame(actual_type = DT_test$goal,
                                      predict_type = predict_class_DT,
                                      prob_Yes = round(predict_class_DT_prob[ , 2], 5),
                                      prob_No = round(predict_class_DT_prob[ , 1], 5)) 
  
  appendedDf_DT <- rbind(appendedDf_DT, predict_class_list_DT)
  
}

CrossTable(predict_class_DT, DT_test$goal, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted default', 'actual default' ))


##AUC and ROC curve
pred_object_DT <- prediction(appendedDf_DT$prob_Yes, appendedDf_DT$actual_type) # the parameters are the probability for prediction of the positive class and the actual class
roc_DT<- performance(pred_object_DT, measure = "tpr", x.measure = "fpr")
plot(roc_DT, main = "DT ROC curve for ICO", col = "blue", lwd = 2) 
abline(a = 0, b = 1, lwd = 2, lty = 2,col = "red") 

auc_object_DT <- performance(pred_object_DT, measure = "auc")
auc_DT <- auc_object_DT@y.values[[1]]
auc_DT 

#Measures accuracy
accuracy_list_DT 
accuracy_average_DT <- mean(accuracy_list_DT)
accuracy_average_DT 
sd(accuracy_list_DT) 

sensitivity_list_DT
sensitivity_average_DT <- mean(sensitivity_list_DT)
sensitivity_average_DT 
sd(sensitivity_list_DT) 

specificity_list_DT
specificity_average_DT <- mean(specificity_list_DT)
specificity_average_DT 
sd(specificity_list_DT) 

precision_list_DT
precision_average_DT <- mean(precision_list_DT)
precision_average_DT 
sd(precision_list_DT) 

recall_list_DT
recall_average_DT <- mean(recall_list_DT)
recall_average_DT 
sd(recall_list_DT) 

f1_list_DT
f1_average_DT <- mean(f1_list_DT)
f1_average_DT 
sd(f1_list_DT) 

#_______________________________________________________________________________________

##Adaboost 

K = 10 
set.seed(0599)
folds <- createFolds(data_DT$goal, k = K)
str(folds)  

accuracy_list_boost<-as.numeric() 
sensitivity_list_boost<-as.numeric() 
specificity_list_boost<-as.numeric()
precision_list_boost<-as.numeric()
recall_list_boost<-as.numeric()
f1_list_boost<-as.numeric()
appendedDf_boost<-data.frame()

for(i in 1:K){ 
  DT_test <- data_DT[folds[[i]],] 
  DT_train <- data_DT[-folds[[i]],]
  
  set.seed(2311)
  DT_boost<- C5.0(select(DT_train, -goal), DT_train$goal, 
                  trials = 10) 
  predict_class_boost <- predict(DT_boost, DT_test, type='class')
  
  cm_DT_boost <- confusionMatrix(predict_class_boost, DT_test$goal, positive = "YES") 
  
  accuracy_DT_boost <- cm_DT_boost$overall['Accuracy']
  sensitivity_DT_boost <- cm_DT_boost$byClass['Sensitivity'] 
  specificity_DT_boost <- cm_DT_boost$byClass['Specificity']
  precision_DT_boost <- cm_DT_boost$byClass['Precision'] 
  recall_DT_boost <- cm_DT_boost$byClass['Recall']
  f1_DT_boost <- cm_DT_boost$byClass['F1']
  
  accuracy_list_boost<- append(accuracy_list_boost,accuracy_DT_boost)
  sensitivity_list_boost<- append(sensitivity_list_boost,sensitivity_DT_boost)
  specificity_list_boost<- append(specificity_list_boost,specificity_DT_boost)
  precision_list_boost<- append(precision_list_boost,precision_DT_boost)
  recall_list_boost<- append(recall_list_boost,recall_DT_boost)
  f1_list_boost<- append(f1_list_boost,f1_DT_boost)
  
  
  predict_class_DT_prob_boost <- predict(DT_boost, DT_test, type='prob')
  predict_class_list_boost <- data.frame(actual_type = DT_test$goal,
                                         predict_type = predict_class_boost,
                                         prob_Yes = round( predict_class_DT_prob_boost[ , 2], 5),
                                         prob_No = round( predict_class_DT_prob_boost[ , 1], 5)) 
  
  appendedDf_boost <- rbind(appendedDf_boost, predict_class_list_boost)
  
}

CrossTable(predict_class_boost, DT_test$goal, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted default', 'actual default' ))

##AUC and ROC curve
pred_object_DT_boost <- prediction(appendedDf_boost$prob_Yes, appendedDf_boost$actual_type) # the parameters are the probability for prediction of the positive class and the actual class
roc_DT_boost<- performance(pred_object_DT_boost, measure = "tpr", x.measure = "fpr")
plot(roc_DT_boost, main = "AdaBoost ROC curve for ICO", col = "blue", lwd = 2) 
abline(a = 0, b = 1, lwd = 2, lty = 2, col="red") 

auc_object_DT_boost <- performance(pred_object_DT_boost, measure = "auc")
auc_DT_boost <- auc_object_DT_boost@y.values[[1]]
auc_DT_boost 

#Measures accuracy
accuracy_list_boost 
accuracy_average_DT_boost <- mean(accuracy_list_boost)
accuracy_average_DT_boost 
sd(accuracy_list_boost) 

sensitivity_list_boost
sensitivity_average_DT_boost <- mean(sensitivity_list_boost)
sensitivity_average_DT_boost 
sd(sensitivity_list_boost) 

specificity_list_boost
specificity_average_DT_boost <- mean(specificity_list_boost)
specificity_average_DT_boost 
sd(specificity_list_boost) 

precision_list_boost
precision_average_DT_boost <- mean(precision_list_boost)
precision_average_DT_boost 
sd(precision_list_boost) 

recall_list_boost
recall_average_DT_boost <- mean(recall_list_boost)
recall_average_DT_boost 
sd(recall_list_boost) 

f1_list_boost
f1_average_DT_boost <- mean(f1_list_boost)
f1_average_DT_boost 
sd(f1_list_boost) 

#_________________________________________________________________________________________

#SVM

data_svm1 <- read.csv("ML Coursework Final Dataset.csv", stringsAsFactors = TRUE)
str(data_svm1)

data_svm <- as.data.frame(lapply(data_svm1[2:31], normalize))
data_svm$goal<-data_svm1$goal

K = 10 
set.seed(0599)
folds <- createFolds(data_svm$goal, k = K)
str(folds)  

accuracy_list_svm<-as.numeric() 
sensitivity_list_svm<-as.numeric() 
specificity_list_svm<-as.numeric()
precision_list_svm<-as.numeric()
recall_list_svm<-as.numeric()
f1_list_svm<-as.numeric()
appendedDf_svm<-data.frame()

for(i in 1:K){ 
  svm_test <- data_svm[folds[[i]],] 
  svm_train <- data_svm[-folds[[i]],]
  
  set.seed(2311)
  model_svm <- ksvm(goal ~ ., data = svm_train, kernel = "vanilladot") 
  
  
  predict_class_svm<- predict(model_svm, select(svm_test, -goal), type='response')
  
  cm_svm <- confusionMatrix(predict_class_svm, svm_test$goal, positive = "YES") 
  
  accuracy_svm <- cm_svm$overall['Accuracy']
  sensitivity_svm <- cm_svm$byClass['Sensitivity'] 
  specificity_svm <- cm_svm$byClass['Specificity']
  precision_svm <- cm_svm$byClass['Precision'] 
  recall_svm <- cm_svm$byClass['Recall']
  f1_svm <- cm_svm$byClass['F1']
  
  accuracy_list_svm<- append(accuracy_list_svm,accuracy_svm)
  sensitivity_list_svm<- append(sensitivity_list_svm,sensitivity_svm)
  specificity_list_svm<- append(specificity_list_svm,specificity_svm)
  precision_list_svm<- append(precision_list_svm,precision_svm)
  recall_list_svm<- append(recall_list_svm,recall_svm)
  f1_list_svm<- append(f1_list_svm,f1_svm)
  
  model_svm_prob <- ksvm(goal ~ ., data = svm_train, kernel = "vanilladot",prob.model=TRUE)
  predict_class_svm_prob <- predict(model_svm_prob, select(svm_test,-goal), type='prob')
  predict_class_list_svm <- data.frame(actual_type = svm_test$goal,
                                       predict_type = predict_class_svm,
                                       prob_Yes = round(predict_class_svm_prob[ , 2], 5),
                                       prob_No = round(predict_class_svm_prob[ , 1], 5))
  
  appendedDf_svm <- rbind(appendedDf_svm, predict_class_list_svm)
}

CrossTable(predict_class_svm, svm_test$goal, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted default', 'actual default' ))

##AUC and ROC curve
pred_object_svm <- prediction(appendedDf_svm$prob_Yes, appendedDf_svm$actual_type) 
roc_svm<- performance(pred_object_svm, measure = "tpr", x.measure = "fpr")
plot(roc_svm, main = "SVM Model ROC curve for ICO", col = "blue", lwd = 2) 
abline(a = 0, b = 1, lwd = 2, lty = 2, col = "red") 

auc_object_svm <- performance(pred_object_svm, measure = "auc")
auc_svm <- auc_object_svm@y.values[[1]]
auc_svm 

#Measures accuracy
accuracy_list_svm 
accuracy_average_svm <- mean(accuracy_list_svm)
accuracy_average_svm 
sd(accuracy_list_svm) 

sensitivity_list_svm
sensitivity_average_svm <- mean(sensitivity_list_svm)
sensitivity_average_svm 
sd(sensitivity_list_svm) 

specificity_list_svm
specificity_average_svm <- mean(specificity_list_svm)
specificity_average_svm 
sd(specificity_list_svm) 

precision_list_svm
precision_average_svm <- mean(precision_list_svm)
precision_average_svm 
sd(precision_list_svm) 

recall_list_svm
recall_average_svm <- mean(recall_list_svm)
recall_average_svm 
sd(recall_list_svm) 

f1_list_svm
f1_average_svm<- mean(f1_list_svm)
f1_average_svm 
sd(f1_list_svm) 

#Combined ROC Curve
plot(roc_knn_k3, main = "ROC Curve", col = "red", lwd = 2) 
plot(roc_DT, add=TRUE, col='blue')
plot(roc_DT_boost, add=TRUE, col='green')
plot(roc_svm, add=TRUE, col='red')
abline(a = 0, b = 1, lwd = 2, lty = 2,col = "black") 
legend(0.6,0.4,legend = c("K-NN","DT","AdaBoost","SVM"), col = c("red","blue","green","orange"),lty=1:1, cex=0.8)

