#setting working directory
setwd('/Users/SaiSanthosh/Desktop/data-challenge')
library(class)
library(e1071)

#removing all variables in environment
rm(list = ls())

#reading train and test data into two different dataframes
df_train <- read.csv('training_data.csv')
df_test <- read.csv('test_data.csv')

###summary of dataframe.
summary(df_train)
summary(df_test)


#removing columns-weight, payer_code, medical_specialty since they have large number of null values.
#Also, removing patient_nbr since it is randomly generated. 
#Also, removing examide, citoglipton since columns has only one value. It won't have any effect on target variable.
df_train <- subset(df_train, select = -c( patient_nbr,weight, medical_specialty, payer_code, citoglipton, examide) )
df_test <- subset(df_test, select = -c( patient_nbr,weight, medical_specialty, payer_code, citoglipton, examide) )

#checking class of all variables in dataframe
lapply(df_train, class)
lapply(df_test, class)

#Considering variables with num or number, time_in_hospital as intergers and rest all as factors. Converting variables 
#that are integers into factors.
names <- c('admission_type_id','discharge_disposition_id','admission_source_id' )
df_train[,names] <- lapply(df_train[,names], factor)
df_test[,names] <- lapply(df_test[,names], factor)


#checking class of all variables in dataframe
lapply(df_train, class)
lapply(df_test, class)


#Converting '?' to NA's and Unknown/Invalid
df_train[df_train== '?'] <- NA
df_train[df_train == 'Unknown/Invalid'] <- NA

df_test[df_test== '?'] <- NA
df_test[df_test == 'Unknown/Invalid'] <- NA

#Omittiing NA's from train and test dataset
df_train <- na.omit(df_train)
df_test<- na.omit(df_test)

#icd library for converting codes to words
library(icd)

#Making a new dataframe with diag_1, diag_2, diag_3 columns
diag <- df_train[c('diag_1','diag_2','diag_3')]

#Converting all ICD codes to the explaination codes in words
for (i in 1:nrow(diag)){
  diag$desc1[i] <- icd_explain(diag$diag_1[i])
  diag$desc2[i] <- icd_explain(diag$diag_2[i])
  diag$desc3[i] <- icd_explain(diag$diag_3[i])
}

#For analysis purpose considering first word of ICD code. Thus, decreasing factor levels
for (j in 1:nrow(diag)){
  diag$diag1[j] <- strsplit(diag$desc1[j], split=' ')[[1]][1]
  diag$diag2[j] <- strsplit(diag$desc2[j], split=' ')[[1]][1]
  diag$diag3[j] <- strsplit(diag$desc3[j], split=' ')[[1]][1]
}

#Combing two datasets selecting only first word columns for 3 diag's
df_trnew <- cbind(df_train, diag$diag1, diag$diag2, diag$diag3)
df_trnew <- subset(df_trnew, select = -c(diag_1, diag_2,diag_3) )
colnames(df_trnew)[colnames(df_trnew) == 'diag$diag1'] <- 'diag_1'
colnames(df_trnew)[colnames(df_trnew) == 'diag$diag2'] <- 'diag_2'
colnames(df_trnew)[colnames(df_trnew) == 'diag$diag3'] <- 'diag_3'


#Converting ICD codes for test dataset
diag_test <- df_test[c('diag_1','diag_2','diag_3')]

#Converting all ICD codes to the explaination codes in words for test dataset
for (m in 1:nrow(diag_test)){
  diag_test$desc1[m] <- icd_explain(diag_test$diag_1[m])
  diag_test$desc2[m] <- icd_explain(diag_test$diag_2[m])
  diag_test$desc3[m] <- icd_explain(diag_test$diag_3[m])
}

#For analysis purpose considering first word of ICD code. Thus, decreasing factor levels for test dataset
for (n in 1:nrow(diag)){
  diag_test$diag1[n] <- strsplit(diag_test$desc1[n], split=' ')[[1]][1]
  diag_test$diag2[n] <- strsplit(diag_test$desc2[n], split=' ')[[1]][1]
  diag_test$diag3[n] <- strsplit(diag_test$desc3[n], split=' ')[[1]][1]
}

#Combing two datasets selecting only first word columns for 3 diag's for test dataset
df_tenew <- cbind(df_test, diag_test$diag1, diag_test$diag2, diag_test$diag3)
df_tenew <- subset(df_tenew, select = -c(diag_1, diag_2,diag_3) )
colnames(df_tenew)[colnames(df_tenew) == 'diag_test$diag1'] <- 'diag_1'
colnames(df_tenew)[colnames(df_tenew) == 'diag_test$diag2'] <- 'diag_2'
colnames(df_tenew)[colnames(df_tenew) == 'diag_test$diag3'] <- 'diag_3'

#Dropping diag_1, diag_2, diag_3 still coulmns have large factor levels.
df_tenew <- subset(df_tenew, select = -c(diag_1, diag_2, diag_3) )
df_trnew <- subset(df_trnew, select = -c(diag_1, diag_2, diag_3) )

#splitting df_train into train and test subsets with 80:20 ratio
index<-sample(0.8*nrow(df_trnew))
train<-df_trnew[index,]
test<-df_trnew[-index,]


#Performing randomForest on train dataset. Taking random tuning parameters
library(randomForest)
n <- names(train[,-1])
f<-as.formula(paste("readmitted ~", paste(n[!n %in% "readmitted"], collapse = " + ")))

#Model for building randomForest
model <- randomForest(f, data= train[,-1], ntree = 2000, mtry = 25, importance=TRUE)
#Predicting test set with the model
prediction<-predict(model,test[,-40])

#Confusion Matrix for actual vs predicted
table(actual=test$readmitted,prediction)
wrong<-(test$readmitted!=prediction)
error_rate<-sum(wrong)/length(wrong)
error_rate

#Plotting variance importance plots to select the features which are important
varImpPlot(model)

<p>
  RFplot_All.jpeg
</p>
  
  #Below features are important
  t <- c('encounter_id', 'race','gender','age','admission_type_id','discharge_disposition_id','admission_source_id','time_in_hospital',
         'num_lab_procedures','num_procedures','num_medications','number_outpatient','number_emergency',
         'number_inpatient','number_diagnoses','max_glu_serum','A1Cresult','metformin','glimepiride','glipizide',
         'glyburide','pioglitazone','rosiglitazone','insulin','change','diabetesMed','readmitted')

#Selecting only important features and testing again to check for any improvements
train <- subset(train, select = colnames(train) %in% t)
test <- subset(test, select = colnames(test) %in% t)

n <- names(train[,-1])
f<-as.formula(paste("readmitted ~", paste(n[!n %in% "readmitted"], collapse = " + ")))

#model built using only important features
model_rf <- randomForest(f, data= train[,-1], ntree = 1500, mtry = 5, importance=TRUE)
prediction_rf<-predict(model_rf,test[,-40])
table(actual=test$readmitted,prediction_rf)
wrong_rf<-(test$readmitted!=prediction_rf)
error_rate_rf<-sum(wrong_rf)/length(wrong_rf)
error_rate_rf


#naivebayes model built using only important factors
library(e1071)
model_nb <- naiveBayes(f, data=train[,-1])
prediction_nb<-predict(model_nb,test[,-40])
table(actual=test$readmitted,prediction_nb)
wrong_nb<-(test$readmitted!=prediction_nb)
error_rate_nb<-sum(wrong_nb)/length(wrong_nb)
error_rate_nb

#svm model built using only important features
model_svm <- svm(f, data=train[,-1], kernel = 'linear')
prediction_svm<-predict(model_svm,test[,-40])
table(actual=test$readmitted,prediction_svm)
wrong_svm<-(test$readmitted!=prediction_svm)
error_rate_svm<-sum(wrong_svm)/length(wrong_svm)
error_rate_svm

#In above 3 models, randomForest and SVM models predicted less error rate for whether patient will be readmitted or not. 
#So building randomForest model for test dataset using complete train dataset.

#Selecting important features from both train and test
df_trnew <- subset(df_trnew, select = colnames(df_trnew) %in% t)
df_tenew <- subset(df_tenew, select = colnames(df_tenew) %in% t)

n <- names(df_trnew[,-1])
fm<-as.formula(paste("readmitted ~", paste(n[!n %in% "readmitted"], collapse = " + ")))

#Random forest model for train dataset
model_final<- randomForest(fm, data=df_trnew[,-1], ntree = 1500, mtry=6)
df_tenew <- rbind(df_trnew[1, -27] , df_tenew)
df_tenew <- df_tenew[-1,]

#Predicting readmitted for test dataset.
df_tenew$predicted_probability <- predict(model_final, df_tenew[,-1])

write.csv(df_tenew[,c(1,27)], file='sai_tallapally.csv', row.names = FALSE)