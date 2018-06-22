library(caTools)#splitting dataset
library(ggplot2)#for GBPlot
library(Amelia)# Plots for missing values
library(ROCR)#Plotting the ROC curve
library(grid)#used in GBPLOT
library(gridExtra)#used in GBPLOT
library(corrplot) # used for correalation matrics

#import dataset
atrn=read.csv("C:/Users/user/Desktop/wdofrproj/Attrition.csv",na.strings = c(""," ","NA"))
View(atrn)
str(atrn)

#checking response variable
attach(atrn)
ggplot(atrn,aes(Attrition,fill=Attrition))+geom_bar()+labs(x="Attrition", 
        y="Count of Attriation")+ggtitle("Attrition")+ggtitle("Attrition")
prop.table(table(Attrition))  


#checking for missing values
missmap(atrn, main = "Missing values vs observed")
mv_df <- data.frame(colSums(is.na(atrn)))
mv_df

#feature selection
#below are Unique value column this will not impact on prediction.
atrn$EmployeeCount<-NULL
atrn$EmployeeNumber<-NULL
atrn$Over18<-NULL
atrn$StandardHours<-NULL

#for Outliers
boxplot(atrn)
boxplot(atrn$MonthlyIncome)
atrn$MonthlyIncome<-log(atrn$MonthlyIncome)
boxplot(atrn)


#plot b/w dependent and independent variables
ggplot(atrn,aes(Age,fill=Attrition))+geom_density()+facet_grid(~Attrition)
ggplot(atrn,aes(BusinessTravel,fill=Attrition))+geom_bar()
ggplot(atrn,aes(DailyRate,Attrition))+geom_point(size=4,alpha = 0.05)
ggplot(atrn,aes(Department,fill = Attrition))+geom_bar()
ggplot(atrn,aes(DistanceFromHome,fill=Attrition))+geom_bar()
ggplot(atrn,aes(Education,fill=Attrition))+geom_bar()
ggplot(atrn,aes(EducationField,fill=Attrition))+geom_bar()
ggplot(atrn,aes(EnvironmentSatisfaction,fill=Attrition))+geom_bar()
ggplot(atrn,aes(Gender,fill=Attrition))+geom_bar()
ggplot(atrn,aes(HourlyRate,fill=Attrition))+geom_bar()
ggplot(atrn,aes(JobInvolvement,fill=Attrition))+geom_bar()
ggplot(atrn,aes(JobLevel,fill=Attrition))+geom_bar()
ggplot(atrn,aes(JobRole,fill=Attrition))+geom_bar()
ggplot(atrn,aes(JobSatisfaction,fill=Attrition))+geom_bar()
ggplot(atrn,aes(MaritalStatus,fill=Attrition))+geom_bar()
ggplot(atrn,aes(MonthlyIncome,fill=Attrition))+geom_density()
ggplot(atrn,aes(MonthlyRate,fill=Attrition))+geom_density()
ggplot(atrn,aes(NumCompaniesWorked,fill=Attrition))+geom_bar()
ggplot(atrn,aes(OverTime,fill=Attrition))+geom_bar()
ggplot(atrn,aes(PercentSalaryHike,Attrition))+geom_point(size=4,alpha = 0.01)
ggplot(atrn,aes(PerformanceRating,fill = Attrition))+geom_bar()
ggplot(atrn,aes(RelationshipSatisfaction,fill = Attrition))+geom_bar()
ggplot(atrn,aes(StockOptionLevel,fill = Attrition))+geom_bar()
ggplot(atrn,aes(TotalWorkingYears,fill = Attrition))+geom_bar()
ggplot(atrn,aes(TrainingTimesLastYear,fill = Attrition))+geom_bar()
ggplot(atrn,aes(WorkLifeBalance,fill = Attrition))+geom_bar()
ggplot(atrn,aes(YearsAtCompany,fill = Attrition))+geom_bar()
ggplot(atrn,aes(YearsInCurrentRole,fill = Attrition))+geom_bar()
ggplot(atrn,aes(YearsSinceLastPromotion,fill = Attrition))+geom_bar()
ggplot(atrn,aes(YearsWithCurrManager,fill = Attrition))+geom_bar()


#Feature engineering
data1 <- atrn
data1$TenurePerJob <- ifelse(data1$NumCompaniesWorked!=0, data1$TotalWorkingYears/data1$NumCompaniesWorked,0)
data1$YearWithoutChange <- data1$YearsInCurrentRole - data1$YearsSinceLastPromotion
data1$YearsWithoutChange2 <- data1$TotalWorkingYears - data1$YearsSinceLastPromotion

tenurePlot <- ggplot(data1,aes(TenurePerJob))+geom_density()+facet_grid(~Attrition)
changePlot <- ggplot(data1,aes(YearWithoutChange))+geom_density()+facet_grid(~Attrition)
change2Plot <- ggplot(data1,aes(YearsWithoutChange2))+geom_density()+facet_grid(~Attrition)
tenurePlot#this new feature has more density in yes part of attrition
changePlot
change2Plot

Med_HR <- median(data1[data1$Department == 'Human Resources',]$MonthlyIncome)
Med_RnD <- median(data1[data1$Department == 'Research & Development',]$MonthlyIncome)
Med_Sales <- median(data1[data1$Department == 'Sales',]$MonthlyIncome)



Med_overall <- median(data1$MonthlyIncome)

data1$CompaRatioDep <- ifelse(data1$Department == 'Human Resources',data1$MonthlyIncome/Med_HR,ifelse(data1$Department=='Research & Development',data1$MonthlyIncome/Med_RnD,data1$MonthlyIncome/Med_Sales))

data1$CompaRatioOverall <- data1$MonthlyIncome/Med_overall

data1$CompaOverallGroup <- ifelse(data1$CompaRatioOverall>4,4,ifelse(data1$CompaRatioOverall>3,3,ifelse(data1$CompaRatioOverall>2,2,ifelse(data1$CompaRatioOverall>1,1,ifelse(data1$CompaRatioOverall>0.5,0.5,0)))))

data1$CompaDepGroup <- ifelse(data1$CompaRatioDep>4,4,ifelse(data1$CompaRatioDep>3,3,ifelse(data1$CompaRatioDep>2,2,ifelse(data1$CompaRatioDep>1,1,ifelse(data1$CompaRatioDep>0.5,0.5,0)))))


CompaOverallPlot <- ggplot(data1,aes(CompaRatioOverall))+geom_density()+facet_grid(~Attrition)
CompaOverallPlot
CompaDepPlot <- ggplot(data1,aes(CompaRatioDep))+geom_density()+facet_grid(~Attrition)
CompaDepPlot



#converting from catagorical to continous variable
atrn$BusinessTravel <- as.integer(atrn$BusinessTravel)
atrn$Department <- as.integer(atrn$Department)
atrn$Gender <- as.integer(atrn$Gender)
atrn$MaritalStatus <- as.integer(atrn$MaritalStatus)
atrn$OverTime <- as.integer(atrn$OverTime)
atrn$JobRole <- as.integer(atrn$JobRole)
atrn$EducationField <- as.integer(atrn$EducationField)


data1_cor <- atrn

for(i in 1:ncol(data1_cor)){
  
  data1_cor[,i]<- as.integer(data1_cor[,i])
}

corrplot(cor(data1_cor))
#cor b/w 4years col , cor job level& mon-inc, cor sal hike & per rat..so take one out of those
#perf rat ,yrs otherthan yrs with curr man,mon inc removed due to mult coll
data1<-data1[,c(-28,-29,-30,-22,-17)]

#int to factor
data1$Education<-as.factor(data1$Education)
data1$EnvironmentSatisfaction<-as.factor(data1$EnvironmentSatisfaction)
data1$JobInvolvement<-as.factor(data1$JobInvolvement)
data1$JobSatisfaction<-as.factor(data1$JobSatisfaction)
data1$RelationshipSatisfaction<-as.factor(data1$RelationshipSatisfaction)
data1$WorkLifeBalance<-as.factor(data1$WorkLifeBalance)


#splitting dataset
mysplit<-sample.split(Y=data1$Attrition,SplitRatio = 0.7)
Training=subset(data1,mysplit==TRUE)
Testing=subset(data1,mysplit==FALSE)
Train<-Training
Test<-Testing[,-2]

#model building
#randomforest
library(randomForest)
model1<-randomForest(Train$Attrition~.,Train,mtry=5,ntree=20)
model1
importance(model1)
varImpPlot(model1)

y_predict<-predict(model1,Test,type = 'class')

t1<-table(predictions=y_predict,actual=Testing$Attrition)
t1
acc1<-sum(diag(t1))/sum(t1)
acc1
is.vector(y_predict)
View(y_predict)
y_predict=ifelse(y_predict=="Yes",1,0)
is.vector(y_predict)
View(y_predict)
View(Test)
#roc curve for RF
ROCRPred_RF <- prediction(y_predict, Testing$Attrition)
ROCRPerf_RF <- performance(ROCRPred_RF,"tpr","fpr")
plot(ROCRPerf_RF,colorize = TRUE, print.cutoffs.at = seq(0.1, by = 0.1))
auc_ROCR_RF <- performance(ROCRPred_RF, measure = "auc")
auc_ROCR1_RF <- auc_ROCR_RF@y.values[[1]]
auc_ROCR1_RF

#decisiontree
library(rpart)
model2<-rpart(Train$Attrition~.,Train,method = 'class')
pred_dt<-predict(model2,Test,type = 'class')
t_dt<-table(predictions=pred_dt,actual=Testing$Attrition)
acc_dt<-sum(diag(t_dt))/sum(t_dt)
acc_dt
plot(model2)

text(model2,pretty = 5)
#roc curve for DT
pred_dt=ifelse(pred_dt=="Yes",1,0)
ROCRPred_DT <- prediction(pred_dt, Testing$Attrition)
ROCRPerf_DT <- performance(ROCRPred_DT,"tpr","fpr")
plot(ROCRPerf_DT,colorize = TRUE, print.cutoffs.at = seq(0.1, by = 0.1))
auc_ROCR_DT <- performance(ROCRPred_DT, measure = "auc")
auc_ROCR1_DT <- auc_ROCR_DT@y.values[[1]]
auc_ROCR1_DT


#svm
library(e1071)
attach(Train)
classifier1 <- svm(formula = Attrition~.,data = Train,
    type = 'C-classification')
summary(classifier1)

classifier2<-svm(formula = Attrition~.,data = Train,type = 'C-classification',kernel="linear")
classifier3<-svm(formula = Attrition~.,data = Train,type = 'C-classification',kernel="sigmoid")
classifier4<-svm(formula = Attrition~.,data = Train,type = 'C-classification',kernel="polynomial")

y_pred1 = predict(classifier1,Test)
is.vector(y_pred1)
View(y_pred1)
y_pred2<-predict(classifier2,Test)
is.vector(Pred_log)
y_pred3<-predict(classifier3,Test)
y_pred4<-predict(classifier4,Test)

t_svm1<-table(y_pred1,Testing$Attrition)
t_svm2<-table(y_pred2,Testing$Attrition)
t_svm3<-table(y_pred3,Testing$Attrition)
t_svm4<-table(y_pred4,Testing$Attrition)
acc_svm1<-sum(diag(t_svm1))/sum(t_svm1)
acc_svm2<-sum(diag(t_svm2))/sum(t_svm2)
acc_svm3<-sum(diag(t_svm3))/sum(t_svm3)
acc_svm4<-sum(diag(t_svm4))/sum(t_svm4)
acc_svm1
acc_svm2#best accuracy
acc_svm3
acc_svm4

y_pred2=ifelse(y_pred2=="Yes",1,0)

is.vector(y_pred2)
ROCRPred_SVM <- prediction(y_pred2, Testing$Attrition)
ROCRPerf_SVM <- performance(ROCRPred_SVM,"tpr","fpr")
plot(ROCRPerf_SVM,colorize = TRUE, print.cutoffs.at = seq(0.1, by = 0.1))
auc_ROCR_SVM <- performance(ROCRPred_DT, measure = "auc")
auc_ROCR1_SVM <- auc_ROCR@y.values[[1]]
auc_ROCR1_SVM

#logistic  regression
model_log<-glm(Train$Attrition~.,family=binomial(link='logit'),Train)
summary(model_log)
Pred_log<-predict(model_log,Test,type = 'response')
is.vector(Pred_log)
View(Pred_log)
Pred_log=ifelse(Pred_log>=0.5,1,0)
t_log<-table(Pred_log,Testing$Attrition)
t_log
acc_log<-sum(diag(t_log))/sum(t_log)


#logistic
acc_log
View(Test)
#Plotting the ROC curve

View(Pred_log)
ROCRPred <- prediction(Pred_log, Testing$Attrition)
ROCRPerf <- performance(ROCRPred,"tpr","fpr")
plot(ROCRPerf,colorize = TRUE, print.cutoffs.at = seq(0.1, by = 0.1))
auc_ROCR <- performance(ROCRPred, measure = "auc")
auc_ROCR1 <- auc_ROCR@y.values[[1]]
auc_ROCR1

