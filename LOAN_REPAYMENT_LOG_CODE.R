##############-------LOGISTIC REGRESSION CASE STUDY-1----------------############

#Problem Statement: 

#For the given data, build a Logistic Regression model to find out the key variables which significantly 
#explains the probability of 'Default_On_Payment'.The Business Context of the question is as follows:
#----'The client, a financial service institution, want to increase revenue 
#streams and intents to target a segment of their customers who are most likely to default on the 
#loans/Credit taken.'



#------------------------------Preparing the environment for Logistic Regression---------------------------------------#

list.of.packages <- c("caret", "ggplot2","MASS","car","mlogit","caTools","sqldf","Hmisc","aod","BaylorEdPsych","ResourceSelection","pROC","ROCR")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(sqldf)
library(Hmisc)
library(aod)
library(BaylorEdPsych)
library(ResourceSelection)
library(pROC)
library(ROCR)
library(caTools)

#--------------------------------Setting the Working Directory-----------------------------------------#
Path<- "C:/Users/sai ram/Desktop/CHAVI_IVY/MODULE-4_R/CASE STUDIES/LOGISTIC REGRESSION/CASE Study 1"
setwd(Path)
getwd()

ldata<-read.csv("Data_for_Logistic_Regression.csv",stringsAsFactors = FALSE, header = TRUE)
ldatacopy=ldata#To create a backup of original data

#------------------------------------Basic Exploration of the data--------------------------------------------# 
str(ldata)
dim(ldata)
View(ldata)

##-- changing the class of all 'char' variables
names <- -c(1,3,6,14,22)
ldata[,names] <- lapply(ldata[,names] , factor)
str(ldata)
summary(ldata)

#-----------------------------------Missing Value Treatment (if any)-------------------------------------------#
data.frame(colSums(is.na(ldata)))

####--- no missing values are found-----------------######


#--------------------------------Information Value Calculation (A variable reduction technique)----------------------------------#

#-----------> Creating two data sets for numeric and categorical values

## Data set with numeric variable
num <- ldata[,c(1,3,6,14,22)]#Numerical Data Frame
cat <- ldata[,-c(1,3,6,14)]#Categorical Data Frame
View(num)
#---------------------------------------IV for numeric data-------------------------------------------------------#

IVCal <- function(variable, target,data,groups)
{
  data[,"rank"] <- cut2(data[,variable],g=groups)
  tableOutput <-sqldf(sprintf("select rank, 
                              count(%s) n,
                              sum(%s) good
                              from data 
                              group by rank",target,target))
  tableOutput <- sqldf("select *,
                       (n - good) bad
                       from tableOutput")
  tableOutput$bad_rate<- tableOutput$bad/sum(tableOutput$bad)*100
  tableOutput$good_rate<- tableOutput$good/sum(tableOutput$good)*100
  tableOutput$WOE<- (log(tableOutput$good_rate/tableOutput$bad_rate))*100
  tableOutput$IV <- (log(tableOutput$good_rate/tableOutput$bad_rate))*(tableOutput$good_rate-tableOutput$bad_rate)/100
  IV <- sum(tableOutput$IV[is.finite(tableOutput$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}

colnames(num)

a1<- IVCal("Customer_ID","Default_On_Payment",num,groups=10)
a2<- IVCal("Duration_in_Months","Default_On_Payment",num,groups=10)
a3<- IVCal("Credit_Amount","Default_On_Payment",num,groups=10)
a4<- IVCal("Age","Default_On_Payment",num,groups=10)

IV_num <- data.frame(rbind(a1,a2,a3,a4))
IV_num


#-------------------------------------Information Value for categorical data----------------------------------------------------------#

CA <- function(target, variable, data) {
  A1<- fn$sqldf("select $variable,count($target)n, sum($target)good from data group by $variable")
  
  A1<- fn$sqldf("select *, (n-good) bad from A1")
  A1$bad_rate <- A1$bad/sum(A1$bad)*100
  
  A1$good_rate<- A1$good/sum(A1$good)*100
  A1$WOE<- (log(A1$good_rate/A1$bad_rate))*100
  A1$IV <- (log(A1$good_rate/A1$bad_rate))*(A1$good_rate-A1$bad_rate)/100
  IV <- sum(A1$IV[is.finite(A1$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}
colnames(cat)
A<- CA("Default_On_Payment","Status_Checking_Acc",cat)
B<- CA("Default_On_Payment","Credit_History",cat)
C<- CA("Default_On_Payment","Purposre_Credit_Taken",cat)
D<- CA("Default_On_Payment","Savings_Acc",cat)
E<- CA("Default_On_Payment","Years_At_Present_Employment",cat)
F<- CA("Default_On_Payment","Inst_Rt_Income",cat)

G<- CA("Default_On_Payment","Marital_Status_Gender",cat)
H<- CA("Default_On_Payment","Other_Debtors_Guarantors",cat)
I<- CA("Default_On_Payment","Current_Address_Yrs",cat)
J<- CA("Default_On_Payment","Property",cat)
K<- CA("Default_On_Payment","Other_Inst_Plans",cat)
L<- CA("Default_On_Payment","Housing",cat)
M<- CA("Default_On_Payment","Num_CC",cat)
O<- CA("Default_On_Payment","Job",cat)
P<- CA("Default_On_Payment","Dependents",cat)
Q<- CA("Default_On_Payment","Telephone",cat)
R<- CA("Default_On_Payment","Foreign_Worker",cat)
S<- CA("Default_On_Payment","Count",cat)

IV_cat<- data.frame(rbind(A,B,C,D,E,F,G,H,I,J,K,L,M,O,P,Q,R,S))
IV_cat
Final_IV <- data.frame(rbind(IV_num,IV_cat))
Final_IV

########################################################### IV Ends here ##############################################

####---- copy of the original data will be used due to CONTRAST ERROR in the modelling ----####
#--therefore, the solution is that all the variables should be of 'char' class except the dependent variable---###

#--------------------------Splitting the data into training and test data set------------------------#

set.seed(144)#This is used to produce reproducible results, everytime we run the model

spl = sample.split(ldatacopy$Default_On_Payment, 0.7)
dtrain = subset(ldatacopy, spl == TRUE)
str(dtrain)
dim(dtrain)

dtrain$Default_On_Payment<- as.factor(dtrain$Default_On_Payment)

dtest = subset(ldatacopy, spl == FALSE)
str(dtest)
dim(dtest)

#-------------------------------------Logistic Regression Model Building------------------------------------------#

model <- glm(Default_On_Payment~ ., data= dtrain, family= binomial())
summary(model)


## Remove the insignificant variables - 'Count' and 'Current_Address_Yrs'
model1 <- glm(Default_On_Payment~ Customer_ID +Status_Checking_Acc + Duration_in_Months+ Credit_History + 
               Purposre_Credit_Taken + Credit_Amount+ Savings_Acc+ Years_At_Present_Employment + Inst_Rt_Income +
               Marital_Status_Gender + Other_Debtors_Guarantors + Property + Age + 
               Other_Inst_Plans + Housing + Num_CC + Job + Dependents + Telephone + 
               Foreign_Worker ,data= dtrain, family= binomial())
summary(model1)

## Remove the insignificant variable- customer_id , Property(A122,A123) and Other_Inst_Plans(A142)
model2 <- glm(Default_On_Payment~ Status_Checking_Acc + Duration_in_Months+ Credit_History + 
               Purposre_Credit_Taken + Credit_Amount+ Savings_Acc+ Years_At_Present_Employment + Inst_Rt_Income +
               Marital_Status_Gender + Other_Debtors_Guarantors + I(Property=="A124")+ Age + 
               I(Other_Inst_Plans=="A143") + Housing + Num_CC + Job + Dependents + Telephone + 
               Foreign_Worker ,data= dtrain, family= binomial())
summary(model2)

## Remove  Purposre_Credit_Taken(A44, A45, A46) and Savings_Acc(A63) 
model3 <- glm(Default_On_Payment~ Status_Checking_Acc + Duration_in_Months+ Credit_History + 
               I(Purposre_Credit_Taken=="A41") +I(Purposre_Credit_Taken=="A410") + I(Purposre_Credit_Taken=="A42") +
               I(Purposre_Credit_Taken=="A43") +Credit_Amount+ I(Savings_Acc=="A62")+  I(Savings_Acc=="A64")+ I(Savings_Acc=="A65")+  
               Years_At_Present_Employment+ Inst_Rt_Income +
               Marital_Status_Gender + Other_Debtors_Guarantors + I(Property=="A124")+ Age + 
               I(Other_Inst_Plans=="A143") + Housing + Num_CC + Job + Dependents + Telephone + 
               Foreign_Worker ,data= dtrain, family= binomial())
summary(model3)

## Remove Years_At_Present_Employment(A72) , Marital_Status_Gender(A94,A92) 
model4 <- glm(Default_On_Payment~ Status_Checking_Acc + Duration_in_Months + Credit_History+ 
               I(Purposre_Credit_Taken=="A41") +I(Purposre_Credit_Taken=="A410") + I(Purposre_Credit_Taken=="A42") +
               I(Purposre_Credit_Taken=="A43") +Credit_Amount+ I(Savings_Acc=="A62")+  I(Savings_Acc=="A64")+ I(Savings_Acc=="A65")+  
               I(Years_At_Present_Employment=="A73")+ I(Years_At_Present_Employment=="A74")+I(Years_At_Present_Employment=="A75")+
               Inst_Rt_Income + I(Marital_Status_Gender=="A93") + Other_Debtors_Guarantors + I(Property=="A124")+ Age + 
               I(Other_Inst_Plans=="A143") + Housing + Num_CC + Job + Dependents + Telephone + 
               Foreign_Worker ,data= dtrain, family= binomial())
summary(model4)

## Remove Credit_History(A31,A32) , housing (A153) , Years_At_Present_Employment(A73, A75)
model5 <- glm(Default_On_Payment~ Status_Checking_Acc + Duration_in_Months+ I(Credit_History=="A33")+ 
                I(Credit_History=="A34") +I(Purposre_Credit_Taken=="A41") +I(Purposre_Credit_Taken=="A410") + I(Purposre_Credit_Taken=="A42") +
                I(Purposre_Credit_Taken=="A43") +Credit_Amount+ I(Savings_Acc=="A62")+  I(Savings_Acc=="A64")+ I(Savings_Acc=="A65")+  
                I(Years_At_Present_Employment=="A74")+ Inst_Rt_Income + I(Marital_Status_Gender=="A93") + 
                Other_Debtors_Guarantors + Age + Property+ 
                I(Other_Inst_Plans=="A143") + I(Housing=="A152") + Num_CC + Job + Dependents + Telephone + 
                Foreign_Worker ,data= dtrain, family= binomial())
summary(model5)


#-----------------------------------------Final Model---------------------------------------#
# removing Property and Job
trainmodel <- glm(Default_On_Payment~ Status_Checking_Acc + Duration_in_Months+ I(Credit_History=="A33")+ 
                    I(Credit_History=="A34") +I(Purposre_Credit_Taken=="A41") +I(Purposre_Credit_Taken=="A410") + 
                    I(Purposre_Credit_Taken=="A42") + I(Purposre_Credit_Taken=="A43") +Credit_Amount+ 
                    I(Savings_Acc=="A62")+  I(Savings_Acc=="A64")+ I(Savings_Acc=="A65")+  
                    I(Years_At_Present_Employment=="A74")+ Inst_Rt_Income + I(Marital_Status_Gender=="A93") + 
                    Other_Debtors_Guarantors + Age +  
                    I(Other_Inst_Plans=="A143") + I(Housing=="A152") + Num_CC + Dependents + Telephone + 
                    Foreign_Worker ,data= dtrain, family= binomial())
summary(trainmodel)


vif(trainmodel)
# all VIF values are less than 1.7

# Deviance is -2*Log Likelyhood
# AIC = -2LL + 2k
# BIC = -2LL + 2k x log(n)

#------------------------------Checking the overall fitness of the model----------------------------#


#--------------->using Wald Test
wald.test(b=coef(trainmodel), Sigma= vcov(trainmodel), Terms=1:23)#Here Terms, no. of independent variables in your final train model
#Since, p-value is less then 0.001, hence we reject Ho that the all Bi=0


#------------------->Lagrange Multiplier or Score Test (Assess whether the current variable 
#significantly improves the model fit or not)

# Difference betweene null deviance and deviance
trainmodelChi <- trainmodel$null.deviance - trainmodel$deviance
trainmodelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- trainmodel$df.null - trainmodel$df.residual
chidf


# With more decimal places
# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(trainmodelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)

#------------------------------------Predicting power of the model using R2----------------------------#
PseudoR2(trainmodel)
#--- individual functions are less effective hence not used-----###


#--------------------Lackfit Deviance for assessing wether the model where
#Ho: Observed Frequencies/probabilties =Expected FRequencies/probabilties ----------------------------------------#
residuals(trainmodel) # deviance residuals
residuals(trainmodel, "pearson") # pearson residuals

sum(residuals(trainmodel, type = "pearson")^2)
deviance(trainmodel)

#########Larger p value indicate good model fit
1-pchisq(deviance(trainmodel), df.residual(trainmodel))
#Thus, we accept the Null Hypthesis Ho thet Observed Frequencies = Expected Frequencies


#####################################################################################################################
###-------- to intrepret the dependent based on signficant independent variables--###

# Coefficients (Odds)
trainmodel$coefficients
# Coefficients (Odds Ratio)
as.data.frame(exp(trainmodel$coefficients))

##---- for ex- 
#if'dependents' increases by 1 unit then 'Default_on_payment' is 1.4 times more 
# likely to increase.

# Variable Importance of the model
varImp(trainmodel)

# Predicted Probabilities
prediction <- predict(trainmodel,newdata = dtrain,type="response")
prediction

#write.csv(prediction,"pred.csv")

# in order to see roc curve => dependent variable needs to be a factor variable
dtrain$Default_On_Payment <- as.factor(dtrain$Default_On_Payment)
rocCurve   <- roc(response = dtrain$Default_On_Payment, predictor = prediction, 
                  levels = rev(levels(dtrain$Default_On_Payment)))

rocCurve

#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = dtrain$Default_On_Payment)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)


#########################################################################################################################
### KS statistics calculation
dtrain$m1.yhat <- predict(trainmodel, dtrain, type = "response")
m1.scores <- prediction(dtrain$m1.yhat, dtrain$Default_On_Payment)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 0.4 - 0.7
# ks value is 0.4971 ,hence cleared


############################################################################################################


###################### Residual Analysis ################################################################################


logistic_data <- dtrain

logistic_data$predicted.probabilities<-fitted(trainmodel)

logistic_data$standardized.residuals<-rstandard(trainmodel)
logistic_data$studentized.residuals<-rstudent(trainmodel)
logistic_data$dfbeta<-dfbeta(trainmodel)
logistic_data$dffit<-dffits(trainmodel)
logistic_data$leverage<-hatvalues(trainmodel)

logistic_data[, c("leverage", "studentized.residuals", "dfbeta")]
#write.csv(logistic_data, "Res.csv")




###########################################   Model has been build  ##############################################
###########################################   Testing on the test dataset  #######################################




# Logistic Regression on full data 
# with all variables
dem<- glm(Default_On_Payment~., data= dtest, family = binomial())
summary(dem)

#checking with last train model variables
modeltest1 <- glm(Default_On_Payment~ Status_Checking_Acc + Duration_in_Months+ I(Credit_History=="A33")+ 
                I(Credit_History=="A34") +I(Purposre_Credit_Taken=="A41") +I(Purposre_Credit_Taken=="A410") + I(Purposre_Credit_Taken=="A42") +
                I(Purposre_Credit_Taken=="A43") +Credit_Amount+ I(Savings_Acc=="A62")+  I(Savings_Acc=="A64")+ I(Savings_Acc=="A65")+  
                I(Years_At_Present_Employment=="A74")+ Inst_Rt_Income + I(Marital_Status_Gender=="A93") + 
                Other_Debtors_Guarantors + Age + Property+ 
                I(Other_Inst_Plans=="A143") + I(Housing=="A152") + Num_CC + Job + Dependents + Telephone + 
                Foreign_Worker ,data= dtest, family= binomial())
summary(modeltest1)

#removing job, dependents, Age ,Savings_Acc == "A62" , Other_Debtors_GuarantorsA102 and telephone
modelt <- glm(Default_On_Payment~ Status_Checking_Acc + Duration_in_Months+ I(Credit_History=="A33")+ 
                I(Credit_History=="A34") +I(Purposre_Credit_Taken=="A41") +I(Purposre_Credit_Taken=="A410") + I(Purposre_Credit_Taken=="A42") +
                I(Purposre_Credit_Taken=="A43") +Credit_Amount+ I(Savings_Acc=="A64")+ I(Savings_Acc=="A65")+  
                I(Years_At_Present_Employment=="A74")+ Inst_Rt_Income + 
                I(Marital_Status_Gender=="A93") + 
                I(Other_Debtors_Guarantors=="A103") + Property+ 
                I(Other_Inst_Plans=="A143") + I(Housing=="A152") + Num_CC  + 
                Foreign_Worker ,data= dtest, family= binomial())
summary(modelt)



vif(modelt)
# all values are less than 1.7 hence, NO Multicollinearity


# Deviance is -2*Log Likelyhood
# AIC = -2LL + 2k
# BIC = -2LL + 2k x log(n)

library(car)
library(mlogit)

# Difference between -2LL of Null model and model with variables
modelChi <- modelt$null.deviance - modelt$deviance
modelChi

#Finding the degree of freedom for Null model and model with variables
chidf <- modelt$df.null - modelt$df.residual
chidf

# With more decimal places
# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)

##--- calculating R2----###
PseudoR2(modelt)
# all R2 values are greater than 0.2. hence Model is good fit

######### Lackfit Deviance ######################################################
residuals(modelt) # deviance residuals
residuals(modelt, "pearson") # pearson residuals

sum(residuals(modelt, type = "pearson")^2)
deviance(modelt)

#########Large p value indicate good model fit
1-pchisq(deviance(modelt), df.residual(modelt))

# p-value is larger hence, model is good fit

#--------------->using Wald Test
wald.test(b=coef(modelt), Sigma= vcov(modelt), Terms=1:24)#Here Terms, no. of independent variables in your final train model
#Since, p-value is less then 0.001, hence we reject Ho that the all Bi=0

#####################################################################################################################
# Coefficients (Odds)
modelt$coefficients
# Coefficients (Odds Ratio)
as.data.frame(exp(modelt$coefficients))

# Predicted Probabilities
prediction <- predict(modelt,newdata = dtest,type="response")
prediction

#write.csv(prediction, file = "C:\\Users\\Subhojit\\Desktop\\Logistic Regression\\Prepared by me\\pred.csv")


rocCurve   <- roc(response = dtest$Default_On_Payment, predictor = prediction, 
                  levels = rev(levels(dtest$Default_On_Payment)))
dtest$Default_On_Payment <- as.factor(dtest$Default_On_Payment)


#Metrics - Fit Statistics

predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = dtest$Default_On_Payment)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1

AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)



#########################################################################################################################
### KS statistics calculation
dtest$m1.yhat <- predict(modelt, dtest, type = "response")

library(ROCR)
m1.scores <- prediction(dtest$m1.yhat, dtest$Default_On_Payment)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")

m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 40 - 70

############################################################################################################
############----- END OF THE MODELLING-----------############
