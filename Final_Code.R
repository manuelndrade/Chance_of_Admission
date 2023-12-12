#######################################################
##  R code for Team Project
#######################################################
rm(list=ls())
getwd()
setwd("C:/Users/CSUFTitan/Downloads")

require(ISLR)

Admission <- read.csv("Admission.csv")


n = nrow(Admission)
p = ncol(Admission)

##Descriptive Statistics ~ Histogram and Boxplot##
summary(Admission)

ChanceOfAdmit = Admission[,8]; ## Chance of Admit (y)
hist(ChanceOfAdmit,col =2, xlab="Chance of Admit (y)", ylab = "Number of Students", 
     main= " Histogram graph: Chance of Admit (y)")
boxplot(ChanceOfAdmit, col =4, main= " Histogram graph: Chance of Admit (y)")

GREScore = Admission[,1]; ## GRE Score (x1)
hist(GREScore,col =3, xlab="GREScore (x1)", ylab = "Number of Students", 
     main= " Histogram graph: GREScore (x1)")
plot(GREScore, ChanceOfAdmit, xlab="GRE Score (x1)", ylab = "Chance of Admit (y)", 
     main= " Scatter plot: Chance of Admit (y) vs GRE Score (x1)",
     col="blue")

TOEFLScore = Admission[,2]; ## TOEFL Score (x2)
hist(TOEFLScore,col =4, xlab="TOEFLScore (x2)", ylab = "Number of Students", 
     main= " Histogram graph: TOEFLScore (x2)")
plot(TOEFLScore, ChanceOfAdmit, xlab="TOEFL Score (x2)", ylab = "Chance of Admit (y)", 
     main= " Scatter plot: Chance of Admit (y) vs TOEFL Score (x2)",
     col="red")

UniversityRating = Admission[,3]; ## University Rating (x3)
hist(UniversityRating,col =5, xlab="UniversityRating (x3)", ylab = "Number of Students", 
     main= " Histogram graph: UniversityRating (x3)")
plot(UniversityRating, ChanceOfAdmit, xlab="University Rating (x3)", ylab = "Chance of Admit (y)", 
     main= " Scatter plot: Chance of Admit (y) vs University Rating (x3)",
     col="violet")

count <- table( Admission[,3])
barplot(count, horiz = T,  main = "University Rating") 


SOP = Admission[,4]; ## SOP (x4)
hist(SOP,col =6, xlab="SOP (x4)", ylab = "Number of Students", 
     main= " Histogram graph: SOP (x4)")
plot(SOP, ChanceOfAdmit, xlab="SOP (x4)", ylab = "Chance of Admit (y)", 
     main= " Scatter plot: Chance of Admit (y) vs SOP (x4)",
     col="brown")

ct <- table( Admission[,4])
barplot(ct, horiz = T,  main = "SOP") 


LOR = Admission[,5]; ## LOR (x5)
hist(LOR,col =7, xlab="LOR (x5)", ylab = "Number of Students", 
     main= " Histogram graph: LOR (x5)")
plot(LOR, ChanceOfAdmit, xlab="LOR (x5)", ylab = "Chance of Admit (y)", 
     main= " Scatter plot: Chance of Admit (y) vs LOR (x5)",
     col="blue")

cot <- table( Admission[,5])
barplot(cot, horiz = T,  main = "LOR") 


CGPA = Admission[,6]; ## CGPA (x6)
hist(CGPA,col =11, xlab="CGPA (x6)", ylab = "Number of Students", 
     main= " Histogram graph: CGPA (x6)")
plot(CGPA, ChanceOfAdmit, xlab="CGPA (x6)", ylab = "Chance of Admit (y)", 
     main= " Scatter plot: Chance of Admit (y) vs CGPA (x6)",
     col="maroon")



Research = Admission[,7]; ## Research (x7)
hist(Research,col =10, xlab="Research (x7)", ylab = "Number of Students", 
     main= " Histogram graph: Research (x7)")
plot(Research, ChanceOfAdmit, xlab="Research (x7)", ylab = "Chance of Admit (y)", 
     main= " Scatter plot: Chance of Admit (y) vs Research (x7)",
     col="black")
cout <- table( Admission[,7])
barplot(cout, horiz = T,  main = "Research") 




## Correlation
Correlation.Admission <- as.matrix(round(cor(Admission),2))
Correlation.Admission

######Exploratory Data Analysis############################

##Heat Map
library(reshape2)
melted.Correlation.Admission <- melt(Correlation.Admission)    
head(melted.Correlation.Admission)
library(ggplot2)
heat.Map = ggplot(data = melted.Correlation.Admission, aes(x=Var1, y=Var2, fill=value)) + geom_tile() 
heat.Map + scale_fill_gradient(low = "green", high = "red")  

## Scatter Plot Matrix
library(car)
options(warn=-1)
scatterplotMatrix(~ GREScore + TOEFLScore + UniversityRating + SOP + LOR + CGPA + Research + ChanceOfAdmit, regLine = list(col = 2),
                  col = 1, smooth = list(col.smooth = 4, col.spread = 4),
                  data = Admission)

###########################################################
########Multi-variable Linear Regression to predict the Chances of Admission for the given scores data.

######Model 0: Initial draft to compare models with Continuous VS Categorical predictors on complete dataset

##Considering all predictors as Continuous:
Admission.con <- lm(ChanceOfAdmit ~ ., data = Admission)
summary(Admission.con)

##Considering 4 predictors as Categorical: UniversityRating, SOP, LOR, Research
Admission$UniversityRating <- as.factor(Admission$UniversityRating)
Admission$SOP <- as.factor(Admission$SOP)
Admission$LOR <- as.factor(Admission$LOR)

Admission.cat <- lm(ChanceOfAdmit ~ ., data = Admission)
summary(Admission.cat)

######Model 1: Model using all the predictors as continuous with the training set

####Converting variables back to continuous
Admission$UniversityRating <- as.numeric(Admission$UniversityRating)
Admission$SOP <- as.numeric(Admission$SOP)
Admission$LOR <- as.numeric(Admission$LOR)

##Partitioning the data into training (80%) and testing (20%) data sets 
set.seed(540)
train.index <- sample(row.names(Admission), floor(0.8*n))
test.index <- setdiff(row.names(Admission), train.index)
train.data <- Admission[train.index,]
test.data <- Admission[test.index,]

Admission.fit1 <- lm(ChanceOfAdmit ~ ., data = train.data)
summary(Admission.fit1)

##Calculating MSE and RMSE for Model 1
newdata = test.data[,1:7]
pred1 = predict(Admission.fit1, newdata, se.fit = TRUE)
ChanceOfAdmit.pred1 = pred1$fit
MSE1 = mean((test.data$ChanceOfAdmit - ChanceOfAdmit.pred1)^2) 
MSE1
RMSE1 = sqrt(MSE1)
RMSE1

######Model 2: Removing insignificant predictors (UniversityRating, SOP) from Model 1 to improve the model
Admission.fit2 <- lm(ChanceOfAdmit ~ .-UniversityRating -SOP, data = train.data)
summary(Admission.fit2)

##Calculating MSE and RMSE for Model 2
pred2 = predict(Admission.fit2, newdata, se.fit = TRUE)
ChanceOfAdmit.pred2 = pred2$fit
MSE2 = mean((test.data$ChanceOfAdmit - ChanceOfAdmit.pred2)^2) 
MSE2
RMSE2 = sqrt(MSE2)
RMSE2

####Calculating MSE and RMSE for Model 3
set.seed(540)
Admission$UniversityRating <- as.factor(Admission$UniversityRating)
Admission$SOP <- as.factor(Admission$SOP)
Admission$LOR <- as.factor(Admission$LOR)

train.index <- sample(row.names(Admission), floor(0.8*n))
test.index <- setdiff(row.names(Admission), train.index)
train.data <- Admission[train.index,]
test.data <- Admission[test.index,]

Admission.fit3 <- lm(ChanceOfAdmit ~ ., data = train.data)
summary(Admission.fit3)

##Calculating MSE and RMSE for Model 3
newdata = test.data[,1:7]
pred3 = predict(Admission.fit3, newdata, se.fit = TRUE)
ChanceOfAdmit.pred3 = pred3$fit
MSE3 = mean((test.data$ChanceOfAdmit - ChanceOfAdmit.pred3)^2) 
MSE3
RMSE3 = sqrt(MSE3)
RMSE3


####Calculating MSE and RMSE for Model 4
Admission$SOP <- as.numeric(Admission$SOP)
Admission$LOR <- as.numeric(Admission$LOR)


set.seed(540)


train.index <- sample(row.names(Admission), floor(0.8*n))
test.index <- setdiff(row.names(Admission), train.index)
train.data <- Admission[train.index,]
test.data <- Admission[test.index,]

Admission.fit4 <- lm(ChanceOfAdmit ~ .-SOP -LOR, data = train.data)
summary(Admission.fit4)

##Calculating MSE and RMSE for Model 4
newdata = test.data[,1:7]
pred4 = predict(Admission.fit4, newdata, se.fit = TRUE)
ChanceOfAdmit.pred4 = pred4$fit
MSE4 = mean((test.data$ChanceOfAdmit - ChanceOfAdmit.pred4)^2) 
MSE4
RMSE4 = sqrt(MSE4)
RMSE4
Admission$UniversityRating <- as.numeric(Admission$UniversityRating)

##True VS Predicted values of Chance Of Admit
TrueVSPred <- cbind(test.data$ChanceOfAdmit,round(ChanceOfAdmit.pred2,3),round((test.data$ChanceOfAdmit - ChanceOfAdmit.pred2),3))
colnames(TrueVSPred) <- c("Actual_Val","Predicted_Val","Difference")
TrueVSPred

######Model Final: Applying Model 2 on complete data

full_data = rbind(train.data,test.data)
Admission.final <- lm(ChanceOfAdmit ~ . -UniversityRating -SOP, data = full_data)
summary(Admission.final)

