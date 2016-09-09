## Initial setup 
library(lattice)
library(ggplot2)
library(dplyr)
#install.packages("robustbase")
library(robustbase)
library(mice)
library(VIM)
library(mice)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)
library(ineq)
library(lattice)
#install.packages("VIM")
library(VIM)
#install.packages("DMwR")
library(DMwR)

setwd ("C:/Users/Visveswar/Desktop/GL/AV/Big Mart")
train = read.csv("train.csv",header = T,na.strings=c("","NA"))
test = read.csv("test.csv",header = T,na.strings=c("","NA"))
attach(train)
str(train)
str(test)
hist(train$Item_Outlet_Sales)
## The dependent value is highly right skewed. 

summary(train)
summary(test)
## There are totally 11 Independent variables in the training data. In which 2 varaibles are having missing values. 

########################## Univariate Analysis
TrainWoNA = na.omit(train)
summary(TrainWoNA)


boxplot(TrainWoNA$Item_Weight)
hist(TrainWoNA$Item_Weight)
## item weight does not any contain outlier.

boxplot(Item_Visibility)
hist(Item_Visibility)
## Highly right skewed and has outlier.

boxplot(Item_MRP)
hist(Item_MRP)
## Normally distibuted and does not have any outlier. 

boxplot(Item_Outlet_Sales)
hist(Item_Outlet_Sales)
## Our independent variables is highly skewed towards the right  and we need to take log trasformation and has outliers too. 

table(Item_Fat_Content)
## The values are not entered properly, Hence Correcting 

train$Item_Fat_Content <- as.character(train$Item_Fat_Content)
train$Item_Fat_Content[train$Item_Fat_Content == "low fat"] <- "Low Fat"
train$Item_Fat_Content[train$Item_Fat_Content == "LF"] <- "Low Fat"
train$Item_Fat_Content[train$Item_Fat_Content == "reg"] <- "Regular"
train$Item_Fat_Content <- as.factor(train$Item_Fat_Content)

test$Item_Fat_Content <- as.character(test$Item_Fat_Content)
test$Item_Fat_Content[test$Item_Fat_Content == "low fat"] <- "Low Fat"
test$Item_Fat_Content[test$Item_Fat_Content == "LF"] <- "Low Fat"
test$Item_Fat_Content[test$Item_Fat_Content == "reg"] <- "Regular"
test$Item_Fat_Content <- as.factor(test$Item_Fat_Content)

## After correcting  
plot(train$Item_Fat_Content)
##Low fat items are sold more when compared to Regular.

plot(train$Item_Type)
table(train$Item_Type)
par(las=2)
#### Vegitables and Snack Foods are sold more.
##From this, Our initial analysis would be that People are cautious on their diet, We have need to confirm on this.  

plot(Outlet_Identifier)
### Number of item sold by the store, OUT010 and OUT019 sold less teams.

hist(Outlet_Establishment_Year)
## Lets convert to the catogorical value so that we can find exactly the outlet  established years.

train$Outlet_Establishment_Year = as.factor(train$Outlet_Establishment_Year)
test$Outlet_Establishment_Year  = as.factor(test$Outlet_Establishment_Year)
plot(train$Outlet_Establishment_Year)
### The Outlet that established in the year 1985 has more sales than others,
### Mean while, the outlet that had established in the year  1998 has less sales, 
### that can be due to some reason and we will find out soon. 

plot(Outlet_Size,na.rm = TRUE)
### The Outlet that has medium size has more sales than the others.

table(Outlet_Location_Type)
plot(Outlet_Location_Type)
### Tier 3 does more sales than the other location type. 

table(Outlet_Type)
plot(Outlet_Type)
### Supermarket Type1 has the highest sales record than the other.

############## Bi-Variate Analysis

#### Finding Correlation among the continous variable

summary(train)
Contin = c(2,4,6,12)
head(train[,Contin])

cor(train[,Contin],use = "complete.obs")
symnum(cor(train[,Contin],use = "complete.obs"))
## there are significant Correlation between variables, 
## Only  Item_MRP and Item_Outlet_Sales are correlated at 0.6 significant level which is very low significant.

table(train$Item_Fat_Content)/nrow(train)
addmargins(prop.table(table(train$Item_Type,train$Item_Fat_Content),1))
## from the above we can conclude that the Low fat items are being sold more when compared to others.

table(train$Outlet_Identifier,Outlet_Establishment_Year)
### Yah!! We found out the reason for more sales in the 1985, 2 outlets were established in the year 1985. 

plot(train$Outlet_Identifier,train$Item_Outlet_Sales)
## Outlet 27 shows high sales where as the outlet 10 and 19 shows low

plot(train$Outlet_Location_Type,train$Item_Outlet_Sales)
## Tier 3 shows high sales and wide range.
plot(train$Outlet_Size,train$Item_Outlet_Sales)
## medium Size was able to bring more sales
plot(train$Outlet_Type,train$Item_Outlet_Sales)
## Super market 3 shows more sales rate than the others.
plot(train$Item_Type,train$Item_Outlet_Sales)
plot(train$Item_Weight,train$Item_Outlet_Sales)
## All items and weight are sold of same level

##### Missing Value Treatment

table(train$Outlet_Identifier,train$Outlet_Size)
### Outlet OUT010,OUT017,OUT045 does not have any size 

table(train$Outlet_Identifier,train$Outlet_Establishment_Year)
table(train$Outlet_Identifier,train$Outlet_Location_Type)
table(train$Outlet_Identifier,train$Outlet_Type)
### Outlet OUT010 is from Tier 3, was established  in 1998 and it is Grocery Store
### Outlet OUT017 is from Tier 2, was established  in 2007 and it is Supermarket Type1
### Outlet OUT045 is from Tier 2, was established  in 2002 and it is Supermarket Type1

table(train$Outlet_Location_Type,train$Outlet_Size)
## From the above we can see that the Tier 3 deals with only High and medium and not small, 
## this might be because, either Tier 3 does not deal with small or the data for small might be missing.
## But Tier 2 either should have small size or data should be missing. Supermarket Type1 comes with all size. 

table(train$Outlet_Type,train$Outlet_Size)
boxplot(Item_Outlet_Sales~Outlet_Type)
boxplot(Item_Outlet_Sales~Outlet_Size) 
tapply(train$Item_Outlet_Sales,train$Outlet_Identifier,quantile)
tapply(train$Item_Outlet_Sales,train$Outlet_Size,quantile)
## Fom the above we can see that the Grocery store size is small and data was missing for tier 3. 
## So the Outlet010 size was small their mean sales is very low and its range matches with mean sales of Grocery store size

train$Outlet_Size[(which(train$Outlet_Identifier == 'OUT010'))] = "Small"
head(train[Outlet_Identifier == 'OUT010',])
table(train$Outlet_Location_Type,train$Outlet_Size)
## We have successffully imputed missing value for Outlet_Size

trainMV = train[,-12]
str(trainMV)
str(test)

full = bind_rows(trainMV,test)
summary(full)


mice_plot =  aggr(full, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(full), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

## Filling Missing data using prediction model
tempData <- mice(full,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)
head(tempData$imp$Item_Weight)
c = which(train$Item_Identifier == "FDW12")
train[c,]
## Cross checking for the best imputation values. but still the item weight does not have much impact on the sales.
densityplot(tempData)

completeData = complete(tempData,3)
summary(completeData)

## comparing before and after imputation for catogorical variables
table(train$Outlet_Size)/nrow(train)
table(completeData$Outlet_Size)/nrow(completeData)

trainprefinal = completeData[1:8523,]
testfinal = completeData[8524:14204,]
trainprefinal$Item_Outlet_Sales = train$Item_Outlet_Sales
summary(trainprefinal)

library(caTools)
set.seed(1000)
split = sample.split(trainprefinal$Item_Outlet_Sales, SplitRatio = 0.90)
trainfinal = subset(trainprefinal,split == TRUE)
trainHO = subset(trainprefinal,split == FALSE)

nrow(trainfinal) 
nrow(trainHO)

testLm1 = testfinal
testLm2 = testfinal
testCm1 = testfinal
testRm1 = testfinal


###### Prediction Modeling
### Now lets start predicting using all the values. 

levels(trainfinal$Outlet_Size)
LinearModel1 = lm(log(trainfinal$Item_Outlet_Sales)~ Item_Weight  + Item_Fat_Content + Item_Visibility + Item_Type +
                    Item_MRP + Outlet_Size + Outlet_Location_Type +  Outlet_Type,data = trainfinal)
summary(LinearModel1)

SSE = sum(LinearModel1$residuals)
rmse = sqrt(SSE/nrow(trainfinal))
rmse

######### Predicting on training data
predictm1 = exp(predict(LinearModel1))
SSE = sum((predictm1 - trainfinal$Item_Outlet_Sales)^2)
SSE
SST = sum((trainfinal$Item_Outlet_Sales  - mean(trainfinal$Item_Outlet_Sales))^2)
SST
1-SSE/SST
rmse1 = sqrt(SSE/nrow(trainfinal))
rmse1


### Predicting on holdout sample
predictmodel1 = exp(predict(LinearModel1,newdata = trainHO ))
summary(predictmodel1)
SSE = sum((predictmodel1 - trainHO$Item_Outlet_Sales)^2)
SSE
SST = sum((trainHO$Item_Outlet_Sales - mean(trainfinal$Item_Outlet_Sales))^2)
SST
R2model1 = 1-SSE/SST
R2model1
sqrt(SSE/nrow(trainHO))
sqrt(mean((predictmodel1-trainHO$Item_Outlet_Sales)^2))

### Lets predict for the test set
testLm1$Item_Outlet_Sales = exp(predict(LinearModel1,newdata = testfinal ))


###### builing Modeling 2 using significant values. 
### Now lets start predicting using all the  significant values. 

LinearModel2 = lm(log(trainfinal$Item_Outlet_Sales)~ Item_Type +
                    Item_MRP + Outlet_Type,data = trainfinal)
summary(LinearModel2)

######### Predicting on training data
predictm2 = exp(predict(LinearModel2))
SSE = sum((predictm2 - trainfinal$Item_Outlet_Sales)^2)
SSE
SST = sum((trainfinal$Item_Outlet_Sales  - mean(trainfinal$Item_Outlet_Sales))^2)
SST
1-SSE/SST
rmse2 = sqrt(SSE/nrow(trainfinal))
rmse2

### Predicting model 2 on holdout sample
predictmodel2 = exp(predict(LinearModel2,newdata = trainHO ))
summary(predictmodel2)
SSE = sum((predictmodel2 - trainHO$Item_Outlet_Sales)^2)
SSE
SST = sum((trainHO$Item_Outlet_Sales - mean(trainfinal$Item_Outlet_Sales))^2)
SST
R2model2 = 1-SSE/SST
R2model2
sqrt(SSE/nrow(trainHO))
sqrt(mean((predictmodel2-trainHO$Item_Outlet_Sales)^2))

### Lets predict for the test set
testLm2$Item_Outlet_Sales = exp(predict(LinearModel2,newdata = testfinal ))

######## Predicting using CART Model with log tranformation

library(rpart)
library(rpart.plot)
r.control = rpart.control(minsplit=100,minbucket=10,cp=0,xval=10)
CARTModel1 = rpart(trainfinal$Item_Outlet_Sales ~ Item_Weight  + Item_Fat_Content + Item_Visibility + Item_Type +
                     Item_MRP + Outlet_Size + Outlet_Location_Type +  Outlet_Type,data = trainfinal,control = r.control)
prp(CARTModel1)
CARTModel1
printcp(CARTModel1)
plotcp(CARTModel1)

### Pruning the CART TREE
ptree = prune(CARTModel1,cp = 0.0015,"cp" )
printcp(ptree)
prp(ptree)

######### Predicting on training data
predictm3 = predict(ptree,trainfinal) 

SSE = sum((predictm3 - trainfinal$Item_Outlet_Sales)^2)
SSE
SST = sum((trainfinal$Item_Outlet_Sales  - mean(trainfinal$Item_Outlet_Sales))^2)
SST
1-SSE/SST
rmse3 = sqrt(SSE/nrow(trainfinal))
rmse3

######## Predicting of holdout sample
predictmodel3 = predict(ptree,newdata = trainHO )
summary(predictmodel3)
SSE = sum((predictmodel3 - trainHO$Item_Outlet_Sales)^2)
SSE
SST = sum((trainHO$Item_Outlet_Sales - mean(trainfinal$Item_Outlet_Sales))^2)
SST
R2model3 = 1-SSE/SST
R2model3
sqrt(SSE/nrow(trainHO))
sqrt(mean((predictmodel3-trainHO$Item_Outlet_Sales)^2))

### Lets predict for the test set
testCm1$Item_Outlet_Sales = predict(ptree,newdata = testfinal )


###### we will try with the Random forest
library(randomForest)
RFmodel1 = randomForest(trainfinal$Item_Outlet_Sales ~ Item_Weight  + Item_Fat_Content + Item_Visibility + Item_Type +
                          Item_MRP + Outlet_Size + Outlet_Location_Type +  Outlet_Type,data = trainfinal, ntree=500, mtry = 3, nodesize = 60,
                        importance=TRUE)

print(RFmodel1)
plot(RFmodel1)
RFmodel1$err.rate
predictm5  = predict(RFmodel1,trainfinal)

SSE = sum((predictm5 - trainfinal$Item_Outlet_Sales)^2)
SSE
SST = sum((trainfinal$Item_Outlet_Sales  - mean(trainfinal$Item_Outlet_Sales))^2)
SST
1-SSE/SST
rmse5 = sqrt(SSE/nrow(trainfinal))
rmse5
summary(trainfinal)

### Predicting on holdout sample
predictmodel5 = predict(RFmodel1,newdata = trainHO )
summary(predictmodel5)
SSE = sum((predictmodel5 - trainHO$Item_Outlet_Sales)^2)
SSE
SST = sum((trainHO$Item_Outlet_Sales - mean(trainfinal$Item_Outlet_Sales))^2)
SST
R2model5 = 1-SSE/SST
R2model5
sqrt(SSE/nrow(trainHO))

### RMSE Score for test file - model performance comparison 
## Linear model 1 - 1233.721757
## Linear model 2 - 1233.44006
## CART 1 - 1161.74957
## Random Forest 1 - 1159.327029


### Lets predict for the test set
testRm1$Item_Outlet_Sales = predict(RFmodel1,newdata = testfinal )


solution_frame1 <- data.frame(Item_Identifier = testfinal$Item_Identifier,Outlet_Identifier = testfinal$Outlet_Identifier,Item_Outlet_Sales = testLm1$Item_Outlet_Sales)
solution_frame2 <- data.frame(Item_Identifier = testfinal$Item_Identifier,Outlet_Identifier = testfinal$Outlet_Identifier,Item_Outlet_Sales = testLm2$Item_Outlet_Sales)
solution_frame3 <- data.frame(Item_Identifier = testfinal$Item_Identifier,Outlet_Identifier = testfinal$Outlet_Identifier,Item_Outlet_Sales = testCm1$Item_Outlet_Sales)

solution_frame5 <- data.frame(Item_Identifier = testfinal$Item_Identifier,Outlet_Identifier = testfinal$Outlet_Identifier,Item_Outlet_Sales = testRm1$Item_Outlet_Sales)
#solution_frame6 <- data.frame(Item_Identifier = testfinal$Item_Identifier,Outlet_Identifier = testfinal$Outlet_Identifier,Item_Outlet_Sales = testRt1$Item_Outlet_Sales)
#solution_frame7 <- data.frame(Item_Identifier = testfinal$Item_Identifier,Outlet_Identifier = testfinal$Outlet_Identifier,Item_Outlet_Sales = testRm2$Item_Outlet_Sales)
#solution_frame8 <- data.frame(Item_Identifier = testfinal$Item_Identifier,Outlet_Identifier = testfinal$Outlet_Identifier,Item_Outlet_Sales = testRt2$Item_Outlet_Sales)
#solution_frame4 <- data.frame(Item_Identifier = testfinal$Item_Identifier,Outlet_Identifier = testfinal$Outlet_Identifier,Item_Outlet_Sales = testCm2$Item_Outlet_Sales)

####  Submitting my solution
write.csv(solution_frame1,file = "C:/Users/Visveswar/Desktop/GL/AV/Big Mart/final_Solution1.csv")
write.csv(solution_frame2,file = "C:/Users/Visveswar/Desktop/GL/AV/Big Mart/final_Solution2.csv")
write.csv(solution_frame3,file = "C:/Users/Visveswar/Desktop/GL/AV/Big Mart/final_Solution3.csv")
write.csv(solution_frame5,file = "C:/Users/Visveswar/Desktop/GL/AV/Big Mart/final_Solution5.csv")
#write.csv(solution_frame6,file = "C:/Users/Visveswar/Desktop/GL/AV/Big Mart/final_Solution6.csv")
#write.csv(solution_frame7,file = "C:/Users/Visveswar/Desktop/GL/AV/Big Mart/final_Solution7.csv")
#write.csv(solution_frame8,file = "C:/Users/Visveswar/Desktop/GL/AV/Big Mart/final_Solution8.csv")
#write.csv(solution_frame4,file = "C:/Users/Visveswar/Desktop/GL/AV/Big Mart/final_Solution4.csv")


## Among all, Random Forest technique have performed better and i was able to predict the test set at the RMSE of 1159.327029

