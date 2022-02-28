# ---------------------- Linear Regression with EIU Baseline Model  ----------------------------------

#----------Import and clean data-------------
# Import file using fread
data5 <- data.table::fread("standardised_final_asia_dataset.csv")

# Keep only:
# GDP_Growth, Market_Size, Life_Exp, Mean_Years_of_Schooling,Internet_Penetration,Gov_Indicator,Trade_Openness,Labour_Force,Domestic_credit_to_private_sector
# as they are similar to what EIU uses
data6 = subset(data5, select = c(GDP_Growth, Market_Size, Life_Exp, Mean_Years_of_Schooling,Internet_Penetration,Gov_Indicator,Trade_Openness,Labour_Force,Domestic_credit_to_private_sector) )
data6[data6 == "NULL"] = NA
data6<- na.omit(data6)
str(data6)




#------------------VIF----------------------
data7 <- lapply(data6,as.numeric)
m5 <- lm(GDP_Growth ~ ., data = data7)

library(car)
vif(m5) #rem Life_Exp 22.959509 

m5 <- update(m5, paste0(".~. - ",'Life_Exp'))
vif(m5) #remove Gov_Indicator 14.337795 

m5 <- update(m5, paste0(".~. - ",'Gov_Indicator'))
vif(m5) #now highest Internet_Penetration 9.272307

# Update by removing the above removed variables and assign back to data0
data6 = subset(data6, select = -c(Life_Exp,Gov_Indicator) )  




#---------------statistical significance------------------
#Checking adjusted r^2 of EIU surrogate cleaned data
m5 <- lm(GDP_Growth ~ ., data = data6)
summary(m5) #r2:0.3411

m5 <- update(m5, paste0(".~. - ",'Internet_Penetration')) #0.9078
summary(m5) #r2:0.3464

# m5 <- update(m5, paste0(".~. - ",'Domestic_credit_to_private_sector')) #0.0603
# summary(m5) #r2:0.3327
# Backtracked to only remove Internet_Penetration as removing Domestic_credit_to_private_sector resulted in decrease in adjusted r^2

# Update by removing the above removed variables and assign back to data0
data6 = subset(data6, select = -Internet_Penetration)  





#------------------train-test split----------------------
library(caTools)
set.seed(400)

# 7/3 split
train <- sample.split(Y = data6$GDP_Growth, SplitRatio = 0.7)
trainset <- subset(data6, train == T)
testset <- subset(data6, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$GDP_Growth)
summary(testset$GDP_Growth)





#----------------Develop model on trainset------------------
trainset <- lapply(trainset,as.numeric)
m6 <- lm(GDP_Growth ~ Market_Size+Mean_Years_of_Schooling+Trade_Openness+Labour_Force+Domestic_credit_to_private_sector, data = trainset)
summary(m6) #r^2: 0.3367 
residuals(m6) 
# Residuals = Error = Actual mpg - Model Predicted mpg





#-----------------Model Diagnostic Plots--------------------
par(mfrow=c(2,2))
plot(m6)





#---------------RMSE for train and test set------------------
RMSE.m6.train <- sqrt(mean(residuals(m6)^2))  # RMSE on trainset based on m2 model.
summary(abs(residuals(m6)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
testset <- lapply(testset,as.numeric)
predict.m6.test <- predict(m6, newdata = testset)
testset.error <- testset$GDP_Growth - predict.m6.test
testset.error

# Testset Errors
RMSE.m6.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.m6.train #seed400:1.830851
RMSE.m6.test #seed400:1.991584
