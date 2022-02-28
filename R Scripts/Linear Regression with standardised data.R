# ---------------------- Linear Regression with Standardised Data  ----------------------------------


#----------Import and clean data-------------
# Set working directory as file that dataset is in and import using fread
setwd("/Users/jenny/Downloads/Sem09 Team06/R Scripts/")
data <- data.table::fread("standardised_final_asia_dataset.csv")

# Remove V1,Countries,Years since not being used in our linear regression model
data0 = subset(data, select = -c(V1,Countries,Years) )
# Ensure standardization of format for all NULL data
data0[data0 == "NULL"] = NA

# Omit all rows with NA values for clean dataset
data0<- na.omit(data0) # 98/150 rows remaining
str(data0) # views list of variables and their datatypes




#------------------VIF----------------------
data1 <- lapply(data0,as.numeric)
# Create linear model with lm() to test for variables with multicollinnearity
# Set y variable as GDP_Growth
m1 <- lm(GDP_Growth ~ ., data = data1)

# Uncomment below line if package "car" not installed
# install.packages("car")
library(car)
vif(m1) # Remove market_size 1102.111448

m1 <- update(m1, paste0(".~. - ",'Market_Size'))
vif(m1) #  Remove HDI 304.357766

m1 <- update(m1, paste0(".~. - ",'HDI'))
vif(m1) # Remove Gov_Indicator 203.787382

m1 <- update(m1, paste0(".~. - ",'Gov_Indicator'))
vif(m1) # Remove Life_Exp 88.091894

m1 <- update(m1, paste0(".~. - ",'Life_Exp'))
vif(m1) # Remove CPI 52.486238

m1 <- update(m1, paste0(".~. - ",'CPI'))
vif(m1) # Remove GNI 49.088552

m1 <- update(m1, paste0(".~. - ",'GNI'))
vif(m1) # Remove CO2_emissions 24.505653

m1 <- update(m1, paste0(".~. - ",'CO2_emissions'))
vif(m1) # Remove Mean_Years_of_Schooling 20.215001

m1 <- update(m1, paste0(".~. - ",'Mean_Years_of_Schooling'))
vif(m1) # Remove Pol_Stability 15.451475

m1 <- update(m1, paste0(".~. - ",'Pol_Stability'))
vif(m1) # Remove BoT 13.031969

m1 <- update(m1, paste0(".~. - ",'BoT'))
vif(m1) # Remove Internet_Penetration 10.040296

m1 <- update(m1, paste0(".~. - ",'Internet_Penetration'))
vif(m1) # Now highest vif is Domestic_credit_to_private_sector 7.139584 < 10

# Update by removing the above removed variables and assign back to data0
data0 = subset(data0, select = -c(Market_Size,HDI,Gov_Indicator,Life_Exp,CPI,GNI,CO2_emissions,Mean_Years_of_Schooling,Pol_Stability,BoT,Internet_Penetration) )  




#---------------statistical significance------------------
# Checking adjusted r^2 of full cleaned data
m1 <- lm(GDP_Growth ~ ., data = data0)
summary(m1) # Adjusted r2:0.6229

# Remove least significant variable, until right before adjusted r^2 decreases
m1 <- update(m1, paste0(".~. - ",'Renew_Energy')) #p-value: 0.6247
summary(m1) # Adjusted r2:0.6261

# m1 <- update(m1, paste0(".~. - ",'Domestic_credit_to_private_sector'))  #p-value: 0.0996
# summary(m1) # Adjusted r2:0.6188
# Backtracked to only remove Renew_Energy as removing Domestic_credit_to_private_sector resulted in decrease in adjusted r^2

# Update by removing the above removed variables and assign back to data0
data0 = subset(data0, select = -Renew_Energy )  
# View final variable list
str(data0)




#------------------train-test split----------------------
# Uncomment below line if package "car" not installed
# install.packages("caTools")
library(caTools)
set.seed(400)
# Generate a random number sequence that can be reproduced to verify results.
# set.seed(x) will ensure the same splot on all devices and hence same output as well if x remains the same

# 7/3 split
train <- sample.split(Y = data0$GDP_Growth, SplitRatio = 0.7)
trainset <- subset(data0, train == T)
testset <- subset(data0, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$GDP_Growth)
summary(testset$GDP_Growth)





#----------------Develop model on trainset------------------
trainset <- lapply(trainset,as.numeric)
# Creating model with lm()
m2 <- lm(GDP_Growth ~ Trade_Openness+Labour_Force+Domestic_credit_to_private_sector+inflation+Child_Mortality+Urban_pop+ind_val_add+unemployment_rate, data = trainset)
summary(m2) #seed400 r2: 0.5889
residuals(m2) 
# Residuals = Error = Actual mpg - Model Predicted mpg
# Residuals used to calculate RMSE




#-----------------Model Diagnostic Plots--------------------
# Checking suitability for linear regression
par(mfrow=c(2,2))
plot(m2)




#---------------RMSE for train and test set------------------
RMSE.m2.train <- sqrt(mean(residuals(m2)^2))  # RMSE on trainset based on m2 model
summary(abs(residuals(m2)))  # Check Min Abs Error and Max Abs Error

# Apply model from trainset to predict on testset
testset <- lapply(testset,as.numeric)
predict.m2.test <- predict(m2, newdata = testset)
testset.error <- testset$GDP_Growth - predict.m2.test
testset.error

# Testset Errors
RMSE.m2.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.m2.train #1.201735
RMSE.m2.test #1.065562



