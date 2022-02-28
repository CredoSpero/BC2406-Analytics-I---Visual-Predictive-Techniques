# ---------------------- CART with EIU baseline model ----------------------------------

#ORIGINAL DATASET
library(data.table)
library(rpart)
library(rpart.plot) 
library(caTools)
library('Metrics')

setwd('C:\\Users\\YI XUAN\\Desktop\\NTU\\AY2021-2022 SEM 1\\BC2406 Analytics I - Visual & Predictive Techniques\\AY21 Team Assignment and Project\\Sem09 Team06\\Notebooks datasets dictonary\\Noteb datasets dict')
asia_dataset_cleaned_EIU <- fread('final_asia_dataset_cleaned.csv')
asia_dataset_cleaned_EIU <- asia_dataset_cleaned_EIU[, c(1:11)]

summary(asia_dataset_cleaned_EIU)

# Train-Test split-----------------------------
set.seed(2014)

trainasiaEIU <- sample.split(Y = asia_dataset_cleaned_EIU$GDP_Growth, SplitRatio = 0.70)

trainsetasiaEIU <- subset(asia_dataset_cleaned_EIU, trainasiaEIU == T)

testsetasiaEIU <- subset(asia_dataset_cleaned_EIU, trainasiaEIU == F)

cartmaxasiaEIU <- rpart(GDP_Growth ~ .-Countries - Year, data = trainsetasiaEIU, method = 'anova',
                        control = rpart.control(minsplit = 2, cp = 0))

plotcp(cartmaxasiaEIU)

#Grow to max tree
rpart.plot(cartmaxasiaEIU, nn= T, main = "Maximal Tree")

# Automatic method to find optimal CP 
CVerror.capEIU <- cartmaxasiaEIU$cptable[which.min(cartmaxasiaEIU$cptable[,"xerror"]), "xerror"] + cartmaxasiaEIU$cptable[which.min(cartmaxasiaEIU$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree m1.
i <- 1; j<- 4
while (cartmaxasiaEIU$cptable[i,j] > CVerror.capEIU) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.optEIU = ifelse(i > 1, sqrt(cartmaxasiaEIU$cptable[i,1] * cartmaxasiaEIU$cptable[i-1,1]), 1)
cp.optEIU

# Prune the tree to optimal
optcartasiaEIU <- prune(cartmaxasiaEIU, cp = cp.optEIU)
rpart.plot(optcartasiaEIU, nn= T, main = "Optimal Tree")

# Variable Importance
optcartasiaEIU$variable.importance
scaled_variable_importance = round((100*optcartasiaEIU$variable.importance/sum(optcartasiaEIU$variable.importance)))
scaled_variable_importance

# Checking Accuracy 
cart.predicttestEIU <- predict(optcartasiaEIU, newdata = testsetasiaEIU)
cart.predicttrainEIU <- predict(optcartasiaEIU, newdata = trainsetasiaEIU)


rmse(cart.predicttrainEIU, trainsetasiaEIU$GDP_Growth) #0.8067812
rmse(cart.predicttestEIU, testsetasiaEIU$GDP_Growth) #2.236214


#-------------------------------------------------------------------------------------------------------------

#Checking Accuracy of EIU estimates
eiu_est <- fread("eiu_est.csv")
summary(eiu_est) 
rmse(eiu_est$GDP_Growth, eiu_est$EIU_Estimates) #0.7381297