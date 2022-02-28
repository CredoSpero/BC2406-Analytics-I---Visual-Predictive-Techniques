# ---------------------- CART with variables <= 10 NA values ----------------------------------

#CLEANING TO VARIABLES WITH <= 10 NA VALUES

library(data.table)
library(rpart)
library(rpart.plot) 
library(caTools)
library('Metrics')

setwd('C:\\Users\\YI XUAN\\Desktop\\NTU\\AY2021-2022 SEM 1\\BC2406 Analytics I - Visual & Predictive Techniques\\AY21 Team Assignment and Project\\Sem09 Team06\\Notebooks datasets dictonary\\Noteb datasets dict')
asia_dataset_cleaned10 <- fread('final_asia_dataset_cleaned.csv')

summary(asia_dataset_cleaned10)

#data exploration for NA values---------------

asia_dataset_cleaned10[rowSums(is.na(asia_dataset_cleaned10)) > 0,]
asia_dataset_cleaned10[is.na(asia_dataset_cleaned10$CPI)]
asia_dataset_cleaned10 <- subset(asia_dataset_cleaned10, select = -c(CPI))
summary(asia_dataset_cleaned10)
#drop CPI col as it has 30 NAs


asia_dataset_cleaned10[is.na(asia_dataset_cleaned10$Internet_Penetration)]
asia_dataset_cleaned10 <- asia_dataset_cleaned10[!is.na(asia_dataset_cleaned10$Internet_Penetration)]
summary(asia_dataset_cleaned10)
#drop rows with NA in Internet_Penetration as it has 17 NAs


# Train-Test split 
set.seed(2014)

trainasia10 <- sample.split(Y = asia_dataset_cleaned10$GDP_Growth, SplitRatio = 0.7)

trainsetasia10 <- subset(asia_dataset_cleaned10, trainasia10 == T)
testsetasia10 <- subset(asia_dataset_cleaned10, trainasia10 == F)

cartmaxasia10 <- rpart(GDP_Growth ~ .-Countries - Year, data = trainsetasia10, method = 'anova',
                       control = rpart.control(minsplit = 2, cp = 0))
plotcp(cartmaxasia10)

#Grow to max tree
rpart.plot(cartmaxasia10, nn= T, main = "Maximal Tree")


# Automatic method to find optimal CP 
CVerror.cap10 <- cartmaxasia10$cptable[which.min(cartmaxasia10$cptable[,"xerror"]), "xerror"] + cartmaxasia10$cptable[which.min(cartmaxasia10$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree m1.
i <- 1; j<- 4
while (cartmaxasia10$cptable[i,j] > CVerror.cap10) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt10 = ifelse(i > 1, sqrt(cartmaxasia10$cptable[i,1] * cartmaxasia10$cptable[i-1,1]), 1)

# Prune to Optimal
optcartasia10 <- prune(cartmaxasia10, cp = cp.opt10)
rpart.plot(optcartasia10, nn= T, main = "Optimal Tree")

# Variable Importance
optcartasia10$variable.importance
scaled_variable_importance = round((100*optcartasia10$variable.importance/sum(optcartasia10$variable.importance)))
scaled_variable_importance


# Checking Accuracy 
cart.predicttest10 <- predict(optcartasia10, newdata = testsetasia10)
cart.predicttrain10 <- predict(optcartasia10, newdata=trainsetasia10)

rmse(cart.predicttrain10, trainsetasia10$GDP_Growth) #1.068747
rmse(cart.predicttest10, testsetasia10$GDP_Growth)#2.588977 