# ---------------------- CART Model with Original Cleaned Dataset ----------------------------------

#ORIGINAL DATASET
library(data.table)
library(rpart)
library(rpart.plot) 
library(caTools)
library('Metrics')

setwd('C:\\Users\\YI XUAN\\Desktop\\NTU\\AY2021-2022 SEM 1\\BC2406 Analytics I - Visual & Predictive Techniques\\AY21 Team Assignment and Project\\Sem09 Team06\\Notebooks datasets dictonary\\Noteb datasets dict')
asia_dataset_cleaned <- fread('final_asia_dataset_cleaned.csv')

summary(asia_dataset_cleaned)

# Train-Test split-----------------------------
set.seed(2014)

trainasia <- sample.split(Y = asia_dataset_cleaned$GDP_Growth, SplitRatio = 0.70)

trainsetasia <- subset(asia_dataset_cleaned, trainasia == T)

testsetasia <- subset(asia_dataset_cleaned, trainasia == F)

cartmaxasia <- rpart(GDP_Growth ~ .-Countries - Year, data = trainsetasia, method = 'anova',
                     control = rpart.control(minsplit = 2, cp = 0))

plotcp(cartmaxasia)

#Grow to max tree
rpart.plot(cartmaxasia, nn= T, main = "Maximal Tree")


# Automatic method to find optimal CP 
CVerror.cap <- cartmaxasia$cptable[which.min(cartmaxasia$cptable[,"xerror"]), "xerror"] + cartmaxasia$cptable[which.min(cartmaxasia$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree m1.
i <- 1; j<- 4
while (cartmaxasia$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cartmaxasia$cptable[i,1] * cartmaxasia$cptable[i-1,1]), 1)

# Prune the tree to optimal
optcartasia <- prune(cartmaxasia, cp = cp.opt)
rpart.plot(optcartasia, nn= T, main = "Optimal Tree")


# Variable Importance
optcartasia$variable.importance
scaled_variable_importance = round((100*optcartasia$variable.importance/sum(optcartasia$variable.importance)))
scaled_variable_importance


# Checking Accuracy 
cart.predicttest <- predict(optcartasia, newdata = testsetasia)
cart.predicttrain <- predict(optcartasia, newdata = trainsetasia)

rmse(cart.predicttrain, trainsetasia$GDP_Growth) #1.130805
rmse(cart.predicttest, testsetasia$GDP_Growth) #1.838918