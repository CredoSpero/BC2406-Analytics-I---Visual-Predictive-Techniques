# ---------------------- CART with NAs replaced with mean ----------------------------------


# DATATSET WITH MEAN VALUES
library(data.table)
library(rpart)
library(rpart.plot) 
library(caTools)
library('Metrics')

setwd('C:\\Users\\YI XUAN\\Desktop\\NTU\\AY2021-2022 SEM 1\\BC2406 Analytics I - Visual & Predictive Techniques\\AY21 Team Assignment and Project\\Sem09 Team06\\Notebooks datasets dictonary\\Noteb datasets dict')
asia_dataset_cleaned_mean <- fread('final_asia_dataset_cleaned_mean.csv')

summary(asia_dataset_cleaned_mean)


# Train-Test split-----------------------------
set.seed(2014)

trainasiamean <- sample.split(Y = asia_dataset_cleaned_mean$GDP_Growth, SplitRatio = 0.70)

trainsetasiamean <- subset(asia_dataset_cleaned_mean, trainasiamean == T)

testsetasiamean <- subset(asia_dataset_cleaned_mean, trainasiamean == F)

cartmaxasiamean <- rpart(GDP_Growth ~ .-Countries - Year, data = trainsetasiamean, method = 'anova',
                         control = rpart.control(minsplit = 2, cp = 0))

plotcp(cartmaxasiamean)

#Grow to max tree
rpart.plot(cartmaxasiamean, nn= T, main = "Maximal Tree")


# Automatic method to find optimal CP 
CVerror.capmean <- cartmaxasiamean$cptable[which.min(cartmaxasiamean$cptable[,"xerror"]), "xerror"] + cartmaxasiamean$cptable[which.min(cartmaxasiamean$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree m1.
i <- 1; j<- 4
while (cartmaxasiamean$cptable[i,j] > CVerror.capmean) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.optmean = ifelse(i > 1, sqrt(cartmaxasiamean$cptable[i,1] * cartmaxasiamean$cptable[i-1,1]), 1)


# Prune the tree to optimal
optcartasiamean <- prune(cartmaxasiamean, cp = cp.optmean)
rpart.plot(optcartasiamean, nn= T, main = "Optimal Tree")


# Variable Importance
optcartasiamean$variable.importance
scaled_variable_importance = round((100*optcartasiamean$variable.importance/sum(optcartasiamean$variable.importance)))
scaled_variable_importance


# Checking Accuracy 
cart.predicttestmean <- predict(optcartasiamean, newdata = testsetasiamean)
cart.predicttrainmean <- predict(optcartasiamean, newdata = trainsetasiamean)


rmse(cart.predicttrainmean, trainsetasiamean$GDP_Growth) #1.399542
rmse(cart.predicttestmean, testsetasiamean$GDP_Growth) #2.156966