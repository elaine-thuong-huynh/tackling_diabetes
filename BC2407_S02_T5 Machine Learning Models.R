library(data.table)
library(randomForest)
library(caTools)  
library(caret)
library(rpart)
library(rpart.plot)
library(nnet)

setwd('E:/2. Studies/2. Y2/2. Sem 2/3. BC2407/Project/dataset')
diabetes.dt <- fread("BC2407_S02_T5 diabetes_final.csv")
#Chage the data type to correct ones
diabetes.dt[ , c("Diabetes","HighBP", "HighChol","CholCheck","Smoker","Stroke","HeartDiseaseorAttack","PhysActivity","Fruits","Veggies",
                 "HvyAlcoholConsump","AnyHealthcare","NoDocbcCost","DiffWalk","Sex")] = 
  lapply(diabetes.dt[, c("Diabetes","HighBP", "HighChol","CholCheck","Smoker","Stroke","HeartDiseaseorAttack","PhysActivity","Fruits","Veggies",
                         "HvyAlcoholConsump","AnyHealthcare","NoDocbcCost","DiffWalk","Sex")], factor)
#Change the data type of ordinal variables
diabetes.dt$GenHlth <- factor(diabetes.dt$GenHlth, levels = 1:5, ordered=T)
diabetes.dt$PhysHlth <- factor(diabetes.dt$PhysHlth, levels = 1:5, ordered=T)
diabetes.dt$MentHlth <- factor(diabetes.dt$MentHlth, levels = 1:5, ordered=T)
diabetes.dt$Age <- factor(diabetes.dt$Age, levels = 1:13, ordered=T)
diabetes.dt$Education <- factor(diabetes.dt$Education, levels = 1:6,ordered=T)
diabetes.dt$Income <- factor(diabetes.dt$Income, levels = 1:8, ordered=T)

# Train-test split ---------------------------------------------------------
set.seed(2022)
train <- sample.split(Y=diabetes.dt$Diabetes, SplitRatio = 0.7)
trainset <- subset(diabetes.dt, train==T)
testset <- subset(diabetes.dt, train==F)


###### CART ######

set.seed(2022)

cart1 <- rpart(Diabetes ~ ., data = trainset, method = 'class', control = rpart.control(minsplit = 20, cp = 0))

printcp(cart1)
plotcp(cart1)
print(cart1)

CVerror.cap <- cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xerror"] + cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp1 = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1)

set.seed(2022)

cart2 <- prune(cart1, cp = cp1)
print(cart2)
printcp(cart2)


rpart.plot(cart2, nn = T, main = "Optimal Tree in diabetes")
cart2$variable.importance
cart2.predict <- predict(cart2, newdata = testset, type = 'class')


## USING IMPORTANT VARIABLES TO MAKE ANOTHER CART MODEL

set.seed(2022)

cart3 <- rpart(Diabetes ~ GenHlth +  HighBP + Age +  BMI + Income +
                 HighChol + Education + DiffWalk + HeartDiseaseorAttack + PhysHlth, data = trainset, method = 'class', control = rpart.control(minsplit = 20, cp = 0))
printcp(cart3)
plotcp(cart3)
print(cart3)

CVerror2.cap <- cart3$cptable[which.min(cart3$cptable[,"xerror"]), "xerror"] + cart3$cptable[which.min(cart3$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart3$cptable[i,j] > CVerror2.cap) {
  i <- i + 1
}

which.min(cart3$cptable[,"xerror"])

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp2 = ifelse(i > 1, sqrt(cart3$cptable[i,1] * cart3$cptable[i-1,1]), 1)


cart4 <- prune(cart3, cp = cp2)
print(cart4)
printcp(cart4)

rpart.plot(cart4, nn = T, main = "Optimal Tree in diabetes")
cart4$variable.importance
cart4.predict <- predict(cart4, newdata = testset, type = 'class')


t <-table(cart4.predict, testset$Diabetes)

confusionMatrix(t,positive = "1")

mean(cart4.predict == testset$Diabetes)

##### FOR HPB VARIABLES #####

cart5 <- rpart(Diabetes ~ HighBP+BMI+Sex+Age, data = trainset, method = 'class', control = rpart.control(minsplit = 20, cp = 0))
printcp(cart5)
plotcp(cart5)
print(cart5)

CVerror3.cap <- cart5$cptable[which.min(cart5$cptable[,"xerror"]), "xerror"] + cart5$cptable[which.min(cart5$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree cart1.
i <- 1; j<- 4
while (cart5$cptable[i,j] > CVerror3.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp3 = ifelse(i > 1, sqrt(cart5$cptable[i,1] * cart5$cptable[i-1,1]), 1)

# for hpb variables
cart6 <- prune(cart5, cp = cp3)
print(cart6)
printcp(cart6)

rpart.plot(cart6, nn = T, main = "Optimal Tree in diabetes")
cart6$variable.importance
cart6.predict <- predict(cart6, newdata = testset, type = 'class')

t2 <-table(cart6.predict, testset$Diabetes)

confusionMatrix(t2,positive = "1")





##### NEURAL NETWORK #####

## LOG REG ##

logreg <- glm(Diabetes ~ ., family=binomial, data=diabetes.dt)
summary(logreg)


set.seed(2022)

## size 20 is used as it gave the highest accuracy

mynn1 <- nnet(Diabetes ~ HighBP+HighChol+CholCheck+BMI+Stroke+HeartDiseaseorAttack+
               HvyAlcoholConsump+GenHlth+DiffWalk+Sex+Age+Income, data=trainset,
             size=20)


mynn1.predict <- predict(mynn1, testset)
print(mynn1.predict)


pred1 <- ifelse(mynn1.predict > 0.5 ,1, 0)

cm1 <- table(pred1, testset$Diabetes)
print(cm1)
1-sum(diag(cm1))/sum(cm1)

confusionMatrix(cm1, positive = "1")


##### FOR HPB VARIABLES #####

set.seed(2022)

mynn2 <- nnet(Diabetes ~ HighBP+BMI+Sex+Age, data=trainset,
             size=20)
mynn2.predict <- predict(mynn2, testset)
print(mynn2.predict)


pred2 <- ifelse(mynn2.predict > 0.5 ,1, 0)

cm2 <- table(pred2, testset$Diabetes)
print(cm2)
1-sum(diag(cm2))/sum(cm2)

confusionMatrix(cm2, positive = "1")




##### RANDOM FOREST #####

#Preliminary model: Run the model with default settings
set.seed(2022)
RF_diabetes.pre <- randomForest(Diabetes ~ . , data=trainset, importance=F)
RF_diabetes.pre
#Comment: OBB error rate of default settings is 25.5%

###### Tuning hyperparameters ######

mtry <- c(2,3,4,5,6)
nodesize <- c(1,100,200,300,400,500)
ntree <- c(100,200,300,400,500)
OOB.tuning <- c()


for (i in mtry) {
  for (j in nodesize) {
    for (k in ntree) {
      set.seed(2022)
      RF_diabetes.tuning <- randomForest(Diabetes ~ . , data=trainset, importance=F, mtry=i, nodesize=j, ntree=k)
      OOB.err.tuning <- RF_diabetes.tuning$err.rate[k,1]
      OOB.tuning <- append(OOB.tuning, OOB.err.tuning)
    }
  }
}
Tuning.err <- data.table(mtry=rep(mtry,each=30), nodesize=rep(nodesize,each=5),
                         ntree=rep(ntree,times=6),OOB.err=OOB.tuning)
View(Tuning.err)
#Use mtry=4, nodesize=100 and ntree=400

###### Variable importance ######
set.seed(2022)
RF_diabetes.tuned <- randomForest(Diabetes ~ .,data=trainset,importance=T, mtry=4, nodesize=100, ntree=400)
varImpPlot(RF_diabetes.tuned)
#Drop variable with mean decrease in accuracy < 15
var.imp <- importance(RF_diabetes.tuned)
var.below15 <- which(var.imp[,3] < 15)

###### Final model ######
set.seed(2022)
RF_diabetes.final <- randomForest(Diabetes ~ .-Fruits-Smoker-Veggies-PhysActivity-MentHlth-NoDocbcCost-AnyHealthcare, 
                                  data=trainset, importance=F, mtry=4, nodesize=100, ntree=400)
RF_diabetes.final
RF_diabetes.yhat <- predict(RF_diabetes.final, newdata = testset)
confusionMatrix(RF_diabetes.yhat, testset$Diabetes,  mode = "everything", positive="1")

###### Model using HPB variables ######
set.seed(2022)
RF_diabetes.hpb <- randomForest(Diabetes ~ Sex+BMI+Age+HighBP, 
                                data=trainset, importance=F, mtry=4, nodesize=100, ntree=400)
RF_diabetes.yhat.hpb <- predict(RF_diabetes.hpb, newdata = testset)
confusionMatrix(RF_diabetes.yhat.hpb, testset$Diabetes, mode = "everything",positive="1")