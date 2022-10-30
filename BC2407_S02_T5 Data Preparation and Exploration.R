library(data.table)
library(rpart)
library(rpart.plot)
library(caTools)
library(ggplot2)
library(corrplot)
library(vcd)
library(dplyr)
library(arules)


# Set the working directory 
setwd('E:/2. Studies/2. Y2/2. Sem 2/3. BC2407/Project/dataset')

############################  Data Cleaning ############################
diabetes.raw.dt <- fread("BC2407_S02_T5 diabetes_binary_balance.csv")
#Check the dimension
dim(diabetes.raw.dt)

#Find if there are any missing values
sum(is.na(diabetes.raw.dt))

#Check if there are any abnormal values
lapply(diabetes.raw.dt, function(x) unique(x))
summary(diabetes.raw.dt$BMI)
summary(diabetes.raw.dt$PhysHlth)
summary(diabetes.raw.dt$MentHlth)




################ Create a copy of the original dataset. All alterations and analysis will be performed on this ################ 
diabetes.dt <- diabetes.raw.dt

#Check the columns' names 
colnames(diabetes.dt)
#Change the name of the target variable for ease of understanding
colnames(diabetes.dt)[colnames(diabetes.dt) == "Diabetes_binary"] <- "Diabetes"

#Check the data types of variables
str(diabetes.dt)
#Change the data type of binary categorical variables 
diabetes.dt[ , c("Diabetes","HighBP", "HighChol","CholCheck","Smoker","Stroke","HeartDiseaseorAttack","PhysActivity","Fruits","Veggies",
                     "HvyAlcoholConsump","AnyHealthcare","NoDocbcCost","DiffWalk","Sex")] = 
  lapply(diabetes.dt[, c("Diabetes","HighBP", "HighChol","CholCheck","Smoker","Stroke","HeartDiseaseorAttack","PhysActivity","Fruits","Veggies",
                             "HvyAlcoholConsump","AnyHealthcare","NoDocbcCost","DiffWalk","Sex")], factor)
#Change the data type of ordinal variables
diabetes.dt$GenHlth <- factor(diabetes.dt$GenHlth, levels = 1:5, ordered=T)
diabetes.dt$Age <- factor(diabetes.dt$Age, levels = 1:13, ordered=T)
diabetes.dt$Education <- factor(diabetes.dt$Education, levels = 1:6,ordered=T)
diabetes.dt$Income <- factor(diabetes.dt$Income, levels = 1:8, ordered=T)

#Bin the continuous variables into categorical
diabetes.dt$MentHlth <- cut(diabetes.dt$MentHlth, breaks = c(0,7,13,19,25,31), labels = c(1,2,3,4,5), ordered_result = TRUE, right = FALSE)
diabetes.dt$PhysHlth <- cut(diabetes.dt$PhysHlth, breaks = c(0,7,13,19,25,31), labels = c(1,2,3,4,5), ordered_result = TRUE, right = FALSE)

##Check the distribution of target variable  
round(prop.table(table(diabetes.dt$Diabetes)),2)

#Export the data table diabetes.dt to csv file
write.csv(diabetes.dt, "BC2407_S02_T5 diabetes_final.csv", row.names = FALSE)






############################  Data Exploratory & Visualization ############################


###### Correlation matrix between categorical variables ######
cramersV.df <- data.frame(Diabetes = diabetes.dt$Diabetes, 
                 HighBP = diabetes.dt$HighBP, 
                 HighChol = diabetes.dt$HighChol,
                 CholCheck = diabetes.dt$CholCheck,
                 Smoker = diabetes.dt$Smoker,
                 Stroke = diabetes.dt$Stroke,
                 HeartDiseaseorAttack = diabetes.dt$HeartDiseaseorAttack,
                 PhysActivity = diabetes.dt$PhysActivity,
                 Fruits = diabetes.dt$Fruits,
                 Veggies = diabetes.dt$Veggies,
                 HvyAlcoholConsump = diabetes.dt$HvyAlcoholConsump,
                 AnyHealthcare = diabetes.dt$AnyHealthcare,
                 NoDocbcCost = diabetes.dt$NoDocbcCost,
                 GenHlth = diabetes.dt$GenHlth,
                 PhysHlth = diabetes.dt$PhysHlth,
                 MentHlth = diabetes.dt$MentHlth,
                 DiffWalk = diabetes.dt$DiffWalk,
                 Sex = diabetes.dt$Sex,
                 Age = diabetes.dt$Age,
                 Education = diabetes.dt$Education,
                 Income = diabetes.dt$Income)

### Initialize empty matrix to store coefficients
empty_m <- matrix(ncol = length(cramersV.df),
                  nrow = length(cramersV.df),
                  dimnames = list(names(cramersV.df), 
                                  names(cramersV.df)))
### Function that accepts matrix for coefficients and data and returns a correlation matrix
calculate_cramer <- function(m, data) {
  for (r in seq(nrow(m))){
    for (c in seq(ncol(m))){
      m[[r, c]] <- assocstats(table(data[[r]], data[[c]]))$cramer
    }
  }
  return(m)
}

cor_matrix <- calculate_cramer(empty_m ,cramersV.df)
corrplot(cor_matrix, type = "upper", method = "color",
         diag= FALSE,
         na.label = "square",
         title = "Correlation Plot",
         addCoef.col = "black", number.cex=0.75,
         mar=c(0,0,1,0))



###### Data Visualization ######

#Create substitutes for columns that have more than 2 levels to have more understandable labels, which is better for visualization
diabetes.dt$GenHlth_cat <- factor(diabetes.dt$GenHlth, levels = 1:5, labels = c("Excellent", "Very good", "Good", "Fair", "Poor"), ordered=T)
diabetes.dt$MentHlth_cat <- factor(diabetes.dt$MentHlth, levels = 1:5, labels = c("Excellent", "Very good", "Good", "Fair", "Poor"), ordered=T)
diabetes.dt$PhysHlth_cat <- factor(diabetes.dt$PhysHlth, levels = 1:5, labels = c("Excellent", "Very good", "Good", "Fair", "Poor"), ordered=T)
diabetes.dt$Age_cat <- factor(diabetes.dt$Age, levels = 1:13, 
                     labels = c("18-24", "25-29", "30-34", "35-39", "40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79",">80"), ordered=T)
diabetes.dt$Education_cat <- factor(diabetes.dt$Education, levels = 1:6, 
                     labels = c("Kindergarten/No schooling", "Elementary", "In High School", "High School Graduate", "In College", "College Graduate"), ordered=T)
diabetes.dt$Income_cat <- factor(diabetes.dt$Income, levels = 1:8, 
                     labels = c("<$10,000", "$10,000-$15,000", "$15,000-$20,000", "$20,000-$25,000", "$25,000-$35,000", "$35,000-$50,000", "$50,000-$75,000",">$75,000"),
                        ordered=T)

#Distribution of continuous variables
hist(diabetes.dt$BMI, ylim=c(0,50000), breaks = seq(0, 120, by=10), xlab="BMI", main = "Distribution of BMI", labels=T, col ="light blue")

#Distribution of categorical variables
ggplot(diabetes.dt, aes(x=HighBP)) + geom_bar(stat="count", width=0.7, fill="steelblue") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size = 8) + theme_minimal() + 
  labs(x = "Have High Blood Pressure?", y = "Count") + theme(text=element_text(size=18))

ggplot(diabetes.dt, aes(x=HighChol)) + geom_bar(stat="count", width=0.7, fill="steelblue") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size = 8) + theme_minimal() +
  labs(x = "Have High Cholesterol?", y = "Count") + theme(text=element_text(size=18))

ggplot(diabetes.dt, aes(x=CholCheck)) + geom_bar(stat="count", width=0.7, fill="steelblue") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size = 8) + theme_minimal() + 
  labs(x = "Check cholesterol in the last 5 years?", y = "Count") + theme(text=element_text(size=18))

ggplot(diabetes.dt, aes(x=Smoker)) + geom_bar(stat="count", width = 0.85, fill="steelblue") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size = 8) + theme_minimal() + 
  labs(x = "Have had smoke at least 100 cigarettes?", y = "Count") + theme(text=element_text(size=18))

ggplot(diabetes.dt, aes(x=Stroke)) + geom_bar(stat="count", width = 0.85, fill="steelblue") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size = 8) + theme_minimal() + 
  labs(x = "Have had a stroke?", y = "Count") + theme(text=element_text(size=18))

ggplot(diabetes.dt, aes(x=HeartDiseaseorAttack)) + geom_bar(stat="count", width = 0.85, fill="steelblue") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size = 8) + theme_minimal() + 
  labs(x = "Have had heart disease or heart attack?", y = "Count") + theme(text=element_text(size=18))

ggplot(diabetes.dt, aes(x=PhysActivity)) + geom_bar(stat="count", width = 0.85, fill="steelblue") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size = 8) + theme_minimal() + 
  labs(x = "Have physical activity in the last 30 days?", y = "Count") + theme(text=element_text(size=18))

ggplot(diabetes.dt, aes(x=Fruits)) + geom_bar(stat="count", width = 0.85, fill="steelblue") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size = 8) + theme_minimal()
  labs(x = "Eat fruits in the last 30 days?", y = "Count") + theme(text=element_text(size=18))

ggplot(diabetes.dt, aes(x=Veggies)) + geom_bar(stat="count", width = 0.85, fill="steelblue") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size = 8) + theme_minimal() + 
  labs(x = "Eat vegetables in the last 30 days?", y = "Count") + theme(text=element_text(size=18))

ggplot(diabetes.dt, aes(x=HvyAlcoholConsump)) + geom_bar(stat="count", width = 0.85, fill="steelblue") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size = 8) + theme_minimal() + 
  labs(x = "Is a heavy drinker?", y = "Count") + theme(text=element_text(size=18))

ggplot(diabetes.dt, aes(x=AnyHealthcare)) + geom_bar(stat="count", width = 0.85, fill="steelblue") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size = 8) + theme_minimal() + 
  labs(x = "Have any healthcare coverage?", y = "Count") + theme(text=element_text(size=18))

ggplot(diabetes.dt, aes(x=NoDocbcCost)) + geom_bar(stat="count", width = 0.85, fill="steelblue") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size = 8) + theme_minimal() + 
  labs(x = "Cannot see doctor due to cost in the past year?", y = "Count") + theme(text=element_text(size=18))

ggplot(diabetes.dt, aes(x=GenHlth_cat)) + geom_bar(stat="count", width = 0.85, fill="steelblue") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size = 8) + theme_minimal() + 
  labs(x = "General Health Status", y = "Count") + theme(text=element_text(size=18))

ggplot(diabetes.dt, aes(x=PhysHlth_cat)) + geom_bar(stat="count", width = 0.85, fill="steelblue") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size = 8) + theme_minimal() + 
  labs(x = "Physcial Health Status in the last 30 days?", y = "Count") + theme(text=element_text(size=18))

ggplot(diabetes.dt, aes(x=MentHlth_cat)) + geom_bar(stat="count", width = 0.85, fill="steelblue") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size = 8) + theme_minimal() + 
  labs(x = "Mental Health Status in the last 30 days?", y = "Count") + theme(text=element_text(size=18))

ggplot(diabetes.dt, aes(x=Sex)) + geom_bar(stat="count", width = 0.85, fill="steelblue") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size = 8) + theme_minimal() + 
  labs(x = "Gender", y = "Count") + theme(text=element_text(size=18))

ggplot(diabetes.dt, aes(x=Age_cat)) + geom_bar(stat="count", width = 0.85, fill="steelblue") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size = 8) + theme_minimal() + 
  labs(x = "Age Group", y = "Count") + theme(text=element_text(size=18))

ggplot(diabetes.dt, aes(x=Education_cat)) + geom_bar(stat="count", width = 0.85, fill="steelblue") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size = 8) + theme_minimal() + 
  labs(x = "Education", y = "Count") + theme(text=element_text(size=18))

ggplot(diabetes.dt, aes(x=Income_cat)) + geom_bar(stat="count", width = 0.85, fill="steelblue") + 
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "black", size = 8) + theme_minimal() + 
  labs(x = "Income", y = "Count") + theme(text=element_text(size=18))


#3.Box plot between categorical Y and continuous X
boxplot(diabetes.dt$BMI ~ diabetes.dt$Diabetes, xlab = "Diabetes?", ylab = "BMI", 
        main = "Diabetes vs BMI")$stats

#4. Stacked bar chart between categorical Y and categorical Xs
ggplot(diabetes.dt %>% group_by(HighBP) %>% count(HighBP, Diabetes) %>%   #Group by prediction & HighBP, then count number in each group 
         mutate(pct=n/sum(n)),                # Calculate percent within category
       aes(HighBP, n, fill=Diabetes)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position = position_stack(vjust = 0.5), size = 8) + 
  theme(text=element_text(size=18))

ggplot(diabetes.dt %>% group_by(HighChol) %>% count(HighChol, Diabetes) %>%    
         mutate(pct=n/sum(n)),              
       aes(HighChol, n, fill=Diabetes)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position = position_stack(vjust = 0.5), size = 8) + 
  theme(text=element_text(size=18))

ggplot(diabetes.dt %>% group_by(CholCheck) %>% count(CholCheck, Diabetes) %>%    
         mutate(pct=n/sum(n)),              
       aes(CholCheck, n, fill=Diabetes)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position = position_stack(vjust = 0.5), size = 8) + 
  theme(text=element_text(size=18))

ggplot(diabetes.dt %>% group_by(Smoker) %>% count(Smoker, Diabetes) %>%    
         mutate(pct=n/sum(n)),              
       aes(Smoker, n, fill=Diabetes)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position = position_stack(vjust = 0.5), size = 8) + 
  theme(text=element_text(size=18))

ggplot(diabetes.dt %>% group_by(Stroke) %>% count(Stroke, Diabetes) %>%    
         mutate(pct=n/sum(n)),               
       aes(Stroke, n, fill=Diabetes)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position = position_stack(vjust = 0.5), size = 8) + 
  theme(text=element_text(size=18))

ggplot(diabetes.dt %>% group_by(HeartDiseaseorAttack) %>% count(HeartDiseaseorAttack, Diabetes) %>%    
         mutate(pct=n/sum(n)),               
       aes(HeartDiseaseorAttack, n, fill=Diabetes)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position = position_stack(vjust = 0.5), size = 8) + 
  theme(text=element_text(size=18))

ggplot(diabetes.dt %>% group_by(PhysActivity) %>% count(PhysActivity, Diabetes) %>%    
         mutate(pct=n/sum(n)),               
       aes(PhysActivity, n, fill=Diabetes)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position = position_stack(vjust = 0.5), size = 8) + 
  theme(text=element_text(size=18))

ggplot(diabetes.dt %>% group_by(Fruits) %>% count(Fruits, Diabetes) %>%    
         mutate(pct=n/sum(n)),               
       aes(Fruits, n, fill=Diabetes)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position = position_stack(vjust = 0.5), size = 8) + 
  theme(text=element_text(size=18))

ggplot(diabetes.dt %>% group_by(Veggies) %>% count(Veggies, Diabetes) %>%    
         mutate(pct=n/sum(n)),               
       aes(Veggies, n, fill=Diabetes)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position = position_stack(vjust = 0.5), size = 8) + 
  theme(text=element_text(size=18))

ggplot(diabetes.dt %>% group_by(HvyAlcoholConsump) %>% count(HvyAlcoholConsump, Diabetes) %>%    
         mutate(pct=n/sum(n)),               
       aes(HvyAlcoholConsump, n, fill=Diabetes)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position = position_stack(vjust = 0.5), size = 8) + 
  theme(text=element_text(size=18))

ggplot(diabetes.dt %>% group_by(AnyHealthcare) %>% count(AnyHealthcare, Diabetes) %>%    
         mutate(pct=n/sum(n)),               
       aes(AnyHealthcare, n, fill=Diabetes)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position = position_stack(vjust = 0.5), size = 8) + 
  theme(text=element_text(size=18))

ggplot(diabetes.dt %>% group_by(NoDocbcCost) %>% count(NoDocbcCost, Diabetes) %>%    
         mutate(pct=n/sum(n)),               
       aes(NoDocbcCost, n, fill=Diabetes)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position = position_stack(vjust = 0.5), size = 8) + 
  theme(text=element_text(size=18))

ggplot(diabetes.dt %>% group_by(GenHlth_cat) %>% count(GenHlth_cat, Diabetes) %>%    
         mutate(pct=n/sum(n)),              
       aes(GenHlth_cat, n, fill=Diabetes)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position = position_stack(vjust = 0.5), size = 8) + 
  theme(text=element_text(size=18))

ggplot(diabetes.dt %>% group_by(PhysHlth_cat) %>% count(PhysHlth_cat, Diabetes) %>%    
         mutate(pct=n/sum(n)),               
       aes(PhysHlth_cat, n, fill=Diabetes)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position = position_stack(vjust = 0.5), size = 8) + 
  theme(text=element_text(size=18))

ggplot(diabetes.dt %>% group_by(MenHlth_cat) %>% count(MentHlth_cat, Diabetes) %>%    
         mutate(pct=n/sum(n)),               
       aes(MentHlth_cat, n, fill=Diabetes)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position = position_stack(vjust = 0.5), size = 8) + 
  theme(text=element_text(size=18))

ggplot(diabetes.dt %>% group_by(DiffWalk) %>% count(DiffWalk, Diabetes) %>%    
         mutate(pct=n/sum(n)),               
       aes(DiffWalk, n, fill=Diabetes)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position = position_stack(vjust = 0.5), size = 8) + 
  theme(text=element_text(size=18))

ggplot(diabetes.dt %>% group_by(Sex) %>% count(Sex, Diabetes) %>%    
         mutate(pct=n/sum(n)),               
       aes(Sex, n, fill=Diabetes)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position = position_stack(vjust = 0.5), size = 8) + 
  theme(text=element_text(size=18))

ggplot(diabetes.dt %>% group_by(Age_cat) %>% count(Age_cat, Diabetes) %>%    
         mutate(pct=n/sum(n)),              
       aes(Age_cat, n, fill=Diabetes)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position = position_stack(vjust = 0.5), size = 8) + 
  theme(text=element_text(size=18))

ggplot(diabetes.dt %>% group_by(Education_cat) %>% count(Education_cat, Diabetes) %>%    
         mutate(pct=n/sum(n)),              
       aes(Education_cat, n, fill=Diabetes)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position = position_stack(vjust = 0.5), size = 8) + 
  theme(text=element_text(size=18))

ggplot(diabetes.dt %>% group_by(Income_cat) %>% count(Income_cat, Diabetes) %>%    
         mutate(pct=n/sum(n)),              
       aes(Income_cat, n, fill=Diabetes)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), position = position_stack(vjust = 0.5), size = 8) + 
  theme(text=element_text(size=18))

#Remove the substitute columns
diabetes.dt[,c(GenHlth_cat,MentHlth_cat,PhysHlth_cat,Age_cat,Education_cat,Income_cat) := NULL]
     



###### Association Rules ######

#Create a copy of dataset for association rules
diabetes.AR <- diabetes.dt[,c(0:12,18:22)]
View(diabetes.AR)


### Categorize BMI ###
diabetes.AR$BMIRange <- ifelse(diabetes.AR$BMI < 18.5, "1",
                      ifelse(diabetes.AR$BMI >=18.5 & diabetes.AR$BMI < 25, "2",
                             ifelse(diabetes.AR$BMI>=25 & diabetes.AR$BMI<30, "3", "4")
                      )
)
diabetes.AR$BMIRange <- as.factor(diabetes.AR$BMIRange)

View(diabetes.AR[,c('BMI','BMIRange')]) #Check
View(diabetes.AR)

### Data Conversion (num->factor) ###
str(diabetes.AR) #Check data type


#### Trend Analysis ####
## 1) Logistic Regression ##
set.seed = 2022
logreg <- glm(Diabetes ~ . - BMI,family=binomial, data=diabetes.AR)
summary(logreg)
# Identify *** significance for test in Association Rule


## 2) Association Rule ##
##### Factors leading to diabetes #####
View(diabetes.AR)
diabetes.AR1 <- subset(diabetes.AR, select = c(Diabetes,HvyAlcoholConsump,Age,Income,BMIRange))
View(diabetes.AR1)
trans1 <- as(diabetes.AR1,"transactions")
dim(trans1)
itemLabels(trans1)


rules1 <- apriori(data=diabetes.AR1,
                  parameter = list(minlen=2,
                                   maxlen = 2,
                                   support = 0.05,
                                   confidence = 0.02,
                                   target = 'rules'),
                  appearance = list(rhs = c("Diabetes=1")))
inspect(head(sort(rules1, by = "confidence"),10)) 
rules1 <- as(rules1, "data.frame")
View(rules1)


#Consequence of Diabetes
diabetes.AR2 <- subset(diabetes.AR, select = c(Diabetes,HeartDiseaseorAttack,DiffWalk,Stroke))
View(diabetes.AR2)
trans2 <- as(diabetes.AR2,"transactions")
dim(trans2)
itemLabels(trans2)
rules2 <- apriori(data=diabetes.AR2,
                  parameter = list(minlen=2,
                                   support = 0.05,
                                   confidence = 0.2,
                                   target = 'rules'),
                  appearance = list(lhs = c("Diabetes=1","Diabetes=0")))
inspect(head(rules2))
rules2 <- as(rules2, "data.frame")
View(rules2)
##Comment: Strong association between non-diabetic patients and significant lower risk of chronic illnesses


#Relationship between chronic illnesses
diabetes.AR3 <- subset(diabetes.AR, select = c(Diabetes,HighBP,HighChol))
View(diabetes.AR3)
trans3 <- as(diabetes.AR3,"transactions")
dim(trans3)
itemLabels(trans3)
rules3 <- apriori(data=diabetes.AR3,
                  parameter = list(minlen=2,
                                   support = 0.001,
                                   confidence = 0.2,
                                   target = 'rules'))
inspect(head(rules3))
rules3 <- as(rules3, "data.frame")
View(rules3)
