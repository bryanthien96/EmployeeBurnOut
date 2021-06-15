# Importing libraries
library(caret)
library(randomForest)

#Importing the dataset
getwd()
data <- read.csv("train.csv")
str(data)

#Converting data into the the right format
data$Gender <- as.factor(data$Gender)
data$Company.Type <- as.factor(data$Company.Type)
data$WFH.Setup.Available <- as.factor(data$WFH.Setup.Available)
str(data)

#update status by changing continous to categorical
data$Status <- cut(data$Burn.Rate, breaks = c(0,0.3, 0.6,1),
                   labels = c("mild", "moderate", "severe"))
str(data)

#remove missing values
train_data <- na.omit(data)
#percentage of missing values removed in the whole data set
(nrow(data)-nrow(train_data))/nrow(data)*100

#list types for each attribute
sapply(train_data, class)

#remove first two column for classification as well as "Burn Rate" since we already have the Status
df = subset(train_data, select = -c(Employee.ID,Date.of.Joining, Burn.Rate))

# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(df$Status, p=0.8, list = FALSE)
TrainingSet <- df[TrainingIndex,] # Training Set
TestingSet <- df[-TrainingIndex,] # Test Set

# Write the training & testing set into file so we can always get the same result
write.csv(TrainingSet, "trainingset.csv")
write.csv(TestingSet, "testingset.csv")

# Again retrieve Training set from excel to train the model
TrainSet <- read.csv("trainingset.csv", header=TRUE)
# When we import the data set inside R there is an extra first column, so we need to remove it
TrainSet <- TrainSet[,-1]
str(TrainSet)

#Again transfer the data type to the correct type
TrainSet$Gender <- as.factor(TrainSet$Gender)
TrainSet$Company.Type <- as.factor(TrainSet$Company.Type)
TrainSet$WFH.Setup.Available <- as.factor(TrainSet$WFH.Setup.Available)
TrainSet$Status <- as.factor(TrainSet$Status)
str(TrainSet)

#Building RF model
model <- randomForest(Status ~ ., data = TrainSet, ntree = 500, mtry = 4, importance = TRUE)
print(model)

#Save the model, so that no need to rerun everytime opening the app
saveRDS(model, "model.rds")

