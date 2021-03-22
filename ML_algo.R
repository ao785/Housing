housing.dataset <- read.csv("melbourne_housing_data.csv", stringsAsFactors = TRUE)

str(housing.dataset)

########################################################
install.packages("caret", dependencies=TRUE)
library("caret")
set.seed(101)
trainingInd <- createDataPartition(housing.dataset$Regionname, p= 0.75, list = F)
training_data <- housing.dataset[trainingInd,]
test_data <- housing.dataset[-trainingInd,]

table(training_data$Regionname)
table(test_data$Regionname)

housing_model <- lm(Price ~ Type + Distance + Regionname + Rooms + Propertycount + Postcode + Method + CouncilArea, data = training_data)
housing_model

summary(housing_model)

housing_predictions <- predict(housing_model, newdata = test_data)
summary(housing_predictions)
cor(housing_predictions, test_data$Price)
cor(housing_predictions, test_data$Price)^2
RMSE <- sqrt(mean((housing_predictions - test_data$Price)^2))
RMSE

############################################################
normalize_data <- function(x) {return ((x-min(x))/(max(x)-min(x)))}
new_housing.dataset <- housing.dataset

new_housing.dataset$Price = normalize_data(new_housing.dataset$Price)
new_housing.dataset$Distance = normalize_data(new_housing.dataset$Distance)
new_housing.dataset$Propertycount = normalize_data(new_housing.dataset$Propertycount)
new_housing.dataset$Rooms = normalize_data(new_housing.dataset$Rooms)

trainingInd.normalize <- createDataPartition(new_housing.dataset$Regionname, p= 0.75, list = F)
training_data.normalize <- new_housing.dataset[trainingInd.normalize,]
test_data.normalize <- new_housing.dataset[-trainingInd.normalize,]

new_housing_model <- lm(Price ~ Type + Distance + Regionname + Rooms + Propertycount + Postcode + Method + CouncilArea, data = training_data.normalize)
new_housing_model

summary(new_housing_model)

new_housing_predictions <- predict(new_housing_model, newdata = test_data.normalize)
summary(new_housing_predictions)
cor(new_housing_predictions, test_data.normalize$Price)
cor(new_housing_predictions, test_data.normalize$Price)^2
RMSE <- sqrt(mean((new_housing_predictions - test_data.normalize$Price)^2))
RMSE

###############################################################

knn_housing.dataset <- new_housing.dataset
set.seed(101)
knn_trainingInd <- createDataPartition(knn_housing.dataset$Type, p= 0.8, list = F)
knn_training_data <- knn_housing.dataset[knn_trainingInd,]
knn_test_data <- knn_housing.dataset[-knn_trainingInd,]

table(knn_training_data$Type)
table(knn_test_data$Type)

trainctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3) 
knn_fit <- train(Type ~ Distance + Propertycount + Rooms + Price, data = knn_training_data, method = "knn",
                 trControl = trainctrl,
                 tuneLength = 10)
knn_fit

knnPredict <- predict(knn_fit, newdata = knn_test_data )
knnPredict
confusionMatrix(knnPredict, knn_test_data$Type )

##################################################################
install.packages("inum")
library(C50)

c5_housing.dataset <- housing.dataset
c_housing.dataset <- c5_housing.dataset[1:4000,]
set.seed(101)
c_trainingInd <- createDataPartition(c_housing.dataset$Type, p= 0.8, list = F)
c_training_data <- c_housing.dataset[c_trainingInd,]
c_test_data <- c_housing.dataset[-c_trainingInd,]

table(c_training_data$Type)
table(c_test_data$Type)

C5_fit <- train(Type~ Price + Distance + Regionname + Rooms + Propertycount, data = c_training_data, method = "C5.0")
summary(C5_fit) 
C5_predict <- predict(C5_fit, newdata = credit_test )
confusionMatrix(C5_predict, credit_test$default )

##################################################################
install.packages("neuralnet")
library("neuralnet")

ann_housing.dataset <- new_housing.dataset[1:4000,]
set.seed(101)
ann_trainingInd <- createDataPartition(ann_housing.dataset$Type, p= 0.8, list = F)
ann_training_data <- ann_housing.dataset[ann_trainingInd,]
ann_test_data <- knn_housing.dataset[-ann_trainingInd,]

table(ann_training_data$Type)
table(ann_test_data$Type)

ANN_fit <- neuralnet(Type~ Price + Distance + Regionname + Rooms + Propertycount, data = ann_training_data)
plot(ANN_fit)
ANN_results <- compute(ANN_fit, ann_test_data[1:8])
predicted_strength <- ANN_results$net.result
cor(predicted_strength, concrete_test$strength)

ANN_fit <- neuralnet(Type~ Price + Distance + Regionname + Rooms + Propertycount, data = ann_training_data, hidden = 5)
plot(ANN_fit)
ANN_results <- compute(ANN_fit, concrete_test[1:8])
predicted_strength <- ANN_results$net.result
cor(predicted_strength, concrete_test$strength)

###################################################################"
plot(housing.dataset$Price,housing.dataset$Distance)
hist(housing.dataset$Distance,housing.dataset$Rooms)

ggplot(housing.dataset, aes( x=housing.dataset$Type,y=housing.dataset$Price, fill=housing.dataset$Type, colour=housing.dataset$Type)) +
  geom_jitter(width=0.25)+ 
  geom_boxplot(alpha=0.5, outlier.shape=NA)+    
  xlab(label = "Type") +
  ylab(label = "Price") +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))+
  theme(legend.position="none")+
  theme_classic()+
  ggtitle("Boxplot with Types")

h_houses <- subset(housing.dataset,housing.dataset$Type == "u")
mean(h_houses$Price)
plot(h_houses$Price,h_houses$Distance)
plot(h_houses$Price,h_houses$Rooms)

str(housing.dataset$Regionname)
plot(housing.dataset$Regionname,housing.dataset$Rooms) 
eastern_victoria_houses <- subset(housing.dataset,housing.dataset$Regionname == "Eastern Victoria")

one_room_houses <- subset(housing.dataset, housing.dataset$Rooms == 1)
mytable <- table(one_room_houses$Type)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls, col=rainbow(length(lbls)), main="Pie Chart of Types")
summary(one_room_houses$Type)

two_room_houses <- subset(housing.dataset, housing.dataset$Rooms == 2)
summary(two_room_houses$Type)
plot(two_room_houses$Price,two_room_houses$Distance)
mean(two_room_houses$Price)
hist(two_room_houses$Price,two_room_houses$Distance)

sample_two_rooms_house <- sample(two_room_houses$Price,30)
mean(sample_two_rooms_house)

two_rooms_houses_u <- subset(two_room_houses, two_room_houses$Type == "u")
plot(two_rooms_houses_u$Price,two_rooms_houses_u$Distance)
