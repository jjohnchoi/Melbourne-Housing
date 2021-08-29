library(dplyr)
library(ISLR)
require(tree)

#Pull all the files for the project into an array
getwd()
files <- list.files(path = "Data/")

#Put extract data
raw_data <- read.csv(paste0("data/melb_data.csv"), header = T, sep = ",")

#print out summary of the data - shows some NAs
summary(raw_data)

#remove all the rows with NA values
raw_data <- na.omit(raw_data)

#select the prediction target
#y <- raw_data$Price

#select features, note Longitude spelt wrong
melbourneFeatures <- c('Price', 'Rooms', 'Bathroom', 'Landsize', 'Lattitude', 'Longtitude')

#select your training set
train_data <- raw_data[, melbourneFeatures]

#fit to the tree model
set.seed(1)
tree.model <- tree(Price ~ ., data = train_data)

#Review the tree
summary(tree.model)
plot(tree.model)
text(tree.model, pretty = 0, cex = 0.7)

#Check to see if pruning the tree would yield results
cv.tree.model <- cv.tree(tree.model)
plot(cv.tree.model$size, cv.tree.model$dev, type = 'b')

#plots confirm that 13 nodes produces lowers cv error rate
prune.tree.model <- prune.tree(tree.model, best = 13)
plot(prune.tree.model)
text(prune.tree.model, pretty = 0, cex = 0.7)

#make predictions then check MSE on fitted training set
train_data$yhat <- predict(prune.tree.model, newdata = train_data)

#plot predictions on actuals and calculate MSE
plot(train_data$yhat, train_data$Price)
abline(0,1)
mean((train_data$yhat - train_data$Price)^2)
