
#OBJECTIVE: quantify the morphologic variation of iris flowers of three related species.
#Classification problem
#Species, will be the target variable


#Get Data

Iris <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), 
                 header = TRUE)
View(Iris)

#Add column names
names(Iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
View(Iris)
iris_data <- Iris

# Print first lines
head(iris_data)


#Discriptive Statistics 
names(iris_data)
str(iris_data)
summary(iris_data)

# Iris scatter plot
png(file="Scatter Plots.png",width=600,height=450)
plot(iris_data,main="Scatter Plot")
dev.off()

    ##Using ggvis visialization packages
install.packages("ggvis")
library(ggvis)
iris_data  %>% ggvis(~Sepal.Length, ~Sepal.Width, fill = ~Species) %>% layer_points()
iris_data %>% ggvis(~Petal.Length,~Petal.Width, fill = ~Species) %>% layer_points()
  

# Overall correlation `Petal.Length` and `Petal.Width`
cor(iris$Petal.Length, iris$Petal.Width)

# Overall correlation `Sepal.Length` and `Sepal.Width`
cor(iris$Sepal.Length, iris$Sepal.Width)

# Return values of `iris` levels 
x=levels(iris$Species)
x

# Print Setosa correlation matrix
print(x[1])
cor(iris[iris$Species==x[1],1:4])


# Print Versicolor correlation matrix
print(x[2])
cor(iris[iris$Species==x[2],1:4])


# Print Virginica correlation matrix
print(x[3])
cor(iris[iris$Species==x[3],1:4])

# Division of `Species`
table(iris$Species) 

# Percentual division of `Species`
round(prop.table(table(iris$Species)) * 100, digits = 1)


# Normalizing the data (although already normalized)
  # normalize() function
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

iris_data_norm <- as.data.frame(lapply(iris_data[1:4], normalize))
summary(iris_data_norm)
View(iris_data_norm)

#Partitioning Data in Training and Testing
set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))
  # Compose training set
iris.training <- iris[ind==1, 1:4]
head(iris.training)
  # Compose test set
iris.test <- iris[ind==2, 1:4]
head(iris.test)

#We have only taken Sepal.Length, Sepal.Width, Petal.Length and Petal.Width above.
#because we want to predict the fifth attribute, Species. 
#We include it into the KNN algorithm, otherwise there will never be any prediction for it.
#Therefore need to store the class labels(Species) in factor vectors 

  # Compose `iris` training labels
iris.trainLabels <- iris[ind==1,5]
print(iris.trainLabels)
  # Compose `iris` test labels
iris.testLabels <- iris[ind==2, 5]
print(iris.testLabels)

# Build the model
library(class)
iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)
iris_pred

# Put `iris.testLabels` in a data frame
irisTestLabels <- data.frame(iris.testLabels)
# Merge `iris_pred` and `iris.testLabels` 
merge <- data.frame(iris_pred, iris.testLabels)
# Specify column names for `merge`
names(merge) <- c("Predicted Species", "Observed Species")
merge

#Model Evaluation 
install.packages("gmodels")
library(gmodels)
CrossTable(x = iris.testLabels, y = iris_pred, prop.chisq=FALSE) #prop.chisq indicates whether or not the 
                                                                 #chi-square contribution of each cell is included.
                                                                 #The chi-square statistic is the sum of the 
                                                                 #contributions from each of the individual cells 
                                                                 #and is used to decide whether the difference 
                                                                 #between the observed and the expected values 
                                                                 #is significant.

#Select the best K
knn.bestK = function(train, test, y.train, y.test, k.max = 20) {
  k.grid = seq(1, k.max, 2) # a sequence of odd numbers, k.grid = {1, 3, 5, 7, ...., k.max}
  fun.tmp = function(x) {
    y.hat = knn(train, test, y.train, k = x, prob=F) # run knn for each k in k.grid
    return(sum(y.hat != y.test))
  }
  ## create a temporary function (fun.tmp) that we want to apply to each value in k.grid
  error = unlist(lapply(k.grid, fun.tmp))
  ## apply "fun.tmp" to each value in k.grid
  ## it will return a list so I need to unlist it to make it to be a vector
  out = list(k.optimal = k.grid[which.min(error)], error.min = min(error))
  return(out)
}


knn.bestK(iris.training, iris.test, iris.trainLabels, iris.testLabels, 20)

#Build Model 2
iris_pred2 <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=5)
iris_pred2

#Model Evaluation 
# Put `iris.testLabels` in a data frame
irisTestLabels <- data.frame(iris.testLabels)
# Merge `iris_pred` and `iris.testLabels` 
merge2 <- data.frame(iris_pred2, iris.testLabels)
# Specify column names for `merge`
names(merge2) <- c("Predicted Species", "Observed Species")
merge2

#Model Evaluation 
CrossTable(x = iris.testLabels, y = iris_pred2, prop.chisq=FALSE)

#Model 2 best


###############################Machine Learning with caret################################################
#caret(clas and Reg)
install.packages("caret")
library(caret)
#Partitioning Data
# Create index to split based on labels  
index <- createDataPartition(iris_data$Species, p=0.75, list=FALSE)
# Subset training set with index
iris.trainingml <- iris_data[index,]
# Subset test set with index
iris.testml <- iris_data[-index,]

# Overview of algos supported by caret
names(getModelInfo())
# Train a model
model_knn <- train(iris.trainingml[, 1:4], iris.trainingml[,5], method='knn')


# Predict the labels of the test set
predictions<-predict(object=model_knn,iris.testml[,1:4])

# Evaluate the predictions
table(predictions)
  # Confusion matrix 
confusionMatrix(predictions,iris.testml[,5])