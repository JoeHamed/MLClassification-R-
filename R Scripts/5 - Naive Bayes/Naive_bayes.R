# Naive Bayes Classification

# Importing the dataset 
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]

# Splitting the dataset into the training and test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE )
test_set = subset(dataset, split == FALSE)

# Encoding the target as factor (use when y = training_set$Purshaed in naive bayes)
# dataset$Purchased = factor(dataset$Purchased, levels = c(1, 0))

# Feature Scaling the dataset
training_set[-3] = scale(training_set[-3])
test_set[1:2] = scale(test_set[1:2])

# Fitting the classifier to the training set
library(e1071)

classifier = naiveBayes(x = training_set[-3], y = training_set[3])

# Predicting the test results 
y_pred = predict(classifier, newdata = test_set[-3])

# Making the confusion matrix
cm = table(test_set[, 3], y_pred)

# Visualising the training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1 , max(set[, 1]) + 1, by = 0.01) # X axis (Age)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01) # Y axis (Salary)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'K Nearest Neighbors (K-NN)',
     xlab = 'Age', 
     ylab = 'Estimated Salary',
     xlim = range(X1),
     ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch ='.', col = ifelse(y_grid == 1, 'red', 'blue'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'red', 'blue'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1 , max(set[, 1]) + 1, by = 0.01) # X axis (Age)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01) # Y axis (Salary)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'K Nearest Neighbors (K-NN)',
     xlab = 'Age', 
     ylab = 'Estimated Salary',
     xlim = range(X1),
     ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch ='.', col = ifelse(y_grid == 1, 'orange', 'green'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'orange', 'green'))
