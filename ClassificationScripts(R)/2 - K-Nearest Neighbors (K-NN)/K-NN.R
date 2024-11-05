# K-Nearest Neighbors (K-NN)

# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]

# Splitting the dataset into the training set and test set
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# Fitting classifier to the training set and Predicting the test set results
# install.packages('class')
library(class)
y_pred <- knn(train = training_set[, -3],
             test = test_set[, -3],
             cl = training_set[, 3],
             k = 5)

# Making the confusion matrix
cm = table(test_set[, 3], y_pred)

# Visualising the Training set results
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1 , max(set[, 1]) + 1, by = 0.01) # X axis (Age)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01) # Y axis (Salary)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid <- knn(train = training_set[, -3],
             test = grid_set, # Imaginary users pixel points 
             cl = training_set[, 3],
             k = 5)
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
y_grid <- knn(train = training_set[, -3],
              test = grid_set, # Imaginary users pixel points 
              cl = training_set[, 3],
              k = 5)
plot(set[, -3],
     main = 'K Nearest Neighbors (K-NN)',
     xlab = 'Age', 
     ylab = 'Estimated Salary',
     xlim = range(X1),
     ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch ='.', col = ifelse(y_grid == 1, 'orange', 'green'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'orange', 'green'))