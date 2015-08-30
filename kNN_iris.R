---Preprocessing for kNN algorithm
data(iris)

---Which species of flower is based on remaining feature
table(iris$Species)
head(iris)
----Mix up the rows as data seems to be ordered with Species
 
gp <- runif(150)
iris <- iris[order(gp),]
str(iris)
head(iris,10)
---rescale my numerical vector
summary(iris[,c(1,2,3,4)])
---all the column has different range. All feature to be scaled in similar fashion.
--Normalize, value - min / (max - min)
normalize <- function(x) { return ((x-min(x))/(max(x) - min(x))) }
normalize(c(1,2,3,4))
iris[-5]
--Now normalize the sepal length, width, petal length, width
iris_n <- normalize(iris[-5])
summary(iris_n)
---15 (10% for testing), kepping 20 for round number
# create training and test data
iris_train <- iris_n[1:129,]
iris_test <- iris_n[130:150,]
# create labels for training and test data
iris_train_target <- iris[1:129, 5]
iris_test_target <- iris[130:150,5]
sqrt(150)
## Step 3: Training a model on the data ----

# install and load the "class" library
library(class)
m1 <- knn(train=iris_train, test = iris_test, cl=iris_train_target, k=13)
?head(iris)

## Step 4: Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = iris_test_target, y = m1,
           prop.chisq=FALSE)

## Step 5: Improving model performance ----

# use the scale() function to z-score standardize a data frame
wbcd_z <- as.data.frame(scale(wbcd[-1]))

# confirm that the transformation was applied correctly
summary(wbcd_z$area_mean)

# create training and test datasets
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]

# re-classify test cases
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)

# try several different values of k
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
