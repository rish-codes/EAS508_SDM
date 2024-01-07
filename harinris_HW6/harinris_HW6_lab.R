#' ---
#' title: "harinris_HW6_lab"
#' author: "Harin Rishabh"
#' date: "2023-12-10"
#' output: html_document
#' ---
#' 
#' # 9.6 Lab: Support Vector Machines
#' ## 9.6.1 Support Vector Classifier
#' 
## ---------------------------------------------------------------------------------------------------------------------------------------------
library(e1071)

#' 
#' Using the svm() function to fit the support vector classifier for a given value of the cost parameter. We begin by generating the observations, which belong to two classes, and checking whether the classes are linearly separable.
## ---------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1)
x <- matrix(rnorm(20 * 2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y == 1, ] <- x[y == 1, ] + 1
plot(x, col = (3 - y))

#' We now create a data frame with the response coded as a factor.
## ---------------------------------------------------------------------------------------------------------------------------------------------
dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 10, scale = FALSE)
plot(svmfit, dat)

#' 
#' We can inspect the support vectors with the index component of the classifier returned from svm().
## ---------------------------------------------------------------------------------------------------------------------------------------------
svmfit$index
summary(svmfit)

#' 
#' hange the cost parameter and re-run the classifier to see how the support vectors change.
## ---------------------------------------------------------------------------------------------------------------------------------------------
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 0.1, scale = FALSE)
plot(svmfit, dat)
svmfit$index

#' 
#' We can run cross-validation on a range of values for the cost parameter using the tune() function.
## ---------------------------------------------------------------------------------------------------------------------------------------------
tune.out <- tune(svm, y ~ ., data = dat, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

#' 
#' The best performing model is stored in the best.model component of the object returned from tune().
## ---------------------------------------------------------------------------------------------------------------------------------------------
bestmod <- tune.out$best.model
summary(bestmod)

#' 
#' We can also classify a different set of observations using the best model obtained from tune(). First we create a new set of observations with the rnorm() function.
## ---------------------------------------------------------------------------------------------------------------------------------------------
xtest <- matrix(rnorm(20 * 2), ncol = 2)
ytest <- sample(c(-1, 1), 20, rep = TRUE)
xtest[ytest == 1, ] <- xtest[ytest == 1, ] + 1
testdat <- data.frame(x = xtest, y = as.factor(ytest))

#' 
#' We can then use the predict() function to classify the observations in testdat.
## ---------------------------------------------------------------------------------------------------------------------------------------------
ypred <- predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y)

#' 
#' We can also try a different value for the cost parameter and train the classifier again.
## ---------------------------------------------------------------------------------------------------------------------------------------------
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 0.01, scale = FALSE)
ypred <- predict(svmfit, testdat)
table(predict = ypred, truth = testdat$y)

#' 
#' We can linearly separate the two classes and plot the observations.
## ---------------------------------------------------------------------------------------------------------------------------------------------
x[y == 1, ] <- x[y == 1, ] + 0.5
plot(x, col = (y + 5)/2, pch = 19)

#' 
#' We can also try with a smaller cost value.
## ---------------------------------------------------------------------------------------------------------------------------------------------
svmfit <- svm(y ~ ., data = dat, kernel = "linear", cost = 1)
summary(svmfit)
plot(svmfit, dat)

#' 
#' 
#' ## 9.6.2 Support Vector Machine
#' 
#' We begin by generating data with non-linear class boundaries. Plotting the data makes it clear that the class boundary is indeed nonlinear.
## ---------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1)
x <- matrix(rnorm(200 * 2), ncol = 2)
x[1:100, ] <- x[1:100, ] + 2
x[101:150, ] <- x[101:150, ] - 2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))
plot(x, col = y)

#' 
#' We split the data into training and test subsets and run the SVM classifier with kernel = "radial" parameter.
## ---------------------------------------------------------------------------------------------------------------------------------------------
train <- sample(200, 100)
svmfit <- svm(y ~ ., data = dat[train, ], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat[train, ])
summary(svmfit)

#' 
#' We can use a larger value for the cost parameter and see if it reduces the training errors.
## ---------------------------------------------------------------------------------------------------------------------------------------------
svmfit <- svm(y ~ ., data = dat[train, ], kernel = "radial", gamma = 1, cost = 1e+05)
plot(svmfit, dat[train, ])

#' 
#' We can run cross-validation using the tune() function.
## ---------------------------------------------------------------------------------------------------------------------------------------------
tune.out <- tune(svm, y ~ ., data = dat[train, ], kernel = "radial", ranges = list(cost = c(0.1, 1, 10, 100, 1000), gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)

#' 
#' We can predict the classes on the test subset and examine the number of observations misclassified.
## ---------------------------------------------------------------------------------------------------------------------------------------------
table(true = dat[-train, "y"], pred = predict(tune.out$best.model, newdata = dat[-train, ]))

#' 
#' 
#' ## 9.6.4 SVM with Multiple Classes
#' 
#' The svm() function can also be used to classify observations from multiple-classes.
## ---------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1)
x <- rbind(x, matrix(rnorm(50 * 2), ncol = 2))
y <- c(y, rep(0, 50))
x[y == 0, 2] <- x[y == 0, 2] + 2
dat <- data.frame(x = x, y = as.factor(y))
par(mfrow = c(1, 1))
plot(x, col = (y + 1))

#' 
#' The svm() function now will perform multi-class classification since the dataset we generated now has three class labels.
## ---------------------------------------------------------------------------------------------------------------------------------------------
svmfit <- svm(y ~ ., data = dat, kernel = "radial", cost = 10, gamma = 1)
plot(svmfit, dat)

