#' ---
#' title: "harinris_HW5_lab"
#' author: "Harin Rishabh"
#' date: "2023-11-26"
#' output: html_document
#' ---
#' 
#' # 7.8 Lab: Non-linear Modeling
#' 
## ------------------------------------------------------------------------------------------------------------------
library(ISLR2)
attach(Wage)

#' 
#' ## 7.8.1 Polynomial Regression and Step Functions
#' 
#' 
#' Fitting a linear model using poly() function to use a fourth-degree polynomial. The function returns a matrix whose columns are a basis of orthogonal polynomials. We can also use the poly() to get the polynomial directly by using raw = TRUE argument.
## ------------------------------------------------------------------------------------------------------------------
fit = lm(wage ~ poly(age, 4), data = Wage)
coef(summary(fit))
fit2 = lm(wage ~ poly(age , 4, raw = T), data = Wage)
coef(summary(fit2))

#' 
#' Now create a grid of values for age at which we want predictions, and then call the generic predict() function, specifying that we want standard errors as well.
## ------------------------------------------------------------------------------------------------------------------
agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims [2])
preds <- predict(fit, newdata = list(age = age.grid), se = TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)

#' 
#' Plot the data and add the fit from the degree-4 polynomial. mar and oma arguments to par() allow us to control the margins of the plot, and the title() function creates a figure title that spans both title() subplots.
## ------------------------------------------------------------------------------------------------------------------
par(mfrow = c(1, 2), mar = c(4.5 , 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(age , wage , xlim = agelims , cex = .5, col = "darkgrey")
title("Degree - 4 Polynomial", outer = T)
lines(age.grid, preds$fit , lwd = 2, col = "blue")
matlines(age.grid , se.bands, lwd = 1, col = "blue", lty = 3)

#' 
#' Now fit models from linear to degree-5 polynomial and determine which is the simplest model which is sufficient.
#' Clearly Model 1 is not sufficient as p-value is almost 0. Model 2 is also not sufficient. Model 3 and model 4 seem sufficient. Model 5 seems unnecessary as p-value is 0.37.
## ------------------------------------------------------------------------------------------------------------------
fit.1 <- lm(wage ~ age , data = Wage)
fit.2 <- lm(wage ~ poly(age , 2), data = Wage)
fit.3 <- lm(wage ~ poly(age , 3), data = Wage)
fit.4 <- lm(wage ~ poly(age , 4), data = Wage)
fit.5 <- lm(wage ~ poly(age , 5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

#' 
#' ANOVA method works even if other terms are present.
## ------------------------------------------------------------------------------------------------------------------
fit.1 <- lm(wage ~ education + age , data = Wage)
fit.2 <- lm(wage ~ education + poly(age , 2), data = Wage)
fit.3 <- lm(wage ~ education + poly(age , 3), data = Wage)
anova(fit.1, fit.2, fit.3)

#' 
#' We can also use cross-validation to choose the polynomial degree.
## ------------------------------------------------------------------------------------------------------------------
fit <- glm(I(wage > 250) ~ poly(age , 4), data = Wage, family = binomial)
preds <- predict(fit, newdata = list(age = age.grid), se = T)

#' 
#' Calculating and plotting confidence intervals. 
#' We used the jitter() function to jitter the age values a bit so that observations jitter() with the same age value do not cover each other up. This is often called a rug plot.
## ------------------------------------------------------------------------------------------------------------------
pfit <- exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))
plot(age , I(wage > 250), xlim = agelims , type = "n", ylim = c(0, .2))
points(jitter(age), I(( wage > 250) / 5), cex = .5, pch = "|", col = "darkgrey")
lines(age.grid, pfit , lwd = 2, col = "blue")
matlines(age.grid , se.bands , lwd = 1, col = "blue", lty = 3)

#' 
#' In order to fit a step function, we use the cut() function. cut() automatically picked the cutpoints at 33.5, 49, and 64.5 years of age. We could also have specified our own cutpoints directly using the breaks option.
## ------------------------------------------------------------------------------------------------------------------
table(cut(age , 4))
fit <- lm(wage ~ cut(age , 4), data = Wage)
coef(summary(fit))

#' 
#' ## 7.8.2 Splines
#' 
#' Regression splines can be fit by constructing an appropriate matrix of basis functions. The bs() function generates the entire matrix of basis functions for splines with the specified set of knots. By default, cubic splines are produced.
## ------------------------------------------------------------------------------------------------------------------
library(splines)
fit <- lm(wage ~ bs(age , knots = c(25, 40, 60)), data = Wage)
pred <- predict(fit , newdata = list(age = age.grid), se = T)
plot(age , wage , col = "gray")
lines(age.grid, pred$fit , lwd = 2)
lines(age.grid , pred$fit + 2 * pred$se, lty = "dashed")
lines(age.grid , pred$fit - 2 * pred$se, lty = "dashed")

#' 
#' We could also use the df option to produce a spline with knots at uniform quantiles of the data. In this case R chooses knots at ages 33.8, 42.0, and 51.0.
## ------------------------------------------------------------------------------------------------------------------
dim(bs(age , knots = c(25, 40, 60)))
dim(bs(age , df = 6))
attr(bs(age , df = 6), "knots")

#' 
#' To fit a natural spline, we use the ns() function.
## ------------------------------------------------------------------------------------------------------------------
fit2 <- lm(wage ~ ns(age , df = 4), data = Wage)
pred2 <- predict(fit2 , newdata = list(age = age.grid), se = T)
plot(age , wage , xlim = agelims , cex = .5, col = "darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(age , wage , df = 16)
fit2 <- smooth.spline(age , wage , cv = TRUE)
fit2$df
lines(fit , col = "red", lwd = 2)
lines(fit2 , col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"),
col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

#' 
#' In order to perform local regression, we use the loess() function.
## ------------------------------------------------------------------------------------------------------------------
plot(age , wage , xlim = agelims , cex = .5, col = "darkgrey")
title("Local Regression")
fit <- loess(wage ~ age , span = .2, data = Wage)
fit2 <- loess(wage ~ age , span = .5, data = Wage)
lines(age.grid, predict(fit , data.frame(age = age.grid)),
col = "red", lwd = 2)
lines(age.grid, predict(fit2 , data.frame(age = age.grid)),
col = "blue", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"),
col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

#' 
#' 
#' # 8.3 Lab: Decision Trees
#' 
#' ## 8.3.1 Fitting Classification Trees
#' 
## ------------------------------------------------------------------------------------------------------------------
library(tree)
library(ISLR)
attach(Carseats)

#' 
#' ifelse() function to create a variable, called High, which takes on a value of Yes if the Sales variable exceeds 8, and takes on a value of No otherwise.
## ------------------------------------------------------------------------------------------------------------------
High <- factor(ifelse(Sales <= 8, "No", "Yes"))

#' 
#' use the data.frame() function to merge High with the rest of the Carseats data.
## ------------------------------------------------------------------------------------------------------------------
Carseats <- data.frame(Carseats, High)

#' 
#' We now use the tree() function to fit a classification tree in order to predict High using all variables but Sales.
## ------------------------------------------------------------------------------------------------------------------
tree.carseats <- tree(High ~ . - Sales, Carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)
tree.carseats

#' 
#' We split the observations into a training set and a test set, build the tree using the training set, and evaluate its performance on the test data.
## ------------------------------------------------------------------------------------------------------------------
set.seed(2)
train<-sample(1:nrow(Carseats),200)
Carseats.test<-Carseats[-train,]
High.test<-High[-train]
tree.carseats<-tree(High~.-Sales,Carseats,subset=train)
tree.pred<-predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(104 + 50) / 200

#' 
#' The function cv.tree() performs cross-validation in order to determine the optimal level of tree complexity; cost complexity pruning is used in order to select a sequence of trees for consideration. We use the argument FUN = prune.misclass in order to indicate that we want the classification error rate to guide the cross-validation and pruning process, rather than the default for the cv.tree() function, which is deviance.
## ------------------------------------------------------------------------------------------------------------------
set.seed(7)
cv.carseats<-cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")

#' 
#' apply the prune.misclass() function in order to prune the tree to obtain the nine-node tree. Then we predict and evaluate its performance on the test data. 77.5% is slightly better.
## ------------------------------------------------------------------------------------------------------------------
prune.carseats<-prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)


tree.pred<-predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(97 + 58) / 200

#' 
#' If we increase the value of best, we obtain a larger pruned tree with lower
#' classification accuracy.
## ------------------------------------------------------------------------------------------------------------------
prune.carseats<-prune.misclass(tree.carseats,best=14)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred<-predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(102 + 52) / 200

#' 
#' 
#' ## 8.3.2 Fitting Regression Trees
#' 
#' Fitting a regression tree to the Boston data set. First, we create a training set, and fit the tree to the training data.
## ------------------------------------------------------------------------------------------------------------------
library(MASS)
set.seed(1)
train<-sample(1:nrow(Boston),nrow(Boston)/2)
tree.boston<-tree(medv~.,Boston,subset=train)
summary(tree.boston)

plot(tree.boston)
text(tree.boston,pretty=0)

#' 
#' Now we use the cv.tree() function to see whether pruning the tree will improve performance. We use unpruned tree as per cross-validation results.
## ------------------------------------------------------------------------------------------------------------------
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type="b")
yhat<-predict(tree.boston,newdata=Boston[-train,])
boston.test<-Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

#' 
#' 
#' ## 8.3.3 Bagging and Random Forests
#' 
#' Here we apply bagging and random forests to the Boston data, using the randomForest package in R.
## ------------------------------------------------------------------------------------------------------------------
library(randomForest)
set.seed(1)
bag.boston<-randomForest(medv~.,data=Boston,subset=train,mtry=12,importance=TRUE)
bag.boston

#' 
#' The argument mtry = 12 indicates that all 12 predictors should be considered for each split of the tree—in i.e., bagging should be done.
## ------------------------------------------------------------------------------------------------------------------
yhat.bag<-predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag,boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

#' 
#' The test set MSE associated with the bagged regression tree is 23.42. We could change the number of trees grown by randomForest() using the ntree argument.
## ------------------------------------------------------------------------------------------------------------------
bag.boston<-randomForest(medv~.,data=Boston,subset=train,mtry=12,ntree=25)
yhat.bag<-predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)

#' 
#' Growing a random forest proceeds in exactly the same way, except that we use a smaller value of the mtry argument.
## ------------------------------------------------------------------------------------------------------------------
set.seed(1)
rf.boston<-randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf<-predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)

#' 
#' Random forests showed an improvement over bagging. Using the importance() function, we can view the importance of each variable. Plots of these importance measures can be produced using the varImpPlot() function.
## ------------------------------------------------------------------------------------------------------------------
importance(rf.boston)
varImpPlot(rf.boston)

#' 
#' 
#' ## 8.3.4 Boosting
#' 
#' Here we use the gbm package, and within it the gbm() function, to fit boosted regression trees to the Boston data set. We run gbm() with the option distribution = "gaussian" since this is a regression problem
## ------------------------------------------------------------------------------------------------------------------
library(gbm)
set.seed(1)
boost.boston<-gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.boston)

#' 
#' We see that lstat and rm are by far the most important variables. We can also produce partial dependence plots for these two variables.
## ------------------------------------------------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

#' 
#' We now use the boosted model to predict medv on the test set
## ------------------------------------------------------------------------------------------------------------------
yhat.boost<-predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)

#' 
#' Perform boosting with a different value of the shrinkage parameter λ. Here we take λ = 0.2. It leqads to lower MSE here.
## ------------------------------------------------------------------------------------------------------------------
boost.boston<-gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4, shrinkage=0.2, verbose=F)
yhat.boost<-predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)

#' 
#' 
#' ## 8.3.5 Bayesian Additive Regression Trees
#' 
#' We use the BART package, and within it the gbart() function, to fit a Bayesian additive regression tree model to the Boston housing data set.
#' To run the gbart() function, we must first create matrices of predictors for the training and test data. We run BART with default settings.
## ------------------------------------------------------------------------------------------------------------------
library(BART)
x <- Boston[, 1:12]
y <- Boston[, "medv"]
xtrain <- x[train, ]
ytrain <- y[train]
xtest <- x[-train, ]
ytest <- y[-train]
set.seed (1)
bartfit <- gbart(xtrain , ytrain , x.test = xtest)

#' 
#' Computing test error. On this data set, the test error of BART is lower than the test error of random forests and boosting.
## ------------------------------------------------------------------------------------------------------------------
yhat.bart <- bartfit$yhat.test.mean
mean (( ytest - yhat.bart)^2)

#' 
#' Now we can check how many times each variable appeared in the collection of trees.
## ------------------------------------------------------------------------------------------------------------------
ord <- order(bartfit$varcount.mean , decreasing = T)
bartfit$varcount.mean[ord]

