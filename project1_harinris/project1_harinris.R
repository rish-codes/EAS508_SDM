#' ---
#' title: "Project1_harinris"
#' author: "Harin Rishabh"
#' date: "2023-11-13"
#' output: html_document
#' ---
#' 
## ---------------------------------------------------------------------------------------
library(readxl)
library(leaps)
library(boot)
library(glmnet)

#' 
## ---------------------------------------------------------------------------------------
set.seed(1)
data = read_excel("Real estate valuation data set.xlsx", range= "B1:H415")
head(data)

#' 
#' Renaming columns for convention
## ---------------------------------------------------------------------------------------
colnames(data)[1] = "x1"
colnames(data)[2] = "x2"
colnames(data)[3] = "x3"
colnames(data)[4] = "x4"
colnames(data)[5] = "x5"
colnames(data)[6] = "x6"
colnames(data)[7] = "y"
colSums(is.na(data))
dim(data)

#' 
#' Splitting data set into training and test data sets.
## ---------------------------------------------------------------------------------------
indices = sample(1:nrow(data), size = 0.7 * nrow(data))
train_data <- data[indices, ]
test_data <- data[-indices, ]

#' 
#' Plotting relationship between variables.
#' 
## ---------------------------------------------------------------------------------------
plot(data$x1, data$y, main='Plot-1 - Transaction Date vs. House Price per Unit Area',
     xlab='Transaction Date', ylab='House Price per Unit Area', col='blue')
plot(data$x2, data$y, main='Plot-2 - House Age vs. House Price per Unit Area',
     xlab='House Age', ylab='House Price per Unit Area', col='blue')
plot(data$x3, data$y, main='Plot-3 - distance to the nearest MRT station vs. House Price per Unit Area',
     xlab='distance to the nearest MRT station', ylab='House Price per Unit Area', col='blue')
plot(data$x4, data$y, main='Plot-4 - number of convenience stores vs. House Price per Unit Area',
     xlab='number of convenience stores', ylab='House Price per Unit Area', col='blue')
plot(data$x5, data$y, main='Plot-5 - latitude vs. House Price per Unit Area',
     xlab='latitude', ylab='House Price per Unit Area', col='blue')
plot(data$x6, data$y, main='Plot-6 - longitude vs. House Price per Unit Area',
     xlab='longitude', ylab='House Price per Unit Area', col='blue')

#' 
#' ## Analysis of scatterplots
#' 
#' 1. According to plot 1, house prices seem to be increasing since early 2013(approx. February).
#' 
#' 2. According to plot 2, we can see that there is not very significant relationship between House age and price.
#' 
#' 3. Plot 3 shows that price of house decreases rapidly as distance from MRT increases.
#' 
#' 4. Plot 4 shows that there is a slight increase in house price as the number of convenience stores increases.
#' 
#' 5. Plot 5 and 6 shows that higher price houses are concentrated around 24.97, 121.54.
#' 
#' 
#' ## Linear regression with all variables
#' 
#' First we fit a linear regression model including all variables and calculate a test MSE.
#' 
## ---------------------------------------------------------------------------------------
lm_fit_full =  lm(y ~ x1 + x2 + x3 + x4 + x5 + x6, data = train_data)
lm_full_pred = predict(lm_fit_full, newdata= test_data)
lm_full_mse = mean((test_data$y - lm_full_pred)^2)
print(paste("MSE for Linear regression with all variables = ", lm_full_mse))


#' 
#' Next we also try testing using LOOCV cross validation method using glm() and cv.glm() functions.
## ---------------------------------------------------------------------------------------
glm_fit_full = glm(y ~ x1 + x2 + x3 + x4 + x5 + x6, data = data)
cv.err.full = cv.glm(data , glm_fit_full)
print(paste("MSE for LOOCV for  Linear regression with all variables = ", cv.err.full$delta[1]))

#' 
#' Finally, we try calculating test MSE using 5-fold and 10-fold cross-validation.
## ---------------------------------------------------------------------------------------
cv.err.full.5 = cv.glm(data , glm_fit_full, K = 5)
cv.err.full.10 = cv.glm(data , glm_fit_full, K = 10)
print(paste("MSE for 5-fold cross-validation for  Linear regression with all variables = ", cv.err.full.5$delta[1]))
print(paste("MSE for 10-fold cross-validation for  Linear regression with all variables = ", cv.err.full.10$delta[1]))


#' 
#' 
#' ## Linear regression with Forward selection of variables
#' 
#' We are performing forward selection using regsubsets. By plotting RSS and Adjusted RSq against number of variable it becomes clear that only 5 variables are significant. Further, by using regsubsets in-built plot function, we can see x6 can be omitted as the adjr2 is same for 5 vs 6 variables.
#' 
## ---------------------------------------------------------------------------------------
regfit_fwd = regsubsets(y ~ ., data = data, nvmax = 6, method = "forward")
regfit_fwd.summary = summary(regfit_fwd)
names(regfit_fwd.summary)
regfit_fwd.summary$rsq
print(paste("Max number of variables = ", which.max(regfit_fwd.summary$adjr2)))
par(mfrow = c(2, 3))
plot(regfit_fwd.summary$rss , xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(regfit_fwd.summary$adjr2 , xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
plot(regfit_fwd, scale = "adjr2")

#' 
#' 
#' Now to pick the formula, we just check the summary which shows the order of picking variables. According to forward selection summary, the formula is y = x3 + x4 + x2 + x5 + x1. Then we fit a multiple regression model based on the variables selected and calculate MSE. We can see that MSE is slighlty lower than the previous model when all variables were included.
## ---------------------------------------------------------------------------------------
regfit_fwd.summary
model_fwd_fit = lm(y ~ x3 + x4 + x2 + x5 + x1, data = train_data)
model_fwd_predict = predict(model_fwd_fit, newdata = test_data)
model_fwd_mse = mean((test_data$y - model_fwd_predict)^2)
print(paste("MSE for Linear regression with Forward selection of variables = ", model_fwd_mse))

#' 
#' Next we also try testing using LOOCV cross validation method using glm() and cv.glm() functions.
## ---------------------------------------------------------------------------------------
glm_fit_fwd = glm(y ~ x3 + x4 + x2 + x5 + x1, data = data)
cv.err.fwd = cv.glm(data , glm_fit_fwd)
print(paste("MSE for LOOCV for  Linear regression with Forward selection of variables = ", cv.err.fwd$delta[1]))

#' 
#' Finally, we try calculating test MSE using 5-fold and 10-fold cross-validation.
## ---------------------------------------------------------------------------------------
cv.err.fwd.5 = cv.glm(data , glm_fit_fwd, K = 5)
cv.err.fwd.10 = cv.glm(data , glm_fit_fwd, K = 10)
print(paste("MSE for 5-fold cross-validation for  Linear regression with Forward selection of variables = ", cv.err.fwd.5$delta[1]))
print(paste("MSE for 10-fold cross-validation for  Linear regression with Forward selection of variables = ", cv.err.fwd.10$delta[1]))


#' 
#' ## Linear regression with Backward selection of variables
#' 
#' We are performing backward selection using regsubsets. By plotting RSS and Adjusted RSq against number of variables it becomes clear that only 5 variables are significant. Further, by using regsubsets in-built plot function, we can see x6 can be omitted as the adjr2 is same for 5 vs 6 variables.
## ---------------------------------------------------------------------------------------
regfit_bwd = regsubsets(y ~ ., data = data, nvmax = 6, method = "backward")
regfit_bwd.summary = summary(regfit_bwd)
names(regfit_bwd.summary)
regfit_bwd.summary$rsq
print(paste("Max number of variables = ", which.max(regfit_bwd.summary$adjr2)))
par(mfrow = c(2, 3))
plot(regfit_bwd.summary$rss , xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(regfit_bwd.summary$adjr2 , xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
plot(regfit_bwd, scale = "adjr2")

#' 
#' Now to pick the formula, we just check the summary which shows the order of picking variables. According to backward selection summary, the formula is y =  x3 + x4 + x2 + x5 + x1, which is exactly same as forward selection. Then we fit a multiple regression model based on the variables selected and calculate MSE. We can see that MSE is exactly same as the forward selection instance.
## ---------------------------------------------------------------------------------------
regfit_bwd.summary
model_bwd_fit = lm(y ~  x3 + x4 + x2 + x5 + x1, data = train_data)
model_bwd_predict = predict(model_bwd_fit, newdata = test_data)
model_bwd_mse = mean((test_data$y - model_bwd_predict)^2)
print(paste("MSE for Linear regression with backward selection of variables = ", model_bwd_mse))

#' 
#' 
#' ## Ridge Regression
#' 
#' Now we repeat the above steps for ridge regression.
#' 
#' First we make training and test matrices and then we scale the data.
#' 
## ---------------------------------------------------------------------------------------
x_train <- as.matrix(train_data[, c('x1', 'x2', 'x3', 'x4', 'x5', 'x6')])
x_test <- as.matrix(test_data[, c('x1', 'x2', 'x3', 'x4', 'x5', 'x6')])
y_train <- train_data$y
y_test <- test_data$y


#' 
#' Now we fit the ridge model and use Cross-validation to find the best value for lambda. Finally, we calculate the MSE for test data.
## ---------------------------------------------------------------------------------------
ridge_model = cv.glmnet(x_train, y_train, alpha = 0)
print(paste("Optimal lambda:", ridge_model$lambda.min))
ridge_predictions <- predict(ridge_model, newx = x_test, s = "lambda.min")
ridge_model_mse <- mean((y_test - ridge_predictions)^2)
print(paste("MSE for Ridge Regression on testing set =", ridge_model_mse))

#' 
#' 
#' ## Lasso Regression
#' 
#' Repeat previous steps for lasso regression.
#' 
## ---------------------------------------------------------------------------------------
lasso_model = cv.glmnet(x_train, y_train, alpha = 1)
print(paste("Optimal lambda:", lasso_model$lambda.min))
lasso_predictions <- predict(lasso_model, newx = x_test, s = "lambda.min")
lasso_model_mse <- mean((y_test - lasso_predictions)^2)
print(paste("MSE for Lasso Regression on testing set =", lasso_model_mse))

#' 
#' 
#' ## Conclusion 
#' 
#' We received a historical data set of real estate valuation are collected from Sindian Dist., New Taipei City, Taiwan.
#' 
#' The inputs are as follows -
#' X1=the transaction date (for example, 2013.250=2013 March, 2013.500=2013 June, etc.)
#' X2=the house age (unit: year)
#' X3=the distance to the nearest MRT station (unit: meter)
#' X4=the number of convenience stores in the living circle on foot (integer)
#' X5=the geographic coordinate, latitude. (unit: degree)
#' X6=the geographic coordinate, longitude. (unit: degree)
#' 
#' The output is as follow
#' Y = house price of unit area (10000 New Taiwan Dollar/Ping, where Ping is a local unit, 1 Ping = 3.3 meter squared)
#' 
#' I read the data from an excel file, changed the column names. Scaling the data had no effect on the results so I assumed the data was scaled already.
#' 
#' On this data set we performed - 
#' 1. multiple linear regression with all inputs
#' 2. multiple linear regression after selecting inputs using forward step-wise selection
#' 3. multiple linear regression after selecting inputs using backward step-wise selection
#' 4. Ridge regression
#' 5. Lasso Regression
#' 
#' For testing the model, I used 3 methods -
#' 1. 70-30 split of training data
#' 2. LOOCV cross-validation
#' 3. k - fold cross-validation with k=5 and 10
#' 
#' **Results - **
#' 
#' Ridge regression gave the best accuracy. Lasso regression was marginally less accurate than ridge. Multiple linear regression after selection of variables(forward and backward methods had identical results) gave far inferior results as compared to the previous two methods. However, it was marginally better than a multiple linear regression model that included all the variables.
#' 
