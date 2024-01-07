#' ---
#' title: "harinris_HW6_ex"
#' author: "Harin Rishabh"
#' date: "2023-12-10"
#' output: html_document
#' ---
#' 
#' **2 a)**\
#' Answer -\
#' The equation in the question can be rewritten as below. This is the equation of a circle.
#' \begin{align*}
#' (1+X1)^2 + (2−X2)^2 = 4
#' => (X1−(−1))^2 + (X2−2)^2 = 2^2
#' \end{align*}
#' 
#' Plotting the circle-
## ---------------------------------------------------------------------------------------------------------------------------------------------
h = -1
k = 2
r = 2

theta <- seq(0, 2*pi, length.out = 100)
x <- h + r * cos(theta)
y <- k + r * sin(theta)

plot(x, y, type = "l", xlab = "X-axis", ylab = "Y-axis", main = "Circle Plot")
points(h, k, col = "blue", pch = 16)

#' 
#' 
#' **3 a) **\
#' Answer -\
## ---------------------------------------------------------------------------------------------------------------------------------------------
X1 = c(3, 2, 4, 1, 2, 4, 4)
X2 = c(4, 2, 4, 4, 1, 3, 1)
Y = c("Red", "Red", "Red", "Red", "Blue", "Blue", "Blue")
df <- data.frame(X1, X2, Y)
plot(df[ , c(1,2)], col=df$Y)

#' 
#' **3 b) **\
#' Answer - On inspecting the graph above, we can see that the best line would be $X2 = X1 + (-0.5)$
## ---------------------------------------------------------------------------------------------------------------------------------------------
plot(df[ , c(1,2)], col=df$Y)
abline(-0.5, 1)

#' 
#' **3 c) **\
#' Answer - Since the optimal separating hyperplane is given by the equation $X2 = X1 + (-0.5)$, from the plot above we can say that the prediction for the maximal margin classifier will be Red when $X2 − X1 + 0.5 > 0$ and blue otherwise. Therefore the values for β0, β1, β2 are 0.5, -1, 1.
#' 
#' **3 d) **\
#' Answer -\
## ---------------------------------------------------------------------------------------------------------------------------------------------
plot(df[ , c(1,2)], col=df$Y)
abline(-0.5, 1)
abline(0, 1, lty=2)
abline(-1, 1, lty=2)

#' 
#' **3 e) **\
#' Answer - The support vectors are observations - 2, 3, 5 and 6.
#' 
#' **3 f) **\
#' Answer - The 7th observation is not a support vector. Moving it will not have any affect on the maximal margin hyperplane.
#' 
#' **3 g) **\
#' On plotting the line equation, $X2 = X1 + (-0.75)$, we can see that even though all the red and blue observations are separated, the boundary is not optimal.
## ---------------------------------------------------------------------------------------------------------------------------------------------
plot(df[ , c(1,2)], col=df$Y)
abline(-0.75, 1)

#' 
#' **3 h) **\
#' Answer - Adding a point (3, 1.5) makes the 2 classes inseparable.
## ---------------------------------------------------------------------------------------------------------------------------------------------
plot(df[ , c(1,2)], col=df$Y)
abline(-0.5, 1)
abline(0, 1, lty=2)
abline(-1, 1, lty=2)
points(3, 1.5, col="red")

#' 
#' **5 a) **\
#' Answer-\
## ---------------------------------------------------------------------------------------------------------------------------------------------
x1=runif(500)-0.5
x2=runif(500)-0.5
y=1*(x1^2-x2^2 > 0)

#' 
#' **5 b) **\
## ---------------------------------------------------------------------------------------------------------------------------------------------
plot(x1,x2,col=ifelse(y,'red','blue'))

#' 
#' **5 c) **\
#' Answer - \
## ---------------------------------------------------------------------------------------------------------------------------------------------
df = data.frame(x1,x2,y)
df.fit = glm(y~. , df, family='binomial')
df.fit

#' 
#' **5 d) **\
#' Answer - From the plot below we can see that the decision boundary is linear.
## ---------------------------------------------------------------------------------------------------------------------------------------------
df.pred=predict(df.fit, data.frame(x1,x2))
plot(x = x1, y = x2, 

     col = ifelse(df.pred > 0, 'red', 'blue'), 

     pch = ifelse(as.integer(df.pred > 0) == y, 1, 4)
)

#' 
#' **5 e) **\
#' Answer -\
## ---------------------------------------------------------------------------------------------------------------------------------------------
glm.fit = glm(y~poly(x1,2)+poly(x2,2), df, family='binomial')

#' 
#' **5 f) **\
#' Answer -  From the graph below, we can see that the decision boundary is clearly non-linear. All observations have been classified correctly.
## ---------------------------------------------------------------------------------------------------------------------------------------------
glm.pred=predict(glm.fit,data.frame(x1,x2))     # returns the log-odds.
plot(x = x1, y = x2, 

     col = ifelse(glm.pred > 0, 'red', 'blue'), 

     pch = ifelse(as.integer(glm.pred > 0) == y, 1, 4)
)

#' 
#' **5 g) **\
#' Answer-\
## ---------------------------------------------------------------------------------------------------------------------------------------------
library(e1071)
svm.fit <- svm(y ~ x1 + x2, data = data.frame(x1,x2,y=as.factor(y)), kernel = "linear", scale = FALSE, cost = 0.001)
svm.pred <- predict(svm.fit, data.frame(x1,x2), type='response')
plot(x1,x2,
     
     col=ifelse(svm.pred == 1,'red','blue'),
     
     pch=ifelse(svm.pred == y,1,4))

#' 
#' **5 h) **\
#' Answer-\
## ---------------------------------------------------------------------------------------------------------------------------------------------
svm.fit <- svm(y ~ x1 + x2, data = data.frame(x1,x2,y=as.factor(y)), kernel = "polynomial", scale = FALSE, cost = 0.001)
svm.pred <- predict(svm.fit, data.frame(x1,x2), type='response')
plot(x1,x2,
     
     col=ifelse(svm.pred == 1,'red','blue'),
     
     pch=ifelse(svm.pred == y,1,4))

#' 
#' **5 i) **\
#' Answer - Clearly the polynomial logarithmic model is the most accurate. It had zero misclassifications. This is not surprising because we knew the equation before-hand and generated the data artificially.
#' 
#' 
#' **8 a) **\
#' Answer - 
## ---------------------------------------------------------------------------------------------------------------------------------------------
library(ISLR2)
attach(OJ)
data = OJ
indices = sample(1:nrow(data), size = 0.8 * nrow(data))
train_data <- data[indices, ]
test_data <- data[-indices, ]

#' 
#' **8 b) **\
#' Answer - From the summary below, we can see that there are 467 support vectors. These are almost evenly distributed between the classifications.
## ---------------------------------------------------------------------------------------------------------------------------------------------
svm.fit = svm(Purchase~., data=train_data, cost=0.01, kernel='linear')
summary(svm.fit)

#' 
#' **8 c) **\
#' Answer -
## ---------------------------------------------------------------------------------------------------------------------------------------------
svm.pred=predict(svm.fit,train_data)
print(paste("Train error rate = ", mean(predict(svm.fit, train_data) != train_data$Purchase)))
print(paste("Test error rate = ", mean(predict(svm.fit, test_data) != test_data$Purchase)))

#' 
#' **8 d) **\
#' Answer - 
## ---------------------------------------------------------------------------------------------------------------------------------------------
svm.tune=tune(svm, Purchase~. ,data=train_data, ranges=data.frame(cost=seq(0.01,10)), kernel='linear')
summary(svm.tune)

#' 
#' **8 e) **\
#' Answer -
## ---------------------------------------------------------------------------------------------------------------------------------------------
svm.pred.best = predict(svm.tune$best.model, train_data)
print(paste("Train error rate = ", mean(predict(svm.tune$best.model, train_data) != train_data$Purchase)))
print(paste("Test error rate = ", mean(predict(svm.tune$best.model, test_data) != test_data$Purchase)))

#' 
#' **8 f) **\
#' Answer -
## ---------------------------------------------------------------------------------------------------------------------------------------------
svm.fit = svm(Purchase~., data=train_data, cost=0.01, kernel='radial')
svm.pred=predict(svm.fit,train_data)
print(paste("Train error rate with radial kernel = ", mean(predict(svm.fit, train_data) != train_data$Purchase)))
print(paste("Test error rate with radial kernel = ", mean(predict(svm.fit, test_data) != test_data$Purchase)))
svm.tune=tune(svm, Purchase~. ,data=train_data, ranges=data.frame(cost=seq(0.01,10)), kernel='radial')
svm.pred.best = predict(svm.tune$best.model, train_data)
print(paste("Train error rate with radial kernel and tuning = ", mean(predict(svm.tune$best.model, train_data) != train_data$Purchase)))
print(paste("Test error rate with radial kernel and tuning = ", mean(predict(svm.tune$best.model, test_data) != test_data$Purchase)))

#' 
#' **8 g) **\
#' Answer -
## ---------------------------------------------------------------------------------------------------------------------------------------------
svm.fit = svm(Purchase~., data=train_data, cost=0.01, kernel='polynomial', degree=2)
svm.pred=predict(svm.fit,train_data)
print(paste("Train error rate with polynomial kernel = ", mean(predict(svm.fit, train_data) != train_data$Purchase)))
print(paste("Test error rate with polynomial kernel = ", mean(predict(svm.fit, test_data) != test_data$Purchase)))
svm.tune=tune(svm, Purchase~. ,data=train_data, ranges=data.frame(cost=seq(0.01,10)), kernel='polynomial', degree=2)
svm.pred.best = predict(svm.tune$best.model, train_data)
print(paste("Train error rate with polynomial kernel and tuning = ", mean(predict(svm.tune$best.model, train_data) != train_data$Purchase)))
print(paste("Test error rate with polynomial kernel and tuning = ", mean(predict(svm.tune$best.model, test_data) != test_data$Purchase)))

#' 
#' **8 h) **\
#' Answer - The performances of all 3 models are very similar after using cross-validation but we should go ahead with linear SVM on this data set as it has the least test error rate.
