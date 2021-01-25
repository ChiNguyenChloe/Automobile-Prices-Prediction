library(readxl)
car_data <- read_excel('/Users/chinguyen/Downloads/Car_Data.xlsx')
View(car_data)
attach(car_data)
# convert data fields to factor
cols <- c("Fuel","Colour")
car_data[,cols] <- lapply(car_data[cols],factor) # variable "Fuel" only has 1 level so it will be removed from NN model
str(car_data)
# check missing or valid data
sum(is.na(car_data))
# check outliers
hist(KM)
hist(Age)
hist(HP)
hist(CC)
hist(Wght)
hist(Grs)
hist(G_P)
# check for variables whose variance is too small
vars <- lapply(car_data[c(2,3,5,9:14)],var)
vars # Variances of variable "Drs" and "Cyl" are equal to 0, so these variables will be removed from NN model
# check if variables are highly correlated
cor(car_data[c(-4,-7,-10,-11,-16,-17,-24)])
# data normalization
maxs <- apply(car_data[c(1,2,3,5,9,12:14)],2,max)
maxs
mins <- apply(car_data[c(1,2,3,5,9,12:14)],2,min)
mins
scaled_data <- scale(car_data[c(1,2,3,5,9,12:14)],center = mins, scale = maxs - mins)
summary(scaled_data)
data <- data.frame(scaled_data, car_data[c(6:8,15,18:23,25:28)])
str(data)

# neural network model
set.seed(123)
indx <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[indx == 1, ]
test <- data[indx == 2, ]
library(nnet)
neuralModel <- nnet(Price ~ ., data = train, linout = T, size = 5, maxit = 1000)
summary(neuralModel)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
plot.nnet(neuralModel)
neuralModel$fitted.values
# performance of model on test data
nn.preds <- predict(neuralModel, data = test)
nn.preds
MSE <- mean((nn.preds - test$Price)^2)
MSE

# validate the model using different number of hidden neurons
set.seed(123)
ind <- sample(2,nrow(train), replace = T, prob = c(0.8,0.2))
training <- train[ind == 1, ]
validation_data <- train[ind == 2, ]
# size = 2
nn1 <- nnet(Price ~ ., data = training, linout = T, size = 2, decay = 0.01, maxit = 1000)
nn1.pred <- predict(nn1, data = validation_data)
error1 <- mean((nn1.pred - validation_data$Price)^2)
# size = 3
nn2 <- nnet(Price ~ ., data = training, linout = T, size = 3, decay = 0.01, maxit = 1000)
nn2.pred <- predict(nn2, data = validation_data)
error2 <- mean((nn2.pred - validation_data$Price)^2)
# size = 4
nn3 <- nnet(Price ~ ., data = training, linout = T, size = 4, decay = 0.01, maxit = 1000)
nn3.pred <- predict(nn3, data = validation_data)
error3 <- mean((nn3.pred - validation_data$Price)^2)
# size = 5
nn4 <- nnet(Price ~ ., data = training, linout = T, size = 5, decay = 0.01, maxit = 1000)
nn4.pred <- predict(nn4, data = validation_data)
error4 <- mean((nn4.pred - validation_data$Price)^2)
# size = 6
nn5 <- nnet(Price ~ ., data = training, linout = T, size = 6, decay = 0.01, maxit = 1000)
nn5.pred <- predict(nn5, data = validation_data)
error5 <- mean((nn5.pred - validation_data$Price)^2)

# compare neural networks and linear regression
data <- data[sample(nrow(data)), ]
k <- 10
nmethod <- 2
folds <- cut(seq(1, nrow(data)), breaks = k, labels = FALSE)
model.err <- matrix(-1,k, nmethod, dimnames = list(paste0("Fold", 1:k), c("NeuralModel","LinearModel")))
model.err
for (i in 1:k)
{ 
  testindexes <- which(folds == i, arr.ind = TRUE)
  test_data <- data[testindexes, ]
  train_data <-data[-testindexes, ]
  
  NeuralModel <- nnet(Price ~ ., data = train_data, linout = T, size = 5, maxit = 1000)  
  pred1 <- predict(NeuralModel, data = test_data)  
  model.err[i,1] <- mean((test_data$Price - pred1)^2) 
  
  LinearModel <- lm(Price ~ ., data = train_data)
  pred2 <- predict(LinearModel, data = test_data)
  model.err[i,2] <- mean((test_data$Price - pred2)^2)
}
mean(model.err[,1])
mean(model.err[,2])
