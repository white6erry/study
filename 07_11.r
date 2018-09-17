#perceptron
set.seed(1234)
x1 <- runif(30, -1,1)
x2 <- runif(30,-1,1)
x <- cbind(x1, x2)
y <- ifelse(x2>0.5+x1,+1,-1) #x2>0.5+x1 크면 +1 작으면 -1

plot(x, pch=ifelse(y>0, "+","-"), xlim = c(-1, 1), ylim = c(-1,1), cex=2) # cex is zoom
abline(0.5,1)

#거리값 계산 함수
calculate_distance = function(x,w,b){
  sum(x*w) + b
}

#선형 분류기 모델
linear_classifier <- function(x,w,b){
  distances = apply(x, 1, calculate_distance, w, b)
  return(ifelse(distances < 0, -1, +1))
}

# distances = apply(x, 1, calculate_distance, c(-1,1)/sqrt(2), -sqrt(2)/4)
#distances = apply(x, 1, calculate_distance, 0.5,1)
(distances = apply(a, 1, calculate_distance, c(-1,1)/sqrt(2), -sqrt(2)/4))

# 선형 분류기 테스트
linear_classifier(x,1,0)
linear_classifier(x,0,1)

linear_classifier(x,1,0.5)
second_norm <- function(x){sqrt(sum(x*x))}
f<-function(x){
 return(x+100)
}

#perceptron 훈련알고리즘
# 신경망을 이용하여 c(-1,1)/sqrt(2), -sqrt(2)/4) 값을 찾아감
perceptron <- function(x,y, learning_rate=1){
  w = vector(length = ncol(x)); b=0; k=0
  R= max(apply(x,1,second_norm))
  incorrect = TRUE
  plot(x, cex=0.2)
  while(incorrect){
    incorrect = FALSE
    yc <- linear_classifier(x,w,b)
    for(i in 1:nrow(x)){
      if(y[i] != yc[i]){
        w <- w + learning_rate * y[i]*x[i,]
        b <- b + learning_rate * y[i]*R^2
        k <- k +1
        if(k%%5 == 0){
          intercept <- -b/w[[2]]
          slope <- -w[[1]]/w[[2]]
          abline(intercept, slope,col="red")
          cat("반복 # ",k,"\n")
          cat("Press[enter] to continue")
          line <- readline()
        }
        incorrect = TRUE
      }
    }}
  s= second_norm(w)
  return(list(w=w/s, b=b/s,updates=k))
}
(p <- perceptron(x,y)) #perceptron을 호출하여 훈련시작
(y <- linear_classifier(x,p$w,p$b)) #분류기로 확인
plot(x, cex=0.2)
points(subset(x,Y==1), col="black",pch="+", cex=2)
points(subset(x,Y==-1), col="red", pch="-". cex=2)
intercept <- -p$b /p$w[[2]]
slop <- -p$w[[1]]/ p$w[[2]]
abline(intercept,slope, col="green")

library(nnet)
str(iris)
m<- nnet(Species ~., data =iris, size=3)
predict(m, newdata = iris)

class.ind(iris$Species)
m2 <- nnet(iris[, 1:4], class.ind(iris$Species), size = 3, softmax=TRUE, linout=T)
species_result = predict(m2, newdata =iris[, 1:4], type="class")
predt <- table(species_result)
predt
predt/sum(predt)

library(devtools)
library(nnet)
library(devtools)
library(mlbench)
library(Matrix)

data(Glass, package="mlbench")
head(Glass)
n_newg <- nnet(Type~., data= Glass, size=25)
length(Glass[1,])
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r') 
plot.nnet(n_newg)
(gpred <- predict(n_newg, newdata = Glass, type="class"))
predt <- table(Glass$Type, gpred)
predt
predt/sum(predt)

# neuralnet
concrete <- read.csv("concrete.csv")
str(concrete)
normalize<- function(x){
  return((x-min(x))/ (max(x)-min(x)))
}

concrete_norm <- as.data.frame(lapply(concrete, normalize))
str(concrete_norm)
summary(concrete_norm$strength)
summary(concrete$strength)

concrete_train <- concrete_norm[1:773,]
concrete_test <- concrete_norm[774:1030,]
install.packages("neuralnet")
library(neuralnet)
# 히든 레이어의 개수 c(5,3)

concrete_model <- neuralnet(formula = strength ~ cement + slag + ash + water + superplastic+ coarseagg + fineagg +age,
                            data = concrete_train, hidden = c(5,3))

plot(concrete_model)
model_results <- compute(concrete_model, concrete_test[1:8])
model_results$neurons
predicted_strength <- model_results$net.result

cor(predicted_strength, concrete_test$strength)

#
data= read.csv("C:\\Users\\acorn\\Downloads\\데이터\\cereals.csv", header=T)
samplesize= 0.60 * nrow(data)
set.seed(80)
index = sample(seq_len(nrow(data)), size=samplesize)
datatrain = data[index,]
datatest = data[-index,]

max = apply(data, 2,max)
min = apply(data,2, min)
scaled = as.data.frame(scale(data, center = min, scale=max- min))

trainNN = scaled[index,]
testNN = scaled[-index,]
set.seed(2)
NN = neuralnet(rating~calories + protein + fat + sodium + fiber, trainNN,
               hidden = 3, linear.output = T)
plot(NN)

predict_testNN = compute(NN, testNN[,c(1:5)])

#예측된 값을 스케일 복원
predict_testNN =(predict_testNN$net.result * (max(data$rating)- min(data$rating))) + min(data$rating)
plot(datatest$rating, predict_testNN, col="blue", pch=16, ylab="예측 rating", xlab="관측 rating")
abline(0,1)
#Rppt mean square error(RMSE)값을 이용하여 판단
RMSE.NN <= (sum((datatest$rating - predict_testNN)^2/ nrow(datatest))^0.5

library(boot)
library(plyr)
library(matrixStats)

set.seed(50)
k = 100
RMSE.NN = NULL

List = list( )

# Fit neural network model within nested for loop
for(j in 10:65){
  for (i in 1:k) {
    index = sample(1:nrow(data),j )
    
    trainNN = scaled[index,]
    testNN = scaled[-index,]
    datatest = data[-index,]
    
    NN = neuralnet(rating ~ calories + protein + fat + sodium + fiber, trainNN, hidden = 3, linear.output= T)
    predict_testNN = compute(NN,testNN[,c(1:5)])
    predict_testNN = (predict_testNN$net.result*(max(data$rating)-min(data$rating)))+min(data$rating)
    
    RMSE.NN [i]<- (sum((datatest$rating - predict_testNN)^2)/nrow(datatest))^0.5
  }
  List[[j]] = RMSE.NN
}

Matrix.RMSE = do.call(cbind, List)
med = colMedians(Matrix.RMSE)
X = seq(10,65)
plot (med~X, type = "l", xlab = "length of training set", ylab = "median RMSE", main = "Variation of RMSE with length of training set")

# 데이터에서 medv 주택가격 중앙값을 예측하는 신경망을 모델화하시오(회귀분석)
library(MASS)
DataFrame <- Boston
set.seed(123)
str(DataFrame)
maxValue <- apply(DataFrame, 2, max)
minValue <- apply(DataFrame, 2, min)
medv_maxValue <- maxValue["medv"]
medv_minValue <- minValue["medv"]

DataFrame <- as.data.frame(scale(DataFrame, center=minValue,
                                 scale = maxValue))


summary(DataFrame)
idx <- sample(1: nrow(DataFrame), 400)
trainDF <- DataFrame[idx,]
testDF <- DataFrame[-idx,]
allVars <- colnames(DataFrame)

predictorVars <- allVars[!allVars%in%"medv"]  # medv를 제외한 여타 변수를 취하는 방법
predictorVars <- paste(predictorVars, collapse = "+")
form <- as.formula(paste("medv~", predictorVars, collapse = "+"))
form

neuralModel <- neuralnet(formula = form, hidden = c(4,2), linear.output = T, data=trainDF)
plot(neuralModel)

predictions <- compute(neuralModel, testDF[,1:13])
str(predictions)
predictions$net.result
cor(predictions$net.result, testDF$medv)

# 예측값 원래의 값의 범위로 복원
predictions$net.result[2] * (medv_maxValue-medv_minValue) + medv_minValue

# RNNS
install.packages("RSNNS")
library(RSNNS)
data(iris)

#shuffle the vector
iris <- iris[sample(1:nrow(iris),length(1:nrow(iris))),1:ncol(iris)]

irisValues <- iris[,1:4]
irisTargets <- decodeClassLabels(iris[,5])
#irisTargets <- decodeClassLabels(iris[,5], valTrue=0.9, valFalse=0.1)

iris <- splitForTrainingAndTest(irisValues, irisTargets, ratio=0.15)
iris <- normTrainingAndTestSet(iris)

model <- mlp(iris$inputsTrain, iris$targetsTrain, size=5, learnFuncParams=c(0.1), 
              maxit=50, inputsTest=iris$inputsTest, targetsTest=iris$targetsTest)

summary(model)
model
weightMatrix(model)
extractNetInfo(model)

par(mfrow=c(2,2))
plotIterativeError(model)

predictions <- predict(model,iris$inputsTest)

plotRegressionError(predictions[,2], iris$targetsTest[,2])

confusionMatrix(iris$targetsTrain,fitted.values(model))
confusionMatrix(iris$targetsTest,predictions)

plotROC(fitted.values(model)[,2], iris$targetsTrain[,2])
plotROC(predictions[,2], iris$targetsTest[,2])

#confusion matrix with 402040-method
confusionMatrix(iris$targetsTrain, encodeClassLabels(fitted.values(model),
                                                       method="402040", l=0.4, h=0.6))

# SVM
# 초평면을 찾아서 optimization을 구현한 판별 모델
# 모델 중 유일하게 고차원을 이용한 방법이고 XOR 문제를 고차원으로의 데이터 변환을 이용해 해결
# gamma, cost인 hyper parameter를 이용하여 모델의 정확도를 조절함. 
# 예측과 분류기로 작동 

library(e1071)
data(cats, package ="MASS")
str(cats)
m<- svm(Sex~., data = cats)
plot(m, cats)
plot(m, cats, svSymbol=1,  dataSymbol = 2, symbolPalette = rainbow(4),
     color.palette = terrain.colors)


data(iris)
m2 <- svm(Species~., data = iris)
plot(m2, iris, Petal.Width ~Petal.Length,
     slice = list(Sepal.Width=3, Sepal.Length =4))
	 
#type 설정
# C- classification
# nu - classification
# one- classification(for novelty detection)
# eps - regression
# nu - regression

data(iris)
set.seed(415)
idx= sample(1:nrow(iris), 0.7*nrow(iris))
training = iris[idx,]
testing = iris[-idx,]
training
testing
dim(training)
dim(testing)
model_svm = svm(Species ~., data = training, na.action = na.omit)
summary(model_svm)
pred <- predict(model_svm, testing)
(res = table(pred, testing$Species))
sum(res[1,1]+res[2,2]+res[3,3])/ nrow(testing)
sum(diag(res)) / sum(res)
plot(model_svm, iris, Petal.Width ~ Petal.Length,
    slice = list(Sepal.Width = 3, Sepal.Length = 4))
	
tuning <- tune.svm(Species~., data = training, gamma=10^(-5:1), cost = 10^(-10:3))
tuning
attributes(tuning)
tuning$best.parameters
tuning$best.parameters[,"gamma"]
tuning$best.parameters[,"cost"]
model_svm2 = svm(Species~., data=training, gamma=0.1, cost=100, na.action= na.omit)
summary(model_svm2)
pred2 <- predict(model_svm2, testing)
res = table(pred2, testing$Species) ; res
sum(res[1,1]+res[2,2]+res[3,3]) / nrow(testing)

library(e1071)
library(rpart)
data(Ozone, package="mlbench")

## split data into a train and test set
index     <- 1:nrow(Ozone)
testindex <- sample(index, trunc(length(index)/3))
testset   <- na.omit(Ozone[testindex,-3])
trainset  <- na.omit(Ozone[-testindex,-3])

## svm
svm.model <- svm(V4 ~ ., data = trainset, cost = 1000, gamma = 0.0001)
svm.pred  <- predict(svm.model, testset[,-3])
crossprod(svm.pred - testset[,3]) / length(testindex)

## rpart
rpart.model <- rpart(V4 ~ ., data = trainset)
rpart.pred  <- predict(rpart.model, testset[,-3])
crossprod(rpart.pred - testset[,3]) / length(testindex)

###################################################
### code chunk number 7: svmdoc.Rnw:248-271
###################################################
rp.res <- c()
sv.res <- c()
reps <- 10
for (i in 1:reps) {
  ## split data into a train and test set
  index     <- 1:nrow(Ozone)
  testindex <- sample(index, trunc(length(index)/3))
  testset   <- na.omit(Ozone[testindex,-3])
  trainset  <- na.omit(Ozone[-testindex,-3])
  
  ## svm
  svm.model <- svm(V4 ~ ., data = trainset, cost = 1000, gamma = 0.0001)
  svm.pred  <- predict(svm.model, testset[,-3])
  sv.res[i] <- crossprod(svm.pred - testset[,3]) / length(testindex)
  
  ## rpart
  rpart.model <- rpart(V4 ~ ., data = trainset)
  rpart.pred  <- predict(rpart.model, testset[,-3])
  rp.res[i] <- crossprod(rpart.pred - testset[,3]) / length(testindex)
}
xtable(rbind(svm = summary(sv.res), rpart = summary(rp.res)), 
       label = "tab:reg", caption = "Performance of \\texttt{svm()} and\
       \\texttt{rpart()} for regression (Mean Squared Error, 10 replications)")

